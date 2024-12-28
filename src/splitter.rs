use anyhow::{anyhow, Context as _, Result};
use clap::Parser as ClapParser;
use itertools::Itertools;
use refactor::{file_editor::*, scip_index::*};
use std::{io::Write, iter, path::PathBuf, str::FromStr, sync::Arc};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node, Parser, Query, QueryCursor};

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the directory or file to refactor
    #[clap(value_parser)]
    path: String,

    /// Perform a dry run (show potential changes without applying them)
    #[clap(long, short = 'd', action)]
    dry: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut refactor = Refactor::new(&std::fs::canonicalize(&args.path)?)?;
    refactor.process();
    if args.dry {
        refactor.editor.display_dry_run_results();
    } else {
        refactor.editor.apply_edits()?;
    }
    Ok(())
}

struct Refactor {
    editor: FileEditor,
    index: Index,
    function_type_query: Query,
    imports_query: Query,
}

impl Refactor {
    fn new(path: &PathBuf) -> Result<Self> {
        let language = tree_sitter_rust::LANGUAGE.into();
        let mut parser = Parser::new();
        parser
            .set_language(&language)
            .context("Failed to set language for parser")?;

        let index_path = Index::find(path)?;
        let root_folder = index_path.parent().unwrap().to_path_buf();
        let index = Index::load(&index_path)?;

        let editor = FileEditor::new(parser, root_folder.clone());

        let function_type_query = Query::new(&language, "(function_type) @function_type").unwrap();
        let imports_query = Query::new(&language, include_str!("./imports.scm")).unwrap();

        Ok(Self {
            editor,
            index,
            function_type_query,
            imports_query,
        })
    }

    fn process(&mut self) {
        let locals = self.all_window_context_locals();

        // display_locals(&signature_to_locals)

        let locals_len = locals.len();
        for (local_ix, local_info) in locals.into_iter().enumerate() {
            let signature: String = local_info.type_signature.to_string();
            if signature != "&mut WindowContext"
                && signature != "&mut WindowContext<'a>"
                && signature != "&mut WindowContext<'_>"
                && signature != "&WindowContext<'_>"
            {
                println!("ERROR: Unexpected type_signature in {local_info:?}");
                continue;
            }

            let relative_path = &local_info.id.relative_path;
            let mut file = self.editor.file(relative_path).unwrap();
            let document = self.index.document(relative_path).unwrap();

            let cx_name = local_info.name;
            let mut window_name = match cx_name.to_string().as_str() {
                "cx" => "window",
                "_cx" => "_window",
                name => {
                    file.record_error(
                        local_info.definition.range.start.row,
                        format!("Unexpected name: {name}"),
                    );
                    continue;
                }
            };

            let mut used_window = false;
            for reference in local_info.references.iter() {
                match Self::split_reference(
                    &reference,
                    &window_name,
                    &cx_name,
                    &self.function_type_query,
                    &mut file,
                    &document,
                    &self.index,
                ) {
                    Ok(reference_used_window) => {
                        used_window |= reference_used_window;
                    }
                    Err(e) => {
                        file.record_error(reference.range.start.row, format!("{e}"));
                    }
                }
            }

            if !used_window {
                window_name = "_window";
            }
            match Self::split_parameter(&local_info.definition, &window_name, &cx_name, &mut file) {
                Ok(names) => names,
                Err(e) => {
                    file.record_error(local_info.definition.range.start.row, format!("{e}"));
                    continue;
                }
            };
            show_progress(
                "Staging local variable splitting edits:",
                local_ix,
                locals_len,
                "variables",
            );
        }

        let window_context_symbol = Symbol::Global(GlobalSymbol(
            "rust-analyzer cargo gpui 0.1.0 window/WindowContext#".into(),
        ));

        for (document_ix, document) in self.index.documents.values().enumerate() {
            let file = self.editor.file(&document.relative_path).unwrap();
            for occurrence in document.occurrences.iter() {
                if occurrence.symbol != window_context_symbol {
                    continue;
                }
                match Self::handle_window_context_mention(&occurrence, file) {
                    Ok(()) => {}
                    Err(e) => {
                        file.record_error(occurrence.range.start.row, format!("{e}"));
                        continue;
                    }
                };
            }
            show_progress(
                "Staging edits for other WindowContext mentions:",
                document_ix,
                self.index.documents.len(),
                "files",
            );
        }

        let files_len = self.editor.files.len();
        for (file_ix, file) in self.editor.files.values_mut().enumerate() {
            if file.edits.borrow().is_empty() {
                continue;
            }
            Self::stage_import_edits(file, &self.imports_query);
            show_progress("Staging import edits:", file_ix, files_len, "files");
        }
        println!();
    }

    fn split_parameter(
        parameter: &Occurrence,
        window: &str,
        cx: &str,
        file: &mut File,
    ) -> Result<()> {
        let node = file.find_node(&parameter.range)?;
        let parent0 = node.parent().unwrap();
        let parent1 = parent0.parent().unwrap();

        let edit_closure_parameter =
            || file.record_node_replacement(node, format!("{window}, {cx}"));
        let edit_fn_parameter = || {
            let replacement = match extract_type_signature(file.node_text(parent0))?.as_str() {
                "&WindowContext" => format!("{window}: &Window, {cx}: &AppContext"),
                "&WindowContext<'_>" => format!("{window}: &'_ Window, {cx}: &'_ AppContext"),
                "&mut WindowContext" => format!("{window}: &mut Window, {cx}: &mut AppContext"),
                "&mut crate::WindowContext" => {
                    format!("{window}: &mut crate::Window, {cx}: &mut crate::AppContext")
                }
                "&mut WindowContext<'_>" => {
                    format!("{window}: &'_ mut Window, {cx}: &'_ mut AppContext")
                }
                "&'a WindowContext" => format!("{window}: &'a Window, {cx}: &'a AppContext"),
                ty => return Err(anyhow!("Unexpected type {}", ty)),
            };
            file.record_node_replacement(parent0, replacement);
            Ok(())
        };

        match (parent0.grammar_name(), parent1.grammar_name()) {
            ("closure_parameters", _) => edit_closure_parameter(),
            ("parameter", _) => edit_fn_parameter()?,
            // In macros invocations, treat it like a fn parameter if it looks like an fn parameter.
            ("token_tree", _) => match edit_fn_parameter() {
                Err(_) => edit_closure_parameter(),
                Ok(()) => {}
            },
            (parent0, parent1) => {
                return Err(anyhow!("Unexpected parameter parents: {parent0} {parent1}"))
            }
        }

        Ok(())
    }

    fn split_reference(
        reference: &Occurrence,
        window: &str,
        cx: &str,
        function_type_query: &Query,
        file: &mut File,
        document: &Document,
        index: &Index,
    ) -> Result<bool> {
        let node = file.find_node(&reference.range)?;
        let parent0 = node.parent().unwrap();
        let parent1 = parent0.parent().unwrap();
        let parent2 = parent1.parent().unwrap();
        match (
            parent0.grammar_name(),
            parent1.grammar_name(),
            parent2.grammar_name(),
        ) {
            // ...method(.. cx ..)
            ("arguments", "call_expression", _) => {
                let argument_index = index_in_parent_named_children(node);
                let function_identifier = call_expression_function_identifier(parent1)?;
                let function_symbol = document
                    .find_occurrence(&function_identifier.start_position())?
                    .symbol
                    .clone();
                let (is_window_context, is_app_context) = match function_symbol.text() {
                    "rust-analyzer cargo ui 0.1.0 styles/spacing/impl#[DynamicSpacing]px()."
                    | "rust-analyzer cargo ui 0.1.0 styles/spacing/impl#[DynamicSpacing]rems()." => {
                        (true, false)
                    }
                    _ => {
                        let function_info = match document.lookup_symbol(&function_symbol, &index) {
                            Some(symbol_info) => symbol_info,
                            None => {
                                return Err(anyhow::anyhow!(
                                    "Missing symbol info for {function_symbol:?}"
                                ))
                            }
                        };
                        let signature = function_info.signature();
                        let (parsed_signature_tree, signature) = parse_signature(signature)?;
                        let parsed_signature = parsed_signature_tree.root_node().child(0).unwrap();
                        let parameters =
                            signature_parameters(parsed_signature, &signature, function_type_query)
                                .with_context(|| {
                                    format!("Parsing function signature {signature}")
                                })?;

                        let parameter_index = parameter_index_from_argument_index(
                            parameters,
                            function_identifier,
                            argument_index,
                        );
                        let parameter =
                            parameters.named_child(parameter_index).with_context(|| {
                                format!("Getting argument {parameter_index} of {signature}")
                            })?;
                        let parameter_text = &signature[parameter.byte_range()];
                        let is_window_context = parameter_text.contains("WindowContext");
                        let is_app_context = parameter_text.contains("AppContext")
                            || parameter_text.ends_with("&mut C")
                            || parameter_text.ends_with("&C");
                        if !is_window_context && !is_app_context {
                            return
                            Err(anyhow!(
                                "Parameter {} contains neither WindowContext or AppContext. Signature: {}",
                                parameter_text,
                                signature
                            ));
                        }
                        (is_window_context, is_app_context)
                    }
                };

                if is_window_context {
                    file.record_node_replacement(node, format!("{window}, {cx}"));
                    Ok(true)
                } else if is_app_context {
                    Ok(false)
                } else {
                    panic!("Impossible")
                }
            }
            // cx.method(...)
            ("field_expression", "call_expression", _)
            | ("field_expression", "generic_function", _) => {
                let method_node = parent0.child_by_field_name("field").unwrap();
                let method_symbol = document
                    .find_occurrence(&method_node.start_position())?
                    .symbol
                    .clone();

                let method_symbol_text = method_symbol.text();
                let is_window_context = method_symbol_text.contains("WindowContext");
                let is_app_context = method_symbol_text.contains("AppContext");
                if is_window_context && is_app_context {
                    return Err(anyhow!(
                        "Both WindowContext and AppContext in method symbol: {:?}",
                        method_symbol_text
                    ));
                }

                if is_window_context {
                    let method_info = document.lookup_symbol(&method_symbol, &index).unwrap();
                    let signature = method_info.signature();
                    let (parsed_signature_tree, signature) = parse_signature(signature)?;
                    let parameter_index = index_of_first_function_type_parameter(
                        parsed_signature_tree.root_node(),
                        &signature,
                        &function_type_query,
                    );
                    let arguments = node_ancestors(node)
                        .find(|ancestor| ancestor.grammar_name() == "call_expression")
                        .unwrap()
                        .child_by_field_name("arguments")
                        .unwrap();
                    let method = file.node_text(method_node);
                    file.record_node_replacement(parent0, format!("{window}.{method}"));
                    if let Some((_parameters, parameter_index)) = parameter_index {
                        file.record_insertion_before_node(
                            // -1 for self param.
                            arguments.named_child(parameter_index - 1).unwrap(),
                            format!("cx, "),
                        );
                    } else {
                        let replacement = if arguments.named_child_count() == 0 {
                            format!("{cx}")
                        } else {
                            format!(", {cx}")
                        };
                        file.record_insertion_before_node(
                            last_child(arguments).unwrap(),
                            replacement,
                        );
                    }
                    Ok(true)
                } else if is_app_context {
                    Ok(false)
                } else {
                    return Err(anyhow!(
                        "Method symbol is neither AppContext nor WindowContext {:?}",
                        method_symbol
                    ));
                }
            }
            ("field_expression", _, _)
                if parent0.named_child(1).map(|field| file.node_text(field)) == Some("window") =>
            {
                file.record_node_replacement(parent1, window.to_string());
                Ok(true)
            }
            ("token_tree", _, _) => {
                // Could probably support this by separately parsing the code within the macro
                // invocation, and then converting ranges to/from for queries, but that would be a
                // lot of work.
                //
                // FIXME
                // return Err(anyhow!("Can't refactor inside macro invocations."));
                Ok(false)
            }
            (parent0, parent1, parent2) => {
                return Err(anyhow!(
                    "Unexpected reference parents: {parent0} {parent1} {parent2}"
                ))
            }
        }
    }

    fn all_window_context_locals(&self) -> Vec<LocalInfo> {
        let mut results: Vec<LocalInfo> = Vec::new();
        for document in self.index.documents.values() {
            if document.relative_path.0
                == PathBuf::from_str("crates/gpui/src/window.rs")
                    .unwrap()
                    .into()
            {
                continue;
            }
            for (symbol, symbol_info) in document.locals.iter() {
                let local_info = match LocalInfo::compute(
                    document,
                    symbol,
                    symbol_info,
                    is_window_context_signature,
                ) {
                    Ok(Some(local_info)) => local_info,
                    Ok(None) => continue,
                    Err(e) => {
                        eprintln!("ERROR: {e:?}");
                        continue;
                    }
                };
                results.push(local_info)
            }
        }
        results
    }

    fn handle_window_context_mention(occurrence: &Occurrence, file: &mut File) -> Result<()> {
        let node = file.find_node(&occurrence.range)?;

        if let Some(_) =
            node_ancestors(node).find(|ancestor| ancestor.grammar_name() == "function_type")
        {
            let parameter = iter::once(node)
                .chain(node_ancestors(node))
                .zip(node_ancestors(node))
                .find(|(_, ancestor)| ancestor.grammar_name() == "parameters")
                .unwrap()
                .0;
            let replacement = match file.node_text(parameter) {
                "&WindowContext" => "&Window, &AppContext",
                "&mut WindowContext" => "&mut Window, &mut AppContext",
                "&mut WindowContext<'_>" => "&'_ mut Window, &'_ mut AppContext",
                reference_text => {
                    return Err(anyhow!(
                        "Unexpected function type parameter {reference_text}"
                    ));
                }
            };
            file.record_node_replacement(parameter, replacement.to_string());
            return Ok(());
        }

        let parent0 = node.parent().unwrap();
        let parent1 = parent0.parent().unwrap();
        let parent2 = parent1.parent();
        let parameter = match (
            parent0.grammar_name(),
            parent1.grammar_name(),
            parent2.map(|parent2| parent2.grammar_name()),
        ) {
            ("reference_type", "parameter", _) => Some(parent1),
            ("generic_type", "reference_type", Some("parameter")) => Some(parent2.unwrap()),
            _ => None,
        };

        if let Some(parameter) = parameter {
            if parameter
                .child_by_field_name("pattern")
                .unwrap()
                .grammar_name()
                == "_"
            {
                let replacement = match file.node_text(parent1) {
                    "_: &WindowContext" => "_: &Window, _: &AppContext",
                    "_: &mut WindowContext" => "_: &mut Window, _: &mut AppContext",
                    parameter_text => {
                        return Err(anyhow!("Unexpected _: parameter {parameter_text}"));
                    }
                };
                file.record_node_replacement(parent1, replacement.to_string());
                return Ok(());
            }
        }

        Ok(())
    }

    fn stage_import_edits(file: &mut File, imports_query: &Query) {
        let root_node = file.tree.root_node();
        let source = file.text();
        let mut query_cursor = QueryCursor::new();
        let mut matches = query_cursor.matches(&imports_query, root_node, source.as_bytes());

        let mut window_context_import = None;
        let mut window_imported = false;
        let mut app_context_imported = false;

        while let Some(match_) = matches.next() {
            let mut import_name = "";
            let mut import_range = 0..0;

            for capture in match_.captures {
                match imports_query.capture_names()[capture.index as usize] {
                    "import_name" => {
                        import_name = &source[capture.node.byte_range()];
                        import_range = capture.node.byte_range();
                    }
                    _ => {}
                }
            }

            match import_name {
                "WindowContext" => window_context_import = Some(import_range),
                "Window" => window_imported = true,
                "AppContext" => app_context_imported = true,
                _ => {}
            }
        }

        if let Some(byte_range) = window_context_import {
            let mut replacement = String::new();
            if !window_imported {
                replacement.push_str("Window");
            }
            if !app_context_imported {
                if !replacement.is_empty() {
                    replacement.push_str(", ");
                }
                replacement.push_str("AppContext");
            }
            if !replacement.is_empty() {
                file.record_edit(byte_range, replacement);
            }
        }
    }
}

fn is_window_context_signature(signature: &str) -> bool {
    signature.contains("WindowContext")
    && !signature.contains("Fn")
    // TODO: handle AsyncWindowContext?
    && !signature.contains("AsyncWindowContext")
}

fn call_expression_function_identifier(call_expression: Node) -> Result<Node> {
    let mut node = call_expression.child_by_field_name("function").unwrap();
    loop {
        node = match node.grammar_name() {
            "identifier" => return Ok(node),
            "field_expression" => node.child_by_field_name("field").unwrap(),
            "scoped_identifier" => node.child_by_field_name("name").unwrap(),
            "generic_function" => node.child_by_field_name("function").unwrap(),
            name => {
                return Err(anyhow!(
                    "Unexpected node type in call_expression_method: {name}"
                ))
            }
        };
    }
}

fn argument_index_from_parameter_index(
    parameters: Node,
    method_identifier: Node,
    parameter_index: usize,
) -> usize {
    if parameters_shifted_by_self(parameters, method_identifier) {
        parameter_index - 1
    } else {
        parameter_index
    }
}

fn parameter_index_from_argument_index(
    parameters: Node,
    method_identifier: Node,
    argument_index: usize,
) -> usize {
    if parameters_shifted_by_self(parameters, method_identifier) {
        argument_index + 1
    } else {
        argument_index
    }
}

fn parameters_shifted_by_self(parameters: Node, method_identifier: Node) -> bool {
    if method_identifier.parent().unwrap().grammar_name() == "scoped_identifier" {
        false
    } else {
        if parameters.named_child(0).unwrap().grammar_name() == "self_parameter" {
            true
        } else {
            false
        }
    }
}

fn signature_parameters<'a>(
    signature: Node<'a>,
    code: &str,
    function_type_query: &Query,
) -> Option<Node<'a>> {
    match signature.child_by_field_name("parameters") {
        Some(parameters) => Some(parameters),
        None => {
            let mut query_cursor = QueryCursor::new();
            let mut matches =
                query_cursor.matches(&function_type_query, signature, code.as_bytes());
            while let Some(match_) = matches.next() {
                let function_type = match_.nodes_for_capture_index(0).next().unwrap();
                return function_type.child_by_field_name("parameters");
            }
            None
        }
    }
}

/// Find the index of the first closure-taking parameter in the function. Probably should have just
/// hardcoded this.
fn index_of_first_function_type_parameter<'a>(
    signature: Node<'a>,
    code: &str,
    function_type_query: &Query,
) -> Option<(Node<'a>, usize)> {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&function_type_query, signature, code.as_bytes());
    while let Some(match_) = matches.next() {
        let function_type = match_.nodes_for_capture_index(0).next().unwrap();
        let parameter = node_ancestors(function_type)
            .find(|ancestor| ancestor.grammar_name() == "parameter")?;
        return Some((
            parameter.parent().unwrap(),
            index_in_parent_named_children(parameter),
        ));
    }
    None
}

fn index_in_parent_named_children(node: Node) -> usize {
    let node_id = node.id();
    let parent = node.parent().unwrap();
    let cursor = &mut parent.walk();
    let result = parent
        .named_children(cursor)
        .enumerate()
        .find(|(_, x)| x.id() == node_id)
        .unwrap()
        .0;
    result
}

fn parse_signature(input_signature: String) -> Result<(tree_sitter::Tree, String)> {
    let signature = format!("{input_signature};");
    let tree = parse_rust(&signature);
    if tree.root_node().child(0).unwrap().grammar_name() == "function_signature_item" {
        return Ok((tree, signature));
    }

    let signature = format!("let {input_signature}");
    let tree = parse_rust(signature.as_str());
    if tree.root_node().child(0).unwrap().grammar_name() == "let_declaration" {
        return Ok((tree, signature));
    }

    Err(anyhow!("Failed to parse signature: {input_signature}"))
}

fn parse_rust(code: &str) -> tree_sitter::Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_rust::LANGUAGE.into())
        .unwrap();
    parser.parse(&code, None).unwrap()
}

#[derive(Debug, Clone)]
struct LocalInfo {
    id: LocalId,
    name: Arc<str>,
    type_signature: Arc<str>,
    definition: Occurrence,
    references: Vec<Occurrence>,
}

impl LocalInfo {
    fn compute(
        document: &Document,
        symbol: &LocalSymbol,
        symbol_info: &SymbolInformation,
        signature_predicate: impl FnOnce(&str) -> bool,
    ) -> Result<Option<LocalInfo>> {
        let Some(signature) = symbol_info.signature_documentation.as_ref() else {
            return Ok(None);
        };
        let signature = &signature.text;
        if !signature_predicate(signature) {
            return Ok(None);
        }
        let type_signature = extract_type_signature(signature)?;

        let (definition, references) = document.find_local_definition_and_references(symbol)?;
        Ok(Some(LocalInfo {
            id: LocalId {
                relative_path: document.relative_path.clone(),
                symbol: symbol.clone(),
            },
            name: symbol_info.display_name.clone(),
            type_signature: type_signature.into(),
            definition: definition.clone(),
            references: references.into_iter().map(|occ| occ.clone()).collect(),
        }))
    }
}

fn extract_type_signature(typed_identifier: &str) -> Result<String> {
    match typed_identifier.find(|c| c == ':') {
        Some(colon_position) => Ok(typed_identifier[colon_position + 2..].to_owned()),
        None => Err(anyhow!("{typed_identifier} does not contain ':'")),
    }
}

fn node_ancestors(mut node: Node) -> impl Iterator<Item = Node> {
    iter::from_fn(move || {
        let result = node.parent();
        if let Some(parent) = result {
            node = parent;
        }
        result
    })
}

fn node_children_to_string(node: Node) -> String {
    let cursor = &mut node.walk();
    format!("{:?}", node.named_children(cursor).collect_vec())
}

fn last_child<'a>(node: Node<'a>) -> Option<Node<'a>> {
    node.child(node.child_count() - 1)
}

fn show_progress(action: &str, index: usize, length: usize, things: &str) {
    let progress = (index + 1) as f32 / length as f32;
    print!(
        "\r\u{1b}[K{} {}/{} {} ({:.1}%)",
        action,
        index + 1,
        length,
        things,
        progress * 100.0
    );
    std::io::stdout().flush().unwrap();
}
