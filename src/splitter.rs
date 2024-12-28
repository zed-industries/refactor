use anyhow::{anyhow, Context as _, Result};
use clap::Parser as ClapParser;
use itertools::Itertools;
use refactor::{file_editor::*, scip_index::*};
use std::{iter, path::PathBuf, str::FromStr, sync::Arc};
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
    root_folder: PathBuf,
    function_type_query: Query,
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

        Ok(Self {
            editor,
            index,
            root_folder,
            function_type_query,
        })
    }

    fn process(&mut self) {
        let locals = self.all_window_context_locals();

        // display_locals(&signature_to_locals)

        for local_info in locals.into_iter() {
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
            match Self::split_parameter(
                &local_info.definition,
                &window_name,
                &cx_name,
                &mut file,
                &document,
            ) {
                Ok(names) => names,
                Err(e) => {
                    file.record_error(local_info.definition.range.start.row, format!("{e}"));
                    continue;
                }
            };
        }
    }

    fn split_parameter(
        parameter: &Occurrence,
        window: &str,
        cx: &str,
        file: &mut File,
        document: &Document,
    ) -> Result<()> {
        let node = file.find_node(&parameter.range)?;
        let parent0 = node.parent().unwrap();
        let parent1 = parent0.parent().unwrap();

        let edit_closure_parameter =
            || file.record_node_replacement(node, format!("{window}, {cx}"));
        let edit_fn_parameter = || {
            match extract_type_signature(file.node_text(parent0))?.as_str() {
                "&WindowContext" | "&WindowContext<'_>" => file.record_node_replacement(
                    parent0,
                    format!("{window}: &Window, {cx}: &AppContext"),
                ),
                "&mut WindowContext" | "&mut WindowContext<'_>" => file.record_node_replacement(
                    parent0,
                    format!("{window}: &mut Window, {cx}: &mut AppContext"),
                ),
                "&'a WindowContext" => file.record_node_replacement(
                    parent0,
                    format!("{window}: &'a Window, {cx}: &'a AppContext"),
                ),
                ty => return Err(anyhow!("Unexpected type {}", ty)),
            }
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
            (parent0, parent1) => return Err(anyhow!("{parent0} {parent1}")),
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
            ("arguments", "call_expression", _) => {
                file.record_node_replacement(node, format!("{window}, {cx}"));
                Ok(true)
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
                    let parameter_index = index_of_first_function_type_parameter(
                        &method_info.signature_documentation.clone().unwrap().text,
                        &function_type_query,
                    );
                    let arguments = node_ancestors(node)
                        .find(|ancestor| ancestor.grammar_name() == "call_expression")
                        .unwrap()
                        .child_by_field_name("arguments")
                        .unwrap();
                    let method = file.node_text(method_node);
                    file.record_node_replacement(parent0, format!("{window}.{method}"));
                    if let Some(parameter_index) = parameter_index {
                        file.record_insertion_before_node(
                            // -1 for self param.
                            arguments.named_child(parameter_index - 1).unwrap(),
                            format!("cx, "),
                        );
                    } else {
                        file.record_insertion_before_node(
                            last_child(arguments).unwrap(),
                            format!(", cx"),
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
            (parent0, parent1, parent2) => return Err(anyhow!("{parent0} {parent1} {parent2}")),
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
}

fn is_window_context_signature(signature: &str) -> bool {
    signature.contains("WindowContext")
    && !signature.contains("Fn")
    // TODO: handle AsyncWindowContext?
    && !signature.contains("AsyncWindowContext")
}

/// Find the index of the first closure-taking parameter in the function. Probably should have just
/// hardcoded this.
fn index_of_first_function_type_parameter(
    signature: &str,
    function_type_query: &Query,
) -> Option<usize> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_rust::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&signature, None).unwrap();
    let mut query_cursor = QueryCursor::new();
    let mut matches =
        query_cursor.matches(&function_type_query, tree.root_node(), signature.as_bytes());
    while let Some(match_) = matches.next() {
        let function_type = match_.nodes_for_capture_index(0).next().unwrap();
        let parameter = node_ancestors(function_type)
            .find(|ancestor| ancestor.grammar_name() == "parameter")?;
        let parameter_id = parameter.id();
        let parameters = parameter.parent().unwrap();
        let cursor = &mut parameters.walk();
        return Some(
            parameters
                .named_children(cursor)
                .enumerate()
                .find(|(_, x)| x.id() == parameter_id)
                .unwrap()
                .0,
        );
    }
    None
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
