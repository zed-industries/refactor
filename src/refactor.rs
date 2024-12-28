use anyhow::{Context as _, Result};
use clap::Parser as ClapParser;
use protobuf::Message;
use scip::types::{
    symbol_information::Kind, DiagnosticTag, PositionEncoding, ProtocolVersion, Severity,
    SyntaxKind, TextEncoding,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{Read, Write},
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node, Parser, Point, Query, QueryCursor, TreeCursor};

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

struct Refactor {
    parser: Parser,
    index: Index,
    index_folder: PathBuf,
    caller_graph: BTreeMap<Arc<str>, BTreeSet<Arc<str>>>,
    window_context_methods: BTreeSet<Arc<str>>,
    fns_taking_window_context_fns: BTreeSet<Arc<str>>,
    edits: BTreeMap<Arc<str>, Vec<Edit>>,
    function_type_parameters_query: Query,
    functions_query: Query,
    imports_query: Query,
    parameters_query: Query,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub metadata: Arc<Metadata>,
    pub documents: Vec<Document>,
    pub external_symbols: Vec<SymbolInformation>,
}

#[derive(Debug, Clone)]
pub struct Document {
    pub language: Arc<str>,
    pub relative_path: Arc<str>,
    pub occurrences: Vec<Occurrence>,
    pub symbols: Vec<SymbolInformation>,
    pub text: Arc<str>,
    pub position_encoding: PositionEncoding,
}

#[derive(Debug, Clone)]
pub struct Occurrence {
    pub range: Range<Point>,
    pub symbol: Arc<str>,
    pub symbol_roles: i32,
    pub override_documentation: Vec<Arc<str>>,
    pub syntax_kind: SyntaxKind,
    pub diagnostics: Vec<Diagnostic>,
    pub enclosing_range: Vec<i32>,
}

#[derive(Debug, Clone)]
pub struct SymbolInformation {
    pub symbol: Arc<str>,
    pub documentation: Vec<Arc<str>>,
    pub relationships: Vec<Relationship>,
    pub kind: Kind,
    pub display_name: Arc<str>,
    pub signature_documentation: Option<Arc<Document>>,
    pub enclosing_symbol: Arc<str>,
}

#[derive(Debug, Clone)]
pub struct Metadata {
    pub version: ProtocolVersion,
    pub tool_info: Arc<ToolInfo>,
    pub project_root: Arc<str>,
    pub text_document_encoding: TextEncoding,
}

#[derive(Debug, Clone)]
pub struct ToolInfo {
    pub name: Arc<str>,
    pub version: Arc<str>,
    pub arguments: Vec<Arc<str>>,
}

#[derive(Debug, Clone)]
pub struct Relationship {
    pub symbol: Arc<str>,
    pub is_reference: bool,
    pub is_implementation: bool,
    pub is_type_definition: bool,
    pub is_definition: bool,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Arc<str>,
    pub message: Arc<str>,
    pub source: Arc<str>,
    pub tags: Vec<DiagnosticTag>,
}

struct Edit {
    byte_range: Range<usize>,
    replacement: String,
}

impl Refactor {
    pub fn new(path: PathBuf) -> Result<Self> {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .context("Failed to set language for parser")?;

        let (index_folder, index) = Self::load_scip_index(&path)?;

        Ok(Self {
            parser,
            index,
            index_folder,
            caller_graph: BTreeMap::new(),
            window_context_methods: BTreeSet::new(),
            fns_taking_window_context_fns: BTreeSet::new(),
            edits: BTreeMap::new(),
            function_type_parameters_query: Self::parse_function_type_params_query(),
            functions_query: Self::parse_functions_query(),
            imports_query: Self::parse_imports_query(),
            parameters_query: Self::parse_parameters_query(),
        })
    }

    fn load_scip_index(path: &PathBuf) -> Result<(PathBuf, Index)> {
        let mut current_dir = path.clone();
        let mut index_path = None;
        while let Some(parent) = current_dir.parent() {
            let potential_index_path = current_dir.join("index.scip");
            if potential_index_path.exists() {
                index_path = Some(potential_index_path);
                break;
            }
            current_dir = parent.to_path_buf();
        }
        let index_path =
            index_path.context("Failed to find index.scip in any ancestor directory")?;
        let index_folder = index_path.parent().unwrap().to_path_buf();
        let mut file = File::open(&index_path)
            .context(format!("Failed to open index file: {:?}", index_path))?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)
            .context("Failed to read index file")?;

        let mut index = scip::types::Index::parse_from_bytes(&buffer)
            .context("Failed to parse index from bytes")?;
        index.documents.sort_by(|a, b| {
            std::path::Path::new(&a.relative_path).cmp(std::path::Path::new(&b.relative_path))
        });
        for doc in &mut index.documents {
            doc.occurrences.sort_by_key(|a| a.range[0]);
        }

        println!("Loaded {} documents", index.documents.len());

        Ok((index_folder, index.into()))
    }

    pub fn process(&mut self) -> Result<()> {
        self.analyze()?;
        let need_window = self.find_transitive_callers(&self.window_context_methods);
        self.stage_edits(&need_window)?;

        Ok(())
    }

    // A read-only pass on every document to compute metadata, index relationships
    fn analyze(&mut self) -> Result<()> {
        let mut documents = std::mem::take(&mut self.index.documents);
        let document_count = documents.len();
        for (index, document) in documents.iter_mut().enumerate() {
            let relative_path = document.relative_path.as_ref();
            let source = self.load_relative_path(relative_path)?;
            let tree = self.parser.parse(&source, None).unwrap();
            // self.analyze_call_graph(document, tree.root_node())?;
            self.find_fns_taking_window_fns(document, tree.root_node(), &source);

            let progress = (index + 1) as f32 / document_count as f32;
            print!(
                "\r\u{1b}[KAnalyzing – {}/{} documents ({:.1}%)",
                index + 1,
                document_count,
                progress * 100.0
            );
            std::io::stdout().flush().unwrap();
        }
        self.index.documents = documents;
        println!("");
        self.find_window_context_methods()?;

        Ok(())
    }

    fn analyze_call_graph(&mut self, document: &Document, node: Node) -> Result<()> {
        let relative_path = document.relative_path.as_ref();
        let source = self.load_relative_path(relative_path)?;

        let query = &self.functions_query;
        let function_item_name_ix = query.capture_index_for_name("function_item.name").unwrap();
        let function_item_ix = query.capture_index_for_name("function_item").unwrap();
        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(&query, node, source.as_bytes());

        while let Some(match_) = matches.next() {
            let function_item = match_
                .nodes_for_capture_index(function_item_ix)
                .next()
                .unwrap();
            let function_name = match_
                .nodes_for_capture_index(function_item_name_ix)
                .next()
                .unwrap();

            let definition_symbol = match document
                .occurrences
                .binary_search_by_key(&function_name.start_position(), |occurrence| {
                    occurrence.range.start
                }) {
                Ok(index) => &document.occurrences[index].symbol,
                Err(_) => {
                    // We may find definitions syntactically that aren't in the current SCIP index.
                    continue;
                }
            };

            let function_item_range = function_item.range();
            let child_occurences_start = match document
                .occurrences
                .binary_search_by_key(&function_item_range.start_point, |occurrence| {
                    occurrence.range.start
                }) {
                Ok(index) => index,
                Err(index) => index,
            };
            let child_occurences_end = match document
                .occurrences
                .binary_search_by_key(&function_item_range.end_point, |occurrence| {
                    occurrence.range.end
                }) {
                Ok(index) => index + 1,
                Err(index) => index,
            };

            for child_occurrence in document.occurrences
                [child_occurences_start..child_occurences_end]
                .iter()
                .filter(|occurrence| {
                    occurrence.symbol_roles == 0 && occurrence.symbol.ends_with("().")
                })
            {
                self.caller_graph
                    .entry(child_occurrence.symbol.clone())
                    .or_insert_with(BTreeSet::new)
                    .insert(definition_symbol.clone());
            }
        }

        Ok(())
    }

    /// Find all "sparse paths" through the Tree-Sitter parse tree
    /// that match the sequence PATTERN in order.
    fn find_fns_taking_window_fns(&mut self, document: &Document, node: Node, source: &str) {
        let pattern = [
            "function_item",
            "parameters",
            "function_type",
            "parameters",
            "type_identifier",
        ];

        self.find_sparse_path_matches(node, &pattern, source, |this, path| {
            let node = path.last().unwrap();
            if node.kind() == "type_identifier" && &source[node.byte_range()] == "WindowContext" {
                let function_item = path.first().unwrap().child_by_field_name("name").unwrap();
                if let Ok(occurrence) =
                    this.find_occurrence(document, function_item.start_position())
                {
                    this.fns_taking_window_context_fns
                        .insert(occurrence.symbol.clone());
                }
            }
        });
    }

    fn find_sparse_path_matches<F>(
        &mut self,
        node: Node,
        pattern: &[&str],
        source: &str,
        mut callback: F,
    ) where
        F: FnMut(&mut Self, &[Node]),
    {
        let mut cursor = node.walk();
        let mut current_path = Vec::new();

        fn dfs<'a, F>(
            this: &mut Refactor,
            cursor: &mut TreeCursor<'a>,
            current_path: &mut Vec<Node<'a>>,
            pattern_index: usize,
            pattern: &[&str],
            source: &str,
            callback: &mut F,
        ) where
            F: FnMut(&mut Refactor, &[Node]),
        {
            let node = cursor.node();

            let mut next_index = pattern_index;
            if node.kind() == pattern[pattern_index] {
                next_index += 1;
                current_path.push(node);

                if next_index == pattern.len() {
                    callback(this, current_path);
                    current_path.pop();
                    return;
                }
            }

            if cursor.goto_first_child() {
                loop {
                    dfs(
                        this,
                        cursor,
                        current_path,
                        next_index,
                        pattern,
                        source,
                        callback,
                    );

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
                cursor.goto_parent();
            }

            if next_index > pattern_index && current_path.last().map(|n| n.id()) == Some(node.id())
            {
                current_path.pop();
            }
        }

        dfs(
            self,
            &mut cursor,
            &mut current_path,
            0,
            pattern,
            source,
            &mut callback,
        );
    }

    fn find_window_context_methods(&mut self) -> Result<()> {
        let window_rs_path = "crates/gpui/src/window.rs";
        let document_index = self
            .index
            .documents
            .binary_search_by(|doc| {
                Path::new(doc.relative_path.as_ref()).cmp(Path::new(window_rs_path))
            })
            .map_err(|_| anyhow::anyhow!("Document not found: {:?}", window_rs_path))?;

        let document = &self.index.documents[document_index];

        for symbol in &document.symbols {
            if symbol.symbol.contains("/WindowContext#") {
                if let Kind::Method = symbol.kind {
                    let hash_count = symbol.symbol.matches('#').count();
                    if hash_count == 1 {
                        self.window_context_methods.insert(symbol.symbol.clone());
                    }
                }
            }
        }

        Ok(())
    }

    fn find_transitive_callers(&self, target_methods: &BTreeSet<Arc<str>>) -> BTreeSet<Arc<str>> {
        println!(
            "Finding transitive callers for {} methods",
            target_methods.len()
        );

        let mut to_visit: Vec<Arc<str>> = target_methods.iter().cloned().collect();
        let mut visited = BTreeSet::new();

        while let Some(method) = to_visit.pop() {
            if !visited.insert(method.clone()) {
                continue;
            }

            if let Some(callers) = self.caller_graph.get(&method) {
                for caller in callers {
                    if !visited.contains(caller) {
                        to_visit.push(caller.clone());
                    }
                }
            }
        }

        visited
    }

    fn stage_edits(&mut self, need_window: &BTreeSet<Arc<str>>) -> Result<()> {
        let documents = std::mem::take(&mut self.index.documents);
        let total_documents = documents.len();
        for (index, document) in documents.iter().enumerate() {
            let relative_path = &document.relative_path;
            let source = self.load_relative_path(relative_path)?;
            let tree = self.parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            self.stage_param_edits(document, &source, root_node, relative_path, need_window);
            self.stage_function_type_edits(&source, root_node, relative_path);
            self.stage_closure_param_edits(document, &source, root_node, relative_path);
            self.stage_import_edits(&source, root_node, relative_path);

            let progress = (index + 1) as f32 / total_documents as f32;
            print!(
                "\r\u{1b}[KStaging Edits – {}/{} documents ({:.1}%)",
                index + 1,
                total_documents,
                progress * 100.0
            );
            std::io::stdout().flush().unwrap();
        }
        println!("");
        self.index.documents = documents;
        Ok(())
    }

    fn stage_param_edits(
        &mut self,
        document: &Document,
        source: &str,
        node: Node,
        relative_path: &Arc<str>,
        needs_window: &BTreeSet<Arc<str>>,
    ) {
        let query = &self.parameters_query;
        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(&query, node, source.as_bytes());
        let param_ix = query.capture_index_for_name("parameter").unwrap();
        let param_name_ix = query.capture_index_for_name("parameter.name").unwrap();
        let function_name_ix = query.capture_index_for_name("function.name").unwrap();

        while let Some(match_) = matches.next() {
            for capture in match_.captures {
                let param_type = &source[capture.node.byte_range()];
                if param_type != "WindowContext" {
                    continue;
                }

                let param_node = match_.nodes_for_capture_index(param_ix).next().unwrap();
                let param_name_node = match_.nodes_for_capture_index(param_name_ix).next();
                let function_name_node = match_
                    .nodes_for_capture_index(function_name_ix)
                    .next()
                    .unwrap();

                let function_symbol =
                    match self.find_occurrence(document, function_name_node.start_position()) {
                        Ok(occurrence) => occurrence.symbol.clone(),
                        Err(_) => {
                            continue;
                        }
                    };

                let param_name = param_name_node.map_or("_", |node| &source[node.byte_range()]);

                if needs_window.contains(&function_symbol) {
                    let underscore = if param_name.starts_with('_') { "_" } else { "" };
                    self.edits
                        .entry(relative_path.clone())
                        .or_default()
                        .push(Edit {
                            byte_range: param_node.byte_range(),
                            replacement: format!(
                                "{}window: &mut Window, {}: &mut AppContext",
                                underscore, param_name
                            )
                            .to_string(),
                        });
                } else {
                    self.edits
                        .entry(relative_path.clone())
                        .or_default()
                        .push(Edit {
                            byte_range: param_node.byte_range(),
                            replacement: format!("{}: &mut AppContext", param_name).to_string(),
                        });
                }
            }
        }
    }

    fn stage_function_type_edits(&mut self, source: &str, node: Node, relative_path: &Arc<str>) {
        let query = &self.function_type_parameters_query;
        let param_ix = query.capture_index_for_name("parameter").unwrap();
        let param_type_ix = query.capture_index_for_name("parameter.type").unwrap();

        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(&query, node, source.as_bytes());

        while let Some(match_) = matches.next() {
            for capture in match_.captures {
                if capture.index == param_type_ix {
                    let param_type = &source[capture.node.byte_range()];

                    if param_type == "WindowContext" {
                        let param_node = match_.nodes_for_capture_index(param_ix).next().unwrap();
                        self.edits
                            .entry(relative_path.clone())
                            .or_default()
                            .push(Edit {
                                byte_range: param_node.byte_range(),
                                replacement: "&mut Window, &mut AppContext".to_string(),
                            });
                    }
                }
            }
        }
    }

    fn stage_closure_param_edits(
        &mut self,
        document: &Document,
        source: &str,
        node: Node,
        relative_path: &Arc<str>,
    ) {
        let filtered_occurrences: Vec<_> = document
            .occurrences
            .iter()
            .filter(|occurrence| {
                occurrence.symbol_roles == 0
                    && self
                        .fns_taking_window_context_fns
                        .contains(&occurrence.symbol)
            })
            .collect();

        for occurrence in filtered_occurrences {
            let node = node
                .named_descendant_for_point_range(occurrence.range.start, occurrence.range.end)
                .unwrap();

            let mut parent = node;
            while parent.kind() != "call_expression" {
                if let Some(new_parent) = parent.parent() {
                    parent = new_parent;
                } else {
                    break;
                }
            }

            let pattern = ["arguments", "closure_expression", "closure_parameters"];

            self.find_sparse_path_matches(node, &pattern, source, |this, path| {
                let node = path.last().unwrap();
                let last_param = node.child(node.child_count() - 2);
                if let Some(last_param) = last_param {
                    let last_param_text = &source[last_param.byte_range()];
                    let underscore = if last_param_text.starts_with('_') {
                        "_"
                    } else {
                        ""
                    };
                    this.edits
                        .entry(relative_path.clone())
                        .or_default()
                        .push(Edit {
                            byte_range: last_param.byte_range(),
                            replacement: format!("{}window, {}", underscore, last_param_text),
                        });
                }
            });
        }
    }

    fn stage_import_edits(&mut self, source: &str, root_node: Node, relative_path: &Arc<str>) {
        let mut query_cursor = QueryCursor::new();
        let mut matches = query_cursor.matches(&self.imports_query, root_node, source.as_bytes());

        let mut window_context_import = None;
        let mut window_imported = false;
        let mut app_context_imported = false;

        while let Some(match_) = matches.next() {
            let mut import_name = "";
            let mut import_range = 0..0;

            for capture in match_.captures {
                match self.imports_query.capture_names()[capture.index as usize] {
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
                self.edits
                    .entry(relative_path.clone())
                    .or_default()
                    .push(Edit {
                        byte_range,
                        replacement,
                    });
            }
        }
    }

    fn load_relative_path(&self, relative_path: &str) -> Result<String> {
        let file_path = self.index_folder.join(relative_path);
        let mut file = File::open(&file_path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        Ok(source)
    }

    fn parse_functions_query() -> Query {
        Query::new(
            &tree_sitter_rust::LANGUAGE.into(),
            include_str!("./functions.scm"),
        )
        .expect("Failed to create query")
    }

    fn parse_parameters_query() -> Query {
        Query::new(
            &tree_sitter_rust::LANGUAGE.into(),
            include_str!("./parameters.scm"),
        )
        .expect("Failed to create query")
    }

    fn parse_function_type_params_query() -> Query {
        Query::new(
            &tree_sitter_rust::LANGUAGE.into(),
            include_str!("./function_type_params.scm"),
        )
        .expect("Failed to create query")
    }

    fn parse_imports_query() -> Query {
        Query::new(
            &tree_sitter_rust::LANGUAGE.into(),
            include_str!("./imports.scm"),
        )
        .expect("Failed to create query")
    }

    fn find_occurrence<'a>(
        &self,
        document: &'a Document,
        start_point: Point,
    ) -> Result<&'a Occurrence> {
        let occurrence_index = document
            .occurrences
            .binary_search_by_key(&start_point, |occurrence| occurrence.range.start)
            .map_err(|_| anyhow::anyhow!("Occurrence not found at {:?}", start_point))?;

        Ok(&document.occurrences[occurrence_index])
    }

    pub fn display_dry_run_results(&self) {
        for (path, edits) in &self.edits {
            let mut file =
                File::open(&self.index_folder.join(path.as_ref())).expect("Failed to open file");
            let mut source = String::new();
            file.read_to_string(&mut source)
                .expect("Failed to read file");

            println!("File: {:?}", path);
            println!("---");

            for edit in edits {
                let range = &edit.byte_range;
                let start_line = source[..range.start].lines().count();
                let end_line = source[..range.end].lines().count();
                let context_start = source[..range.start].rfind('\n').map_or(0, |i| i + 1);
                let context_end = source[range.end..]
                    .find('\n')
                    .map_or(source.len(), |i| range.end + i);

                println!("Lines {}-{}:", start_line, end_line);
                println!("- {}", &source[context_start..context_end]);
                println!(
                    "+ {}{}{}",
                    &source[context_start..range.start],
                    edit.replacement,
                    &source[range.end..context_end]
                );
                println!();
            }

            println!("---\n");
        }
    }

    pub fn apply_edits(&mut self) -> Result<()> {
        let total_files = self.edits.len();
        for (path, edits) in self.edits.iter_mut() {
            let full_path = self.index_folder.join(path.as_ref());
            let mut file = File::open(&full_path).context("Failed to open file")?;
            let mut source = String::new();
            file.read_to_string(&mut source)
                .context("Failed to read file")?;

            let mut new_source = source.clone();
            edits.sort_by(|a, b| b.byte_range.start.cmp(&a.byte_range.start));
            for edit in edits.iter() {
                new_source.replace_range(
                    edit.byte_range.start..edit.byte_range.end,
                    &edit.replacement,
                );
            }

            std::fs::write(full_path, new_source).context("Failed to write file")?;

            std::io::stdout().flush().unwrap();
        }
        println!("\r\u{1b}[KChanged {} files", total_files);
        Ok(())
    }
}

impl From<scip::types::Index> for Index {
    fn from(index: scip::types::Index) -> Self {
        Index {
            metadata: Arc::new(Metadata::from(*index.metadata.0.unwrap())),
            documents: index.documents.into_iter().map(Document::from).collect(),
            external_symbols: index
                .external_symbols
                .into_iter()
                .map(SymbolInformation::from)
                .collect(),
        }
    }
}

impl From<scip::types::Metadata> for Metadata {
    fn from(metadata: scip::types::Metadata) -> Self {
        Metadata {
            version: metadata.version.enum_value().unwrap(),
            tool_info: Arc::new(ToolInfo::from(*metadata.tool_info.0.unwrap())),
            project_root: metadata.project_root.into(),
            text_document_encoding: metadata.text_document_encoding.enum_value().unwrap(),
        }
    }
}

impl From<scip::types::ToolInfo> for ToolInfo {
    fn from(tool_info: scip::types::ToolInfo) -> Self {
        ToolInfo {
            name: tool_info.name.into(),
            version: tool_info.version.into(),
            arguments: tool_info.arguments.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<scip::types::Document> for Document {
    fn from(document: scip::types::Document) -> Self {
        Document {
            language: document.language.into(),
            relative_path: document.relative_path.into(),
            occurrences: document
                .occurrences
                .into_iter()
                .map(Occurrence::from)
                .collect(),
            symbols: document
                .symbols
                .into_iter()
                .map(SymbolInformation::from)
                .collect(),
            text: document.text.into(),
            position_encoding: document.position_encoding.enum_value().unwrap(),
        }
    }
}

impl From<scip::types::Occurrence> for Occurrence {
    fn from(occurrence: scip::types::Occurrence) -> Self {
        let range = match occurrence.range.len() {
            3 => Range {
                start: Point::new(occurrence.range[0] as usize, occurrence.range[1] as usize),
                end: Point::new(occurrence.range[0] as usize, occurrence.range[2] as usize),
            },
            4 => Range {
                start: Point::new(occurrence.range[0] as usize, occurrence.range[1] as usize),
                end: Point::new(occurrence.range[2] as usize, occurrence.range[3] as usize),
            },
            l => panic!("SCIP Occurrence.range has {l} values when 3 or 4 are expected."),
        };
        Occurrence {
            range: range,
            symbol: occurrence.symbol.into(),
            symbol_roles: occurrence.symbol_roles,
            override_documentation: occurrence
                .override_documentation
                .into_iter()
                .map(Into::into)
                .collect(),
            syntax_kind: occurrence.syntax_kind.enum_value().unwrap(),
            diagnostics: occurrence
                .diagnostics
                .into_iter()
                .map(Diagnostic::from)
                .collect(),
            enclosing_range: occurrence.enclosing_range,
        }
    }
}

impl From<scip::types::SymbolInformation> for SymbolInformation {
    fn from(symbol_info: scip::types::SymbolInformation) -> Self {
        SymbolInformation {
            symbol: symbol_info.symbol.into(),
            documentation: symbol_info
                .documentation
                .into_iter()
                .map(Into::into)
                .collect(),
            relationships: symbol_info
                .relationships
                .into_iter()
                .map(Relationship::from)
                .collect(),
            kind: symbol_info.kind.enum_value().unwrap(),
            display_name: symbol_info.display_name.into(),
            signature_documentation: symbol_info
                .signature_documentation
                .0
                .map(|doc| Arc::new(Document::from(*doc))),
            enclosing_symbol: symbol_info.enclosing_symbol.into(),
        }
    }
}

impl From<scip::types::Relationship> for Relationship {
    fn from(relationship: scip::types::Relationship) -> Self {
        Relationship {
            symbol: relationship.symbol.into(),
            is_reference: relationship.is_reference,
            is_implementation: relationship.is_implementation,
            is_type_definition: relationship.is_type_definition,
            is_definition: relationship.is_definition,
        }
    }
}

impl From<scip::types::Diagnostic> for Diagnostic {
    fn from(diagnostic: scip::types::Diagnostic) -> Self {
        Diagnostic {
            severity: diagnostic.severity.enum_value().unwrap(),
            code: diagnostic.code.into(),
            message: diagnostic.message.into(),
            source: diagnostic.source.into(),
            tags: diagnostic
                .tags
                .iter()
                .map(|tag| tag.enum_value().unwrap())
                .collect(),
        }
    }
}

fn main() -> Result<()> {
    let start_time = SystemTime::now();

    let args = Args::parse();

    let path = std::fs::canonicalize(&args.path)?;
    let _relative_path = path.strip_prefix(std::env::current_dir()?).unwrap_or(&path);

    let refactor = Refactor::new(path);
    let mut refactor = refactor?;
    refactor.process()?;

    if args.dry {
        refactor.display_dry_run_results();
    } else {
        refactor.apply_edits()?;
    }

    println!(
        "Elapsed time: {:?}",
        SystemTime::now().duration_since(start_time)
    );

    Ok(())
}
