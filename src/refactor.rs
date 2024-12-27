use anyhow::{Context as _, Result};
use clap::Parser as ClapParser;
use protobuf::Message;
use scip::types::{
    symbol_information::Kind, DiagnosticTag, PositionEncoding, ProtocolVersion, Severity,
    SyntaxKind, TextEncoding,
};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    sync::Arc,
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Parser, Query, QueryCursor};

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
    caller_graph: HashMap<Arc<str>, BTreeSet<Arc<str>>>,
    transitive_window_context_callers: HashSet<Arc<str>>,
    path: PathBuf,
    edits: HashMap<PathBuf, Vec<Edit>>,
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
    pub range: OccurenceRange,
    pub symbol: Arc<str>,
    pub symbol_roles: i32,
    pub override_documentation: Vec<Arc<str>>,
    pub syntax_kind: SyntaxKind,
    pub diagnostics: Vec<Diagnostic>,
    pub enclosing_range: Vec<i32>,
}

#[derive(Debug, Clone)]
pub struct OccurenceRange {
    row: usize,
    start_column: usize,
    end_column: usize,
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
    start: usize,
    end: usize,
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
            caller_graph: HashMap::new(),
            transitive_window_context_callers: HashSet::new(),
            path,
            edits: HashMap::new(),
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
        // Walk over every document in the index
        // Parse every function definition in the document
        // Find every method symbol occurence with range[0]

        let mut capture_count = std::collections::HashMap::new();

        for (index, document) in self.index.documents.iter().enumerate() {
            let relative_path = document.relative_path.as_ref();
            let source = self.load_relative_path(relative_path)?;
            let tree = self.parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            let query = Self::first_pass_query();
            let function_item_name_ix = query.capture_index_for_name("function_item.name").unwrap();
            let mut cursor = QueryCursor::new();
            let mut matches = cursor.matches(&query, root_node, source.as_bytes());

            while let Some(match_) = matches.next() {
                for capture in match_.captures {
                    if query.capture_names()[capture.index as usize] == "function_item.name" {
                        continue;
                    }

                    if query.capture_names()[capture.index as usize] == "function_item" {
                        for capture in match_.captures {
                            if capture.index == function_item_name_ix {
                                let _function_name = &source[capture.node.byte_range()];
                                // *type_count.entry("function_item".to_string()).or_insert(0) += 1;
                            }
                        }
                        continue;
                    }

                    let param_type = &source[capture.node.byte_range()];
                    if param_type != "WindowContext" {
                        continue;
                    }

                    let capture_name = query.capture_names()[capture.index as usize];
                    if let Some(count) = capture_count.get_mut(capture_name) {
                        *count += 1;
                    } else {
                        capture_count.insert(capture_name.to_string(), 1);
                    }

                    let start_pos = capture.node.start_position();
                    let line = source.lines().nth(start_pos.row).unwrap_or("");
                    println!("Capture: {}", line);

                    // match query.capture_names()[capture.index as usize] {
                    //     "function.param.type" => {
                    //         *type_count.entry("function.param.type").or_insert(0) += 1;
                    //     }
                    //     "function_sig.param.type" => {
                    //         *type_count.entry("function_sig.param.type").or_insert(0) += 1;
                    //     }
                    //     "closure.param.type" => {
                    //         *type_count.entry("closure.param.type").or_insert(0) += 1;
                    //     }
                    //     "fn_pointer.param.type" => {
                    //         *type_count.entry("fn_pointer.param.type").or_insert(0) += 1;
                    //     }
                    //     "generic_bound.type" => {
                    //         *type_count.entry("generic_bound.type").or_insert(0) += 1;
                    //     }
                    //     "where_bound.type" => {
                    //         *type_count.entry("where_bound.type").or_insert(0) += 1;
                    //     }
                    //     "associated_type.type" => {
                    //         *type_count.entry("associated_type.type").or_insert(0) += 1;
                    //     }
                    //     "ref.param.type" => {
                    //         *type_count.entry("ref.param.type").or_insert(0) += 1;
                    //     }
                    //     "ptr.param.type" => {
                    //         *type_count.entry("ptr.param.type").or_insert(0) += 1;
                    //     }
                    //     "tuple_struct.param.type" => {
                    //         *type_count.entry("tuple_struct.param.type").or_insert(0) += 1;
                    //     }
                    //     _ => {
                    //         println!(
                    //             "Unhandled type: {}",
                    //             query.capture_names()[capture.index as usize]
                    //         );
                    //         *type_count.entry("unhandled").or_insert(0) += 1;
                    //     }
                    // }
                }
            }

            println!(
                "\rProcessed {} of {} documents ({:.1}%)",
                index + 1,
                self.index.documents.len(),
                (index + 1) as f32 / self.index.documents.len() as f32 * 100.0
            );
            std::io::stdout().flush().unwrap();

            // self.track_callers(&source, root_node, relative_path)?;
            // self.process_functions(&source, root_node, relative_path);
            // self.process_imports(&source);
        }

        for (type_name, count) in capture_count.iter() {
            println!("{}: {}", type_name, count);
        }

        // self.trace_window_context_callers();

        Ok(())
    }

    fn load_relative_path(&self, relative_path: &str) -> Result<String> {
        let file_path = self.index_folder.join(relative_path);
        let mut file = File::open(&file_path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        Ok(source)
    }

    fn first_pass_query() -> Query {
        Query::new(
            &tree_sitter_rust::LANGUAGE.into(),
            r#"
            ; Capture entire function items and their names in one pattern
            (function_item
              name: (identifier) @function_item.name
              parameters: (parameters
                (parameter
                  type: [
                    (reference_type
                      type: [
                        (type_identifier) @function.param.type
                        (scoped_type_identifier
                          name: (type_identifier) @function.param.type
                        )
                      ]
                    )
                  ]
                )
              )
            ) @function_item

            ; Function signature parameters
            (function_signature_item
              parameters: (parameters
                (parameter
                  type: (reference_type
                    type: [(type_identifier) @function_sig.param.type (scoped_type_identifier name: (type_identifier) @function_sig.param.type)]))))

            ; Closure parameters
            (closure_expression
                parameters: (closure_parameters
                (parameter
                    type: (reference_type
                    type: [(type_identifier) @closure.param.type (scoped_type_identifier name: (type_identifier) @closure.param.type)]))))

            "#,
        )
        .expect("Failed to create query")
    }

    pub fn process2(&mut self) -> Result<()> {
        // for document in &self.index.documents {
        //     let relative_path = document.relative_path.as_ref();
        //     let file_path = self.index_folder.join(relative_path);
        //     let mut file = File::open(&file_path)?;
        //     let mut source = String::new();
        //     file.read_to_string(&mut source)?;

        //     let tree = self.parser.parse(&source, None).unwrap();
        //     let root_node = tree.root_node();

        //     // self.track_callers(&source, root_node, relative_path)?;
        //     // self.process_functions(&source, root_node, relative_path);
        //     // self.process_imports(&source);
        // }

        // self.trace_window_context_callers();

        Ok(())
    }

    fn track_callers(
        &mut self,
        source: &str,
        root_node: tree_sitter::Node,
        relative_path: &str,
    ) -> Result<()> {
        let function_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), self.function_query())
            .expect("Failed to create function query");
        let call_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), self.call_query())
            .expect("Failed to create call query");

        let mut function_query_cursor = QueryCursor::new();
        let mut matches =
            function_query_cursor.matches(&function_query, root_node, source.as_bytes());

        while let Some(match_) = matches.next() {
            let function_name_node = match_
                .captures
                .iter()
                .find(|c| {
                    c.index
                        == function_query
                            .capture_index_for_name("function_name")
                            .unwrap()
                })
                .map(|c| c.node);

            let function_body = match_
                .captures
                .iter()
                .find(|c| {
                    c.index
                        == function_query
                            .capture_index_for_name("function_body")
                            .unwrap()
                })
                .map(|c| c.node);

            if let (Some(name_node), Some(body_node)) = (function_name_node, function_body) {
                let function_occurrence = match self.find_occurrence(
                    relative_path,
                    name_node.start_position().row,
                    name_node.start_position().column,
                    source,
                ) {
                    Ok(occurrence) => occurrence,
                    Err(e) => {
                        eprintln!("{}", e);
                        continue;
                    }
                };

                let parent_symbol = function_occurrence.symbol.clone();

                let mut call_cursor = QueryCursor::new();
                let mut calls = call_cursor.matches(&call_query, body_node, source.as_bytes());

                while let Some(call) = calls.next() {
                    if let Some(method) = call
                        .captures
                        .iter()
                        .find(|c| c.index == call_query.capture_index_for_name("method").unwrap())
                    {
                        match self.find_occurrence(
                            relative_path,
                            method.node.start_position().row,
                            method.node.start_position().column,
                            source,
                        ) {
                            Ok(method_occurrence) => {
                                let method_symbol = method_occurrence.symbol.clone();
                                self.caller_graph
                                    .entry(method_symbol)
                                    .or_insert_with(BTreeSet::new)
                                    .insert(parent_symbol.clone());
                            }
                            Err(e) => eprintln!("{}", e),
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn trace_window_context_callers(&mut self) {
        let mut to_process = Vec::new();
        for (callee, callers) in &self.caller_graph {
            if callee.contains("WindowContext#") {
                for caller in callers {
                    if self
                        .transitive_window_context_callers
                        .insert(caller.clone())
                    {
                        to_process.push(caller.clone());
                    }
                }
            }
        }

        while let Some(caller) = to_process.pop() {
            if let Some(callers) = self.caller_graph.get(&caller) {
                for caller in callers {
                    if self
                        .transitive_window_context_callers
                        .insert(caller.clone())
                    {
                        to_process.push(caller.clone());
                    }
                }
            }
        }

        println!("!!!!!!!!!!!!!  Window context callers:");
        for caller in &self.transitive_window_context_callers {
            println!("  {}", caller);
        }
    }

    fn process_functions(
        &mut self,
        source: &str,
        root_node: tree_sitter::Node,
        relative_path: &str,
    ) {
        let function_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), self.function_query())
            .expect("Failed to create function query");
        let call_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), self.call_query())
            .expect("Failed to create call query");

        let mut function_query_cursor = QueryCursor::new();
        let mut matches =
            function_query_cursor.matches(&function_query, root_node, source.as_bytes());

        let function_name_index = function_query
            .capture_index_for_name("function_name")
            .unwrap();
        let param_name_index = function_query.capture_index_for_name("param_name").unwrap();
        let param_type_index = function_query.capture_index_for_name("param_type").unwrap();
        let target_param_index = function_query
            .capture_index_for_name("target_param")
            .unwrap();
        let function_body_index = function_query
            .capture_index_for_name("function_body")
            .unwrap();

        while let Some(match_) = matches.next() {
            if match_.captures.is_empty() {
                continue;
            }

            let mut param_name = "";
            let mut target_param_start = None;
            let mut target_param_end = None;
            let mut function_body_node = None;
            let mut function_name = "";
            let mut function_name_node = None;

            for capture in match_.captures {
                match capture.index {
                    i if i == function_name_index => {
                        function_name = &source[capture.node.byte_range()];
                        function_name_node = Some(capture.node);
                    }
                    i if i == param_name_index => {
                        param_name = &source[capture.node.byte_range()];
                    }
                    i if i == param_type_index => {}
                    i if i == target_param_index => {
                        target_param_start = Some(capture.node.start_byte());
                        target_param_end = Some(capture.node.end_byte());
                    }
                    i if i == function_body_index => {
                        function_body_node = Some(capture.node);
                    }
                    _ => {}
                }
            }

            let is_function_type = match_
                .captures
                .iter()
                .any(|c| c.node.kind() == "function_type");

            if is_function_type {
                if let Some((start, end)) = target_param_start.zip(target_param_end) {
                    let new_param = if is_function_type {
                        "&mut Window, &mut AppContext"
                    } else {
                        "window: &mut Window, cx: &mut AppContext"
                    };

                    self.edits
                        .entry(PathBuf::from(relative_path))
                        .or_default()
                        .push(Edit {
                            start,
                            end,
                            replacement: new_param.to_string(),
                        });
                }
            } else {
                if let (Some(start), Some(end)) = (target_param_start, target_param_end) {
                    self.edits
                        .entry(PathBuf::from(relative_path))
                        .or_default()
                        .push(Edit {
                            start,
                            end,
                            replacement: "window: &mut Window, cx: &mut AppContext".to_string(),
                        });

                    if let Some(body_node) = function_body_node {
                        self.process_function_body(source, body_node, &call_query, param_name);
                    }
                }
            }
        }
    }

    fn process_function_body(
        &mut self,
        source: &str,
        body_node: tree_sitter::Node,
        call_query: &Query,
        param_name: &str,
    ) {
        let mut call_cursor = QueryCursor::new();
        let mut calls = call_cursor.matches(call_query, body_node, source.as_bytes());

        while let Some(call) = calls.next() {
            let method = call
                .captures
                .iter()
                .find(|c| c.index == call_query.capture_index_for_name("method").unwrap());
            let object = call
                .captures
                .iter()
                .find(|c| c.index == call_query.capture_index_for_name("object").unwrap());
            let args = call
                .captures
                .iter()
                .find(|c| c.index == call_query.capture_index_for_name("args").unwrap());

            if let Some(args) = args {
                self.process_call(source, args, object, method, param_name);
            }
        }
    }

    fn process_call(
        &mut self,
        source: &str,
        args: &tree_sitter::QueryCapture,
        object: Option<&tree_sitter::QueryCapture>,
        method: Option<&tree_sitter::QueryCapture>,
        param_name: &str,
    ) {
        let mut cursor = args.node.walk();
        for arg in args.node.children(&mut cursor) {
            if arg.kind() != "," {
                let arg_text = &source[arg.byte_range()];
                if arg_text == param_name {
                    self.process_cx_arg(
                        source,
                        &tree_sitter::QueryCapture {
                            node: arg,
                            index: 0,
                        },
                        object,
                        method,
                        param_name,
                    );
                }
            }
        }
    }

    fn process_cx_arg(
        &mut self,
        source: &str,
        arg: &tree_sitter::QueryCapture,
        object: Option<&tree_sitter::QueryCapture>,
        method: Option<&tree_sitter::QueryCapture>,
        param_name: &str,
    ) {
        if let (Some(object), Some(method)) = (object, method) {
            // let object_text = &source[object.node.byte_range()];
            // let method_name = &source[method.node.byte_range()];

            // If this a method call on the context itself, decide whether to call it on window or cx.
            // if object_text == param_name {
            //     if let Some(&needs_cx) = self.window_methods.get(method_name) {
            //         self.edits.entry(self.path.clone()).or_default().push(Edit {
            //             start: object.node.start_byte(),
            //             end: object.node.end_byte(),
            //             replacement: "window".to_string(),
            //         });

            //         if needs_cx {
            //             let arg_end = arg.node.end_byte();
            //             self.edits.entry(self.path.clone()).or_default().push(Edit {
            //                 start: arg_end,
            //                 end: arg_end,
            //                 replacement: ", cx".to_string(),
            //             });
            //         }
            //     } else {
            //         let arg_start = arg.node.start_byte();
            //         self.edits.entry(self.path.clone()).or_default().push(Edit {
            //             start: arg_start,
            //             end: arg_start,
            //             replacement: "window, ".to_string(),
            //         });
            //     }
            // } else {
            //     // For any other call, pass a window and cx through instead of a cx.
            //     // TODO! Check the definition of the method being called to decide if we need to pass window.
            //     let arg_start = arg.node.start_byte();
            //     let arg_end = arg.node.end_byte();
            //     self.edits.entry(self.path.clone()).or_default().push(Edit {
            //         start: arg_start,
            //         end: arg_end,
            //         replacement: "window, cx".to_string(),
            //     });
            // }
        } else {
            // For any other call, pass a window and cx through instead of a cx.
            // TODO! Check the definition of the method being called to decide if we need to pass window.
            let arg_start = arg.node.start_byte();
            let arg_end = arg.node.end_byte();
            self.edits.entry(self.path.clone()).or_default().push(Edit {
                start: arg_start,
                end: arg_end,
                replacement: "window, cx".to_string(),
            });
        }
    }

    fn process_imports(&mut self, source: &str) {
        let tree = self.parser.parse(source, None).unwrap();
        let root_node = tree.root_node();

        let import_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), self.imports_query())
            .expect("Failed to create import query");

        let mut query_cursor = QueryCursor::new();
        let mut matches = query_cursor.matches(&import_query, root_node, source.as_bytes());

        let mut window_context_import: Option<(usize, usize)> = None;
        let mut window_imported = false;
        let mut app_context_imported = false;

        while let Some(match_) = matches.next() {
            let mut import_name = "";
            let mut import_start = 0;
            let mut import_end = 0;

            for capture in match_.captures {
                match import_query.capture_names()[capture.index as usize] {
                    "import_name" => {
                        import_name = &source[capture.node.byte_range()];
                        import_start = capture.node.start_byte();
                        import_end = capture.node.end_byte();
                    }
                    _ => {}
                }
            }

            match import_name {
                "WindowContext" => window_context_import = Some((import_start, import_end)),
                "Window" => window_imported = true,
                "AppContext" => app_context_imported = true,
                _ => {}
            }
        }

        if let Some((start, end)) = window_context_import {
            if !window_imported && !app_context_imported {
                // Replace WindowContext with Window and AppContext
                self.edits.entry(self.path.clone()).or_default().push(Edit {
                    start,
                    end,
                    replacement: "{Window, AppContext}".to_string(),
                });
            } else if !window_imported {
                // Only add Window
                self.edits.entry(self.path.clone()).or_default().push(Edit {
                    start,
                    end,
                    replacement: "Window".to_string(),
                });
            } else if !app_context_imported {
                // Only add AppContext
                self.edits.entry(self.path.clone()).or_default().push(Edit {
                    start,
                    end,
                    replacement: "AppContext".to_string(),
                });
            }
            // If both are already imported, we don't need to do anything
        }
    }

    fn find_occurrence(
        &self,
        relative_path: &str,
        line: usize,
        column: usize,
        source: &str,
    ) -> Result<&Occurrence> {
        let document = self
            .index
            .documents
            .binary_search_by(|doc| doc.relative_path.as_ref().cmp(relative_path))
            .map_err(|_| anyhow::anyhow!("Document not found: {:?}", relative_path))?;
        let doc = &self.index.documents[document];

        let occurrence_index = doc
            .occurrences
            .binary_search_by(|occ| {
                let start_row = occ.range.row;
                let start_column = occ.range.start_column;

                if start_row != line {
                    start_row.cmp(&line)
                } else {
                    start_column.cmp(&column)
                }
            })
            .map_err(|_| {
                anyhow::anyhow!("Occurrence not found at line {} column {}", line, column)
            })?;

        Ok(&doc.occurrences[occurrence_index])
    }

    pub fn display_dry_run_results(&self) {
        for (path, edits) in &self.edits {
            let mut file = File::open(path).expect("Failed to open file");
            let mut source = String::new();
            file.read_to_string(&mut source)
                .expect("Failed to read file");

            println!("File: {:?}", path);
            println!("---");

            for edit in edits {
                let start_line = source[..edit.start].lines().count();
                let end_line = source[..edit.end].lines().count();
                let context_start = source[..edit.start].rfind('\n').map_or(0, |i| i + 1);
                let context_end = source[edit.end..]
                    .find('\n')
                    .map_or(source.len(), |i| edit.end + i);

                println!("Lines {}-{}:", start_line, end_line);
                println!("- {}", &source[context_start..context_end]);
                println!(
                    "+ {}{}{}",
                    &source[context_start..edit.start],
                    edit.replacement,
                    &source[edit.end..context_end]
                );
                println!();
            }

            println!("---\n");
        }
    }

    fn display_callers(&self) {
        println!("Callers:");
        for (method, callers) in &self.caller_graph {
            println!("  Method: {}", method);
            for caller in callers {
                println!("    Called by: {}", caller);
            }
        }
    }

    pub fn apply_edits(&mut self) -> Result<()> {
        let mut changed_files = 0;
        for (path, edits) in self.edits.iter_mut() {
            let mut file = File::open(path).context("Failed to open file")?;
            let mut source = String::new();
            file.read_to_string(&mut source)
                .context("Failed to read file")?;

            let mut new_source = source.clone();
            edits.sort_by(|a, b| b.start.cmp(&a.start));
            for edit in edits.iter() {
                new_source.replace_range(edit.start..edit.end, &edit.replacement);
            }

            std::fs::write(path, new_source).context("Failed to write file")?;
            changed_files += 1;
        }
        println!("Changed {} files", changed_files);
        Ok(())
    }

    fn function_query(&self) -> &'static str {
        r#"
            [
              (function_item
                name: (identifier) @function_name
                parameters: (parameters
                  (parameter
                    pattern: (identifier) @param_name
                    type: (reference_type
                      (mutable_specifier)?
                      type: (type_identifier) @param_type
                    )
                  ) @target_param
                )
                body: (block) @function_body
              )
              (#eq? @param_type "WindowContext")
              (function_signature_item
                name: (identifier) @function_name
                parameters: (parameters
                  (parameter
                    pattern: (identifier) @param_name
                    type: (reference_type
                      (mutable_specifier)?
                      type: (type_identifier) @param_type
                    )
                  ) @target_param
                )
              )
              (#eq? @param_type "WindowContext")
              (function_type
                parameters: (parameters
                  (reference_type
                    (mutable_specifier)?
                    type: (generic_type
                      type: (type_identifier) @param_type
                      type_arguments: (type_arguments
                        (lifetime)?
                      )
                    )
                  ) @target_param
                )
              )
              (#eq? @param_type "WindowContext")
              (impl_item
                body: (declaration_list
                  (function_item
                    name: (identifier) @function_name
                    parameters: (parameters
                      (self_parameter)?
                      (parameter
                        type: (reference_type
                          (mutable_specifier)?
                          type: (type_identifier) @param_type
                        )
                      ) @target_param
                    )
                    body: (block) @function_body
                  )
                )
              )
              (#eq? @param_type "WindowContext")
            ]
        "#
    }

    fn call_query(&self) -> &'static str {
        r#"
        (call_expression
            function: [
                (identifier) @method
                (scoped_identifier) @object
                (field_expression
                    value: [
                        (identifier) @object
                        (field_expression) @object
                        (self) @object
                        (call_expression)
                    ]
                    field: [
                        (field_identifier) @method
                        (integer_literal) @field_index
                    ]
                )
            ]
            arguments: (arguments) @args
        )
        "#
    }

    fn imports_query(&self) -> &'static str {
        r#"
        (use_declaration
          argument: [
            (scoped_identifier
              path: (_) @path
              name: (identifier) @import_name
            )
            (scoped_use_list
              path: (_) @path
              list: (use_list
                (identifier) @import_name
              )
            )
            (use_list
              (identifier) @import_name
            )
          ]
          (#match? @import_name "^(WindowContext|Window|AppContext)$")
        )
        "#
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
        Occurrence {
            range: OccurenceRange {
                row: occurrence.range[0] as usize,
                start_column: occurrence.range[1] as usize,
                end_column: occurrence.range[2] as usize,
            },
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
    let args = Args::parse();

    let path = std::fs::canonicalize(&args.path)?;
    let _relative_path = path.strip_prefix(std::env::current_dir()?).unwrap_or(&path);

    let refactor = Refactor::new(path);
    let mut refactor = refactor?;
    refactor.process()?;

    if args.dry {
        refactor.display_callers();
        // refactor.display_dry_run_results();
    } else {
        refactor.apply_edits()?;
    }

    // TODO: Implement the refactoring logic using the Refactor struct

    Ok(())
}
