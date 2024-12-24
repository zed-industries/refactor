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
    io::Read,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, LazyLock},
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Parser, Query, QueryCursor, QueryMatch};

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
    caller_graph: HashMap<Symbol, BTreeSet<Symbol>>,
    transitive_window_context_callers: HashSet<Symbol>,
    window_methods: HashMap<&'static str, bool>,
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
    pub range: Vec<i32>,
    pub symbol: Symbol,
    pub symbol_roles: i32,
    pub override_documentation: Vec<Arc<str>>,
    pub syntax_kind: SyntaxKind,
    pub diagnostics: Vec<Diagnostic>,
    pub enclosing_range: Vec<i32>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(Arc<str>);

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
    pub symbol: Symbol,
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
    pub fn new(path: &PathBuf) -> Result<Self> {
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
            window_methods: Self::build_window_methods(),
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

    pub fn process(&mut self, path: &PathBuf) -> Result<()> {
        for entry in walkdir::WalkDir::new(path)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_type().is_file() && e.path().extension() == Some(std::ffi::OsStr::new("rs"))
            })
        {
            let file_path = entry.path();
            let relative_path = file_path
                .strip_prefix(&self.index_folder)
                .unwrap_or(file_path);
            let mut file = File::open(file_path)?;
            let mut source = String::new();
            file.read_to_string(&mut source)?;

            let tree = self.parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            self.track_callers(relative_path, &source, root_node)?;
            self.process_functions(relative_path, &source, root_node);
            // self.process_imports(relative_path, &source, root_node);
        }

        self.trace_window_context_callers();

        Ok(())
    }

    fn track_callers(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        root_node: tree_sitter::Node,
    ) -> Result<()> {
        for_each_function_that_has_window_context_param(root_node, source, |match_| {
            self.track_callers_in_function(relative_path, source, match_);
        });

        Ok(())
    }

    fn track_callers_in_function(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        match_: FunctionMatch,
    ) {
        let Some((name_node, body_node)) = match_.name.zip(match_.body) else {
            return;
        };

        let function_occurrence =
            match self.find_occurrence(&name_node.start_position(), relative_path) {
                Ok(occurrence) => occurrence,
                Err(e) => {
                    eprintln!("{}", e);
                    return;
                }
            };

        let parent_symbol = function_occurrence.symbol.clone();

        for_each_call(body_node, source, |call| {
            if let Some(method) = call.method {
                match self.find_occurrence(&method.start_position(), relative_path) {
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
        });
    }

    fn trace_window_context_callers(&mut self) {
        let mut to_process = Vec::new();
        for (callee, callers) in &self.caller_graph {
            if callee.0.contains("WindowContext#") {
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
            println!("  {:?}", caller);
        }
    }

    fn process_functions(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        root_node: tree_sitter::Node,
    ) {
        for_each_function_that_has_window_context_param(root_node, source, |match_| {
            if match_.is_function_type {
                self.record_node_replacement(
                    relative_path,
                    &match_.target_param,
                    "&mut Window, &mut AppContext",
                );
            } else {
                self.record_node_replacement(
                    relative_path,
                    &match_.target_param,
                    "window: &mut Window, cx: &mut AppContext",
                );
                if let Some((body_node, param_name)) = match_.body.zip(match_.param_name) {
                    let param_name = &source[param_name.byte_range()];
                    for_each_call(body_node, source, |call| {
                        self.process_call(relative_path, source, param_name, call)
                    });
                }
            }
        });
    }

    fn process_call(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        param_name: &str,
        call: CallMatch,
    ) {
        let mut cursor = call.args.walk();
        for arg in call.args.children(&mut cursor) {
            if arg.kind() != "," {
                let arg_text = &source[arg.byte_range()];
                if arg_text == param_name {
                    self.process_cx_arg(relative_path, source, param_name, &call, &arg);
                }
            }
        }
    }

    fn process_cx_arg(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        param_name: &str,
        call: &CallMatch,
        arg: &tree_sitter::Node,
    ) {
        if let Some((object, method)) = call.object.zip(call.method) {
            let object_text = &source[object.byte_range()];
            let method_name = &source[method.byte_range()];

            // If this a method call on the context itself, decide whether to call it on window or cx.
            if object_text == param_name {
                if let Some(&needs_cx) = self.window_methods.get(method_name) {
                    self.record_node_replacement(relative_path, &object, "window");
                    if needs_cx {
                        self.record_insertion_after_node(relative_path, arg, ", cx");
                    }
                } else {
                    self.record_insertion_before_node(relative_path, arg, "window, ");
                }
            } else {
                // For any other call, pass a window and cx through instead of a cx.
                // TODO! Check the definition of the method being called to decide if we need to pass window.
                self.record_node_replacement(relative_path, arg, "window, cx");
            }
        } else {
            // For any other call, pass a window and cx through instead of a cx.
            // TODO! Check the definition of the method being called to decide if we need to pass window.
            self.record_node_replacement(relative_path, arg, "window, cx");
        }
    }

    fn process_imports(
        &mut self,
        relative_path: &std::path::Path,
        source: &str,
        root_node: tree_sitter::Node,
    ) {
        let mut window_context_import: Option<Range<usize>> = None;
        let mut window_imported = false;
        let mut app_context_imported = false;
        for_each_import(root_node, source, |match_| {
            match &source[match_.import_name.byte_range()] {
                "WindowContext" => window_context_import = Some(match_.import_name.byte_range()),
                "Window" => window_imported = true,
                "AppContext" => app_context_imported = true,
                _ => {}
            }
        });

        if let Some(window_context_import) = window_context_import {
            if !window_imported && !app_context_imported {
                // Replace WindowContext with Window and AppContext
                self.record_edit(relative_path, window_context_import, "{Window, AppContext}");
            } else if !window_imported {
                // Only add Window
                self.record_edit(relative_path, window_context_import, "Window");
            } else if !app_context_imported {
                // Only add AppContext
                self.record_edit(relative_path, window_context_import, "AppContext");
            }
            // If both are already imported, we don't need to do anything
        }
    }

    fn find_occurrence(
        &self,
        point: &tree_sitter::Point,
        relative_path: &std::path::Path,
    ) -> Result<&Occurrence> {
        let line = point.row;
        let column = point.column;
        let document = self
            .index
            .documents
            .binary_search_by(|doc| Path::new(doc.relative_path.as_ref()).cmp(relative_path))
            .map_err(|_| anyhow::anyhow!("Document not found: {:?}", relative_path))?;
        let doc = &self.index.documents[document];

        let occurrence_index = doc
            .occurrences
            .binary_search_by(|occ| {
                let start_line = occ.range[0] as usize;
                let start_column = occ.range[1] as usize;

                if start_line != line {
                    start_line.cmp(&line)
                } else {
                    start_column.cmp(&column)
                }
            })
            .map_err(|_| {
                anyhow::anyhow!(
                    "Occurrence not found at {:?}:{}:{}",
                    relative_path,
                    line,
                    column
                )
            })?;

        Ok(&doc.occurrences[occurrence_index])
    }

    pub fn display_dry_run_results(&self) {
        for (path, edits) in &self.edits {
            let path = &self.index_folder.join(path);
            let mut file = File::open(path)
                .with_context(|| format!("Failed to open file {:?}", path))
                .unwrap();
            let mut source = String::new();
            file.read_to_string(&mut source)
                .with_context(|| format!("Failed to read file {:?}", path))
                .unwrap();

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

    fn display_callers(&self) {
        println!("Callers:");
        for (method, callers) in &self.caller_graph {
            println!("  Method: {:?}", method);
            for caller in callers {
                println!("    Called by: {:?}", caller);
            }
        }
    }

    fn record_node_replacement(
        &mut self,
        relative_path: &std::path::Path,
        node: &tree_sitter::Node,
        replacement: &str,
    ) {
        self.record_edit(relative_path, node.byte_range(), replacement);
    }

    fn record_insertion_before_node(
        &mut self,
        relative_path: &std::path::Path,
        node: &tree_sitter::Node,
        insertion: &str,
    ) {
        let start = node.start_byte();
        self.record_edit(relative_path, start..start, insertion);
    }

    fn record_insertion_after_node(
        &mut self,
        relative_path: &std::path::Path,
        node: &tree_sitter::Node,
        insertion: &str,
    ) {
        let end = node.end_byte();
        self.record_edit(relative_path, end..end, insertion);
    }

    fn record_edit(
        &mut self,
        relative_path: &std::path::Path,
        byte_range: Range<usize>,
        replacement: &str,
    ) {
        self.edits
            .entry(relative_path.to_path_buf())
            .or_default()
            .push(Edit {
                byte_range,
                replacement: replacement.to_string(),
            });
    }

    pub fn apply_edits(&mut self) -> Result<()> {
        let mut changed_files = 0;
        for (path, edits) in self.edits.iter_mut() {
            let path = &self.index_folder.join(path);
            let mut file =
                File::open(path).with_context(|| format!("Failed to open file {:?}", path))?;
            let mut source = String::new();
            file.read_to_string(&mut source)
                .with_context(|| format!("Failed to read file {:?}", path))?;

            let mut new_source = source.clone();
            edits.sort_by(|a, b| b.byte_range.start.cmp(&a.byte_range.start));
            for edit in edits.iter() {
                new_source.replace_range(edit.byte_range.clone(), &edit.replacement);
            }

            std::fs::write(path, new_source)
                .with_context(|| format!("Failed to write file {:?}", path))?;
            changed_files += 1;
        }
        println!("Changed {} files", changed_files);
        Ok(())
    }

    fn build_window_methods() -> HashMap<&'static str, bool> {
        [
            ("window_handle", false),
            ("refresh", true),
            ("notify", true),
            ("remove_window", true),
            ("focused", true),
            ("focus", true),
            ("blur", true),
            ("disable_focus", true),
            ("text_system", false),
            ("text_style", true),
            ("is_maximized", false),
            ("request_decorations", false),
            ("start_window_resize", false),
            ("window_bounds", false),
            ("dispatch_action", true),
            ("defer", true),
            ("observe", true),
            ("subscribe", true),
            ("observe_release", true),
            ("to_async", true),
            ("on_next_frame", true),
            ("request_animation_frame", true),
            ("spawn", true),
            ("bounds_changed", true),
            ("bounds", false),
            ("is_fullscreen", false),
            ("appearance_changed", true),
            ("appearance", false),
            ("viewport_size", false),
            ("is_window_active", false),
            ("is_window_hovered", true),
            ("zoom_window", false),
            ("show_window_menu", false),
            ("start_window_move", false),
            ("set_client_inset", false),
            ("window_decorations", false),
            ("window_controls", false),
            ("set_window_title", false),
            ("set_app_id", false),
            ("set_background_appearance", false),
            ("set_window_edited", false),
            ("display", true),
            ("show_character_palette", false),
            ("scale_factor", false),
            ("rem_size", true),
            ("set_rem_size", true),
            ("with_rem_size", true),
            ("line_height", true),
            ("prevent_default", true),
            ("default_prevented", false),
            ("is_action_available", true),
            ("mouse_position", false),
            ("modifiers", false),
            ("complete_frame", false),
            ("draw", true),
            ("present", false),
            ("draw_roots", true),
            ("prepaint_tooltip", true),
            ("prepaint_deferred_draws", true),
            ("paint_deferred_draws", true),
            ("prepaint_index", false),
            ("reuse_prepaint", true),
            ("paint_index", false),
            ("reuse_paint", true),
            ("with_text_style", true),
            ("set_cursor_style", true),
            ("set_tooltip", true),
            ("with_content_mask", true),
            ("with_element_offset", true),
            ("with_absolute_element_offset", true),
            ("with_element_opacity", true),
            ("transact", true),
            ("request_autoscroll", true),
            ("take_autoscroll", true),
            ("use_asset", true),
            ("element_offset", false),
            ("element_opacity", false),
            ("content_mask", false),
            ("with_element_namespace", true),
            ("with_element_state", true),
            ("with_optional_element_state", true),
            ("defer_draw", true),
            ("paint_layer", true),
            ("paint_shadows", true),
            ("paint_quad", true),
            ("paint_path", true),
            ("paint_underline", true),
            ("paint_strikethrough", true),
            ("paint_glyph", true),
            ("paint_emoji", true),
            ("paint_svg", true),
            ("paint_image", true),
            ("paint_surface", true),
            ("drop_image", true),
            ("request_layout", true),
            ("request_measured_layout", true),
            ("compute_layout", true),
            ("layout_bounds", true),
            ("insert_hitbox", true),
            ("set_key_context", true),
            ("set_focus_handle", true),
            ("set_view_id", true),
            ("parent_view_id", false),
            ("handle_input", true),
            ("on_mouse_event", true),
            ("on_key_event", true),
            ("on_modifiers_changed", true),
            ("on_focus_in", true),
            ("on_focus_out", true),
            ("reset_cursor_style", true),
            ("dispatch_keystroke", true),
            ("keystroke_text_for", true),
            ("dispatch_event", true),
            ("dispatch_mouse_event", true),
            ("dispatch_key_event", true),
            ("has_pending_keystrokes", false),
            ("clear_pending_keystrokes", true),
            ("pending_input_keystrokes", false),
            ("replay_pending_input", true),
            ("dispatch_action_on_node", true),
            ("observe_global", true),
            ("activate_window", false),
            ("minimize_window", false),
            ("toggle_fullscreen", false),
            ("invalidate_character_coordinates", true),
            ("prompt", true),
            ("context_stack", true),
            ("available_actions", true),
            ("bindings_for_action", true),
            ("all_bindings_for_input", true),
            ("bindings_for_action_in", true),
            ("listener_for", true),
            ("handler_for", true),
            ("on_window_should_close", true),
            ("on_action", true),
            ("gpu_specs", false),
            ("get_raw_handle", false),
        ]
        .iter()
        .cloned()
        .collect()
    }
}

fn for_each_function_that_has_window_context_param<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(FunctionMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&FUNCTION_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(FunctionMatch::from_match(match_));
    }
}

static FUNCTION_QUERY: LazyLock<FunctionsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), FUNCTION_QUERY_STR)
        .expect("Failed to create functions query");
    FunctionsQuery {
        name: query.capture_index_for_name("function_name").unwrap(),
        body: query.capture_index_for_name("function_body").unwrap(),
        param_name: query.capture_index_for_name("param_name").unwrap(),
        param_type: query.capture_index_for_name("param_type").unwrap(),
        target_param: query.capture_index_for_name("target_param").unwrap(),
        query,
    }
});

struct FunctionsQuery {
    query: Query,
    name: u32,
    body: u32,
    param_name: u32,
    param_type: u32,
    target_param: u32,
}

struct FunctionMatch<'a> {
    name: Option<tree_sitter::Node<'a>>,
    body: Option<tree_sitter::Node<'a>>,
    param_name: Option<tree_sitter::Node<'a>>,
    param_type: tree_sitter::Node<'a>,
    target_param: tree_sitter::Node<'a>,
    is_function_type: bool,
}

impl<'a> FunctionMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &FUNCTION_QUERY;
        let mut name = None;
        let mut body = None;
        let mut param_name = None;
        let mut param_type = None;
        let mut target_param = None;
        let mut is_function_type = false;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if capture.node.kind() == "function_type" {
                is_function_type = true;
            }
            if i == query.name {
                name = node;
            } else if i == query.body {
                body = node;
            } else if i == query.param_name {
                param_name = node;
            } else if i == query.param_type {
                param_type = node;
            } else if i == query.target_param {
                target_param = node;
            }
        }

        FunctionMatch {
            name,
            body,
            param_name,
            param_type: param_type.unwrap(),
            target_param: target_param.unwrap(),
            is_function_type,
        }
    }
}

const FUNCTION_QUERY_STR: &str = r#"
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
"#;

fn for_each_call<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(CallMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&CALLS_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(CallMatch::from_match(match_));
    }
}

static CALLS_QUERY: LazyLock<CallsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), CALLS_QUERY_STR)
        .expect("Failed to create calls query");
    CallsQuery {
        method: query.capture_index_for_name("method").unwrap(),
        object: query.capture_index_for_name("object").unwrap(),
        args: query.capture_index_for_name("args").unwrap(),
        query,
    }
});

struct CallsQuery {
    query: Query,
    method: u32,
    object: u32,
    args: u32,
}

struct CallMatch<'a> {
    method: Option<tree_sitter::Node<'a>>,
    object: Option<tree_sitter::Node<'a>>,
    args: tree_sitter::Node<'a>,
}

impl<'a> CallMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &CALLS_QUERY;
        let mut method = None;
        let mut object = None;
        let mut args = None;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if i == query.method {
                method = node;
            } else if i == query.object {
                object = node;
            } else if i == query.args {
                args = node;
            }
        }

        CallMatch {
            method,
            object,
            args: args.unwrap(),
        }
    }
}

const CALLS_QUERY_STR: &str = r#"
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
    "#;

fn for_each_import<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(ImportMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&IMPORTS_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(ImportMatch::from_match(match_));
    }
}

static IMPORTS_QUERY: LazyLock<ImportsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), IMPORTS_QUERY_STR)
        .expect("Failed to create imports query");
    ImportsQuery {
        path: query.capture_index_for_name("path").unwrap(),
        import_name: query.capture_index_for_name("import_name").unwrap(),
        query,
    }
});

struct ImportsQuery {
    query: Query,
    path: u32,
    import_name: u32,
}

struct ImportMatch<'a> {
    path: Option<tree_sitter::Node<'a>>,
    import_name: tree_sitter::Node<'a>,
}

impl<'a> ImportMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &IMPORTS_QUERY;
        let mut path = None;
        let mut import_name = None;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if i == query.path {
                path = node;
            } else if i == query.import_name {
                import_name = node;
            }
        }

        ImportMatch {
            path,
            import_name: import_name.unwrap(),
        }
    }
}

const IMPORTS_QUERY_STR: &str = r#"
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
    "#;

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
            range: occurrence.range,
            symbol: Symbol(occurrence.symbol.into()),
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
            symbol: Symbol(relationship.symbol.into()),
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

    let refactor = Refactor::new(&path);
    let mut refactor = refactor?;
    refactor.process(&path)?;

    if args.dry {
        refactor.display_callers();
        // refactor.display_dry_run_results();
    } else {
        refactor.apply_edits()?;
    }

    // TODO: Implement the refactoring logic using the Refactor struct

    Ok(())
}

// // Main SCIP structures

// pub struct Index {
//     pub metadata: MessageField<Metadata>,
//     pub documents: Vec<Document>,
//     pub external_symbols: Vec<SymbolInformation>,
// }

// pub struct Metadata {
//     pub version: i32,  // Actually an enum: ProtocolVersion
//     pub tool_info: MessageField<ToolInfo>,
//     pub project_root: String,
//     pub text_document_encoding: i32,  // Actually an enum: TextEncoding
// }

// pub struct ToolInfo {
//     pub name: String,
//     pub version: String,
//     pub arguments: Vec<String>,
// }

// pub struct Document {
//     pub language: String,
//     pub relative_path: String,
//     pub occurrences: Vec<Occurrence>,
//     pub symbols: Vec<SymbolInformation>,
//     pub text: String,
//     pub position_encoding: i32,  // Actually an enum: PositionEncoding
// }

// pub struct Occurrence {
//     pub range: Vec<i32>,  // [start_line, start_character, end_line, end_character]
//     pub symbol: String,
//     pub symbol_roles: i32,  // Bitfield of SymbolRole
//     pub override_documentation: Vec<String>,
//     pub syntax_kind: i32,  // Actually an enum: SyntaxKind
//     pub diagnostics: Vec<Diagnostic>,
//     pub enclosing_range: Vec<i32>,
// }

// pub struct SymbolInformation {
//     pub symbol: String,
//     pub documentation: Vec<String>,
//     pub relationships: Vec<Relationship>,
//     pub kind: i32,  // Actually an enum: Kind
//     pub display_name: String,
//     pub signature_documentation: MessageField<Document>,
//     pub enclosing_symbol: String,
// }

// pub struct Relationship {
//     pub symbol: String,
//     pub is_reference: bool,
//     pub is_implementation: bool,
//     pub is_type_definition: bool,
//     pub is_definition: bool,
// }

// pub struct Diagnostic {
//     pub severity: i32,  // Actually an enum: Severity
//     pub code: String,
//     pub message: String,
//     pub source: String,
//     pub tags: Vec<i32>,  // Actually a vec of enum: DiagnosticTag
// }

// // Important enums

// pub enum ProtocolVersion {
//     UnspecifiedProtocolVersion = 0,
// }

// pub enum TextEncoding {
//     UnspecifiedTextEncoding = 0,
//     UTF8 = 1,
//     UTF16 = 2,
// }

// pub enum PositionEncoding {
//     UnspecifiedPositionEncoding = 0,
//     UTF8CodeUnitOffsetFromLineStart = 1,
//     UTF16CodeUnitOffsetFromLineStart = 2,
//     UTF32CodeUnitOffsetFromLineStart = 3,
// }

// pub enum SymbolRole {
//     UnspecifiedSymbolRole = 0,
//     Definition = 1,
//     Import = 2,
//     WriteAccess = 4,
//     ReadAccess = 8,
//     Generated = 16,
//     Test = 32,
//     ForwardDefinition = 64,
// }

// pub enum SyntaxKind {
//     UnspecifiedSyntaxKind = 0,
//     // ... many more variants ...
// }

// pub enum Severity {
//     UnspecifiedSeverity = 0,
//     Error = 1,
//     Warning = 2,
//     Information = 3,
//     Hint = 4,
// }

// pub enum DiagnosticTag {
//     UnspecifiedDiagnosticTag = 0,
//     Unnecessary = 1,
//     Deprecated = 2,
// }

// // Key methods for working with SCIP

// impl Index {
//     pub fn parse_from_bytes(data: &[u8]) -> Result<Self, protobuf::Error>;
// }

// // Symbol parsing and formatting (from scip::symbol module)

// pub fn parse_symbol(symbol: &str) -> Result<Symbol, SymbolError>;
// pub fn format_symbol(symbol: Symbol) -> String;
// pub fn format_symbol_with(symbol: Symbol, options: SymbolFormatOptions) -> String;

// pub struct Symbol {
//     pub scheme: String,
//     pub package: MessageField<Package>,
//     pub descriptors: Vec<Descriptor>,
// }

// pub struct Package {
//     pub manager: String,
//     pub name: String,
//     pub version: String,
// }

// pub struct Descriptor {
//     pub name: String,
//     pub disambiguator: String,
//     pub suffix: i32,  // Actually an enum: Suffix
// }

// pub enum Suffix {
//     UnspecifiedSuffix = 0,
//     Namespace = 1,
//     Package = 1,
//     Type = 2,
//     Term = 3,
//     Method = 4,
//     TypeParameter = 5,
//     Parameter = 6,
//     Meta = 7,
//     Local = 8,
//     Macro = 9,
// }

// pub struct SymbolFormatOptions {
//     pub include_scheme: bool,
//     pub include_package_manager: bool,
//     pub include_package_name: bool,
//     pub include_package_version: bool,
//     pub include_descriptor: bool,
// }

// // Utility functions

// pub fn is_global_symbol(sym: &str) -> bool;
// pub fn is_local_symbol(sym: &str) -> bool;
// pub fn try_parse_local_symbol(sym: &str) -> Result<Option<&str>, SymbolError>;

// // Writing SCIP index to file

// pub fn write_message_to_file<P>(
//     path: P,
//     msg: impl protobuf::Message,
// ) -> Result<(), Box<dyn std::error::Error>>
// where
//     P: AsRef<std::path::Path>;

// This file is generated by rust-protobuf 3.7.1. Do not edit
// .proto file is parsed by protoc --rs_out=...
// @generated

// // https://github.com/rust-lang/rust-clippy/issues/702
// #![allow(unknown_lints)]
// #![allow(clippy::all)]

// #![allow(unused_attributes)]
// #![cfg_attr(rustfmt, rustfmt::skip)]

// #![allow(dead_code)]
// #![allow(missing_docs)]
// #![allow(non_camel_case_types)]
// #![allow(non_snake_case)]
// #![allow(non_upper_case_globals)]
// #![allow(trivial_casts)]
// #![allow(unused_results)]
// #![allow(unused_mut)]

// //! Generated file from `scip.proto`

// /// Generated files are compatible only with the same version
// /// of protobuf runtime.
// const _PROTOBUF_VERSION_CHECK: () = ::protobuf::VERSION_3_7_1;

// ///  Index represents a complete SCIP index for a workspace this is rooted at a
// ///  single directory. An Index message payload can have a large memory footprint
// ///  and it's therefore recommended to emit and consume an Index payload one field
// ///  value at a time. To permit streaming consumption of an Index payload, the
// ///  `metadata` field must appear at the start of the stream and must only appear
// ///  once in the stream. Other field values may appear in any order.
// // @@protoc_insertion_point(message:scip.Index)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Index {
//     // message fields
//     ///  Metadata about this index.
//     // @@protoc_insertion_point(field:scip.Index.metadata)
//     pub metadata: ::protobuf::MessageField<Metadata>,
//     ///  Documents that belong to this index.
//     // @@protoc_insertion_point(field:scip.Index.documents)
//     pub documents: ::std::vec::Vec<Document>,
//     ///  (optional) Symbols that are referenced from this index but are defined in
//     ///  an external package (a separate `Index` message). Leave this field empty
//     ///  if you assume the external package will get indexed separately. If the
//     ///  external package won't get indexed for some reason then you can use this
//     ///  field to provide hover documentation for those external symbols.
//     // @@protoc_insertion_point(field:scip.Index.external_symbols)
//     pub external_symbols: ::std::vec::Vec<SymbolInformation>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Index.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Index {
//     fn default() -> &'a Index {
//         <Index as ::protobuf::Message>::default_instance()
//     }
// }

// impl Index {
//     pub fn new() -> Index {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(3);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_message_field_accessor::<_, Metadata>(
//             "metadata",
//             |m: &Index| { &m.metadata },
//             |m: &mut Index| { &mut m.metadata },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "documents",
//             |m: &Index| { &m.documents },
//             |m: &mut Index| { &mut m.documents },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "external_symbols",
//             |m: &Index| { &m.external_symbols },
//             |m: &mut Index| { &mut m.external_symbols },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Index>(
//             "Index",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Index {
//     const NAME: &'static str = "Index";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     ::protobuf::rt::read_singular_message_into_field(is, &mut self.metadata)?;
//                 },
//                 18 => {
//                     self.documents.push(is.read_message()?);
//                 },
//                 26 => {
//                     self.external_symbols.push(is.read_message()?);
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if let Some(v) = self.metadata.as_ref() {
//             let len = v.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         }
//         for value in &self.documents {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         for value in &self.external_symbols {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if let Some(v) = self.metadata.as_ref() {
//             ::protobuf::rt::write_message_field_with_cached_size(1, v, os)?;
//         }
//         for v in &self.documents {
//             ::protobuf::rt::write_message_field_with_cached_size(2, v, os)?;
//         };
//         for v in &self.external_symbols {
//             ::protobuf::rt::write_message_field_with_cached_size(3, v, os)?;
//         };
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Index {
//         Index::new()
//     }

//     fn clear(&mut self) {
//         self.metadata.clear();
//         self.documents.clear();
//         self.external_symbols.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Index {
//         static instance: Index = Index {
//             metadata: ::protobuf::MessageField::none(),
//             documents: ::std::vec::Vec::new(),
//             external_symbols: ::std::vec::Vec::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Index {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Index").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Index {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Index {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// // @@protoc_insertion_point(message:scip.Metadata)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Metadata {
//     // message fields
//     ///  Which version of this protocol was used to generate this index?
//     // @@protoc_insertion_point(field:scip.Metadata.version)
//     pub version: ::protobuf::EnumOrUnknown<ProtocolVersion>,
//     ///  Information about the tool that produced this index.
//     // @@protoc_insertion_point(field:scip.Metadata.tool_info)
//     pub tool_info: ::protobuf::MessageField<ToolInfo>,
//     ///  URI-encoded absolute path to the root directory of this index. All
//     ///  documents in this index must appear in a subdirectory of this root
//     ///  directory.
//     // @@protoc_insertion_point(field:scip.Metadata.project_root)
//     pub project_root: ::std::string::String,
//     ///  Text encoding of the source files on disk that are referenced from
//     ///  `Document.relative_path`. This value is unrelated to the `Document.text`
//     ///  field, which is a Protobuf string and hence must be UTF-8 encoded.
//     // @@protoc_insertion_point(field:scip.Metadata.text_document_encoding)
//     pub text_document_encoding: ::protobuf::EnumOrUnknown<TextEncoding>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Metadata.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Metadata {
//     fn default() -> &'a Metadata {
//         <Metadata as ::protobuf::Message>::default_instance()
//     }
// }

// impl Metadata {
//     pub fn new() -> Metadata {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(4);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "version",
//             |m: &Metadata| { &m.version },
//             |m: &mut Metadata| { &mut m.version },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_message_field_accessor::<_, ToolInfo>(
//             "tool_info",
//             |m: &Metadata| { &m.tool_info },
//             |m: &mut Metadata| { &mut m.tool_info },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "project_root",
//             |m: &Metadata| { &m.project_root },
//             |m: &mut Metadata| { &mut m.project_root },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "text_document_encoding",
//             |m: &Metadata| { &m.text_document_encoding },
//             |m: &mut Metadata| { &mut m.text_document_encoding },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Metadata>(
//             "Metadata",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Metadata {
//     const NAME: &'static str = "Metadata";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 8 => {
//                     self.version = is.read_enum_or_unknown()?;
//                 },
//                 18 => {
//                     ::protobuf::rt::read_singular_message_into_field(is, &mut self.tool_info)?;
//                 },
//                 26 => {
//                     self.project_root = is.read_string()?;
//                 },
//                 32 => {
//                     self.text_document_encoding = is.read_enum_or_unknown()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if self.version != ::protobuf::EnumOrUnknown::new(ProtocolVersion::UnspecifiedProtocolVersion) {
//             my_size += ::protobuf::rt::int32_size(1, self.version.value());
//         }
//         if let Some(v) = self.tool_info.as_ref() {
//             let len = v.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         }
//         if !self.project_root.is_empty() {
//             my_size += ::protobuf::rt::string_size(3, &self.project_root);
//         }
//         if self.text_document_encoding != ::protobuf::EnumOrUnknown::new(TextEncoding::UnspecifiedTextEncoding) {
//             my_size += ::protobuf::rt::int32_size(4, self.text_document_encoding.value());
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if self.version != ::protobuf::EnumOrUnknown::new(ProtocolVersion::UnspecifiedProtocolVersion) {
//             os.write_enum(1, ::protobuf::EnumOrUnknown::value(&self.version))?;
//         }
//         if let Some(v) = self.tool_info.as_ref() {
//             ::protobuf::rt::write_message_field_with_cached_size(2, v, os)?;
//         }
//         if !self.project_root.is_empty() {
//             os.write_string(3, &self.project_root)?;
//         }
//         if self.text_document_encoding != ::protobuf::EnumOrUnknown::new(TextEncoding::UnspecifiedTextEncoding) {
//             os.write_enum(4, ::protobuf::EnumOrUnknown::value(&self.text_document_encoding))?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Metadata {
//         Metadata::new()
//     }

//     fn clear(&mut self) {
//         self.version = ::protobuf::EnumOrUnknown::new(ProtocolVersion::UnspecifiedProtocolVersion);
//         self.tool_info.clear();
//         self.project_root.clear();
//         self.text_document_encoding = ::protobuf::EnumOrUnknown::new(TextEncoding::UnspecifiedTextEncoding);
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Metadata {
//         static instance: Metadata = Metadata {
//             version: ::protobuf::EnumOrUnknown::from_i32(0),
//             tool_info: ::protobuf::MessageField::none(),
//             project_root: ::std::string::String::new(),
//             text_document_encoding: ::protobuf::EnumOrUnknown::from_i32(0),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Metadata {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Metadata").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Metadata {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Metadata {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// // @@protoc_insertion_point(message:scip.ToolInfo)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct ToolInfo {
//     // message fields
//     ///  Name of the indexer that produced this index.
//     // @@protoc_insertion_point(field:scip.ToolInfo.name)
//     pub name: ::std::string::String,
//     ///  Version of the indexer that produced this index.
//     // @@protoc_insertion_point(field:scip.ToolInfo.version)
//     pub version: ::std::string::String,
//     ///  Command-line arguments that were used to invoke this indexer.
//     // @@protoc_insertion_point(field:scip.ToolInfo.arguments)
//     pub arguments: ::std::vec::Vec<::std::string::String>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.ToolInfo.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a ToolInfo {
//     fn default() -> &'a ToolInfo {
//         <ToolInfo as ::protobuf::Message>::default_instance()
//     }
// }

// impl ToolInfo {
//     pub fn new() -> ToolInfo {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(3);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "name",
//             |m: &ToolInfo| { &m.name },
//             |m: &mut ToolInfo| { &mut m.name },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "version",
//             |m: &ToolInfo| { &m.version },
//             |m: &mut ToolInfo| { &mut m.version },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "arguments",
//             |m: &ToolInfo| { &m.arguments },
//             |m: &mut ToolInfo| { &mut m.arguments },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<ToolInfo>(
//             "ToolInfo",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for ToolInfo {
//     const NAME: &'static str = "ToolInfo";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.name = is.read_string()?;
//                 },
//                 18 => {
//                     self.version = is.read_string()?;
//                 },
//                 26 => {
//                     self.arguments.push(is.read_string()?);
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.name.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.name);
//         }
//         if !self.version.is_empty() {
//             my_size += ::protobuf::rt::string_size(2, &self.version);
//         }
//         for value in &self.arguments {
//             my_size += ::protobuf::rt::string_size(3, &value);
//         };
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.name.is_empty() {
//             os.write_string(1, &self.name)?;
//         }
//         if !self.version.is_empty() {
//             os.write_string(2, &self.version)?;
//         }
//         for v in &self.arguments {
//             os.write_string(3, &v)?;
//         };
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> ToolInfo {
//         ToolInfo::new()
//     }

//     fn clear(&mut self) {
//         self.name.clear();
//         self.version.clear();
//         self.arguments.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static ToolInfo {
//         static instance: ToolInfo = ToolInfo {
//             name: ::std::string::String::new(),
//             version: ::std::string::String::new(),
//             arguments: ::std::vec::Vec::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for ToolInfo {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("ToolInfo").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for ToolInfo {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for ToolInfo {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// ///  Document defines the metadata about a source file on disk.
// // @@protoc_insertion_point(message:scip.Document)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Document {
//     // message fields
//     ///  The string ID for the programming language this file is written in.
//     ///  The `Language` enum contains the names of most common programming languages.
//     ///  This field is typed as a string to permit any programming language, including
//     ///  ones that are not specified by the `Language` enum.
//     // @@protoc_insertion_point(field:scip.Document.language)
//     pub language: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Document.relative_path)
//     pub relative_path: ::std::string::String,
//     ///  Occurrences that appear in this file.
//     // @@protoc_insertion_point(field:scip.Document.occurrences)
//     pub occurrences: ::std::vec::Vec<Occurrence>,
//     ///  Symbols that are "defined" within this document.
//     ///
//     ///  This should include symbols which technically do not have any definition,
//     ///  but have a reference and are defined by some other symbol (see
//     ///  Relationship.is_definition).
//     // @@protoc_insertion_point(field:scip.Document.symbols)
//     pub symbols: ::std::vec::Vec<SymbolInformation>,
//     ///  (optional) Text contents of the this document. Indexers are not expected to
//     ///  include the text by default. It's preferrable that clients read the text
//     ///  contents from the file system by resolving the absolute path from joining
//     ///  `Index.metadata.project_root` and `Document.relative_path`. This field was
//     ///  introduced to support `SymbolInformation.signature_documentation`, but it
//     ///  can be used for other purposes as well, for example testing or when working
//     ///  with virtual/in-memory documents.
//     // @@protoc_insertion_point(field:scip.Document.text)
//     pub text: ::std::string::String,
//     ///  Specifies the encoding used for source ranges in this Document.
//     ///
//     ///  Usually, this will match the type used to index the string type
//     ///  in the indexer's implementation language in O(1) time.
//     ///  - For an indexer implemented in JVM/.NET language or JavaScript/TypeScript,
//     ///    use UTF16CodeUnitOffsetFromLineStart.
//     ///  - For an indexer implemented in Python,
//     ///    use UTF32CodeUnitOffsetFromLineStart.
//     ///  - For an indexer implemented in Go, Rust or C++,
//     ///    use UTF8ByteOffsetFromLineStart.
//     // @@protoc_insertion_point(field:scip.Document.position_encoding)
//     pub position_encoding: ::protobuf::EnumOrUnknown<PositionEncoding>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Document.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Document {
//     fn default() -> &'a Document {
//         <Document as ::protobuf::Message>::default_instance()
//     }
// }

// impl Document {
//     pub fn new() -> Document {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(6);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "language",
//             |m: &Document| { &m.language },
//             |m: &mut Document| { &mut m.language },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "relative_path",
//             |m: &Document| { &m.relative_path },
//             |m: &mut Document| { &mut m.relative_path },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "occurrences",
//             |m: &Document| { &m.occurrences },
//             |m: &mut Document| { &mut m.occurrences },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "symbols",
//             |m: &Document| { &m.symbols },
//             |m: &mut Document| { &mut m.symbols },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "text",
//             |m: &Document| { &m.text },
//             |m: &mut Document| { &mut m.text },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "position_encoding",
//             |m: &Document| { &m.position_encoding },
//             |m: &mut Document| { &mut m.position_encoding },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Document>(
//             "Document",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Document {
//     const NAME: &'static str = "Document";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 34 => {
//                     self.language = is.read_string()?;
//                 },
//                 10 => {
//                     self.relative_path = is.read_string()?;
//                 },
//                 18 => {
//                     self.occurrences.push(is.read_message()?);
//                 },
//                 26 => {
//                     self.symbols.push(is.read_message()?);
//                 },
//                 42 => {
//                     self.text = is.read_string()?;
//                 },
//                 48 => {
//                     self.position_encoding = is.read_enum_or_unknown()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.language.is_empty() {
//             my_size += ::protobuf::rt::string_size(4, &self.language);
//         }
//         if !self.relative_path.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.relative_path);
//         }
//         for value in &self.occurrences {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         for value in &self.symbols {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         if !self.text.is_empty() {
//             my_size += ::protobuf::rt::string_size(5, &self.text);
//         }
//         if self.position_encoding != ::protobuf::EnumOrUnknown::new(PositionEncoding::UnspecifiedPositionEncoding) {
//             my_size += ::protobuf::rt::int32_size(6, self.position_encoding.value());
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.language.is_empty() {
//             os.write_string(4, &self.language)?;
//         }
//         if !self.relative_path.is_empty() {
//             os.write_string(1, &self.relative_path)?;
//         }
//         for v in &self.occurrences {
//             ::protobuf::rt::write_message_field_with_cached_size(2, v, os)?;
//         };
//         for v in &self.symbols {
//             ::protobuf::rt::write_message_field_with_cached_size(3, v, os)?;
//         };
//         if !self.text.is_empty() {
//             os.write_string(5, &self.text)?;
//         }
//         if self.position_encoding != ::protobuf::EnumOrUnknown::new(PositionEncoding::UnspecifiedPositionEncoding) {
//             os.write_enum(6, ::protobuf::EnumOrUnknown::value(&self.position_encoding))?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Document {
//         Document::new()
//     }

//     fn clear(&mut self) {
//         self.language.clear();
//         self.relative_path.clear();
//         self.occurrences.clear();
//         self.symbols.clear();
//         self.text.clear();
//         self.position_encoding = ::protobuf::EnumOrUnknown::new(PositionEncoding::UnspecifiedPositionEncoding);
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Document {
//         static instance: Document = Document {
//             language: ::std::string::String::new(),
//             relative_path: ::std::string::String::new(),
//             occurrences: ::std::vec::Vec::new(),
//             symbols: ::std::vec::Vec::new(),
//             text: ::std::string::String::new(),
//             position_encoding: ::protobuf::EnumOrUnknown::from_i32(0),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Document {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Document").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Document {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Document {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// ///  Symbol is similar to a URI, it identifies a class, method, or a local
// ///  variable. `SymbolInformation` contains rich metadata about symbols such as
// ///  the docstring.
// ///
// ///  Symbol has a standardized string representation, which can be used
// ///  interchangeably with `Symbol`. The syntax for Symbol is the following:
// ///  ```
// ///  # (<x>)+ stands for one or more repetitions of <x>
// ///  # (<x>)? stands for zero or one occurrence of <x>
// ///  <symbol>               ::= <scheme> ' ' <package> ' ' (<descriptor>)+ | 'local ' <local-id>
// ///  <package>              ::= <manager> ' ' <package-name> ' ' <version>
// ///  <scheme>               ::= any UTF-8, escape spaces with double space. Must not be empty nor start with 'local'
// ///  <manager>              ::= any UTF-8, escape spaces with double space. Use the placeholder '.' to indicate an empty value
// ///  <package-name>         ::= same as above
// ///  <version>              ::= same as above
// ///  <descriptor>           ::= <namespace> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta> | <macro>
// ///  <namespace>            ::= <name> '/'
// ///  <type>                 ::= <name> '#'
// ///  <term>                 ::= <name> '.'
// ///  <meta>                 ::= <name> ':'
// ///  <macro>                ::= <name> '!'
// ///  <method>               ::= <name> '(' (<method-disambiguator>)? ').'
// ///  <type-parameter>       ::= '[' <name> ']'
// ///  <parameter>            ::= '(' <name> ')'
// ///  <name>                 ::= <identifier>
// ///  <method-disambiguator> ::= <simple-identifier>
// ///  <identifier>           ::= <simple-identifier> | <escaped-identifier>
// ///  <simple-identifier>    ::= (<identifier-character>)+
// ///  <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit
// ///  <escaped-identifier>   ::= '`' (<escaped-character>)+ '`', must contain at least one non-<identifier-character>
// ///  <escaped-characters>   ::= any UTF-8, escape backticks with double backtick.
// ///  <local-id>             ::= <simple-identifier>
// ///  ```
// ///
// ///  The list of descriptors for a symbol should together form a fully
// ///  qualified name for the symbol. That is, it should serve as a unique
// ///  identifier across the package. Typically, it will include one descriptor
// ///  for every node in the AST (along the ancestry path) between the root of
// ///  the file and the node corresponding to the symbol.
// ///
// ///  Local symbols MUST only be used for entities which are local to a Document,
// ///  and cannot be accessed from outside the Document.
// // @@protoc_insertion_point(message:scip.Symbol)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Symbol {
//     // message fields
//     // @@protoc_insertion_point(field:scip.Symbol.scheme)
//     pub scheme: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Symbol.package)
//     pub package: ::protobuf::MessageField<Package>,
//     // @@protoc_insertion_point(field:scip.Symbol.descriptors)
//     pub descriptors: ::std::vec::Vec<Descriptor>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Symbol.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Symbol {
//     fn default() -> &'a Symbol {
//         <Symbol as ::protobuf::Message>::default_instance()
//     }
// }

// impl Symbol {
//     pub fn new() -> Symbol {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(3);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "scheme",
//             |m: &Symbol| { &m.scheme },
//             |m: &mut Symbol| { &mut m.scheme },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_message_field_accessor::<_, Package>(
//             "package",
//             |m: &Symbol| { &m.package },
//             |m: &mut Symbol| { &mut m.package },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "descriptors",
//             |m: &Symbol| { &m.descriptors },
//             |m: &mut Symbol| { &mut m.descriptors },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Symbol>(
//             "Symbol",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Symbol {
//     const NAME: &'static str = "Symbol";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.scheme = is.read_string()?;
//                 },
//                 18 => {
//                     ::protobuf::rt::read_singular_message_into_field(is, &mut self.package)?;
//                 },
//                 26 => {
//                     self.descriptors.push(is.read_message()?);
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.scheme.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.scheme);
//         }
//         if let Some(v) = self.package.as_ref() {
//             let len = v.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         }
//         for value in &self.descriptors {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.scheme.is_empty() {
//             os.write_string(1, &self.scheme)?;
//         }
//         if let Some(v) = self.package.as_ref() {
//             ::protobuf::rt::write_message_field_with_cached_size(2, v, os)?;
//         }
//         for v in &self.descriptors {
//             ::protobuf::rt::write_message_field_with_cached_size(3, v, os)?;
//         };
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Symbol {
//         Symbol::new()
//     }

//     fn clear(&mut self) {
//         self.scheme.clear();
//         self.package.clear();
//         self.descriptors.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Symbol {
//         static instance: Symbol = Symbol {
//             scheme: ::std::string::String::new(),
//             package: ::protobuf::MessageField::none(),
//             descriptors: ::std::vec::Vec::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Symbol {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Symbol").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Symbol {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Symbol {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// ///  Unit of packaging and distribution.
// ///
// ///  NOTE: This corresponds to a module in Go and JVM languages.
// // @@protoc_insertion_point(message:scip.Package)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Package {
//     // message fields
//     // @@protoc_insertion_point(field:scip.Package.manager)
//     pub manager: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Package.name)
//     pub name: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Package.version)
//     pub version: ::std::string::String,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Package.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Package {
//     fn default() -> &'a Package {
//         <Package as ::protobuf::Message>::default_instance()
//     }
// }

// impl Package {
//     pub fn new() -> Package {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(3);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "manager",
//             |m: &Package| { &m.manager },
//             |m: &mut Package| { &mut m.manager },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "name",
//             |m: &Package| { &m.name },
//             |m: &mut Package| { &mut m.name },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "version",
//             |m: &Package| { &m.version },
//             |m: &mut Package| { &mut m.version },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Package>(
//             "Package",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Package {
//     const NAME: &'static str = "Package";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.manager = is.read_string()?;
//                 },
//                 18 => {
//                     self.name = is.read_string()?;
//                 },
//                 26 => {
//                     self.version = is.read_string()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.manager.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.manager);
//         }
//         if !self.name.is_empty() {
//             my_size += ::protobuf::rt::string_size(2, &self.name);
//         }
//         if !self.version.is_empty() {
//             my_size += ::protobuf::rt::string_size(3, &self.version);
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.manager.is_empty() {
//             os.write_string(1, &self.manager)?;
//         }
//         if !self.name.is_empty() {
//             os.write_string(2, &self.name)?;
//         }
//         if !self.version.is_empty() {
//             os.write_string(3, &self.version)?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Package {
//         Package::new()
//     }

//     fn clear(&mut self) {
//         self.manager.clear();
//         self.name.clear();
//         self.version.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Package {
//         static instance: Package = Package {
//             manager: ::std::string::String::new(),
//             name: ::std::string::String::new(),
//             version: ::std::string::String::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Package {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Package").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Package {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Package {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// // @@protoc_insertion_point(message:scip.Descriptor)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Descriptor {
//     // message fields
//     // @@protoc_insertion_point(field:scip.Descriptor.name)
//     pub name: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Descriptor.disambiguator)
//     pub disambiguator: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Descriptor.suffix)
//     pub suffix: ::protobuf::EnumOrUnknown<descriptor::Suffix>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Descriptor.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Descriptor {
//     fn default() -> &'a Descriptor {
//         <Descriptor as ::protobuf::Message>::default_instance()
//     }
// }

// impl Descriptor {
//     pub fn new() -> Descriptor {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(3);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "name",
//             |m: &Descriptor| { &m.name },
//             |m: &mut Descriptor| { &mut m.name },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "disambiguator",
//             |m: &Descriptor| { &m.disambiguator },
//             |m: &mut Descriptor| { &mut m.disambiguator },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "suffix",
//             |m: &Descriptor| { &m.suffix },
//             |m: &mut Descriptor| { &mut m.suffix },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Descriptor>(
//             "Descriptor",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Descriptor {
//     const NAME: &'static str = "Descriptor";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.name = is.read_string()?;
//                 },
//                 18 => {
//                     self.disambiguator = is.read_string()?;
//                 },
//                 24 => {
//                     self.suffix = is.read_enum_or_unknown()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.name.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.name);
//         }
//         if !self.disambiguator.is_empty() {
//             my_size += ::protobuf::rt::string_size(2, &self.disambiguator);
//         }
//         if self.suffix != ::protobuf::EnumOrUnknown::new(descriptor::Suffix::UnspecifiedSuffix) {
//             my_size += ::protobuf::rt::int32_size(3, self.suffix.value());
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.name.is_empty() {
//             os.write_string(1, &self.name)?;
//         }
//         if !self.disambiguator.is_empty() {
//             os.write_string(2, &self.disambiguator)?;
//         }
//         if self.suffix != ::protobuf::EnumOrUnknown::new(descriptor::Suffix::UnspecifiedSuffix) {
//             os.write_enum(3, ::protobuf::EnumOrUnknown::value(&self.suffix))?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Descriptor {
//         Descriptor::new()
//     }

//     fn clear(&mut self) {
//         self.name.clear();
//         self.disambiguator.clear();
//         self.suffix = ::protobuf::EnumOrUnknown::new(descriptor::Suffix::UnspecifiedSuffix);
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Descriptor {
//         static instance: Descriptor = Descriptor {
//             name: ::std::string::String::new(),
//             disambiguator: ::std::string::String::new(),
//             suffix: ::protobuf::EnumOrUnknown::from_i32(0),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Descriptor {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Descriptor").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Descriptor {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Descriptor {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// /// Nested message and enums of message `Descriptor`
// pub mod descriptor {
//     // Note: you cannot use pattern matching for enums with allow_alias option
//     #[derive(Clone,Copy,Eq,Debug)]
//     // @@protoc_insertion_point(enum:scip.Descriptor.Suffix)
//     pub enum Suffix {
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.UnspecifiedSuffix)
//         UnspecifiedSuffix, // 0
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Namespace)
//         Namespace, // 1
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Package)
//         Package, // 1
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Type)
//         Type, // 2
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Term)
//         Term, // 3
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Method)
//         Method, // 4
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.TypeParameter)
//         TypeParameter, // 5
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Parameter)
//         Parameter, // 6
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Meta)
//         Meta, // 7
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Local)
//         Local, // 8
//         // @@protoc_insertion_point(enum_value:scip.Descriptor.Suffix.Macro)
//         Macro, // 9
//     }

//     impl ::std::cmp::PartialEq for Suffix {
//         fn eq(&self, other: &Self) -> bool {
//             ::protobuf::Enum::value(self) == ::protobuf::Enum::value(other)
//         }
//     }

//     impl ::std::hash::Hash for Suffix {
//         fn hash<H : ::std::hash::Hasher>(&self, state: &mut H) {
//             state.write_i32(::protobuf::Enum::value(self))
//         }
//     }

//     impl ::protobuf::Enum for Suffix {
//         const NAME: &'static str = "Suffix";

//         fn value(&self) -> i32 {
//             match *self {
//                 Suffix::UnspecifiedSuffix => 0,
//                 Suffix::Namespace => 1,
//                 Suffix::Package => 1,
//                 Suffix::Type => 2,
//                 Suffix::Term => 3,
//                 Suffix::Method => 4,
//                 Suffix::TypeParameter => 5,
//                 Suffix::Parameter => 6,
//                 Suffix::Meta => 7,
//                 Suffix::Local => 8,
//                 Suffix::Macro => 9,
//             }
//         }

//         fn from_i32(value: i32) -> ::std::option::Option<Suffix> {
//             match value {
//                 0 => ::std::option::Option::Some(Suffix::UnspecifiedSuffix),
//                 1 => ::std::option::Option::Some(Suffix::Namespace),
//                 2 => ::std::option::Option::Some(Suffix::Type),
//                 3 => ::std::option::Option::Some(Suffix::Term),
//                 4 => ::std::option::Option::Some(Suffix::Method),
//                 5 => ::std::option::Option::Some(Suffix::TypeParameter),
//                 6 => ::std::option::Option::Some(Suffix::Parameter),
//                 7 => ::std::option::Option::Some(Suffix::Meta),
//                 8 => ::std::option::Option::Some(Suffix::Local),
//                 9 => ::std::option::Option::Some(Suffix::Macro),
//                 _ => ::std::option::Option::None
//             }
//         }

//         fn from_str(str: &str) -> ::std::option::Option<Suffix> {
//             match str {
//                 "UnspecifiedSuffix" => ::std::option::Option::Some(Suffix::UnspecifiedSuffix),
//                 "Namespace" => ::std::option::Option::Some(Suffix::Namespace),
//                 "Type" => ::std::option::Option::Some(Suffix::Type),
//                 "Term" => ::std::option::Option::Some(Suffix::Term),
//                 "Method" => ::std::option::Option::Some(Suffix::Method),
//                 "TypeParameter" => ::std::option::Option::Some(Suffix::TypeParameter),
//                 "Parameter" => ::std::option::Option::Some(Suffix::Parameter),
//                 "Meta" => ::std::option::Option::Some(Suffix::Meta),
//                 "Local" => ::std::option::Option::Some(Suffix::Local),
//                 "Macro" => ::std::option::Option::Some(Suffix::Macro),
//                 _ => ::std::option::Option::None
//             }
//         }

//         const VALUES: &'static [Suffix] = &[
//             Suffix::UnspecifiedSuffix,
//             Suffix::Namespace,
//             Suffix::Package,
//             Suffix::Type,
//             Suffix::Term,
//             Suffix::Method,
//             Suffix::TypeParameter,
//             Suffix::Parameter,
//             Suffix::Meta,
//             Suffix::Local,
//             Suffix::Macro,
//         ];
//     }

//     impl ::protobuf::EnumFull for Suffix {
//         fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//             static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//             descriptor.get(|| super::file_descriptor().enum_by_package_relative_name("Descriptor.Suffix").unwrap()).clone()
//         }

//         fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//             let index = match self {
//                 Suffix::UnspecifiedSuffix => 0,
//                 Suffix::Namespace => 1,
//                 Suffix::Package => 2,
//                 Suffix::Type => 3,
//                 Suffix::Term => 4,
//                 Suffix::Method => 5,
//                 Suffix::TypeParameter => 6,
//                 Suffix::Parameter => 7,
//                 Suffix::Meta => 8,
//                 Suffix::Local => 9,
//                 Suffix::Macro => 10,
//             };
//             Self::enum_descriptor().value_by_index(index)
//         }
//     }

//     impl ::std::default::Default for Suffix {
//         fn default() -> Self {
//             Suffix::UnspecifiedSuffix
//         }
//     }

//     impl Suffix {
//         pub(in super) fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//             ::protobuf::reflect::GeneratedEnumDescriptorData::new::<Suffix>("Descriptor.Suffix")
//         }
//     }
// }

// ///  SymbolInformation defines metadata about a symbol, such as the symbol's
// ///  docstring or what package it's defined it.
// // @@protoc_insertion_point(message:scip.SymbolInformation)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct SymbolInformation {
//     // message fields
//     ///  Identifier of this symbol, which can be referenced from `Occurence.symbol`.
//     ///  The string must be formatted according to the grammar in `Symbol`.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.symbol)
//     pub symbol: ::std::string::String,
//     ///  (optional, but strongly recommended) The markdown-formatted documentation
//     ///  for this symbol. Use `SymbolInformation.signature_documentation` to
//     ///  document the method/class/type signature of this symbol.
//     ///  Due to historical reasons, indexers may include signature documentation in
//     ///  this field by rendering markdown code blocks. New indexers should only
//     ///  include non-code documentation in this field, for example docstrings.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.documentation)
//     pub documentation: ::std::vec::Vec<::std::string::String>,
//     ///  (optional) Relationships to other symbols (e.g., implements, type definition).
//     // @@protoc_insertion_point(field:scip.SymbolInformation.relationships)
//     pub relationships: ::std::vec::Vec<Relationship>,
//     ///  The kind of this symbol. Use this field instead of
//     ///  `SymbolDescriptor.Suffix` to determine whether something is, for example, a
//     ///  class or a method.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.kind)
//     pub kind: ::protobuf::EnumOrUnknown<symbol_information::Kind>,
//     ///  (optional) The name of this symbol as it should be displayed to the user.
//     ///  For example, the symbol "com/example/MyClass#myMethod(+1)." should have the
//     ///  display name "myMethod". The `symbol` field is not a reliable source of
//     ///  the display name for several reasons:
//     ///
//     ///  - Local symbols don't encode the name.
//     ///  - Some languages have case-insensitive names, so the symbol is all-lowercase.
//     ///  - The symbol may encode names with special characters that should not be
//     ///    displayed to the user.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.display_name)
//     pub display_name: ::std::string::String,
//     ///  (optional) The signature of this symbol as it's displayed in API
//     ///  documentation or in hover tooltips. For example, a Java method that adds
//     ///  two numbers this would have `Document.language = "java"` and `Document.text
//     ///  = "void add(int a, int b)". The `language` and `text` fields are required
//     ///  while other fields such as `Documentation.occurrences` can be optionally
//     ///  included to support hyperlinking referenced symbols in the signature.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.signature_documentation)
//     pub signature_documentation: ::protobuf::MessageField<Document>,
//     ///  (optional) The enclosing symbol if this is a local symbol.  For non-local
//     ///  symbols, the enclosing symbol should be parsed from the `symbol` field
//     ///  using the `Descriptor` grammar.
//     ///
//     ///  The primary use-case for this field is to allow local symbol to be displayed
//     ///  in a symbol hierarchy for API documentation. It's OK to leave this field
//     ///  empty for local variables since local variables usually don't belong in API
//     ///  documentation. However, in the situation that you wish to include a local
//     ///  symbol in the hierarchy, then you can use `enclosing_symbol` to locate the
//     ///  "parent" or "owner" of this local symbol. For example, a Java indexer may
//     ///  choose to use local symbols for private class fields while providing an
//     ///  `enclosing_symbol` to reference the enclosing class to allow the field to
//     ///  be part of the class documentation hierarchy. From the perspective of an
//     ///  author of an indexer, the decision to use a local symbol or global symbol
//     ///  should exclusively be determined whether the local symbol is accessible
//     ///  outside the document, not by the capability to find the enclosing
//     ///  symbol.
//     // @@protoc_insertion_point(field:scip.SymbolInformation.enclosing_symbol)
//     pub enclosing_symbol: ::std::string::String,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.SymbolInformation.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a SymbolInformation {
//     fn default() -> &'a SymbolInformation {
//         <SymbolInformation as ::protobuf::Message>::default_instance()
//     }
// }

// impl SymbolInformation {
//     pub fn new() -> SymbolInformation {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(7);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "symbol",
//             |m: &SymbolInformation| { &m.symbol },
//             |m: &mut SymbolInformation| { &mut m.symbol },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "documentation",
//             |m: &SymbolInformation| { &m.documentation },
//             |m: &mut SymbolInformation| { &mut m.documentation },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "relationships",
//             |m: &SymbolInformation| { &m.relationships },
//             |m: &mut SymbolInformation| { &mut m.relationships },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "kind",
//             |m: &SymbolInformation| { &m.kind },
//             |m: &mut SymbolInformation| { &mut m.kind },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "display_name",
//             |m: &SymbolInformation| { &m.display_name },
//             |m: &mut SymbolInformation| { &mut m.display_name },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_message_field_accessor::<_, Document>(
//             "signature_documentation",
//             |m: &SymbolInformation| { &m.signature_documentation },
//             |m: &mut SymbolInformation| { &mut m.signature_documentation },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "enclosing_symbol",
//             |m: &SymbolInformation| { &m.enclosing_symbol },
//             |m: &mut SymbolInformation| { &mut m.enclosing_symbol },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<SymbolInformation>(
//             "SymbolInformation",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for SymbolInformation {
//     const NAME: &'static str = "SymbolInformation";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.symbol = is.read_string()?;
//                 },
//                 26 => {
//                     self.documentation.push(is.read_string()?);
//                 },
//                 34 => {
//                     self.relationships.push(is.read_message()?);
//                 },
//                 40 => {
//                     self.kind = is.read_enum_or_unknown()?;
//                 },
//                 50 => {
//                     self.display_name = is.read_string()?;
//                 },
//                 58 => {
//                     ::protobuf::rt::read_singular_message_into_field(is, &mut self.signature_documentation)?;
//                 },
//                 66 => {
//                     self.enclosing_symbol = is.read_string()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.symbol.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.symbol);
//         }
//         for value in &self.documentation {
//             my_size += ::protobuf::rt::string_size(3, &value);
//         };
//         for value in &self.relationships {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         if self.kind != ::protobuf::EnumOrUnknown::new(symbol_information::Kind::UnspecifiedKind) {
//             my_size += ::protobuf::rt::int32_size(5, self.kind.value());
//         }
//         if !self.display_name.is_empty() {
//             my_size += ::protobuf::rt::string_size(6, &self.display_name);
//         }
//         if let Some(v) = self.signature_documentation.as_ref() {
//             let len = v.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         }
//         if !self.enclosing_symbol.is_empty() {
//             my_size += ::protobuf::rt::string_size(8, &self.enclosing_symbol);
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.symbol.is_empty() {
//             os.write_string(1, &self.symbol)?;
//         }
//         for v in &self.documentation {
//             os.write_string(3, &v)?;
//         };
//         for v in &self.relationships {
//             ::protobuf::rt::write_message_field_with_cached_size(4, v, os)?;
//         };
//         if self.kind != ::protobuf::EnumOrUnknown::new(symbol_information::Kind::UnspecifiedKind) {
//             os.write_enum(5, ::protobuf::EnumOrUnknown::value(&self.kind))?;
//         }
//         if !self.display_name.is_empty() {
//             os.write_string(6, &self.display_name)?;
//         }
//         if let Some(v) = self.signature_documentation.as_ref() {
//             ::protobuf::rt::write_message_field_with_cached_size(7, v, os)?;
//         }
//         if !self.enclosing_symbol.is_empty() {
//             os.write_string(8, &self.enclosing_symbol)?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> SymbolInformation {
//         SymbolInformation::new()
//     }

//     fn clear(&mut self) {
//         self.symbol.clear();
//         self.documentation.clear();
//         self.relationships.clear();
//         self.kind = ::protobuf::EnumOrUnknown::new(symbol_information::Kind::UnspecifiedKind);
//         self.display_name.clear();
//         self.signature_documentation.clear();
//         self.enclosing_symbol.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static SymbolInformation {
//         static instance: SymbolInformation = SymbolInformation {
//             symbol: ::std::string::String::new(),
//             documentation: ::std::vec::Vec::new(),
//             relationships: ::std::vec::Vec::new(),
//             kind: ::protobuf::EnumOrUnknown::from_i32(0),
//             display_name: ::std::string::String::new(),
//             signature_documentation: ::protobuf::MessageField::none(),
//             enclosing_symbol: ::std::string::String::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for SymbolInformation {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("SymbolInformation").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for SymbolInformation {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for SymbolInformation {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// /// Nested message and enums of message `SymbolInformation`
// pub mod symbol_information {
//     ///  (optional) Kind represents the fine-grained category of a symbol, suitable for presenting
//     ///  information about the symbol's meaning in the language.
//     ///
//     ///  For example:
//     ///  - A Java method would have the kind `Method` while a Go function would
//     ///    have the kind `Function`, even if the symbols for these use the same
//     ///    syntax for the descriptor `SymbolDescriptor.Suffix.Method`.
//     ///  - A Go struct has the symbol kind `Struct` while a Java class has
//     ///    the symbol kind `Class` even if they both have the same descriptor:
//     ///    `SymbolDescriptor.Suffix.Type`.
//     ///
//     ///  Since Kind is more fine-grained than Suffix:
//     ///  - If two symbols have the same Kind, they should share the same Suffix.
//     ///  - If two symbols have different Suffixes, they should have different Kinds.
//     #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
//     // @@protoc_insertion_point(enum:scip.SymbolInformation.Kind)
//     pub enum Kind {
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.UnspecifiedKind)
//         UnspecifiedKind = 0,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.AbstractMethod)
//         AbstractMethod = 66,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Accessor)
//         Accessor = 72,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Array)
//         Array = 1,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Assertion)
//         Assertion = 2,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.AssociatedType)
//         AssociatedType = 3,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Attribute)
//         Attribute = 4,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Axiom)
//         Axiom = 5,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Boolean)
//         Boolean = 6,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Class)
//         Class = 7,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Constant)
//         Constant = 8,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Constructor)
//         Constructor = 9,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Contract)
//         Contract = 62,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.DataFamily)
//         DataFamily = 10,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Delegate)
//         Delegate = 73,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Enum)
//         Enum = 11,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.EnumMember)
//         EnumMember = 12,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Error)
//         Error = 63,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Event)
//         Event = 13,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Extension)
//         Extension = 84,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Fact)
//         Fact = 14,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Field)
//         Field = 15,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.File)
//         File = 16,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Function)
//         Function = 17,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Getter)
//         Getter = 18,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Grammar)
//         Grammar = 19,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Instance)
//         Instance = 20,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Interface)
//         Interface = 21,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Key)
//         Key = 22,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Lang)
//         Lang = 23,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Lemma)
//         Lemma = 24,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Library)
//         Library = 64,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Macro)
//         Macro = 25,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Method)
//         Method = 26,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.MethodAlias)
//         MethodAlias = 74,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.MethodReceiver)
//         MethodReceiver = 27,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.MethodSpecification)
//         MethodSpecification = 67,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Message)
//         Message = 28,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Mixin)
//         Mixin = 85,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Modifier)
//         Modifier = 65,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Module)
//         Module = 29,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Namespace)
//         Namespace = 30,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Null)
//         Null = 31,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Number)
//         Number = 32,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Object)
//         Object = 33,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Operator)
//         Operator = 34,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Package)
//         Package = 35,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.PackageObject)
//         PackageObject = 36,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Parameter)
//         Parameter = 37,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.ParameterLabel)
//         ParameterLabel = 38,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Pattern)
//         Pattern = 39,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Predicate)
//         Predicate = 40,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Property)
//         Property = 41,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Protocol)
//         Protocol = 42,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.ProtocolMethod)
//         ProtocolMethod = 68,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.PureVirtualMethod)
//         PureVirtualMethod = 69,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Quasiquoter)
//         Quasiquoter = 43,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.SelfParameter)
//         SelfParameter = 44,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Setter)
//         Setter = 45,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Signature)
//         Signature = 46,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.SingletonClass)
//         SingletonClass = 75,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.SingletonMethod)
//         SingletonMethod = 76,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticDataMember)
//         StaticDataMember = 77,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticEvent)
//         StaticEvent = 78,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticField)
//         StaticField = 79,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticMethod)
//         StaticMethod = 80,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticProperty)
//         StaticProperty = 81,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.StaticVariable)
//         StaticVariable = 82,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.String)
//         String = 48,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Struct)
//         Struct = 49,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Subscript)
//         Subscript = 47,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Tactic)
//         Tactic = 50,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Theorem)
//         Theorem = 51,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.ThisParameter)
//         ThisParameter = 52,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Trait)
//         Trait = 53,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TraitMethod)
//         TraitMethod = 70,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Type)
//         Type = 54,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TypeAlias)
//         TypeAlias = 55,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TypeClass)
//         TypeClass = 56,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TypeClassMethod)
//         TypeClassMethod = 71,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TypeFamily)
//         TypeFamily = 57,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.TypeParameter)
//         TypeParameter = 58,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Union)
//         Union = 59,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Value)
//         Value = 60,
//         // @@protoc_insertion_point(enum_value:scip.SymbolInformation.Kind.Variable)
//         Variable = 61,
//     }

//     impl ::protobuf::Enum for Kind {
//         const NAME: &'static str = "Kind";

//         fn value(&self) -> i32 {
//             *self as i32
//         }

//         fn from_i32(value: i32) -> ::std::option::Option<Kind> {
//             match value {
//                 0 => ::std::option::Option::Some(Kind::UnspecifiedKind),
//                 66 => ::std::option::Option::Some(Kind::AbstractMethod),
//                 72 => ::std::option::Option::Some(Kind::Accessor),
//                 1 => ::std::option::Option::Some(Kind::Array),
//                 2 => ::std::option::Option::Some(Kind::Assertion),
//                 3 => ::std::option::Option::Some(Kind::AssociatedType),
//                 4 => ::std::option::Option::Some(Kind::Attribute),
//                 5 => ::std::option::Option::Some(Kind::Axiom),
//                 6 => ::std::option::Option::Some(Kind::Boolean),
//                 7 => ::std::option::Option::Some(Kind::Class),
//                 8 => ::std::option::Option::Some(Kind::Constant),
//                 9 => ::std::option::Option::Some(Kind::Constructor),
//                 62 => ::std::option::Option::Some(Kind::Contract),
//                 10 => ::std::option::Option::Some(Kind::DataFamily),
//                 73 => ::std::option::Option::Some(Kind::Delegate),
//                 11 => ::std::option::Option::Some(Kind::Enum),
//                 12 => ::std::option::Option::Some(Kind::EnumMember),
//                 63 => ::std::option::Option::Some(Kind::Error),
//                 13 => ::std::option::Option::Some(Kind::Event),
//                 84 => ::std::option::Option::Some(Kind::Extension),
//                 14 => ::std::option::Option::Some(Kind::Fact),
//                 15 => ::std::option::Option::Some(Kind::Field),
//                 16 => ::std::option::Option::Some(Kind::File),
//                 17 => ::std::option::Option::Some(Kind::Function),
//                 18 => ::std::option::Option::Some(Kind::Getter),
//                 19 => ::std::option::Option::Some(Kind::Grammar),
//                 20 => ::std::option::Option::Some(Kind::Instance),
//                 21 => ::std::option::Option::Some(Kind::Interface),
//                 22 => ::std::option::Option::Some(Kind::Key),
//                 23 => ::std::option::Option::Some(Kind::Lang),
//                 24 => ::std::option::Option::Some(Kind::Lemma),
//                 64 => ::std::option::Option::Some(Kind::Library),
//                 25 => ::std::option::Option::Some(Kind::Macro),
//                 26 => ::std::option::Option::Some(Kind::Method),
//                 74 => ::std::option::Option::Some(Kind::MethodAlias),
//                 27 => ::std::option::Option::Some(Kind::MethodReceiver),
//                 67 => ::std::option::Option::Some(Kind::MethodSpecification),
//                 28 => ::std::option::Option::Some(Kind::Message),
//                 85 => ::std::option::Option::Some(Kind::Mixin),
//                 65 => ::std::option::Option::Some(Kind::Modifier),
//                 29 => ::std::option::Option::Some(Kind::Module),
//                 30 => ::std::option::Option::Some(Kind::Namespace),
//                 31 => ::std::option::Option::Some(Kind::Null),
//                 32 => ::std::option::Option::Some(Kind::Number),
//                 33 => ::std::option::Option::Some(Kind::Object),
//                 34 => ::std::option::Option::Some(Kind::Operator),
//                 35 => ::std::option::Option::Some(Kind::Package),
//                 36 => ::std::option::Option::Some(Kind::PackageObject),
//                 37 => ::std::option::Option::Some(Kind::Parameter),
//                 38 => ::std::option::Option::Some(Kind::ParameterLabel),
//                 39 => ::std::option::Option::Some(Kind::Pattern),
//                 40 => ::std::option::Option::Some(Kind::Predicate),
//                 41 => ::std::option::Option::Some(Kind::Property),
//                 42 => ::std::option::Option::Some(Kind::Protocol),
//                 68 => ::std::option::Option::Some(Kind::ProtocolMethod),
//                 69 => ::std::option::Option::Some(Kind::PureVirtualMethod),
//                 43 => ::std::option::Option::Some(Kind::Quasiquoter),
//                 44 => ::std::option::Option::Some(Kind::SelfParameter),
//                 45 => ::std::option::Option::Some(Kind::Setter),
//                 46 => ::std::option::Option::Some(Kind::Signature),
//                 75 => ::std::option::Option::Some(Kind::SingletonClass),
//                 76 => ::std::option::Option::Some(Kind::SingletonMethod),
//                 77 => ::std::option::Option::Some(Kind::StaticDataMember),
//                 78 => ::std::option::Option::Some(Kind::StaticEvent),
//                 79 => ::std::option::Option::Some(Kind::StaticField),
//                 80 => ::std::option::Option::Some(Kind::StaticMethod),
//                 81 => ::std::option::Option::Some(Kind::StaticProperty),
//                 82 => ::std::option::Option::Some(Kind::StaticVariable),
//                 48 => ::std::option::Option::Some(Kind::String),
//                 49 => ::std::option::Option::Some(Kind::Struct),
//                 47 => ::std::option::Option::Some(Kind::Subscript),
//                 50 => ::std::option::Option::Some(Kind::Tactic),
//                 51 => ::std::option::Option::Some(Kind::Theorem),
//                 52 => ::std::option::Option::Some(Kind::ThisParameter),
//                 53 => ::std::option::Option::Some(Kind::Trait),
//                 70 => ::std::option::Option::Some(Kind::TraitMethod),
//                 54 => ::std::option::Option::Some(Kind::Type),
//                 55 => ::std::option::Option::Some(Kind::TypeAlias),
//                 56 => ::std::option::Option::Some(Kind::TypeClass),
//                 71 => ::std::option::Option::Some(Kind::TypeClassMethod),
//                 57 => ::std::option::Option::Some(Kind::TypeFamily),
//                 58 => ::std::option::Option::Some(Kind::TypeParameter),
//                 59 => ::std::option::Option::Some(Kind::Union),
//                 60 => ::std::option::Option::Some(Kind::Value),
//                 61 => ::std::option::Option::Some(Kind::Variable),
//                 _ => ::std::option::Option::None
//             }
//         }

//         fn from_str(str: &str) -> ::std::option::Option<Kind> {
//             match str {
//                 "UnspecifiedKind" => ::std::option::Option::Some(Kind::UnspecifiedKind),
//                 "AbstractMethod" => ::std::option::Option::Some(Kind::AbstractMethod),
//                 "Accessor" => ::std::option::Option::Some(Kind::Accessor),
//                 "Array" => ::std::option::Option::Some(Kind::Array),
//                 "Assertion" => ::std::option::Option::Some(Kind::Assertion),
//                 "AssociatedType" => ::std::option::Option::Some(Kind::AssociatedType),
//                 "Attribute" => ::std::option::Option::Some(Kind::Attribute),
//                 "Axiom" => ::std::option::Option::Some(Kind::Axiom),
//                 "Boolean" => ::std::option::Option::Some(Kind::Boolean),
//                 "Class" => ::std::option::Option::Some(Kind::Class),
//                 "Constant" => ::std::option::Option::Some(Kind::Constant),
//                 "Constructor" => ::std::option::Option::Some(Kind::Constructor),
//                 "Contract" => ::std::option::Option::Some(Kind::Contract),
//                 "DataFamily" => ::std::option::Option::Some(Kind::DataFamily),
//                 "Delegate" => ::std::option::Option::Some(Kind::Delegate),
//                 "Enum" => ::std::option::Option::Some(Kind::Enum),
//                 "EnumMember" => ::std::option::Option::Some(Kind::EnumMember),
//                 "Error" => ::std::option::Option::Some(Kind::Error),
//                 "Event" => ::std::option::Option::Some(Kind::Event),
//                 "Extension" => ::std::option::Option::Some(Kind::Extension),
//                 "Fact" => ::std::option::Option::Some(Kind::Fact),
//                 "Field" => ::std::option::Option::Some(Kind::Field),
//                 "File" => ::std::option::Option::Some(Kind::File),
//                 "Function" => ::std::option::Option::Some(Kind::Function),
//                 "Getter" => ::std::option::Option::Some(Kind::Getter),
//                 "Grammar" => ::std::option::Option::Some(Kind::Grammar),
//                 "Instance" => ::std::option::Option::Some(Kind::Instance),
//                 "Interface" => ::std::option::Option::Some(Kind::Interface),
//                 "Key" => ::std::option::Option::Some(Kind::Key),
//                 "Lang" => ::std::option::Option::Some(Kind::Lang),
//                 "Lemma" => ::std::option::Option::Some(Kind::Lemma),
//                 "Library" => ::std::option::Option::Some(Kind::Library),
//                 "Macro" => ::std::option::Option::Some(Kind::Macro),
//                 "Method" => ::std::option::Option::Some(Kind::Method),
//                 "MethodAlias" => ::std::option::Option::Some(Kind::MethodAlias),
//                 "MethodReceiver" => ::std::option::Option::Some(Kind::MethodReceiver),
//                 "MethodSpecification" => ::std::option::Option::Some(Kind::MethodSpecification),
//                 "Message" => ::std::option::Option::Some(Kind::Message),
//                 "Mixin" => ::std::option::Option::Some(Kind::Mixin),
//                 "Modifier" => ::std::option::Option::Some(Kind::Modifier),
//                 "Module" => ::std::option::Option::Some(Kind::Module),
//                 "Namespace" => ::std::option::Option::Some(Kind::Namespace),
//                 "Null" => ::std::option::Option::Some(Kind::Null),
//                 "Number" => ::std::option::Option::Some(Kind::Number),
//                 "Object" => ::std::option::Option::Some(Kind::Object),
//                 "Operator" => ::std::option::Option::Some(Kind::Operator),
//                 "Package" => ::std::option::Option::Some(Kind::Package),
//                 "PackageObject" => ::std::option::Option::Some(Kind::PackageObject),
//                 "Parameter" => ::std::option::Option::Some(Kind::Parameter),
//                 "ParameterLabel" => ::std::option::Option::Some(Kind::ParameterLabel),
//                 "Pattern" => ::std::option::Option::Some(Kind::Pattern),
//                 "Predicate" => ::std::option::Option::Some(Kind::Predicate),
//                 "Property" => ::std::option::Option::Some(Kind::Property),
//                 "Protocol" => ::std::option::Option::Some(Kind::Protocol),
//                 "ProtocolMethod" => ::std::option::Option::Some(Kind::ProtocolMethod),
//                 "PureVirtualMethod" => ::std::option::Option::Some(Kind::PureVirtualMethod),
//                 "Quasiquoter" => ::std::option::Option::Some(Kind::Quasiquoter),
//                 "SelfParameter" => ::std::option::Option::Some(Kind::SelfParameter),
//                 "Setter" => ::std::option::Option::Some(Kind::Setter),
//                 "Signature" => ::std::option::Option::Some(Kind::Signature),
//                 "SingletonClass" => ::std::option::Option::Some(Kind::SingletonClass),
//                 "SingletonMethod" => ::std::option::Option::Some(Kind::SingletonMethod),
//                 "StaticDataMember" => ::std::option::Option::Some(Kind::StaticDataMember),
//                 "StaticEvent" => ::std::option::Option::Some(Kind::StaticEvent),
//                 "StaticField" => ::std::option::Option::Some(Kind::StaticField),
//                 "StaticMethod" => ::std::option::Option::Some(Kind::StaticMethod),
//                 "StaticProperty" => ::std::option::Option::Some(Kind::StaticProperty),
//                 "StaticVariable" => ::std::option::Option::Some(Kind::StaticVariable),
//                 "String" => ::std::option::Option::Some(Kind::String),
//                 "Struct" => ::std::option::Option::Some(Kind::Struct),
//                 "Subscript" => ::std::option::Option::Some(Kind::Subscript),
//                 "Tactic" => ::std::option::Option::Some(Kind::Tactic),
//                 "Theorem" => ::std::option::Option::Some(Kind::Theorem),
//                 "ThisParameter" => ::std::option::Option::Some(Kind::ThisParameter),
//                 "Trait" => ::std::option::Option::Some(Kind::Trait),
//                 "TraitMethod" => ::std::option::Option::Some(Kind::TraitMethod),
//                 "Type" => ::std::option::Option::Some(Kind::Type),
//                 "TypeAlias" => ::std::option::Option::Some(Kind::TypeAlias),
//                 "TypeClass" => ::std::option::Option::Some(Kind::TypeClass),
//                 "TypeClassMethod" => ::std::option::Option::Some(Kind::TypeClassMethod),
//                 "TypeFamily" => ::std::option::Option::Some(Kind::TypeFamily),
//                 "TypeParameter" => ::std::option::Option::Some(Kind::TypeParameter),
//                 "Union" => ::std::option::Option::Some(Kind::Union),
//                 "Value" => ::std::option::Option::Some(Kind::Value),
//                 "Variable" => ::std::option::Option::Some(Kind::Variable),
//                 _ => ::std::option::Option::None
//             }
//         }

//         const VALUES: &'static [Kind] = &[
//             Kind::UnspecifiedKind,
//             Kind::AbstractMethod,
//             Kind::Accessor,
//             Kind::Array,
//             Kind::Assertion,
//             Kind::AssociatedType,
//             Kind::Attribute,
//             Kind::Axiom,
//             Kind::Boolean,
//             Kind::Class,
//             Kind::Constant,
//             Kind::Constructor,
//             Kind::Contract,
//             Kind::DataFamily,
//             Kind::Delegate,
//             Kind::Enum,
//             Kind::EnumMember,
//             Kind::Error,
//             Kind::Event,
//             Kind::Extension,
//             Kind::Fact,
//             Kind::Field,
//             Kind::File,
//             Kind::Function,
//             Kind::Getter,
//             Kind::Grammar,
//             Kind::Instance,
//             Kind::Interface,
//             Kind::Key,
//             Kind::Lang,
//             Kind::Lemma,
//             Kind::Library,
//             Kind::Macro,
//             Kind::Method,
//             Kind::MethodAlias,
//             Kind::MethodReceiver,
//             Kind::MethodSpecification,
//             Kind::Message,
//             Kind::Mixin,
//             Kind::Modifier,
//             Kind::Module,
//             Kind::Namespace,
//             Kind::Null,
//             Kind::Number,
//             Kind::Object,
//             Kind::Operator,
//             Kind::Package,
//             Kind::PackageObject,
//             Kind::Parameter,
//             Kind::ParameterLabel,
//             Kind::Pattern,
//             Kind::Predicate,
//             Kind::Property,
//             Kind::Protocol,
//             Kind::ProtocolMethod,
//             Kind::PureVirtualMethod,
//             Kind::Quasiquoter,
//             Kind::SelfParameter,
//             Kind::Setter,
//             Kind::Signature,
//             Kind::SingletonClass,
//             Kind::SingletonMethod,
//             Kind::StaticDataMember,
//             Kind::StaticEvent,
//             Kind::StaticField,
//             Kind::StaticMethod,
//             Kind::StaticProperty,
//             Kind::StaticVariable,
//             Kind::String,
//             Kind::Struct,
//             Kind::Subscript,
//             Kind::Tactic,
//             Kind::Theorem,
//             Kind::ThisParameter,
//             Kind::Trait,
//             Kind::TraitMethod,
//             Kind::Type,
//             Kind::TypeAlias,
//             Kind::TypeClass,
//             Kind::TypeClassMethod,
//             Kind::TypeFamily,
//             Kind::TypeParameter,
//             Kind::Union,
//             Kind::Value,
//             Kind::Variable,
//         ];
//     }

//     impl ::protobuf::EnumFull for Kind {
//         fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//             static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//             descriptor.get(|| super::file_descriptor().enum_by_package_relative_name("SymbolInformation.Kind").unwrap()).clone()
//         }

//         fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//             let index = match self {
//                 Kind::UnspecifiedKind => 0,
//                 Kind::AbstractMethod => 1,
//                 Kind::Accessor => 2,
//                 Kind::Array => 3,
//                 Kind::Assertion => 4,
//                 Kind::AssociatedType => 5,
//                 Kind::Attribute => 6,
//                 Kind::Axiom => 7,
//                 Kind::Boolean => 8,
//                 Kind::Class => 9,
//                 Kind::Constant => 10,
//                 Kind::Constructor => 11,
//                 Kind::Contract => 12,
//                 Kind::DataFamily => 13,
//                 Kind::Delegate => 14,
//                 Kind::Enum => 15,
//                 Kind::EnumMember => 16,
//                 Kind::Error => 17,
//                 Kind::Event => 18,
//                 Kind::Extension => 19,
//                 Kind::Fact => 20,
//                 Kind::Field => 21,
//                 Kind::File => 22,
//                 Kind::Function => 23,
//                 Kind::Getter => 24,
//                 Kind::Grammar => 25,
//                 Kind::Instance => 26,
//                 Kind::Interface => 27,
//                 Kind::Key => 28,
//                 Kind::Lang => 29,
//                 Kind::Lemma => 30,
//                 Kind::Library => 31,
//                 Kind::Macro => 32,
//                 Kind::Method => 33,
//                 Kind::MethodAlias => 34,
//                 Kind::MethodReceiver => 35,
//                 Kind::MethodSpecification => 36,
//                 Kind::Message => 37,
//                 Kind::Mixin => 38,
//                 Kind::Modifier => 39,
//                 Kind::Module => 40,
//                 Kind::Namespace => 41,
//                 Kind::Null => 42,
//                 Kind::Number => 43,
//                 Kind::Object => 44,
//                 Kind::Operator => 45,
//                 Kind::Package => 46,
//                 Kind::PackageObject => 47,
//                 Kind::Parameter => 48,
//                 Kind::ParameterLabel => 49,
//                 Kind::Pattern => 50,
//                 Kind::Predicate => 51,
//                 Kind::Property => 52,
//                 Kind::Protocol => 53,
//                 Kind::ProtocolMethod => 54,
//                 Kind::PureVirtualMethod => 55,
//                 Kind::Quasiquoter => 56,
//                 Kind::SelfParameter => 57,
//                 Kind::Setter => 58,
//                 Kind::Signature => 59,
//                 Kind::SingletonClass => 60,
//                 Kind::SingletonMethod => 61,
//                 Kind::StaticDataMember => 62,
//                 Kind::StaticEvent => 63,
//                 Kind::StaticField => 64,
//                 Kind::StaticMethod => 65,
//                 Kind::StaticProperty => 66,
//                 Kind::StaticVariable => 67,
//                 Kind::String => 68,
//                 Kind::Struct => 69,
//                 Kind::Subscript => 70,
//                 Kind::Tactic => 71,
//                 Kind::Theorem => 72,
//                 Kind::ThisParameter => 73,
//                 Kind::Trait => 74,
//                 Kind::TraitMethod => 75,
//                 Kind::Type => 76,
//                 Kind::TypeAlias => 77,
//                 Kind::TypeClass => 78,
//                 Kind::TypeClassMethod => 79,
//                 Kind::TypeFamily => 80,
//                 Kind::TypeParameter => 81,
//                 Kind::Union => 82,
//                 Kind::Value => 83,
//                 Kind::Variable => 84,
//             };
//             Self::enum_descriptor().value_by_index(index)
//         }
//     }

//     impl ::std::default::Default for Kind {
//         fn default() -> Self {
//             Kind::UnspecifiedKind
//         }
//     }

//     impl Kind {
//         pub(in super) fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//             ::protobuf::reflect::GeneratedEnumDescriptorData::new::<Kind>("SymbolInformation.Kind")
//         }
//     }
// }

// // @@protoc_insertion_point(message:scip.Relationship)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Relationship {
//     // message fields
//     // @@protoc_insertion_point(field:scip.Relationship.symbol)
//     pub symbol: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Relationship.is_reference)
//     pub is_reference: bool,
//     ///  Similar to `is_reference` but for "Find implementations".
//     ///  It's common for `is_implementation` and `is_reference` to both be true but
//     ///  it's not always the case.
//     ///  In the TypeScript example above, observe that `Dog#` has an
//     ///  `is_implementation` relationship with `"Animal#"` but not `is_reference`.
//     ///  This is because "Find references" on the "Animal#" symbol should not return
//     ///  "Dog#". We only want "Dog#" to return as a result for "Find
//     ///  implementations" on the "Animal#" symbol.
//     // @@protoc_insertion_point(field:scip.Relationship.is_implementation)
//     pub is_implementation: bool,
//     ///  Similar to `references_symbols` but for "Go to type definition".
//     // @@protoc_insertion_point(field:scip.Relationship.is_type_definition)
//     pub is_type_definition: bool,
//     ///  Allows overriding the behavior of "Go to definition" and "Find references"
//     ///  for symbols which do not have a definition of their own or could
//     ///  potentially have multiple definitions.
//     ///
//     ///  For example, in a language with single inheritance and no field overriding,
//     ///  inherited fields can reuse the same symbol as the ancestor which declares
//     ///  the field. In such a situation, is_definition is not needed.
//     ///
//     ///  On the other hand, in languages with single inheritance and some form
//     ///  of mixins, you can use is_definition to relate the symbol to the
//     ///  matching symbol in ancestor classes, and is_reference to relate the
//     ///  symbol to the matching symbol in mixins.
//     ///
//     ///  NOTE: At the moment, due to limitations of the SCIP to LSIF conversion,
//     ///  only global symbols in an index are allowed to use is_definition.
//     ///  The relationship may not get recorded if either symbol is local.
//     // @@protoc_insertion_point(field:scip.Relationship.is_definition)
//     pub is_definition: bool,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Relationship.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Relationship {
//     fn default() -> &'a Relationship {
//         <Relationship as ::protobuf::Message>::default_instance()
//     }
// }

// impl Relationship {
//     pub fn new() -> Relationship {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(5);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "symbol",
//             |m: &Relationship| { &m.symbol },
//             |m: &mut Relationship| { &mut m.symbol },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "is_reference",
//             |m: &Relationship| { &m.is_reference },
//             |m: &mut Relationship| { &mut m.is_reference },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "is_implementation",
//             |m: &Relationship| { &m.is_implementation },
//             |m: &mut Relationship| { &mut m.is_implementation },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "is_type_definition",
//             |m: &Relationship| { &m.is_type_definition },
//             |m: &mut Relationship| { &mut m.is_type_definition },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "is_definition",
//             |m: &Relationship| { &m.is_definition },
//             |m: &mut Relationship| { &mut m.is_definition },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Relationship>(
//             "Relationship",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Relationship {
//     const NAME: &'static str = "Relationship";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     self.symbol = is.read_string()?;
//                 },
//                 16 => {
//                     self.is_reference = is.read_bool()?;
//                 },
//                 24 => {
//                     self.is_implementation = is.read_bool()?;
//                 },
//                 32 => {
//                     self.is_type_definition = is.read_bool()?;
//                 },
//                 40 => {
//                     self.is_definition = is.read_bool()?;
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if !self.symbol.is_empty() {
//             my_size += ::protobuf::rt::string_size(1, &self.symbol);
//         }
//         if self.is_reference != false {
//             my_size += 1 + 1;
//         }
//         if self.is_implementation != false {
//             my_size += 1 + 1;
//         }
//         if self.is_type_definition != false {
//             my_size += 1 + 1;
//         }
//         if self.is_definition != false {
//             my_size += 1 + 1;
//         }
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if !self.symbol.is_empty() {
//             os.write_string(1, &self.symbol)?;
//         }
//         if self.is_reference != false {
//             os.write_bool(2, self.is_reference)?;
//         }
//         if self.is_implementation != false {
//             os.write_bool(3, self.is_implementation)?;
//         }
//         if self.is_type_definition != false {
//             os.write_bool(4, self.is_type_definition)?;
//         }
//         if self.is_definition != false {
//             os.write_bool(5, self.is_definition)?;
//         }
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Relationship {
//         Relationship::new()
//     }

//     fn clear(&mut self) {
//         self.symbol.clear();
//         self.is_reference = false;
//         self.is_implementation = false;
//         self.is_type_definition = false;
//         self.is_definition = false;
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Relationship {
//         static instance: Relationship = Relationship {
//             symbol: ::std::string::String::new(),
//             is_reference: false,
//             is_implementation: false,
//             is_type_definition: false,
//             is_definition: false,
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Relationship {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Relationship").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Relationship {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Relationship {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// ///  Occurrence associates a source position with a symbol and/or highlighting
// ///  information.
// ///
// ///  If possible, indexers should try to bundle logically related information
// ///  across occurrences into a single occurrence to reduce payload sizes.
// // @@protoc_insertion_point(message:scip.Occurrence)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Occurrence {
//     // message fields
//     ///  Half-open [start, end) range of this occurrence. Must be exactly three or four
//     ///  elements:
//     ///
//     ///  - Four elements: `[startLine, startCharacter, endLine, endCharacter]`
//     ///  - Three elements: `[startLine, startCharacter, endCharacter]`. The end line
//     ///    is inferred to have the same value as the start line.
//     ///
//     ///  It is allowed for the range to be empty (i.e. start==end).
//     ///
//     ///  Line numbers and characters are always 0-based. Make sure to increment the
//     ///  line/character values before displaying them in an editor-like UI because
//     ///  editors conventionally use 1-based numbers.
//     ///
//     ///  The 'character' value is interpreted based on the PositionEncoding for
//     ///  the Document.
//     ///
//     ///  Historical note: the original draft of this schema had a `Range` message
//     ///  type with `start` and `end` fields of type `Position`, mirroring LSP.
//     ///  Benchmarks revealed that this encoding was inefficient and that we could
//     ///  reduce the total payload size of an index by 50% by using `repeated int32`
//     ///  instead. The `repeated int32` encoding is admittedly more embarrassing to
//     ///  work with in some programming languages but we hope the performance
//     ///  improvements make up for it.
//     // @@protoc_insertion_point(field:scip.Occurrence.range)
//     pub range: ::std::vec::Vec<i32>,
//     ///  (optional) The symbol that appears at this position. See
//     ///  `SymbolInformation.symbol` for how to format symbols as strings.
//     // @@protoc_insertion_point(field:scip.Occurrence.symbol)
//     pub symbol: ::std::string::String,
//     ///  (optional) Bitset containing `SymbolRole`s in this occurrence.
//     ///  See `SymbolRole`'s documentation for how to read and write this field.
//     // @@protoc_insertion_point(field:scip.Occurrence.symbol_roles)
//     pub symbol_roles: i32,
//     ///  (optional) CommonMark-formatted documentation for this specific range. If
//     ///  empty, the `Symbol.documentation` field is used instead. One example
//     ///  where this field might be useful is when the symbol represents a generic
//     ///  function (with abstract type parameters such as `List<T>`) and at this
//     ///  occurrence we know the exact values (such as `List<String>`).
//     ///
//     ///  This field can also be used for dynamically or gradually typed languages,
//     ///  which commonly allow for type-changing assignment.
//     // @@protoc_insertion_point(field:scip.Occurrence.override_documentation)
//     pub override_documentation: ::std::vec::Vec<::std::string::String>,
//     ///  (optional) What syntax highlighting class should be used for this range?
//     // @@protoc_insertion_point(field:scip.Occurrence.syntax_kind)
//     pub syntax_kind: ::protobuf::EnumOrUnknown<SyntaxKind>,
//     ///  (optional) Diagnostics that have been reported for this specific range.
//     // @@protoc_insertion_point(field:scip.Occurrence.diagnostics)
//     pub diagnostics: ::std::vec::Vec<Diagnostic>,
//     // @@protoc_insertion_point(field:scip.Occurrence.enclosing_range)
//     pub enclosing_range: ::std::vec::Vec<i32>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Occurrence.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Occurrence {
//     fn default() -> &'a Occurrence {
//         <Occurrence as ::protobuf::Message>::default_instance()
//     }
// }

// impl Occurrence {
//     pub fn new() -> Occurrence {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(7);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "range",
//             |m: &Occurrence| { &m.range },
//             |m: &mut Occurrence| { &mut m.range },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "symbol",
//             |m: &Occurrence| { &m.symbol },
//             |m: &mut Occurrence| { &mut m.symbol },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "symbol_roles",
//             |m: &Occurrence| { &m.symbol_roles },
//             |m: &mut Occurrence| { &mut m.symbol_roles },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "override_documentation",
//             |m: &Occurrence| { &m.override_documentation },
//             |m: &mut Occurrence| { &mut m.override_documentation },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "syntax_kind",
//             |m: &Occurrence| { &m.syntax_kind },
//             |m: &mut Occurrence| { &mut m.syntax_kind },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "diagnostics",
//             |m: &Occurrence| { &m.diagnostics },
//             |m: &mut Occurrence| { &mut m.diagnostics },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "enclosing_range",
//             |m: &Occurrence| { &m.enclosing_range },
//             |m: &mut Occurrence| { &mut m.enclosing_range },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Occurrence>(
//             "Occurrence",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Occurrence {
//     const NAME: &'static str = "Occurrence";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 10 => {
//                     is.read_repeated_packed_int32_into(&mut self.range)?;
//                 },
//                 8 => {
//                     self.range.push(is.read_int32()?);
//                 },
//                 18 => {
//                     self.symbol = is.read_string()?;
//                 },
//                 24 => {
//                     self.symbol_roles = is.read_int32()?;
//                 },
//                 34 => {
//                     self.override_documentation.push(is.read_string()?);
//                 },
//                 40 => {
//                     self.syntax_kind = is.read_enum_or_unknown()?;
//                 },
//                 50 => {
//                     self.diagnostics.push(is.read_message()?);
//                 },
//                 58 => {
//                     is.read_repeated_packed_int32_into(&mut self.enclosing_range)?;
//                 },
//                 56 => {
//                     self.enclosing_range.push(is.read_int32()?);
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         my_size += ::protobuf::rt::vec_packed_int32_size(1, &self.range);
//         if !self.symbol.is_empty() {
//             my_size += ::protobuf::rt::string_size(2, &self.symbol);
//         }
//         if self.symbol_roles != 0 {
//             my_size += ::protobuf::rt::int32_size(3, self.symbol_roles);
//         }
//         for value in &self.override_documentation {
//             my_size += ::protobuf::rt::string_size(4, &value);
//         };
//         if self.syntax_kind != ::protobuf::EnumOrUnknown::new(SyntaxKind::UnspecifiedSyntaxKind) {
//             my_size += ::protobuf::rt::int32_size(5, self.syntax_kind.value());
//         }
//         for value in &self.diagnostics {
//             let len = value.compute_size();
//             my_size += 1 + ::protobuf::rt::compute_raw_varint64_size(len) + len;
//         };
//         my_size += ::protobuf::rt::vec_packed_int32_size(7, &self.enclosing_range);
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         os.write_repeated_packed_int32(1, &self.range)?;
//         if !self.symbol.is_empty() {
//             os.write_string(2, &self.symbol)?;
//         }
//         if self.symbol_roles != 0 {
//             os.write_int32(3, self.symbol_roles)?;
//         }
//         for v in &self.override_documentation {
//             os.write_string(4, &v)?;
//         };
//         if self.syntax_kind != ::protobuf::EnumOrUnknown::new(SyntaxKind::UnspecifiedSyntaxKind) {
//             os.write_enum(5, ::protobuf::EnumOrUnknown::value(&self.syntax_kind))?;
//         }
//         for v in &self.diagnostics {
//             ::protobuf::rt::write_message_field_with_cached_size(6, v, os)?;
//         };
//         os.write_repeated_packed_int32(7, &self.enclosing_range)?;
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Occurrence {
//         Occurrence::new()
//     }

//     fn clear(&mut self) {
//         self.range.clear();
//         self.symbol.clear();
//         self.symbol_roles = 0;
//         self.override_documentation.clear();
//         self.syntax_kind = ::protobuf::EnumOrUnknown::new(SyntaxKind::UnspecifiedSyntaxKind);
//         self.diagnostics.clear();
//         self.enclosing_range.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Occurrence {
//         static instance: Occurrence = Occurrence {
//             range: ::std::vec::Vec::new(),
//             symbol: ::std::string::String::new(),
//             symbol_roles: 0,
//             override_documentation: ::std::vec::Vec::new(),
//             syntax_kind: ::protobuf::EnumOrUnknown::from_i32(0),
//             diagnostics: ::std::vec::Vec::new(),
//             enclosing_range: ::std::vec::Vec::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Occurrence {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Occurrence").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Occurrence {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Occurrence {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// ///  Represents a diagnostic, such as a compiler error or warning, which should be
// ///  reported for a document.
// // @@protoc_insertion_point(message:scip.Diagnostic)
// #[derive(PartialEq,Clone,Default,Debug)]
// pub struct Diagnostic {
//     // message fields
//     ///  Should this diagnostic be reported as an error, warning, info, or hint?
//     // @@protoc_insertion_point(field:scip.Diagnostic.severity)
//     pub severity: ::protobuf::EnumOrUnknown<Severity>,
//     ///  (optional) Code of this diagnostic, which might appear in the user interface.
//     // @@protoc_insertion_point(field:scip.Diagnostic.code)
//     pub code: ::std::string::String,
//     ///  Message of this diagnostic.
//     // @@protoc_insertion_point(field:scip.Diagnostic.message)
//     pub message: ::std::string::String,
//     ///  (optional) Human-readable string describing the source of this diagnostic, e.g.
//     ///  'typescript' or 'super lint'.
//     // @@protoc_insertion_point(field:scip.Diagnostic.source)
//     pub source: ::std::string::String,
//     // @@protoc_insertion_point(field:scip.Diagnostic.tags)
//     pub tags: ::std::vec::Vec<::protobuf::EnumOrUnknown<DiagnosticTag>>,
//     // special fields
//     // @@protoc_insertion_point(special_field:scip.Diagnostic.special_fields)
//     pub special_fields: ::protobuf::SpecialFields,
// }

// impl<'a> ::std::default::Default for &'a Diagnostic {
//     fn default() -> &'a Diagnostic {
//         <Diagnostic as ::protobuf::Message>::default_instance()
//     }
// }

// impl Diagnostic {
//     pub fn new() -> Diagnostic {
//         ::std::default::Default::default()
//     }

//     fn generated_message_descriptor_data() -> ::protobuf::reflect::GeneratedMessageDescriptorData {
//         let mut fields = ::std::vec::Vec::with_capacity(5);
//         let mut oneofs = ::std::vec::Vec::with_capacity(0);
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "severity",
//             |m: &Diagnostic| { &m.severity },
//             |m: &mut Diagnostic| { &mut m.severity },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "code",
//             |m: &Diagnostic| { &m.code },
//             |m: &mut Diagnostic| { &mut m.code },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "message",
//             |m: &Diagnostic| { &m.message },
//             |m: &mut Diagnostic| { &mut m.message },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_simpler_field_accessor::<_, _>(
//             "source",
//             |m: &Diagnostic| { &m.source },
//             |m: &mut Diagnostic| { &mut m.source },
//         ));
//         fields.push(::protobuf::reflect::rt::v2::make_vec_simpler_accessor::<_, _>(
//             "tags",
//             |m: &Diagnostic| { &m.tags },
//             |m: &mut Diagnostic| { &mut m.tags },
//         ));
//         ::protobuf::reflect::GeneratedMessageDescriptorData::new_2::<Diagnostic>(
//             "Diagnostic",
//             fields,
//             oneofs,
//         )
//     }
// }

// impl ::protobuf::Message for Diagnostic {
//     const NAME: &'static str = "Diagnostic";

//     fn is_initialized(&self) -> bool {
//         true
//     }

//     fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::Result<()> {
//         while let Some(tag) = is.read_raw_tag_or_eof()? {
//             match tag {
//                 8 => {
//                     self.severity = is.read_enum_or_unknown()?;
//                 },
//                 18 => {
//                     self.code = is.read_string()?;
//                 },
//                 26 => {
//                     self.message = is.read_string()?;
//                 },
//                 34 => {
//                     self.source = is.read_string()?;
//                 },
//                 40 => {
//                     self.tags.push(is.read_enum_or_unknown()?);
//                 },
//                 42 => {
//                     ::protobuf::rt::read_repeated_packed_enum_or_unknown_into(is, &mut self.tags)?
//                 },
//                 tag => {
//                     ::protobuf::rt::read_unknown_or_skip_group(tag, is, self.special_fields.mut_unknown_fields())?;
//                 },
//             };
//         }
//         ::std::result::Result::Ok(())
//     }

//     // Compute sizes of nested messages
//     #[allow(unused_variables)]
//     fn compute_size(&self) -> u64 {
//         let mut my_size = 0;
//         if self.severity != ::protobuf::EnumOrUnknown::new(Severity::UnspecifiedSeverity) {
//             my_size += ::protobuf::rt::int32_size(1, self.severity.value());
//         }
//         if !self.code.is_empty() {
//             my_size += ::protobuf::rt::string_size(2, &self.code);
//         }
//         if !self.message.is_empty() {
//             my_size += ::protobuf::rt::string_size(3, &self.message);
//         }
//         if !self.source.is_empty() {
//             my_size += ::protobuf::rt::string_size(4, &self.source);
//         }
//         my_size += ::protobuf::rt::vec_packed_enum_or_unknown_size(5, &self.tags);
//         my_size += ::protobuf::rt::unknown_fields_size(self.special_fields.unknown_fields());
//         self.special_fields.cached_size().set(my_size as u32);
//         my_size
//     }

//     fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::Result<()> {
//         if self.severity != ::protobuf::EnumOrUnknown::new(Severity::UnspecifiedSeverity) {
//             os.write_enum(1, ::protobuf::EnumOrUnknown::value(&self.severity))?;
//         }
//         if !self.code.is_empty() {
//             os.write_string(2, &self.code)?;
//         }
//         if !self.message.is_empty() {
//             os.write_string(3, &self.message)?;
//         }
//         if !self.source.is_empty() {
//             os.write_string(4, &self.source)?;
//         }
//         os.write_repeated_packed_enum_or_unknown(5, &self.tags)?;
//         os.write_unknown_fields(self.special_fields.unknown_fields())?;
//         ::std::result::Result::Ok(())
//     }

//     fn special_fields(&self) -> &::protobuf::SpecialFields {
//         &self.special_fields
//     }

//     fn mut_special_fields(&mut self) -> &mut ::protobuf::SpecialFields {
//         &mut self.special_fields
//     }

//     fn new() -> Diagnostic {
//         Diagnostic::new()
//     }

//     fn clear(&mut self) {
//         self.severity = ::protobuf::EnumOrUnknown::new(Severity::UnspecifiedSeverity);
//         self.code.clear();
//         self.message.clear();
//         self.source.clear();
//         self.tags.clear();
//         self.special_fields.clear();
//     }

//     fn default_instance() -> &'static Diagnostic {
//         static instance: Diagnostic = Diagnostic {
//             severity: ::protobuf::EnumOrUnknown::from_i32(0),
//             code: ::std::string::String::new(),
//             message: ::std::string::String::new(),
//             source: ::std::string::String::new(),
//             tags: ::std::vec::Vec::new(),
//             special_fields: ::protobuf::SpecialFields::new(),
//         };
//         &instance
//     }
// }

// impl ::protobuf::MessageFull for Diagnostic {
//     fn descriptor() -> ::protobuf::reflect::MessageDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().message_by_package_relative_name("Diagnostic").unwrap()).clone()
//     }
// }

// impl ::std::fmt::Display for Diagnostic {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
//         ::protobuf::text_format::fmt(self, f)
//     }
// }

// impl ::protobuf::reflect::ProtobufValue for Diagnostic {
//     type RuntimeType = ::protobuf::reflect::rt::RuntimeTypeMessage<Self>;
// }

// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.ProtocolVersion)
// pub enum ProtocolVersion {
//     // @@protoc_insertion_point(enum_value:scip.ProtocolVersion.UnspecifiedProtocolVersion)
//     UnspecifiedProtocolVersion = 0,
// }

// impl ::protobuf::Enum for ProtocolVersion {
//     const NAME: &'static str = "ProtocolVersion";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<ProtocolVersion> {
//         match value {
//             0 => ::std::option::Option::Some(ProtocolVersion::UnspecifiedProtocolVersion),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<ProtocolVersion> {
//         match str {
//             "UnspecifiedProtocolVersion" => ::std::option::Option::Some(ProtocolVersion::UnspecifiedProtocolVersion),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [ProtocolVersion] = &[
//         ProtocolVersion::UnspecifiedProtocolVersion,
//     ];
// }

// impl ::protobuf::EnumFull for ProtocolVersion {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("ProtocolVersion").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = *self as usize;
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for ProtocolVersion {
//     fn default() -> Self {
//         ProtocolVersion::UnspecifiedProtocolVersion
//     }
// }

// impl ProtocolVersion {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<ProtocolVersion>("ProtocolVersion")
//     }
// }

// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.TextEncoding)
// pub enum TextEncoding {
//     // @@protoc_insertion_point(enum_value:scip.TextEncoding.UnspecifiedTextEncoding)
//     UnspecifiedTextEncoding = 0,
//     // @@protoc_insertion_point(enum_value:scip.TextEncoding.UTF8)
//     UTF8 = 1,
//     // @@protoc_insertion_point(enum_value:scip.TextEncoding.UTF16)
//     UTF16 = 2,
// }

// impl ::protobuf::Enum for TextEncoding {
//     const NAME: &'static str = "TextEncoding";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<TextEncoding> {
//         match value {
//             0 => ::std::option::Option::Some(TextEncoding::UnspecifiedTextEncoding),
//             1 => ::std::option::Option::Some(TextEncoding::UTF8),
//             2 => ::std::option::Option::Some(TextEncoding::UTF16),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<TextEncoding> {
//         match str {
//             "UnspecifiedTextEncoding" => ::std::option::Option::Some(TextEncoding::UnspecifiedTextEncoding),
//             "UTF8" => ::std::option::Option::Some(TextEncoding::UTF8),
//             "UTF16" => ::std::option::Option::Some(TextEncoding::UTF16),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [TextEncoding] = &[
//         TextEncoding::UnspecifiedTextEncoding,
//         TextEncoding::UTF8,
//         TextEncoding::UTF16,
//     ];
// }

// impl ::protobuf::EnumFull for TextEncoding {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("TextEncoding").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = *self as usize;
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for TextEncoding {
//     fn default() -> Self {
//         TextEncoding::UnspecifiedTextEncoding
//     }
// }

// impl TextEncoding {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<TextEncoding>("TextEncoding")
//     }
// }

// ///  Encoding used to interpret the 'character' value in source ranges.
// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.PositionEncoding)
// pub enum PositionEncoding {
//     // @@protoc_insertion_point(enum_value:scip.PositionEncoding.UnspecifiedPositionEncoding)
//     UnspecifiedPositionEncoding = 0,
//     // @@protoc_insertion_point(enum_value:scip.PositionEncoding.UTF8CodeUnitOffsetFromLineStart)
//     UTF8CodeUnitOffsetFromLineStart = 1,
//     // @@protoc_insertion_point(enum_value:scip.PositionEncoding.UTF16CodeUnitOffsetFromLineStart)
//     UTF16CodeUnitOffsetFromLineStart = 2,
//     // @@protoc_insertion_point(enum_value:scip.PositionEncoding.UTF32CodeUnitOffsetFromLineStart)
//     UTF32CodeUnitOffsetFromLineStart = 3,
// }

// impl ::protobuf::Enum for PositionEncoding {
//     const NAME: &'static str = "PositionEncoding";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<PositionEncoding> {
//         match value {
//             0 => ::std::option::Option::Some(PositionEncoding::UnspecifiedPositionEncoding),
//             1 => ::std::option::Option::Some(PositionEncoding::UTF8CodeUnitOffsetFromLineStart),
//             2 => ::std::option::Option::Some(PositionEncoding::UTF16CodeUnitOffsetFromLineStart),
//             3 => ::std::option::Option::Some(PositionEncoding::UTF32CodeUnitOffsetFromLineStart),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<PositionEncoding> {
//         match str {
//             "UnspecifiedPositionEncoding" => ::std::option::Option::Some(PositionEncoding::UnspecifiedPositionEncoding),
//             "UTF8CodeUnitOffsetFromLineStart" => ::std::option::Option::Some(PositionEncoding::UTF8CodeUnitOffsetFromLineStart),
//             "UTF16CodeUnitOffsetFromLineStart" => ::std::option::Option::Some(PositionEncoding::UTF16CodeUnitOffsetFromLineStart),
//             "UTF32CodeUnitOffsetFromLineStart" => ::std::option::Option::Some(PositionEncoding::UTF32CodeUnitOffsetFromLineStart),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [PositionEncoding] = &[
//         PositionEncoding::UnspecifiedPositionEncoding,
//         PositionEncoding::UTF8CodeUnitOffsetFromLineStart,
//         PositionEncoding::UTF16CodeUnitOffsetFromLineStart,
//         PositionEncoding::UTF32CodeUnitOffsetFromLineStart,
//     ];
// }

// impl ::protobuf::EnumFull for PositionEncoding {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("PositionEncoding").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = *self as usize;
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for PositionEncoding {
//     fn default() -> Self {
//         PositionEncoding::UnspecifiedPositionEncoding
//     }
// }

// impl PositionEncoding {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<PositionEncoding>("PositionEncoding")
//     }
// }

// ///  SymbolRole declares what "role" a symbol has in an occurrence. A role is
// ///  encoded as a bitset where each bit represents a different role. For example,
// ///  to determine if the `Import` role is set, test whether the second bit of the
// ///  enum value is defined. In pseudocode, this can be implemented with the
// ///  logic: `const isImportRole = (role.value & SymbolRole.Import.value) > 0`.
// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.SymbolRole)
// pub enum SymbolRole {
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.UnspecifiedSymbolRole)
//     UnspecifiedSymbolRole = 0,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.Definition)
//     Definition = 1,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.Import)
//     Import = 2,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.WriteAccess)
//     WriteAccess = 4,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.ReadAccess)
//     ReadAccess = 8,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.Generated)
//     Generated = 16,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.Test)
//     Test = 32,
//     // @@protoc_insertion_point(enum_value:scip.SymbolRole.ForwardDefinition)
//     ForwardDefinition = 64,
// }

// impl ::protobuf::Enum for SymbolRole {
//     const NAME: &'static str = "SymbolRole";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<SymbolRole> {
//         match value {
//             0 => ::std::option::Option::Some(SymbolRole::UnspecifiedSymbolRole),
//             1 => ::std::option::Option::Some(SymbolRole::Definition),
//             2 => ::std::option::Option::Some(SymbolRole::Import),
//             4 => ::std::option::Option::Some(SymbolRole::WriteAccess),
//             8 => ::std::option::Option::Some(SymbolRole::ReadAccess),
//             16 => ::std::option::Option::Some(SymbolRole::Generated),
//             32 => ::std::option::Option::Some(SymbolRole::Test),
//             64 => ::std::option::Option::Some(SymbolRole::ForwardDefinition),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<SymbolRole> {
//         match str {
//             "UnspecifiedSymbolRole" => ::std::option::Option::Some(SymbolRole::UnspecifiedSymbolRole),
//             "Definition" => ::std::option::Option::Some(SymbolRole::Definition),
//             "Import" => ::std::option::Option::Some(SymbolRole::Import),
//             "WriteAccess" => ::std::option::Option::Some(SymbolRole::WriteAccess),
//             "ReadAccess" => ::std::option::Option::Some(SymbolRole::ReadAccess),
//             "Generated" => ::std::option::Option::Some(SymbolRole::Generated),
//             "Test" => ::std::option::Option::Some(SymbolRole::Test),
//             "ForwardDefinition" => ::std::option::Option::Some(SymbolRole::ForwardDefinition),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [SymbolRole] = &[
//         SymbolRole::UnspecifiedSymbolRole,
//         SymbolRole::Definition,
//         SymbolRole::Import,
//         SymbolRole::WriteAccess,
//         SymbolRole::ReadAccess,
//         SymbolRole::Generated,
//         SymbolRole::Test,
//         SymbolRole::ForwardDefinition,
//     ];
// }

// impl ::protobuf::EnumFull for SymbolRole {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("SymbolRole").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = match self {
//             SymbolRole::UnspecifiedSymbolRole => 0,
//             SymbolRole::Definition => 1,
//             SymbolRole::Import => 2,
//             SymbolRole::WriteAccess => 3,
//             SymbolRole::ReadAccess => 4,
//             SymbolRole::Generated => 5,
//             SymbolRole::Test => 6,
//             SymbolRole::ForwardDefinition => 7,
//         };
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for SymbolRole {
//     fn default() -> Self {
//         SymbolRole::UnspecifiedSymbolRole
//     }
// }

// impl SymbolRole {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<SymbolRole>("SymbolRole")
//     }
// }

// // Note: you cannot use pattern matching for enums with allow_alias option
// #[derive(Clone,Copy,Eq,Debug)]
// // @@protoc_insertion_point(enum:scip.SyntaxKind)
// pub enum SyntaxKind {
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.UnspecifiedSyntaxKind)
//     UnspecifiedSyntaxKind, // 0
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.Comment)
//     Comment, // 1
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.PunctuationDelimiter)
//     PunctuationDelimiter, // 2
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.PunctuationBracket)
//     PunctuationBracket, // 3
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.Keyword)
//     Keyword, // 4
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierKeyword)
//     IdentifierKeyword, // 4
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierOperator)
//     IdentifierOperator, // 5
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.Identifier)
//     Identifier, // 6
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierBuiltin)
//     IdentifierBuiltin, // 7
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierNull)
//     IdentifierNull, // 8
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierConstant)
//     IdentifierConstant, // 9
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierMutableGlobal)
//     IdentifierMutableGlobal, // 10
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierParameter)
//     IdentifierParameter, // 11
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierLocal)
//     IdentifierLocal, // 12
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierShadowed)
//     IdentifierShadowed, // 13
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierNamespace)
//     IdentifierNamespace, // 14
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierModule)
//     IdentifierModule, // 14
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierFunction)
//     IdentifierFunction, // 15
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierFunctionDefinition)
//     IdentifierFunctionDefinition, // 16
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierMacro)
//     IdentifierMacro, // 17
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierMacroDefinition)
//     IdentifierMacroDefinition, // 18
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierType)
//     IdentifierType, // 19
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierBuiltinType)
//     IdentifierBuiltinType, // 20
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.IdentifierAttribute)
//     IdentifierAttribute, // 21
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.RegexEscape)
//     RegexEscape, // 22
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.RegexRepeated)
//     RegexRepeated, // 23
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.RegexWildcard)
//     RegexWildcard, // 24
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.RegexDelimiter)
//     RegexDelimiter, // 25
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.RegexJoin)
//     RegexJoin, // 26
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.StringLiteral)
//     StringLiteral, // 27
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.StringLiteralEscape)
//     StringLiteralEscape, // 28
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.StringLiteralSpecial)
//     StringLiteralSpecial, // 29
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.StringLiteralKey)
//     StringLiteralKey, // 30
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.CharacterLiteral)
//     CharacterLiteral, // 31
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.NumericLiteral)
//     NumericLiteral, // 32
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.BooleanLiteral)
//     BooleanLiteral, // 33
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.Tag)
//     Tag, // 34
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.TagAttribute)
//     TagAttribute, // 35
//     // @@protoc_insertion_point(enum_value:scip.SyntaxKind.TagDelimiter)
//     TagDelimiter, // 36
// }

// impl ::std::cmp::PartialEq for SyntaxKind {
//     fn eq(&self, other: &Self) -> bool {
//         ::protobuf::Enum::value(self) == ::protobuf::Enum::value(other)
//     }
// }

// impl ::std::hash::Hash for SyntaxKind {
//     fn hash<H : ::std::hash::Hasher>(&self, state: &mut H) {
//         state.write_i32(::protobuf::Enum::value(self))
//     }
// }

// impl ::protobuf::Enum for SyntaxKind {
//     const NAME: &'static str = "SyntaxKind";

//     fn value(&self) -> i32 {
//         match *self {
//             SyntaxKind::UnspecifiedSyntaxKind => 0,
//             SyntaxKind::Comment => 1,
//             SyntaxKind::PunctuationDelimiter => 2,
//             SyntaxKind::PunctuationBracket => 3,
//             SyntaxKind::Keyword => 4,
//             SyntaxKind::IdentifierKeyword => 4,
//             SyntaxKind::IdentifierOperator => 5,
//             SyntaxKind::Identifier => 6,
//             SyntaxKind::IdentifierBuiltin => 7,
//             SyntaxKind::IdentifierNull => 8,
//             SyntaxKind::IdentifierConstant => 9,
//             SyntaxKind::IdentifierMutableGlobal => 10,
//             SyntaxKind::IdentifierParameter => 11,
//             SyntaxKind::IdentifierLocal => 12,
//             SyntaxKind::IdentifierShadowed => 13,
//             SyntaxKind::IdentifierNamespace => 14,
//             SyntaxKind::IdentifierModule => 14,
//             SyntaxKind::IdentifierFunction => 15,
//             SyntaxKind::IdentifierFunctionDefinition => 16,
//             SyntaxKind::IdentifierMacro => 17,
//             SyntaxKind::IdentifierMacroDefinition => 18,
//             SyntaxKind::IdentifierType => 19,
//             SyntaxKind::IdentifierBuiltinType => 20,
//             SyntaxKind::IdentifierAttribute => 21,
//             SyntaxKind::RegexEscape => 22,
//             SyntaxKind::RegexRepeated => 23,
//             SyntaxKind::RegexWildcard => 24,
//             SyntaxKind::RegexDelimiter => 25,
//             SyntaxKind::RegexJoin => 26,
//             SyntaxKind::StringLiteral => 27,
//             SyntaxKind::StringLiteralEscape => 28,
//             SyntaxKind::StringLiteralSpecial => 29,
//             SyntaxKind::StringLiteralKey => 30,
//             SyntaxKind::CharacterLiteral => 31,
//             SyntaxKind::NumericLiteral => 32,
//             SyntaxKind::BooleanLiteral => 33,
//             SyntaxKind::Tag => 34,
//             SyntaxKind::TagAttribute => 35,
//             SyntaxKind::TagDelimiter => 36,
//         }
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<SyntaxKind> {
//         match value {
//             0 => ::std::option::Option::Some(SyntaxKind::UnspecifiedSyntaxKind),
//             1 => ::std::option::Option::Some(SyntaxKind::Comment),
//             2 => ::std::option::Option::Some(SyntaxKind::PunctuationDelimiter),
//             3 => ::std::option::Option::Some(SyntaxKind::PunctuationBracket),
//             4 => ::std::option::Option::Some(SyntaxKind::Keyword),
//             5 => ::std::option::Option::Some(SyntaxKind::IdentifierOperator),
//             6 => ::std::option::Option::Some(SyntaxKind::Identifier),
//             7 => ::std::option::Option::Some(SyntaxKind::IdentifierBuiltin),
//             8 => ::std::option::Option::Some(SyntaxKind::IdentifierNull),
//             9 => ::std::option::Option::Some(SyntaxKind::IdentifierConstant),
//             10 => ::std::option::Option::Some(SyntaxKind::IdentifierMutableGlobal),
//             11 => ::std::option::Option::Some(SyntaxKind::IdentifierParameter),
//             12 => ::std::option::Option::Some(SyntaxKind::IdentifierLocal),
//             13 => ::std::option::Option::Some(SyntaxKind::IdentifierShadowed),
//             14 => ::std::option::Option::Some(SyntaxKind::IdentifierNamespace),
//             15 => ::std::option::Option::Some(SyntaxKind::IdentifierFunction),
//             16 => ::std::option::Option::Some(SyntaxKind::IdentifierFunctionDefinition),
//             17 => ::std::option::Option::Some(SyntaxKind::IdentifierMacro),
//             18 => ::std::option::Option::Some(SyntaxKind::IdentifierMacroDefinition),
//             19 => ::std::option::Option::Some(SyntaxKind::IdentifierType),
//             20 => ::std::option::Option::Some(SyntaxKind::IdentifierBuiltinType),
//             21 => ::std::option::Option::Some(SyntaxKind::IdentifierAttribute),
//             22 => ::std::option::Option::Some(SyntaxKind::RegexEscape),
//             23 => ::std::option::Option::Some(SyntaxKind::RegexRepeated),
//             24 => ::std::option::Option::Some(SyntaxKind::RegexWildcard),
//             25 => ::std::option::Option::Some(SyntaxKind::RegexDelimiter),
//             26 => ::std::option::Option::Some(SyntaxKind::RegexJoin),
//             27 => ::std::option::Option::Some(SyntaxKind::StringLiteral),
//             28 => ::std::option::Option::Some(SyntaxKind::StringLiteralEscape),
//             29 => ::std::option::Option::Some(SyntaxKind::StringLiteralSpecial),
//             30 => ::std::option::Option::Some(SyntaxKind::StringLiteralKey),
//             31 => ::std::option::Option::Some(SyntaxKind::CharacterLiteral),
//             32 => ::std::option::Option::Some(SyntaxKind::NumericLiteral),
//             33 => ::std::option::Option::Some(SyntaxKind::BooleanLiteral),
//             34 => ::std::option::Option::Some(SyntaxKind::Tag),
//             35 => ::std::option::Option::Some(SyntaxKind::TagAttribute),
//             36 => ::std::option::Option::Some(SyntaxKind::TagDelimiter),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<SyntaxKind> {
//         match str {
//             "UnspecifiedSyntaxKind" => ::std::option::Option::Some(SyntaxKind::UnspecifiedSyntaxKind),
//             "Comment" => ::std::option::Option::Some(SyntaxKind::Comment),
//             "PunctuationDelimiter" => ::std::option::Option::Some(SyntaxKind::PunctuationDelimiter),
//             "PunctuationBracket" => ::std::option::Option::Some(SyntaxKind::PunctuationBracket),
//             "Keyword" => ::std::option::Option::Some(SyntaxKind::Keyword),
//             "IdentifierOperator" => ::std::option::Option::Some(SyntaxKind::IdentifierOperator),
//             "Identifier" => ::std::option::Option::Some(SyntaxKind::Identifier),
//             "IdentifierBuiltin" => ::std::option::Option::Some(SyntaxKind::IdentifierBuiltin),
//             "IdentifierNull" => ::std::option::Option::Some(SyntaxKind::IdentifierNull),
//             "IdentifierConstant" => ::std::option::Option::Some(SyntaxKind::IdentifierConstant),
//             "IdentifierMutableGlobal" => ::std::option::Option::Some(SyntaxKind::IdentifierMutableGlobal),
//             "IdentifierParameter" => ::std::option::Option::Some(SyntaxKind::IdentifierParameter),
//             "IdentifierLocal" => ::std::option::Option::Some(SyntaxKind::IdentifierLocal),
//             "IdentifierShadowed" => ::std::option::Option::Some(SyntaxKind::IdentifierShadowed),
//             "IdentifierNamespace" => ::std::option::Option::Some(SyntaxKind::IdentifierNamespace),
//             "IdentifierFunction" => ::std::option::Option::Some(SyntaxKind::IdentifierFunction),
//             "IdentifierFunctionDefinition" => ::std::option::Option::Some(SyntaxKind::IdentifierFunctionDefinition),
//             "IdentifierMacro" => ::std::option::Option::Some(SyntaxKind::IdentifierMacro),
//             "IdentifierMacroDefinition" => ::std::option::Option::Some(SyntaxKind::IdentifierMacroDefinition),
//             "IdentifierType" => ::std::option::Option::Some(SyntaxKind::IdentifierType),
//             "IdentifierBuiltinType" => ::std::option::Option::Some(SyntaxKind::IdentifierBuiltinType),
//             "IdentifierAttribute" => ::std::option::Option::Some(SyntaxKind::IdentifierAttribute),
//             "RegexEscape" => ::std::option::Option::Some(SyntaxKind::RegexEscape),
//             "RegexRepeated" => ::std::option::Option::Some(SyntaxKind::RegexRepeated),
//             "RegexWildcard" => ::std::option::Option::Some(SyntaxKind::RegexWildcard),
//             "RegexDelimiter" => ::std::option::Option::Some(SyntaxKind::RegexDelimiter),
//             "RegexJoin" => ::std::option::Option::Some(SyntaxKind::RegexJoin),
//             "StringLiteral" => ::std::option::Option::Some(SyntaxKind::StringLiteral),
//             "StringLiteralEscape" => ::std::option::Option::Some(SyntaxKind::StringLiteralEscape),
//             "StringLiteralSpecial" => ::std::option::Option::Some(SyntaxKind::StringLiteralSpecial),
//             "StringLiteralKey" => ::std::option::Option::Some(SyntaxKind::StringLiteralKey),
//             "CharacterLiteral" => ::std::option::Option::Some(SyntaxKind::CharacterLiteral),
//             "NumericLiteral" => ::std::option::Option::Some(SyntaxKind::NumericLiteral),
//             "BooleanLiteral" => ::std::option::Option::Some(SyntaxKind::BooleanLiteral),
//             "Tag" => ::std::option::Option::Some(SyntaxKind::Tag),
//             "TagAttribute" => ::std::option::Option::Some(SyntaxKind::TagAttribute),
//             "TagDelimiter" => ::std::option::Option::Some(SyntaxKind::TagDelimiter),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [SyntaxKind] = &[
//         SyntaxKind::UnspecifiedSyntaxKind,
//         SyntaxKind::Comment,
//         SyntaxKind::PunctuationDelimiter,
//         SyntaxKind::PunctuationBracket,
//         SyntaxKind::Keyword,
//         SyntaxKind::IdentifierKeyword,
//         SyntaxKind::IdentifierOperator,
//         SyntaxKind::Identifier,
//         SyntaxKind::IdentifierBuiltin,
//         SyntaxKind::IdentifierNull,
//         SyntaxKind::IdentifierConstant,
//         SyntaxKind::IdentifierMutableGlobal,
//         SyntaxKind::IdentifierParameter,
//         SyntaxKind::IdentifierLocal,
//         SyntaxKind::IdentifierShadowed,
//         SyntaxKind::IdentifierNamespace,
//         SyntaxKind::IdentifierModule,
//         SyntaxKind::IdentifierFunction,
//         SyntaxKind::IdentifierFunctionDefinition,
//         SyntaxKind::IdentifierMacro,
//         SyntaxKind::IdentifierMacroDefinition,
//         SyntaxKind::IdentifierType,
//         SyntaxKind::IdentifierBuiltinType,
//         SyntaxKind::IdentifierAttribute,
//         SyntaxKind::RegexEscape,
//         SyntaxKind::RegexRepeated,
//         SyntaxKind::RegexWildcard,
//         SyntaxKind::RegexDelimiter,
//         SyntaxKind::RegexJoin,
//         SyntaxKind::StringLiteral,
//         SyntaxKind::StringLiteralEscape,
//         SyntaxKind::StringLiteralSpecial,
//         SyntaxKind::StringLiteralKey,
//         SyntaxKind::CharacterLiteral,
//         SyntaxKind::NumericLiteral,
//         SyntaxKind::BooleanLiteral,
//         SyntaxKind::Tag,
//         SyntaxKind::TagAttribute,
//         SyntaxKind::TagDelimiter,
//     ];
// }

// impl ::protobuf::EnumFull for SyntaxKind {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("SyntaxKind").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = match self {
//             SyntaxKind::UnspecifiedSyntaxKind => 0,
//             SyntaxKind::Comment => 1,
//             SyntaxKind::PunctuationDelimiter => 2,
//             SyntaxKind::PunctuationBracket => 3,
//             SyntaxKind::Keyword => 4,
//             SyntaxKind::IdentifierKeyword => 5,
//             SyntaxKind::IdentifierOperator => 6,
//             SyntaxKind::Identifier => 7,
//             SyntaxKind::IdentifierBuiltin => 8,
//             SyntaxKind::IdentifierNull => 9,
//             SyntaxKind::IdentifierConstant => 10,
//             SyntaxKind::IdentifierMutableGlobal => 11,
//             SyntaxKind::IdentifierParameter => 12,
//             SyntaxKind::IdentifierLocal => 13,
//             SyntaxKind::IdentifierShadowed => 14,
//             SyntaxKind::IdentifierNamespace => 15,
//             SyntaxKind::IdentifierModule => 16,
//             SyntaxKind::IdentifierFunction => 17,
//             SyntaxKind::IdentifierFunctionDefinition => 18,
//             SyntaxKind::IdentifierMacro => 19,
//             SyntaxKind::IdentifierMacroDefinition => 20,
//             SyntaxKind::IdentifierType => 21,
//             SyntaxKind::IdentifierBuiltinType => 22,
//             SyntaxKind::IdentifierAttribute => 23,
//             SyntaxKind::RegexEscape => 24,
//             SyntaxKind::RegexRepeated => 25,
//             SyntaxKind::RegexWildcard => 26,
//             SyntaxKind::RegexDelimiter => 27,
//             SyntaxKind::RegexJoin => 28,
//             SyntaxKind::StringLiteral => 29,
//             SyntaxKind::StringLiteralEscape => 30,
//             SyntaxKind::StringLiteralSpecial => 31,
//             SyntaxKind::StringLiteralKey => 32,
//             SyntaxKind::CharacterLiteral => 33,
//             SyntaxKind::NumericLiteral => 34,
//             SyntaxKind::BooleanLiteral => 35,
//             SyntaxKind::Tag => 36,
//             SyntaxKind::TagAttribute => 37,
//             SyntaxKind::TagDelimiter => 38,
//         };
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for SyntaxKind {
//     fn default() -> Self {
//         SyntaxKind::UnspecifiedSyntaxKind
//     }
// }

// impl SyntaxKind {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<SyntaxKind>("SyntaxKind")
//     }
// }

// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.Severity)
// pub enum Severity {
//     // @@protoc_insertion_point(enum_value:scip.Severity.UnspecifiedSeverity)
//     UnspecifiedSeverity = 0,
//     // @@protoc_insertion_point(enum_value:scip.Severity.Error)
//     Error = 1,
//     // @@protoc_insertion_point(enum_value:scip.Severity.Warning)
//     Warning = 2,
//     // @@protoc_insertion_point(enum_value:scip.Severity.Information)
//     Information = 3,
//     // @@protoc_insertion_point(enum_value:scip.Severity.Hint)
//     Hint = 4,
// }

// impl ::protobuf::Enum for Severity {
//     const NAME: &'static str = "Severity";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<Severity> {
//         match value {
//             0 => ::std::option::Option::Some(Severity::UnspecifiedSeverity),
//             1 => ::std::option::Option::Some(Severity::Error),
//             2 => ::std::option::Option::Some(Severity::Warning),
//             3 => ::std::option::Option::Some(Severity::Information),
//             4 => ::std::option::Option::Some(Severity::Hint),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<Severity> {
//         match str {
//             "UnspecifiedSeverity" => ::std::option::Option::Some(Severity::UnspecifiedSeverity),
//             "Error" => ::std::option::Option::Some(Severity::Error),
//             "Warning" => ::std::option::Option::Some(Severity::Warning),
//             "Information" => ::std::option::Option::Some(Severity::Information),
//             "Hint" => ::std::option::Option::Some(Severity::Hint),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [Severity] = &[
//         Severity::UnspecifiedSeverity,
//         Severity::Error,
//         Severity::Warning,
//         Severity::Information,
//         Severity::Hint,
//     ];
// }

// impl ::protobuf::EnumFull for Severity {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("Severity").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = *self as usize;
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for Severity {
//     fn default() -> Self {
//         Severity::UnspecifiedSeverity
//     }
// }

// impl Severity {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<Severity>("Severity")
//     }
// }

// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.DiagnosticTag)
// pub enum DiagnosticTag {
//     // @@protoc_insertion_point(enum_value:scip.DiagnosticTag.UnspecifiedDiagnosticTag)
//     UnspecifiedDiagnosticTag = 0,
//     // @@protoc_insertion_point(enum_value:scip.DiagnosticTag.Unnecessary)
//     Unnecessary = 1,
//     // @@protoc_insertion_point(enum_value:scip.DiagnosticTag.Deprecated)
//     Deprecated = 2,
// }

// impl ::protobuf::Enum for DiagnosticTag {
//     const NAME: &'static str = "DiagnosticTag";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<DiagnosticTag> {
//         match value {
//             0 => ::std::option::Option::Some(DiagnosticTag::UnspecifiedDiagnosticTag),
//             1 => ::std::option::Option::Some(DiagnosticTag::Unnecessary),
//             2 => ::std::option::Option::Some(DiagnosticTag::Deprecated),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<DiagnosticTag> {
//         match str {
//             "UnspecifiedDiagnosticTag" => ::std::option::Option::Some(DiagnosticTag::UnspecifiedDiagnosticTag),
//             "Unnecessary" => ::std::option::Option::Some(DiagnosticTag::Unnecessary),
//             "Deprecated" => ::std::option::Option::Some(DiagnosticTag::Deprecated),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [DiagnosticTag] = &[
//         DiagnosticTag::UnspecifiedDiagnosticTag,
//         DiagnosticTag::Unnecessary,
//         DiagnosticTag::Deprecated,
//     ];
// }

// impl ::protobuf::EnumFull for DiagnosticTag {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("DiagnosticTag").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = *self as usize;
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for DiagnosticTag {
//     fn default() -> Self {
//         DiagnosticTag::UnspecifiedDiagnosticTag
//     }
// }

// impl DiagnosticTag {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<DiagnosticTag>("DiagnosticTag")
//     }
// }

// ///  Language standardises names of common programming languages that can be used
// ///  for the `Document.language` field. The primary purpose of this enum is to
// ///  prevent a situation where we have a single programming language ends up with
// ///  multiple string representations. For example, the C++ language uses the name
// ///  "CPP" in this enum and other names such as "cpp" are incompatible.
// ///  Feel free to send a pull-request to add missing programming languages.
// #[derive(Clone,Copy,PartialEq,Eq,Debug,Hash)]
// // @@protoc_insertion_point(enum:scip.Language)
// pub enum Language {
//     // @@protoc_insertion_point(enum_value:scip.Language.UnspecifiedLanguage)
//     UnspecifiedLanguage = 0,
//     // @@protoc_insertion_point(enum_value:scip.Language.ABAP)
//     ABAP = 60,
//     // @@protoc_insertion_point(enum_value:scip.Language.Apex)
//     Apex = 96,
//     // @@protoc_insertion_point(enum_value:scip.Language.APL)
//     APL = 49,
//     // @@protoc_insertion_point(enum_value:scip.Language.Ada)
//     Ada = 39,
//     // @@protoc_insertion_point(enum_value:scip.Language.Agda)
//     Agda = 45,
//     // @@protoc_insertion_point(enum_value:scip.Language.AsciiDoc)
//     AsciiDoc = 86,
//     // @@protoc_insertion_point(enum_value:scip.Language.Assembly)
//     Assembly = 58,
//     // @@protoc_insertion_point(enum_value:scip.Language.Awk)
//     Awk = 66,
//     // @@protoc_insertion_point(enum_value:scip.Language.Bat)
//     Bat = 68,
//     // @@protoc_insertion_point(enum_value:scip.Language.BibTeX)
//     BibTeX = 81,
//     // @@protoc_insertion_point(enum_value:scip.Language.C)
//     C = 34,
//     // @@protoc_insertion_point(enum_value:scip.Language.COBOL)
//     COBOL = 59,
//     // @@protoc_insertion_point(enum_value:scip.Language.CPP)
//     CPP = 35,
//     // @@protoc_insertion_point(enum_value:scip.Language.CSS)
//     CSS = 26,
//     // @@protoc_insertion_point(enum_value:scip.Language.CSharp)
//     CSharp = 1,
//     // @@protoc_insertion_point(enum_value:scip.Language.Clojure)
//     Clojure = 8,
//     // @@protoc_insertion_point(enum_value:scip.Language.Coffeescript)
//     Coffeescript = 21,
//     // @@protoc_insertion_point(enum_value:scip.Language.CommonLisp)
//     CommonLisp = 9,
//     // @@protoc_insertion_point(enum_value:scip.Language.Coq)
//     Coq = 47,
//     // @@protoc_insertion_point(enum_value:scip.Language.CUDA)
//     CUDA = 97,
//     // @@protoc_insertion_point(enum_value:scip.Language.Dart)
//     Dart = 3,
//     // @@protoc_insertion_point(enum_value:scip.Language.Delphi)
//     Delphi = 57,
//     // @@protoc_insertion_point(enum_value:scip.Language.Diff)
//     Diff = 88,
//     // @@protoc_insertion_point(enum_value:scip.Language.Dockerfile)
//     Dockerfile = 80,
//     // @@protoc_insertion_point(enum_value:scip.Language.Dyalog)
//     Dyalog = 50,
//     // @@protoc_insertion_point(enum_value:scip.Language.Elixir)
//     Elixir = 17,
//     // @@protoc_insertion_point(enum_value:scip.Language.Erlang)
//     Erlang = 18,
//     // @@protoc_insertion_point(enum_value:scip.Language.FSharp)
//     FSharp = 42,
//     // @@protoc_insertion_point(enum_value:scip.Language.Fish)
//     Fish = 65,
//     // @@protoc_insertion_point(enum_value:scip.Language.Flow)
//     Flow = 24,
//     // @@protoc_insertion_point(enum_value:scip.Language.Fortran)
//     Fortran = 56,
//     // @@protoc_insertion_point(enum_value:scip.Language.Git_Commit)
//     Git_Commit = 91,
//     // @@protoc_insertion_point(enum_value:scip.Language.Git_Config)
//     Git_Config = 89,
//     // @@protoc_insertion_point(enum_value:scip.Language.Git_Rebase)
//     Git_Rebase = 92,
//     // @@protoc_insertion_point(enum_value:scip.Language.Go)
//     Go = 33,
//     // @@protoc_insertion_point(enum_value:scip.Language.GraphQL)
//     GraphQL = 98,
//     // @@protoc_insertion_point(enum_value:scip.Language.Groovy)
//     Groovy = 7,
//     // @@protoc_insertion_point(enum_value:scip.Language.HTML)
//     HTML = 30,
//     // @@protoc_insertion_point(enum_value:scip.Language.Hack)
//     Hack = 20,
//     // @@protoc_insertion_point(enum_value:scip.Language.Handlebars)
//     Handlebars = 90,
//     // @@protoc_insertion_point(enum_value:scip.Language.Haskell)
//     Haskell = 44,
//     // @@protoc_insertion_point(enum_value:scip.Language.Idris)
//     Idris = 46,
//     // @@protoc_insertion_point(enum_value:scip.Language.Ini)
//     Ini = 72,
//     // @@protoc_insertion_point(enum_value:scip.Language.J)
//     J = 51,
//     // @@protoc_insertion_point(enum_value:scip.Language.JSON)
//     JSON = 75,
//     // @@protoc_insertion_point(enum_value:scip.Language.Java)
//     Java = 6,
//     // @@protoc_insertion_point(enum_value:scip.Language.JavaScript)
//     JavaScript = 22,
//     // @@protoc_insertion_point(enum_value:scip.Language.JavaScriptReact)
//     JavaScriptReact = 93,
//     // @@protoc_insertion_point(enum_value:scip.Language.Jsonnet)
//     Jsonnet = 76,
//     // @@protoc_insertion_point(enum_value:scip.Language.Julia)
//     Julia = 55,
//     // @@protoc_insertion_point(enum_value:scip.Language.Justfile)
//     Justfile = 109,
//     // @@protoc_insertion_point(enum_value:scip.Language.Kotlin)
//     Kotlin = 4,
//     // @@protoc_insertion_point(enum_value:scip.Language.LaTeX)
//     LaTeX = 83,
//     // @@protoc_insertion_point(enum_value:scip.Language.Lean)
//     Lean = 48,
//     // @@protoc_insertion_point(enum_value:scip.Language.Less)
//     Less = 27,
//     // @@protoc_insertion_point(enum_value:scip.Language.Lua)
//     Lua = 12,
//     // @@protoc_insertion_point(enum_value:scip.Language.Luau)
//     Luau = 108,
//     // @@protoc_insertion_point(enum_value:scip.Language.Makefile)
//     Makefile = 79,
//     // @@protoc_insertion_point(enum_value:scip.Language.Markdown)
//     Markdown = 84,
//     // @@protoc_insertion_point(enum_value:scip.Language.Matlab)
//     Matlab = 52,
//     // @@protoc_insertion_point(enum_value:scip.Language.Nickel)
//     Nickel = 110,
//     // @@protoc_insertion_point(enum_value:scip.Language.Nix)
//     Nix = 77,
//     // @@protoc_insertion_point(enum_value:scip.Language.OCaml)
//     OCaml = 41,
//     // @@protoc_insertion_point(enum_value:scip.Language.Objective_C)
//     Objective_C = 36,
//     // @@protoc_insertion_point(enum_value:scip.Language.Objective_CPP)
//     Objective_CPP = 37,
//     // @@protoc_insertion_point(enum_value:scip.Language.Pascal)
//     Pascal = 99,
//     // @@protoc_insertion_point(enum_value:scip.Language.PHP)
//     PHP = 19,
//     // @@protoc_insertion_point(enum_value:scip.Language.PLSQL)
//     PLSQL = 70,
//     // @@protoc_insertion_point(enum_value:scip.Language.Perl)
//     Perl = 13,
//     // @@protoc_insertion_point(enum_value:scip.Language.PowerShell)
//     PowerShell = 67,
//     // @@protoc_insertion_point(enum_value:scip.Language.Prolog)
//     Prolog = 71,
//     // @@protoc_insertion_point(enum_value:scip.Language.Protobuf)
//     Protobuf = 100,
//     // @@protoc_insertion_point(enum_value:scip.Language.Python)
//     Python = 15,
//     // @@protoc_insertion_point(enum_value:scip.Language.R)
//     R = 54,
//     // @@protoc_insertion_point(enum_value:scip.Language.Racket)
//     Racket = 11,
//     // @@protoc_insertion_point(enum_value:scip.Language.Raku)
//     Raku = 14,
//     // @@protoc_insertion_point(enum_value:scip.Language.Razor)
//     Razor = 62,
//     // @@protoc_insertion_point(enum_value:scip.Language.Repro)
//     Repro = 102,
//     // @@protoc_insertion_point(enum_value:scip.Language.ReST)
//     ReST = 85,
//     // @@protoc_insertion_point(enum_value:scip.Language.Ruby)
//     Ruby = 16,
//     // @@protoc_insertion_point(enum_value:scip.Language.Rust)
//     Rust = 40,
//     // @@protoc_insertion_point(enum_value:scip.Language.SAS)
//     SAS = 61,
//     // @@protoc_insertion_point(enum_value:scip.Language.SCSS)
//     SCSS = 29,
//     // @@protoc_insertion_point(enum_value:scip.Language.SML)
//     SML = 43,
//     // @@protoc_insertion_point(enum_value:scip.Language.SQL)
//     SQL = 69,
//     // @@protoc_insertion_point(enum_value:scip.Language.Sass)
//     Sass = 28,
//     // @@protoc_insertion_point(enum_value:scip.Language.Scala)
//     Scala = 5,
//     // @@protoc_insertion_point(enum_value:scip.Language.Scheme)
//     Scheme = 10,
//     // @@protoc_insertion_point(enum_value:scip.Language.ShellScript)
//     ShellScript = 64,
//     // @@protoc_insertion_point(enum_value:scip.Language.Skylark)
//     Skylark = 78,
//     // @@protoc_insertion_point(enum_value:scip.Language.Slang)
//     Slang = 107,
//     // @@protoc_insertion_point(enum_value:scip.Language.Solidity)
//     Solidity = 95,
//     // @@protoc_insertion_point(enum_value:scip.Language.Svelte)
//     Svelte = 106,
//     // @@protoc_insertion_point(enum_value:scip.Language.Swift)
//     Swift = 2,
//     // @@protoc_insertion_point(enum_value:scip.Language.Tcl)
//     Tcl = 101,
//     // @@protoc_insertion_point(enum_value:scip.Language.TOML)
//     TOML = 73,
//     // @@protoc_insertion_point(enum_value:scip.Language.TeX)
//     TeX = 82,
//     // @@protoc_insertion_point(enum_value:scip.Language.Thrift)
//     Thrift = 103,
//     // @@protoc_insertion_point(enum_value:scip.Language.TypeScript)
//     TypeScript = 23,
//     // @@protoc_insertion_point(enum_value:scip.Language.TypeScriptReact)
//     TypeScriptReact = 94,
//     // @@protoc_insertion_point(enum_value:scip.Language.Verilog)
//     Verilog = 104,
//     // @@protoc_insertion_point(enum_value:scip.Language.VHDL)
//     VHDL = 105,
//     // @@protoc_insertion_point(enum_value:scip.Language.VisualBasic)
//     VisualBasic = 63,
//     // @@protoc_insertion_point(enum_value:scip.Language.Vue)
//     Vue = 25,
//     // @@protoc_insertion_point(enum_value:scip.Language.Wolfram)
//     Wolfram = 53,
//     // @@protoc_insertion_point(enum_value:scip.Language.XML)
//     XML = 31,
//     // @@protoc_insertion_point(enum_value:scip.Language.XSL)
//     XSL = 32,
//     // @@protoc_insertion_point(enum_value:scip.Language.YAML)
//     YAML = 74,
//     // @@protoc_insertion_point(enum_value:scip.Language.Zig)
//     Zig = 38,
// }

// impl ::protobuf::Enum for Language {
//     const NAME: &'static str = "Language";

//     fn value(&self) -> i32 {
//         *self as i32
//     }

//     fn from_i32(value: i32) -> ::std::option::Option<Language> {
//         match value {
//             0 => ::std::option::Option::Some(Language::UnspecifiedLanguage),
//             60 => ::std::option::Option::Some(Language::ABAP),
//             96 => ::std::option::Option::Some(Language::Apex),
//             49 => ::std::option::Option::Some(Language::APL),
//             39 => ::std::option::Option::Some(Language::Ada),
//             45 => ::std::option::Option::Some(Language::Agda),
//             86 => ::std::option::Option::Some(Language::AsciiDoc),
//             58 => ::std::option::Option::Some(Language::Assembly),
//             66 => ::std::option::Option::Some(Language::Awk),
//             68 => ::std::option::Option::Some(Language::Bat),
//             81 => ::std::option::Option::Some(Language::BibTeX),
//             34 => ::std::option::Option::Some(Language::C),
//             59 => ::std::option::Option::Some(Language::COBOL),
//             35 => ::std::option::Option::Some(Language::CPP),
//             26 => ::std::option::Option::Some(Language::CSS),
//             1 => ::std::option::Option::Some(Language::CSharp),
//             8 => ::std::option::Option::Some(Language::Clojure),
//             21 => ::std::option::Option::Some(Language::Coffeescript),
//             9 => ::std::option::Option::Some(Language::CommonLisp),
//             47 => ::std::option::Option::Some(Language::Coq),
//             97 => ::std::option::Option::Some(Language::CUDA),
//             3 => ::std::option::Option::Some(Language::Dart),
//             57 => ::std::option::Option::Some(Language::Delphi),
//             88 => ::std::option::Option::Some(Language::Diff),
//             80 => ::std::option::Option::Some(Language::Dockerfile),
//             50 => ::std::option::Option::Some(Language::Dyalog),
//             17 => ::std::option::Option::Some(Language::Elixir),
//             18 => ::std::option::Option::Some(Language::Erlang),
//             42 => ::std::option::Option::Some(Language::FSharp),
//             65 => ::std::option::Option::Some(Language::Fish),
//             24 => ::std::option::Option::Some(Language::Flow),
//             56 => ::std::option::Option::Some(Language::Fortran),
//             91 => ::std::option::Option::Some(Language::Git_Commit),
//             89 => ::std::option::Option::Some(Language::Git_Config),
//             92 => ::std::option::Option::Some(Language::Git_Rebase),
//             33 => ::std::option::Option::Some(Language::Go),
//             98 => ::std::option::Option::Some(Language::GraphQL),
//             7 => ::std::option::Option::Some(Language::Groovy),
//             30 => ::std::option::Option::Some(Language::HTML),
//             20 => ::std::option::Option::Some(Language::Hack),
//             90 => ::std::option::Option::Some(Language::Handlebars),
//             44 => ::std::option::Option::Some(Language::Haskell),
//             46 => ::std::option::Option::Some(Language::Idris),
//             72 => ::std::option::Option::Some(Language::Ini),
//             51 => ::std::option::Option::Some(Language::J),
//             75 => ::std::option::Option::Some(Language::JSON),
//             6 => ::std::option::Option::Some(Language::Java),
//             22 => ::std::option::Option::Some(Language::JavaScript),
//             93 => ::std::option::Option::Some(Language::JavaScriptReact),
//             76 => ::std::option::Option::Some(Language::Jsonnet),
//             55 => ::std::option::Option::Some(Language::Julia),
//             109 => ::std::option::Option::Some(Language::Justfile),
//             4 => ::std::option::Option::Some(Language::Kotlin),
//             83 => ::std::option::Option::Some(Language::LaTeX),
//             48 => ::std::option::Option::Some(Language::Lean),
//             27 => ::std::option::Option::Some(Language::Less),
//             12 => ::std::option::Option::Some(Language::Lua),
//             108 => ::std::option::Option::Some(Language::Luau),
//             79 => ::std::option::Option::Some(Language::Makefile),
//             84 => ::std::option::Option::Some(Language::Markdown),
//             52 => ::std::option::Option::Some(Language::Matlab),
//             110 => ::std::option::Option::Some(Language::Nickel),
//             77 => ::std::option::Option::Some(Language::Nix),
//             41 => ::std::option::Option::Some(Language::OCaml),
//             36 => ::std::option::Option::Some(Language::Objective_C),
//             37 => ::std::option::Option::Some(Language::Objective_CPP),
//             99 => ::std::option::Option::Some(Language::Pascal),
//             19 => ::std::option::Option::Some(Language::PHP),
//             70 => ::std::option::Option::Some(Language::PLSQL),
//             13 => ::std::option::Option::Some(Language::Perl),
//             67 => ::std::option::Option::Some(Language::PowerShell),
//             71 => ::std::option::Option::Some(Language::Prolog),
//             100 => ::std::option::Option::Some(Language::Protobuf),
//             15 => ::std::option::Option::Some(Language::Python),
//             54 => ::std::option::Option::Some(Language::R),
//             11 => ::std::option::Option::Some(Language::Racket),
//             14 => ::std::option::Option::Some(Language::Raku),
//             62 => ::std::option::Option::Some(Language::Razor),
//             102 => ::std::option::Option::Some(Language::Repro),
//             85 => ::std::option::Option::Some(Language::ReST),
//             16 => ::std::option::Option::Some(Language::Ruby),
//             40 => ::std::option::Option::Some(Language::Rust),
//             61 => ::std::option::Option::Some(Language::SAS),
//             29 => ::std::option::Option::Some(Language::SCSS),
//             43 => ::std::option::Option::Some(Language::SML),
//             69 => ::std::option::Option::Some(Language::SQL),
//             28 => ::std::option::Option::Some(Language::Sass),
//             5 => ::std::option::Option::Some(Language::Scala),
//             10 => ::std::option::Option::Some(Language::Scheme),
//             64 => ::std::option::Option::Some(Language::ShellScript),
//             78 => ::std::option::Option::Some(Language::Skylark),
//             107 => ::std::option::Option::Some(Language::Slang),
//             95 => ::std::option::Option::Some(Language::Solidity),
//             106 => ::std::option::Option::Some(Language::Svelte),
//             2 => ::std::option::Option::Some(Language::Swift),
//             101 => ::std::option::Option::Some(Language::Tcl),
//             73 => ::std::option::Option::Some(Language::TOML),
//             82 => ::std::option::Option::Some(Language::TeX),
//             103 => ::std::option::Option::Some(Language::Thrift),
//             23 => ::std::option::Option::Some(Language::TypeScript),
//             94 => ::std::option::Option::Some(Language::TypeScriptReact),
//             104 => ::std::option::Option::Some(Language::Verilog),
//             105 => ::std::option::Option::Some(Language::VHDL),
//             63 => ::std::option::Option::Some(Language::VisualBasic),
//             25 => ::std::option::Option::Some(Language::Vue),
//             53 => ::std::option::Option::Some(Language::Wolfram),
//             31 => ::std::option::Option::Some(Language::XML),
//             32 => ::std::option::Option::Some(Language::XSL),
//             74 => ::std::option::Option::Some(Language::YAML),
//             38 => ::std::option::Option::Some(Language::Zig),
//             _ => ::std::option::Option::None
//         }
//     }

//     fn from_str(str: &str) -> ::std::option::Option<Language> {
//         match str {
//             "UnspecifiedLanguage" => ::std::option::Option::Some(Language::UnspecifiedLanguage),
//             "ABAP" => ::std::option::Option::Some(Language::ABAP),
//             "Apex" => ::std::option::Option::Some(Language::Apex),
//             "APL" => ::std::option::Option::Some(Language::APL),
//             "Ada" => ::std::option::Option::Some(Language::Ada),
//             "Agda" => ::std::option::Option::Some(Language::Agda),
//             "AsciiDoc" => ::std::option::Option::Some(Language::AsciiDoc),
//             "Assembly" => ::std::option::Option::Some(Language::Assembly),
//             "Awk" => ::std::option::Option::Some(Language::Awk),
//             "Bat" => ::std::option::Option::Some(Language::Bat),
//             "BibTeX" => ::std::option::Option::Some(Language::BibTeX),
//             "C" => ::std::option::Option::Some(Language::C),
//             "COBOL" => ::std::option::Option::Some(Language::COBOL),
//             "CPP" => ::std::option::Option::Some(Language::CPP),
//             "CSS" => ::std::option::Option::Some(Language::CSS),
//             "CSharp" => ::std::option::Option::Some(Language::CSharp),
//             "Clojure" => ::std::option::Option::Some(Language::Clojure),
//             "Coffeescript" => ::std::option::Option::Some(Language::Coffeescript),
//             "CommonLisp" => ::std::option::Option::Some(Language::CommonLisp),
//             "Coq" => ::std::option::Option::Some(Language::Coq),
//             "CUDA" => ::std::option::Option::Some(Language::CUDA),
//             "Dart" => ::std::option::Option::Some(Language::Dart),
//             "Delphi" => ::std::option::Option::Some(Language::Delphi),
//             "Diff" => ::std::option::Option::Some(Language::Diff),
//             "Dockerfile" => ::std::option::Option::Some(Language::Dockerfile),
//             "Dyalog" => ::std::option::Option::Some(Language::Dyalog),
//             "Elixir" => ::std::option::Option::Some(Language::Elixir),
//             "Erlang" => ::std::option::Option::Some(Language::Erlang),
//             "FSharp" => ::std::option::Option::Some(Language::FSharp),
//             "Fish" => ::std::option::Option::Some(Language::Fish),
//             "Flow" => ::std::option::Option::Some(Language::Flow),
//             "Fortran" => ::std::option::Option::Some(Language::Fortran),
//             "Git_Commit" => ::std::option::Option::Some(Language::Git_Commit),
//             "Git_Config" => ::std::option::Option::Some(Language::Git_Config),
//             "Git_Rebase" => ::std::option::Option::Some(Language::Git_Rebase),
//             "Go" => ::std::option::Option::Some(Language::Go),
//             "GraphQL" => ::std::option::Option::Some(Language::GraphQL),
//             "Groovy" => ::std::option::Option::Some(Language::Groovy),
//             "HTML" => ::std::option::Option::Some(Language::HTML),
//             "Hack" => ::std::option::Option::Some(Language::Hack),
//             "Handlebars" => ::std::option::Option::Some(Language::Handlebars),
//             "Haskell" => ::std::option::Option::Some(Language::Haskell),
//             "Idris" => ::std::option::Option::Some(Language::Idris),
//             "Ini" => ::std::option::Option::Some(Language::Ini),
//             "J" => ::std::option::Option::Some(Language::J),
//             "JSON" => ::std::option::Option::Some(Language::JSON),
//             "Java" => ::std::option::Option::Some(Language::Java),
//             "JavaScript" => ::std::option::Option::Some(Language::JavaScript),
//             "JavaScriptReact" => ::std::option::Option::Some(Language::JavaScriptReact),
//             "Jsonnet" => ::std::option::Option::Some(Language::Jsonnet),
//             "Julia" => ::std::option::Option::Some(Language::Julia),
//             "Justfile" => ::std::option::Option::Some(Language::Justfile),
//             "Kotlin" => ::std::option::Option::Some(Language::Kotlin),
//             "LaTeX" => ::std::option::Option::Some(Language::LaTeX),
//             "Lean" => ::std::option::Option::Some(Language::Lean),
//             "Less" => ::std::option::Option::Some(Language::Less),
//             "Lua" => ::std::option::Option::Some(Language::Lua),
//             "Luau" => ::std::option::Option::Some(Language::Luau),
//             "Makefile" => ::std::option::Option::Some(Language::Makefile),
//             "Markdown" => ::std::option::Option::Some(Language::Markdown),
//             "Matlab" => ::std::option::Option::Some(Language::Matlab),
//             "Nickel" => ::std::option::Option::Some(Language::Nickel),
//             "Nix" => ::std::option::Option::Some(Language::Nix),
//             "OCaml" => ::std::option::Option::Some(Language::OCaml),
//             "Objective_C" => ::std::option::Option::Some(Language::Objective_C),
//             "Objective_CPP" => ::std::option::Option::Some(Language::Objective_CPP),
//             "Pascal" => ::std::option::Option::Some(Language::Pascal),
//             "PHP" => ::std::option::Option::Some(Language::PHP),
//             "PLSQL" => ::std::option::Option::Some(Language::PLSQL),
//             "Perl" => ::std::option::Option::Some(Language::Perl),
//             "PowerShell" => ::std::option::Option::Some(Language::PowerShell),
//             "Prolog" => ::std::option::Option::Some(Language::Prolog),
//             "Protobuf" => ::std::option::Option::Some(Language::Protobuf),
//             "Python" => ::std::option::Option::Some(Language::Python),
//             "R" => ::std::option::Option::Some(Language::R),
//             "Racket" => ::std::option::Option::Some(Language::Racket),
//             "Raku" => ::std::option::Option::Some(Language::Raku),
//             "Razor" => ::std::option::Option::Some(Language::Razor),
//             "Repro" => ::std::option::Option::Some(Language::Repro),
//             "ReST" => ::std::option::Option::Some(Language::ReST),
//             "Ruby" => ::std::option::Option::Some(Language::Ruby),
//             "Rust" => ::std::option::Option::Some(Language::Rust),
//             "SAS" => ::std::option::Option::Some(Language::SAS),
//             "SCSS" => ::std::option::Option::Some(Language::SCSS),
//             "SML" => ::std::option::Option::Some(Language::SML),
//             "SQL" => ::std::option::Option::Some(Language::SQL),
//             "Sass" => ::std::option::Option::Some(Language::Sass),
//             "Scala" => ::std::option::Option::Some(Language::Scala),
//             "Scheme" => ::std::option::Option::Some(Language::Scheme),
//             "ShellScript" => ::std::option::Option::Some(Language::ShellScript),
//             "Skylark" => ::std::option::Option::Some(Language::Skylark),
//             "Slang" => ::std::option::Option::Some(Language::Slang),
//             "Solidity" => ::std::option::Option::Some(Language::Solidity),
//             "Svelte" => ::std::option::Option::Some(Language::Svelte),
//             "Swift" => ::std::option::Option::Some(Language::Swift),
//             "Tcl" => ::std::option::Option::Some(Language::Tcl),
//             "TOML" => ::std::option::Option::Some(Language::TOML),
//             "TeX" => ::std::option::Option::Some(Language::TeX),
//             "Thrift" => ::std::option::Option::Some(Language::Thrift),
//             "TypeScript" => ::std::option::Option::Some(Language::TypeScript),
//             "TypeScriptReact" => ::std::option::Option::Some(Language::TypeScriptReact),
//             "Verilog" => ::std::option::Option::Some(Language::Verilog),
//             "VHDL" => ::std::option::Option::Some(Language::VHDL),
//             "VisualBasic" => ::std::option::Option::Some(Language::VisualBasic),
//             "Vue" => ::std::option::Option::Some(Language::Vue),
//             "Wolfram" => ::std::option::Option::Some(Language::Wolfram),
//             "XML" => ::std::option::Option::Some(Language::XML),
//             "XSL" => ::std::option::Option::Some(Language::XSL),
//             "YAML" => ::std::option::Option::Some(Language::YAML),
//             "Zig" => ::std::option::Option::Some(Language::Zig),
//             _ => ::std::option::Option::None
//         }
//     }

//     const VALUES: &'static [Language] = &[
//         Language::UnspecifiedLanguage,
//         Language::ABAP,
//         Language::Apex,
//         Language::APL,
//         Language::Ada,
//         Language::Agda,
//         Language::AsciiDoc,
//         Language::Assembly,
//         Language::Awk,
//         Language::Bat,
//         Language::BibTeX,
//         Language::C,
//         Language::COBOL,
//         Language::CPP,
//         Language::CSS,
//         Language::CSharp,
//         Language::Clojure,
//         Language::Coffeescript,
//         Language::CommonLisp,
//         Language::Coq,
//         Language::CUDA,
//         Language::Dart,
//         Language::Delphi,
//         Language::Diff,
//         Language::Dockerfile,
//         Language::Dyalog,
//         Language::Elixir,
//         Language::Erlang,
//         Language::FSharp,
//         Language::Fish,
//         Language::Flow,
//         Language::Fortran,
//         Language::Git_Commit,
//         Language::Git_Config,
//         Language::Git_Rebase,
//         Language::Go,
//         Language::GraphQL,
//         Language::Groovy,
//         Language::HTML,
//         Language::Hack,
//         Language::Handlebars,
//         Language::Haskell,
//         Language::Idris,
//         Language::Ini,
//         Language::J,
//         Language::JSON,
//         Language::Java,
//         Language::JavaScript,
//         Language::JavaScriptReact,
//         Language::Jsonnet,
//         Language::Julia,
//         Language::Justfile,
//         Language::Kotlin,
//         Language::LaTeX,
//         Language::Lean,
//         Language::Less,
//         Language::Lua,
//         Language::Luau,
//         Language::Makefile,
//         Language::Markdown,
//         Language::Matlab,
//         Language::Nickel,
//         Language::Nix,
//         Language::OCaml,
//         Language::Objective_C,
//         Language::Objective_CPP,
//         Language::Pascal,
//         Language::PHP,
//         Language::PLSQL,
//         Language::Perl,
//         Language::PowerShell,
//         Language::Prolog,
//         Language::Protobuf,
//         Language::Python,
//         Language::R,
//         Language::Racket,
//         Language::Raku,
//         Language::Razor,
//         Language::Repro,
//         Language::ReST,
//         Language::Ruby,
//         Language::Rust,
//         Language::SAS,
//         Language::SCSS,
//         Language::SML,
//         Language::SQL,
//         Language::Sass,
//         Language::Scala,
//         Language::Scheme,
//         Language::ShellScript,
//         Language::Skylark,
//         Language::Slang,
//         Language::Solidity,
//         Language::Svelte,
//         Language::Swift,
//         Language::Tcl,
//         Language::TOML,
//         Language::TeX,
//         Language::Thrift,
//         Language::TypeScript,
//         Language::TypeScriptReact,
//         Language::Verilog,
//         Language::VHDL,
//         Language::VisualBasic,
//         Language::Vue,
//         Language::Wolfram,
//         Language::XML,
//         Language::XSL,
//         Language::YAML,
//         Language::Zig,
//     ];
// }

// impl ::protobuf::EnumFull for Language {
//     fn enum_descriptor() -> ::protobuf::reflect::EnumDescriptor {
//         static descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::rt::Lazy::new();
//         descriptor.get(|| file_descriptor().enum_by_package_relative_name("Language").unwrap()).clone()
//     }

//     fn descriptor(&self) -> ::protobuf::reflect::EnumValueDescriptor {
//         let index = match self {
//             Language::UnspecifiedLanguage => 0,
//             Language::ABAP => 1,
//             Language::Apex => 2,
//             Language::APL => 3,
//             Language::Ada => 4,
//             Language::Agda => 5,
//             Language::AsciiDoc => 6,
//             Language::Assembly => 7,
//             Language::Awk => 8,
//             Language::Bat => 9,
//             Language::BibTeX => 10,
//             Language::C => 11,
//             Language::COBOL => 12,
//             Language::CPP => 13,
//             Language::CSS => 14,
//             Language::CSharp => 15,
//             Language::Clojure => 16,
//             Language::Coffeescript => 17,
//             Language::CommonLisp => 18,
//             Language::Coq => 19,
//             Language::CUDA => 20,
//             Language::Dart => 21,
//             Language::Delphi => 22,
//             Language::Diff => 23,
//             Language::Dockerfile => 24,
//             Language::Dyalog => 25,
//             Language::Elixir => 26,
//             Language::Erlang => 27,
//             Language::FSharp => 28,
//             Language::Fish => 29,
//             Language::Flow => 30,
//             Language::Fortran => 31,
//             Language::Git_Commit => 32,
//             Language::Git_Config => 33,
//             Language::Git_Rebase => 34,
//             Language::Go => 35,
//             Language::GraphQL => 36,
//             Language::Groovy => 37,
//             Language::HTML => 38,
//             Language::Hack => 39,
//             Language::Handlebars => 40,
//             Language::Haskell => 41,
//             Language::Idris => 42,
//             Language::Ini => 43,
//             Language::J => 44,
//             Language::JSON => 45,
//             Language::Java => 46,
//             Language::JavaScript => 47,
//             Language::JavaScriptReact => 48,
//             Language::Jsonnet => 49,
//             Language::Julia => 50,
//             Language::Justfile => 51,
//             Language::Kotlin => 52,
//             Language::LaTeX => 53,
//             Language::Lean => 54,
//             Language::Less => 55,
//             Language::Lua => 56,
//             Language::Luau => 57,
//             Language::Makefile => 58,
//             Language::Markdown => 59,
//             Language::Matlab => 60,
//             Language::Nickel => 61,
//             Language::Nix => 62,
//             Language::OCaml => 63,
//             Language::Objective_C => 64,
//             Language::Objective_CPP => 65,
//             Language::Pascal => 66,
//             Language::PHP => 67,
//             Language::PLSQL => 68,
//             Language::Perl => 69,
//             Language::PowerShell => 70,
//             Language::Prolog => 71,
//             Language::Protobuf => 72,
//             Language::Python => 73,
//             Language::R => 74,
//             Language::Racket => 75,
//             Language::Raku => 76,
//             Language::Razor => 77,
//             Language::Repro => 78,
//             Language::ReST => 79,
//             Language::Ruby => 80,
//             Language::Rust => 81,
//             Language::SAS => 82,
//             Language::SCSS => 83,
//             Language::SML => 84,
//             Language::SQL => 85,
//             Language::Sass => 86,
//             Language::Scala => 87,
//             Language::Scheme => 88,
//             Language::ShellScript => 89,
//             Language::Skylark => 90,
//             Language::Slang => 91,
//             Language::Solidity => 92,
//             Language::Svelte => 93,
//             Language::Swift => 94,
//             Language::Tcl => 95,
//             Language::TOML => 96,
//             Language::TeX => 97,
//             Language::Thrift => 98,
//             Language::TypeScript => 99,
//             Language::TypeScriptReact => 100,
//             Language::Verilog => 101,
//             Language::VHDL => 102,
//             Language::VisualBasic => 103,
//             Language::Vue => 104,
//             Language::Wolfram => 105,
//             Language::XML => 106,
//             Language::XSL => 107,
//             Language::YAML => 108,
//             Language::Zig => 109,
//         };
//         Self::enum_descriptor().value_by_index(index)
//     }
// }

// impl ::std::default::Default for Language {
//     fn default() -> Self {
//         Language::UnspecifiedLanguage
//     }
// }

// impl Language {
//     fn generated_enum_descriptor_data() -> ::protobuf::reflect::GeneratedEnumDescriptorData {
//         ::protobuf::reflect::GeneratedEnumDescriptorData::new::<Language>("Language")
//     }
// }

// static file_descriptor_proto_data: &'static [u8] = b"\
//     \n\nscip.proto\x12\x04scip\"\xa5\x01\n\x05Index\x12*\n\x08metadata\x18\
//     \x01\x20\x01(\x0b2\x0e.scip.MetadataR\x08metadata\x12,\n\tdocuments\x18\
//     \x02\x20\x03(\x0b2\x0e.scip.DocumentR\tdocuments\x12B\n\x10external_symb\
//     ols\x18\x03\x20\x03(\x0b2\x17.scip.SymbolInformationR\x0fexternalSymbols\
//     \"\xd5\x01\n\x08Metadata\x12/\n\x07version\x18\x01\x20\x01(\x0e2\x15.sci\
//     p.ProtocolVersionR\x07version\x12+\n\ttool_info\x18\x02\x20\x01(\x0b2\
//     \x0e.scip.ToolInfoR\x08toolInfo\x12!\n\x0cproject_root\x18\x03\x20\x01(\
//     \tR\x0bprojectRoot\x12H\n\x16text_document_encoding\x18\x04\x20\x01(\x0e\
//     2\x12.scip.TextEncodingR\x14textDocumentEncoding\"V\n\x08ToolInfo\x12\
//     \x12\n\x04name\x18\x01\x20\x01(\tR\x04name\x12\x18\n\x07version\x18\x02\
//     \x20\x01(\tR\x07version\x12\x1c\n\targuments\x18\x03\x20\x03(\tR\targume\
//     nts\"\x8b\x02\n\x08Document\x12\x1a\n\x08language\x18\x04\x20\x01(\tR\
//     \x08language\x12#\n\rrelative_path\x18\x01\x20\x01(\tR\x0crelativePath\
//     \x122\n\x0boccurrences\x18\x02\x20\x03(\x0b2\x10.scip.OccurrenceR\x0bocc\
//     urrences\x121\n\x07symbols\x18\x03\x20\x03(\x0b2\x17.scip.SymbolInformat\
//     ionR\x07symbols\x12\x12\n\x04text\x18\x05\x20\x01(\tR\x04text\x12C\n\x11\
//     position_encoding\x18\x06\x20\x01(\x0e2\x16.scip.PositionEncodingR\x10po\
//     sitionEncoding\"}\n\x06Symbol\x12\x16\n\x06scheme\x18\x01\x20\x01(\tR\
//     \x06scheme\x12'\n\x07package\x18\x02\x20\x01(\x0b2\r.scip.PackageR\x07pa\
//     ckage\x122\n\x0bdescriptors\x18\x03\x20\x03(\x0b2\x10.scip.DescriptorR\
//     \x0bdescriptors\"Q\n\x07Package\x12\x18\n\x07manager\x18\x01\x20\x01(\tR\
//     \x07manager\x12\x12\n\x04name\x18\x02\x20\x01(\tR\x04name\x12\x18\n\x07v\
//     ersion\x18\x03\x20\x01(\tR\x07version\"\x9f\x02\n\nDescriptor\x12\x12\n\
//     \x04name\x18\x01\x20\x01(\tR\x04name\x12$\n\rdisambiguator\x18\x02\x20\
//     \x01(\tR\rdisambiguator\x12/\n\x06suffix\x18\x03\x20\x01(\x0e2\x17.scip.\
//     Descriptor.SuffixR\x06suffix\"\xa5\x01\n\x06Suffix\x12\x15\n\x11Unspecif\
//     iedSuffix\x10\0\x12\r\n\tNamespace\x10\x01\x12\x0f\n\x07Package\x10\x01\
//     \x1a\x02\x08\x01\x12\x08\n\x04Type\x10\x02\x12\x08\n\x04Term\x10\x03\x12\
//     \n\n\x06Method\x10\x04\x12\x11\n\rTypeParameter\x10\x05\x12\r\n\tParamet\
//     er\x10\x06\x12\x08\n\x04Meta\x10\x07\x12\t\n\x05Local\x10\x08\x12\t\n\
//     \x05Macro\x10\t\x1a\x02\x10\x01\"\xc5\x0c\n\x11SymbolInformation\x12\x16\
//     \n\x06symbol\x18\x01\x20\x01(\tR\x06symbol\x12$\n\rdocumentation\x18\x03\
//     \x20\x03(\tR\rdocumentation\x128\n\rrelationships\x18\x04\x20\x03(\x0b2\
//     \x12.scip.RelationshipR\rrelationships\x120\n\x04kind\x18\x05\x20\x01(\
//     \x0e2\x1c.scip.SymbolInformation.KindR\x04kind\x12!\n\x0cdisplay_name\
//     \x18\x06\x20\x01(\tR\x0bdisplayName\x12G\n\x17signature_documentation\
//     \x18\x07\x20\x01(\x0b2\x0e.scip.DocumentR\x16signatureDocumentation\x12)\
//     \n\x10enclosing_symbol\x18\x08\x20\x01(\tR\x0fenclosingSymbol\"\xee\t\n\
//     \x04Kind\x12\x13\n\x0fUnspecifiedKind\x10\0\x12\x12\n\x0eAbstractMethod\
//     \x10B\x12\x0c\n\x08Accessor\x10H\x12\t\n\x05Array\x10\x01\x12\r\n\tAsser\
//     tion\x10\x02\x12\x12\n\x0eAssociatedType\x10\x03\x12\r\n\tAttribute\x10\
//     \x04\x12\t\n\x05Axiom\x10\x05\x12\x0b\n\x07Boolean\x10\x06\x12\t\n\x05Cl\
//     ass\x10\x07\x12\x0c\n\x08Constant\x10\x08\x12\x0f\n\x0bConstructor\x10\t\
//     \x12\x0c\n\x08Contract\x10>\x12\x0e\n\nDataFamily\x10\n\x12\x0c\n\x08Del\
//     egate\x10I\x12\x08\n\x04Enum\x10\x0b\x12\x0e\n\nEnumMember\x10\x0c\x12\t\
//     \n\x05Error\x10?\x12\t\n\x05Event\x10\r\x12\r\n\tExtension\x10T\x12\x08\
//     \n\x04Fact\x10\x0e\x12\t\n\x05Field\x10\x0f\x12\x08\n\x04File\x10\x10\
//     \x12\x0c\n\x08Function\x10\x11\x12\n\n\x06Getter\x10\x12\x12\x0b\n\x07Gr\
//     ammar\x10\x13\x12\x0c\n\x08Instance\x10\x14\x12\r\n\tInterface\x10\x15\
//     \x12\x07\n\x03Key\x10\x16\x12\x08\n\x04Lang\x10\x17\x12\t\n\x05Lemma\x10\
//     \x18\x12\x0b\n\x07Library\x10@\x12\t\n\x05Macro\x10\x19\x12\n\n\x06Metho\
//     d\x10\x1a\x12\x0f\n\x0bMethodAlias\x10J\x12\x12\n\x0eMethodReceiver\x10\
//     \x1b\x12\x17\n\x13MethodSpecification\x10C\x12\x0b\n\x07Message\x10\x1c\
//     \x12\t\n\x05Mixin\x10U\x12\x0c\n\x08Modifier\x10A\x12\n\n\x06Module\x10\
//     \x1d\x12\r\n\tNamespace\x10\x1e\x12\x08\n\x04Null\x10\x1f\x12\n\n\x06Num\
//     ber\x10\x20\x12\n\n\x06Object\x10!\x12\x0c\n\x08Operator\x10\"\x12\x0b\n\
//     \x07Package\x10#\x12\x11\n\rPackageObject\x10$\x12\r\n\tParameter\x10%\
//     \x12\x12\n\x0eParameterLabel\x10&\x12\x0b\n\x07Pattern\x10'\x12\r\n\tPre\
//     dicate\x10(\x12\x0c\n\x08Property\x10)\x12\x0c\n\x08Protocol\x10*\x12\
//     \x12\n\x0eProtocolMethod\x10D\x12\x15\n\x11PureVirtualMethod\x10E\x12\
//     \x0f\n\x0bQuasiquoter\x10+\x12\x11\n\rSelfParameter\x10,\x12\n\n\x06Sett\
//     er\x10-\x12\r\n\tSignature\x10.\x12\x12\n\x0eSingletonClass\x10K\x12\x13\
//     \n\x0fSingletonMethod\x10L\x12\x14\n\x10StaticDataMember\x10M\x12\x0f\n\
//     \x0bStaticEvent\x10N\x12\x0f\n\x0bStaticField\x10O\x12\x10\n\x0cStaticMe\
//     thod\x10P\x12\x12\n\x0eStaticProperty\x10Q\x12\x12\n\x0eStaticVariable\
//     \x10R\x12\n\n\x06String\x100\x12\n\n\x06Struct\x101\x12\r\n\tSubscript\
//     \x10/\x12\n\n\x06Tactic\x102\x12\x0b\n\x07Theorem\x103\x12\x11\n\rThisPa\
//     rameter\x104\x12\t\n\x05Trait\x105\x12\x0f\n\x0bTraitMethod\x10F\x12\x08\
//     \n\x04Type\x106\x12\r\n\tTypeAlias\x107\x12\r\n\tTypeClass\x108\x12\x13\
//     \n\x0fTypeClassMethod\x10G\x12\x0e\n\nTypeFamily\x109\x12\x11\n\rTypePar\
//     ameter\x10:\x12\t\n\x05Union\x10;\x12\t\n\x05Value\x10<\x12\x0c\n\x08Var\
//     iable\x10=\"\xc9\x01\n\x0cRelationship\x12\x16\n\x06symbol\x18\x01\x20\
//     \x01(\tR\x06symbol\x12!\n\x0cis_reference\x18\x02\x20\x01(\x08R\x0bisRef\
//     erence\x12+\n\x11is_implementation\x18\x03\x20\x01(\x08R\x10isImplementa\
//     tion\x12,\n\x12is_type_definition\x18\x04\x20\x01(\x08R\x10isTypeDefinit\
//     ion\x12#\n\ris_definition\x18\x05\x20\x01(\x08R\x0cisDefinition\"\xa4\
//     \x02\n\nOccurrence\x12\x14\n\x05range\x18\x01\x20\x03(\x05R\x05range\x12\
//     \x16\n\x06symbol\x18\x02\x20\x01(\tR\x06symbol\x12!\n\x0csymbol_roles\
//     \x18\x03\x20\x01(\x05R\x0bsymbolRoles\x125\n\x16override_documentation\
//     \x18\x04\x20\x03(\tR\x15overrideDocumentation\x121\n\x0bsyntax_kind\x18\
//     \x05\x20\x01(\x0e2\x10.scip.SyntaxKindR\nsyntaxKind\x122\n\x0bdiagnostic\
//     s\x18\x06\x20\x03(\x0b2\x10.scip.DiagnosticR\x0bdiagnostics\x12'\n\x0fen\
//     closing_range\x18\x07\x20\x03(\x05R\x0eenclosingRange\"\xa7\x01\n\nDiagn\
//     ostic\x12*\n\x08severity\x18\x01\x20\x01(\x0e2\x0e.scip.SeverityR\x08sev\
//     erity\x12\x12\n\x04code\x18\x02\x20\x01(\tR\x04code\x12\x18\n\x07message\
//     \x18\x03\x20\x01(\tR\x07message\x12\x16\n\x06source\x18\x04\x20\x01(\tR\
//     \x06source\x12'\n\x04tags\x18\x05\x20\x03(\x0e2\x13.scip.DiagnosticTagR\
//     \x04tags*1\n\x0fProtocolVersion\x12\x1e\n\x1aUnspecifiedProtocolVersion\
//     \x10\0*@\n\x0cTextEncoding\x12\x1b\n\x17UnspecifiedTextEncoding\x10\0\
//     \x12\x08\n\x04UTF8\x10\x01\x12\t\n\x05UTF16\x10\x02*\xa4\x01\n\x10Positi\
//     onEncoding\x12\x1f\n\x1bUnspecifiedPositionEncoding\x10\0\x12#\n\x1fUTF8\
//     CodeUnitOffsetFromLineStart\x10\x01\x12$\n\x20UTF16CodeUnitOffsetFromLin\
//     eStart\x10\x02\x12$\n\x20UTF32CodeUnitOffsetFromLineStart\x10\x03*\x94\
//     \x01\n\nSymbolRole\x12\x19\n\x15UnspecifiedSymbolRole\x10\0\x12\x0e\n\nD\
//     efinition\x10\x01\x12\n\n\x06Import\x10\x02\x12\x0f\n\x0bWriteAccess\x10\
//     \x04\x12\x0e\n\nReadAccess\x10\x08\x12\r\n\tGenerated\x10\x10\x12\x08\n\
//     \x04Test\x10\x20\x12\x15\n\x11ForwardDefinition\x10@*\xea\x06\n\nSyntaxK\
//     ind\x12\x19\n\x15UnspecifiedSyntaxKind\x10\0\x12\x0b\n\x07Comment\x10\
//     \x01\x12\x18\n\x14PunctuationDelimiter\x10\x02\x12\x16\n\x12PunctuationB\
//     racket\x10\x03\x12\x0b\n\x07Keyword\x10\x04\x12\x19\n\x11IdentifierKeywo\
//     rd\x10\x04\x1a\x02\x08\x01\x12\x16\n\x12IdentifierOperator\x10\x05\x12\
//     \x0e\n\nIdentifier\x10\x06\x12\x15\n\x11IdentifierBuiltin\x10\x07\x12\
//     \x12\n\x0eIdentifierNull\x10\x08\x12\x16\n\x12IdentifierConstant\x10\t\
//     \x12\x1b\n\x17IdentifierMutableGlobal\x10\n\x12\x17\n\x13IdentifierParam\
//     eter\x10\x0b\x12\x13\n\x0fIdentifierLocal\x10\x0c\x12\x16\n\x12Identifie\
//     rShadowed\x10\r\x12\x17\n\x13IdentifierNamespace\x10\x0e\x12\x18\n\x10Id\
//     entifierModule\x10\x0e\x1a\x02\x08\x01\x12\x16\n\x12IdentifierFunction\
//     \x10\x0f\x12\x20\n\x1cIdentifierFunctionDefinition\x10\x10\x12\x13\n\x0f\
//     IdentifierMacro\x10\x11\x12\x1d\n\x19IdentifierMacroDefinition\x10\x12\
//     \x12\x12\n\x0eIdentifierType\x10\x13\x12\x19\n\x15IdentifierBuiltinType\
//     \x10\x14\x12\x17\n\x13IdentifierAttribute\x10\x15\x12\x0f\n\x0bRegexEsca\
//     pe\x10\x16\x12\x11\n\rRegexRepeated\x10\x17\x12\x11\n\rRegexWildcard\x10\
//     \x18\x12\x12\n\x0eRegexDelimiter\x10\x19\x12\r\n\tRegexJoin\x10\x1a\x12\
//     \x11\n\rStringLiteral\x10\x1b\x12\x17\n\x13StringLiteralEscape\x10\x1c\
//     \x12\x18\n\x14StringLiteralSpecial\x10\x1d\x12\x14\n\x10StringLiteralKey\
//     \x10\x1e\x12\x14\n\x10CharacterLiteral\x10\x1f\x12\x12\n\x0eNumericLiter\
//     al\x10\x20\x12\x12\n\x0eBooleanLiteral\x10!\x12\x07\n\x03Tag\x10\"\x12\
//     \x10\n\x0cTagAttribute\x10#\x12\x10\n\x0cTagDelimiter\x10$\x1a\x02\x10\
//     \x01*V\n\x08Severity\x12\x17\n\x13UnspecifiedSeverity\x10\0\x12\t\n\x05E\
//     rror\x10\x01\x12\x0b\n\x07Warning\x10\x02\x12\x0f\n\x0bInformation\x10\
//     \x03\x12\x08\n\x04Hint\x10\x04*N\n\rDiagnosticTag\x12\x1c\n\x18Unspecifi\
//     edDiagnosticTag\x10\0\x12\x0f\n\x0bUnnecessary\x10\x01\x12\x0e\n\nDeprec\
//     ated\x10\x02*\x9b\n\n\x08Language\x12\x17\n\x13UnspecifiedLanguage\x10\0\
//     \x12\x08\n\x04ABAP\x10<\x12\x08\n\x04Apex\x10`\x12\x07\n\x03APL\x101\x12\
//     \x07\n\x03Ada\x10'\x12\x08\n\x04Agda\x10-\x12\x0c\n\x08AsciiDoc\x10V\x12\
//     \x0c\n\x08Assembly\x10:\x12\x07\n\x03Awk\x10B\x12\x07\n\x03Bat\x10D\x12\
//     \n\n\x06BibTeX\x10Q\x12\x05\n\x01C\x10\"\x12\t\n\x05COBOL\x10;\x12\x07\n\
//     \x03CPP\x10#\x12\x07\n\x03CSS\x10\x1a\x12\n\n\x06CSharp\x10\x01\x12\x0b\
//     \n\x07Clojure\x10\x08\x12\x10\n\x0cCoffeescript\x10\x15\x12\x0e\n\nCommo\
//     nLisp\x10\t\x12\x07\n\x03Coq\x10/\x12\x08\n\x04CUDA\x10a\x12\x08\n\x04Da\
//     rt\x10\x03\x12\n\n\x06Delphi\x109\x12\x08\n\x04Diff\x10X\x12\x0e\n\nDock\
//     erfile\x10P\x12\n\n\x06Dyalog\x102\x12\n\n\x06Elixir\x10\x11\x12\n\n\x06\
//     Erlang\x10\x12\x12\n\n\x06FSharp\x10*\x12\x08\n\x04Fish\x10A\x12\x08\n\
//     \x04Flow\x10\x18\x12\x0b\n\x07Fortran\x108\x12\x0e\n\nGit_Commit\x10[\
//     \x12\x0e\n\nGit_Config\x10Y\x12\x0e\n\nGit_Rebase\x10\\\x12\x06\n\x02Go\
//     \x10!\x12\x0b\n\x07GraphQL\x10b\x12\n\n\x06Groovy\x10\x07\x12\x08\n\x04H\
//     TML\x10\x1e\x12\x08\n\x04Hack\x10\x14\x12\x0e\n\nHandlebars\x10Z\x12\x0b\
//     \n\x07Haskell\x10,\x12\t\n\x05Idris\x10.\x12\x07\n\x03Ini\x10H\x12\x05\n\
//     \x01J\x103\x12\x08\n\x04JSON\x10K\x12\x08\n\x04Java\x10\x06\x12\x0e\n\nJ\
//     avaScript\x10\x16\x12\x13\n\x0fJavaScriptReact\x10]\x12\x0b\n\x07Jsonnet\
//     \x10L\x12\t\n\x05Julia\x107\x12\x0c\n\x08Justfile\x10m\x12\n\n\x06Kotlin\
//     \x10\x04\x12\t\n\x05LaTeX\x10S\x12\x08\n\x04Lean\x100\x12\x08\n\x04Less\
//     \x10\x1b\x12\x07\n\x03Lua\x10\x0c\x12\x08\n\x04Luau\x10l\x12\x0c\n\x08Ma\
//     kefile\x10O\x12\x0c\n\x08Markdown\x10T\x12\n\n\x06Matlab\x104\x12\n\n\
//     \x06Nickel\x10n\x12\x07\n\x03Nix\x10M\x12\t\n\x05OCaml\x10)\x12\x0f\n\
//     \x0bObjective_C\x10$\x12\x11\n\rObjective_CPP\x10%\x12\n\n\x06Pascal\x10\
//     c\x12\x07\n\x03PHP\x10\x13\x12\t\n\x05PLSQL\x10F\x12\x08\n\x04Perl\x10\r\
//     \x12\x0e\n\nPowerShell\x10C\x12\n\n\x06Prolog\x10G\x12\x0c\n\x08Protobuf\
//     \x10d\x12\n\n\x06Python\x10\x0f\x12\x05\n\x01R\x106\x12\n\n\x06Racket\
//     \x10\x0b\x12\x08\n\x04Raku\x10\x0e\x12\t\n\x05Razor\x10>\x12\t\n\x05Repr\
//     o\x10f\x12\x08\n\x04ReST\x10U\x12\x08\n\x04Ruby\x10\x10\x12\x08\n\x04Rus\
//     t\x10(\x12\x07\n\x03SAS\x10=\x12\x08\n\x04SCSS\x10\x1d\x12\x07\n\x03SML\
//     \x10+\x12\x07\n\x03SQL\x10E\x12\x08\n\x04Sass\x10\x1c\x12\t\n\x05Scala\
//     \x10\x05\x12\n\n\x06Scheme\x10\n\x12\x0f\n\x0bShellScript\x10@\x12\x0b\n\
//     \x07Skylark\x10N\x12\t\n\x05Slang\x10k\x12\x0c\n\x08Solidity\x10_\x12\n\
//     \n\x06Svelte\x10j\x12\t\n\x05Swift\x10\x02\x12\x07\n\x03Tcl\x10e\x12\x08\
//     \n\x04TOML\x10I\x12\x07\n\x03TeX\x10R\x12\n\n\x06Thrift\x10g\x12\x0e\n\n\
//     TypeScript\x10\x17\x12\x13\n\x0fTypeScriptReact\x10^\x12\x0b\n\x07Verilo\
//     g\x10h\x12\x08\n\x04VHDL\x10i\x12\x0f\n\x0bVisualBasic\x10?\x12\x07\n\
//     \x03Vue\x10\x19\x12\x0b\n\x07Wolfram\x105\x12\x07\n\x03XML\x10\x1f\x12\
//     \x07\n\x03XSL\x10\x20\x12\x08\n\x04YAML\x10J\x12\x07\n\x03Zig\x10&B/Z-gi\
//     thub.com/sourcegraph/scip/bindings/go/scip/J\xd7\xb7\x02\n\x07\x12\x05\n\
//     \0\xf0\x06\x01\n\x82\x04\n\x01\x0c\x12\x03\n\0\x122\xf7\x03\x20An\x20ind\
//     ex\x20contains\x20one\x20or\x20more\x20pieces\x20of\x20information\x20ab\
//     out\x20a\x20given\x20piece\x20of\n\x20source\x20code\x20or\x20software\
//     \x20artifact.\x20Complementary\x20information\x20can\x20be\x20merged\n\
//     \x20together\x20from\x20multiple\x20sources\x20to\x20provide\x20a\x20uni\
//     fied\x20code\x20intelligence\n\x20experience.\n\n\x20Programs\x20produci\
//     ng\x20a\x20file\x20of\x20this\x20format\x20is\x20an\x20\"indexer\"\x20an\
//     d\x20may\x20operate\n\x20somewhere\x20on\x20the\x20spectrum\x20between\
//     \x20precision,\x20such\x20as\x20indexes\x20produced\x20by\n\x20compiler-\
//     backed\x20indexers,\x20and\x20heurstics,\x20such\x20as\x20indexes\x20pro\
//     duced\x20by\x20local\n\x20syntax-directed\x20analysis\x20for\x20scope\
//     \x20rules.\n\n\x08\n\x01\x02\x12\x03\x0c\0\r\n\x08\n\x01\x08\x12\x03\x0e\
//     \0D\n\t\n\x02\x08\x0b\x12\x03\x0e\0D\n\xd0\x03\n\x02\x04\0\x12\x04\x16\0\
//     #\x01\x1a\xc3\x03\x20Index\x20represents\x20a\x20complete\x20SCIP\x20ind\
//     ex\x20for\x20a\x20workspace\x20this\x20is\x20rooted\x20at\x20a\n\x20sing\
//     le\x20directory.\x20An\x20Index\x20message\x20payload\x20can\x20have\x20\
//     a\x20large\x20memory\x20footprint\n\x20and\x20it's\x20therefore\x20recom\
//     mended\x20to\x20emit\x20and\x20consume\x20an\x20Index\x20payload\x20one\
//     \x20field\n\x20value\x20at\x20a\x20time.\x20To\x20permit\x20streaming\
//     \x20consumption\x20of\x20an\x20Index\x20payload,\x20the\n\x20`metadata`\
//     \x20field\x20must\x20appear\x20at\x20the\x20start\x20of\x20the\x20stream\
//     \x20and\x20must\x20only\x20appear\n\x20once\x20in\x20the\x20stream.\x20O\
//     ther\x20field\x20values\x20may\x20appear\x20in\x20any\x20order.\n\n\n\n\
//     \x03\x04\0\x01\x12\x03\x16\x08\r\n)\n\x04\x04\0\x02\0\x12\x03\x18\x02\
//     \x18\x1a\x1c\x20Metadata\x20about\x20this\x20index.\n\n\x0c\n\x05\x04\0\
//     \x02\0\x06\x12\x03\x18\x02\n\n\x0c\n\x05\x04\0\x02\0\x01\x12\x03\x18\x0b\
//     \x13\n\x0c\n\x05\x04\0\x02\0\x03\x12\x03\x18\x16\x17\n3\n\x04\x04\0\x02\
//     \x01\x12\x03\x1a\x02\"\x1a&\x20Documents\x20that\x20belong\x20to\x20this\
//     \x20index.\n\n\x0c\n\x05\x04\0\x02\x01\x04\x12\x03\x1a\x02\n\n\x0c\n\x05\
//     \x04\0\x02\x01\x06\x12\x03\x1a\x0b\x13\n\x0c\n\x05\x04\0\x02\x01\x01\x12\
//     \x03\x1a\x14\x1d\n\x0c\n\x05\x04\0\x02\x01\x03\x12\x03\x1a\x20!\n\xf6\
//     \x03\n\x04\x04\0\x02\x02\x12\x03\x20\x022\x1a\xe9\x02\x20(optional)\x20S\
//     ymbols\x20that\x20are\x20referenced\x20from\x20this\x20index\x20but\x20a\
//     re\x20defined\x20in\n\x20an\x20external\x20package\x20(a\x20separate\x20\
//     `Index`\x20message).\x20Leave\x20this\x20field\x20empty\n\x20if\x20you\
//     \x20assume\x20the\x20external\x20package\x20will\x20get\x20indexed\x20se\
//     parately.\x20If\x20the\n\x20external\x20package\x20won't\x20get\x20index\
//     ed\x20for\x20some\x20reason\x20then\x20you\x20can\x20use\x20this\n\x20fi\
//     eld\x20to\x20provide\x20hover\x20documentation\x20for\x20those\x20extern\
//     al\x20symbols.\n\"}\x20IMPORTANT:\x20When\x20adding\x20a\x20new\x20field\
//     \x20to\x20`Index`\x20here,\x20add\x20a\x20matching\n\x20function\x20in\
//     \x20`IndexVisitor`\x20and\x20update\x20`ParseStreaming`.\n\n\x0c\n\x05\
//     \x04\0\x02\x02\x04\x12\x03\x20\x02\n\n\x0c\n\x05\x04\0\x02\x02\x06\x12\
//     \x03\x20\x0b\x1c\n\x0c\n\x05\x04\0\x02\x02\x01\x12\x03\x20\x1d-\n\x0c\n\
//     \x05\x04\0\x02\x02\x03\x12\x03\x2001\n\n\n\x02\x04\x01\x12\x04%\02\x01\n\
//     \n\n\x03\x04\x01\x01\x12\x03%\x08\x10\nN\n\x04\x04\x01\x02\0\x12\x03'\
//     \x02\x1e\x1aA\x20Which\x20version\x20of\x20this\x20protocol\x20was\x20us\
//     ed\x20to\x20generate\x20this\x20index?\n\n\x0c\n\x05\x04\x01\x02\0\x06\
//     \x12\x03'\x02\x11\n\x0c\n\x05\x04\x01\x02\0\x01\x12\x03'\x12\x19\n\x0c\n\
//     \x05\x04\x01\x02\0\x03\x12\x03'\x1c\x1d\nC\n\x04\x04\x01\x02\x01\x12\x03\
//     )\x02\x19\x1a6\x20Information\x20about\x20the\x20tool\x20that\x20produce\
//     d\x20this\x20index.\n\n\x0c\n\x05\x04\x01\x02\x01\x06\x12\x03)\x02\n\n\
//     \x0c\n\x05\x04\x01\x02\x01\x01\x12\x03)\x0b\x14\n\x0c\n\x05\x04\x01\x02\
//     \x01\x03\x12\x03)\x17\x18\n\xa2\x01\n\x04\x04\x01\x02\x02\x12\x03-\x02\
//     \x1a\x1a\x94\x01\x20URI-encoded\x20absolute\x20path\x20to\x20the\x20root\
//     \x20directory\x20of\x20this\x20index.\x20All\n\x20documents\x20in\x20thi\
//     s\x20index\x20must\x20appear\x20in\x20a\x20subdirectory\x20of\x20this\
//     \x20root\n\x20directory.\n\n\x0c\n\x05\x04\x01\x02\x02\x05\x12\x03-\x02\
//     \x08\n\x0c\n\x05\x04\x01\x02\x02\x01\x12\x03-\t\x15\n\x0c\n\x05\x04\x01\
//     \x02\x02\x03\x12\x03-\x18\x19\n\xe0\x01\n\x04\x04\x01\x02\x03\x12\x031\
//     \x02*\x1a\xd2\x01\x20Text\x20encoding\x20of\x20the\x20source\x20files\
//     \x20on\x20disk\x20that\x20are\x20referenced\x20from\n\x20`Document.relat\
//     ive_path`.\x20This\x20value\x20is\x20unrelated\x20to\x20the\x20`Document\
//     .text`\n\x20field,\x20which\x20is\x20a\x20Protobuf\x20string\x20and\x20h\
//     ence\x20must\x20be\x20UTF-8\x20encoded.\n\n\x0c\n\x05\x04\x01\x02\x03\
//     \x06\x12\x031\x02\x0e\n\x0c\n\x05\x04\x01\x02\x03\x01\x12\x031\x0f%\n\
//     \x0c\n\x05\x04\x01\x02\x03\x03\x12\x031()\n\n\n\x02\x05\0\x12\x044\06\
//     \x01\n\n\n\x03\x05\0\x01\x12\x034\x05\x14\n\x0b\n\x04\x05\0\x02\0\x12\
//     \x035\x02!\n\x0c\n\x05\x05\0\x02\0\x01\x12\x035\x02\x1c\n\x0c\n\x05\x05\
//     \0\x02\0\x02\x12\x035\x1f\x20\n\n\n\x02\x05\x01\x12\x048\0<\x01\n\n\n\
//     \x03\x05\x01\x01\x12\x038\x05\x11\n\x0b\n\x04\x05\x01\x02\0\x12\x039\x02\
//     \x1e\n\x0c\n\x05\x05\x01\x02\0\x01\x12\x039\x02\x19\n\x0c\n\x05\x05\x01\
//     \x02\0\x02\x12\x039\x1c\x1d\n\x0b\n\x04\x05\x01\x02\x01\x12\x03:\x02\x0b\
//     \n\x0c\n\x05\x05\x01\x02\x01\x01\x12\x03:\x02\x06\n\x0c\n\x05\x05\x01\
//     \x02\x01\x02\x12\x03:\t\n\n\x0b\n\x04\x05\x01\x02\x02\x12\x03;\x02\x0c\n\
//     \x0c\n\x05\x05\x01\x02\x02\x01\x12\x03;\x02\x07\n\x0c\n\x05\x05\x01\x02\
//     \x02\x02\x12\x03;\n\x0b\n\n\n\x02\x04\x02\x12\x04>\0E\x01\n\n\n\x03\x04\
//     \x02\x01\x12\x03>\x08\x10\n<\n\x04\x04\x02\x02\0\x12\x03@\x02\x12\x1a/\
//     \x20Name\x20of\x20the\x20indexer\x20that\x20produced\x20this\x20index.\n\
//     \n\x0c\n\x05\x04\x02\x02\0\x05\x12\x03@\x02\x08\n\x0c\n\x05\x04\x02\x02\
//     \0\x01\x12\x03@\t\r\n\x0c\n\x05\x04\x02\x02\0\x03\x12\x03@\x10\x11\n?\n\
//     \x04\x04\x02\x02\x01\x12\x03B\x02\x15\x1a2\x20Version\x20of\x20the\x20in\
//     dexer\x20that\x20produced\x20this\x20index.\n\n\x0c\n\x05\x04\x02\x02\
//     \x01\x05\x12\x03B\x02\x08\n\x0c\n\x05\x04\x02\x02\x01\x01\x12\x03B\t\x10\
//     \n\x0c\n\x05\x04\x02\x02\x01\x03\x12\x03B\x13\x14\nL\n\x04\x04\x02\x02\
//     \x02\x12\x03D\x02\x20\x1a?\x20Command-line\x20arguments\x20that\x20were\
//     \x20used\x20to\x20invoke\x20this\x20indexer.\n\n\x0c\n\x05\x04\x02\x02\
//     \x02\x04\x12\x03D\x02\n\n\x0c\n\x05\x04\x02\x02\x02\x05\x12\x03D\x0b\x11\
//     \n\x0c\n\x05\x04\x02\x02\x02\x01\x12\x03D\x12\x1b\n\x0c\n\x05\x04\x02\
//     \x02\x02\x03\x12\x03D\x1e\x1f\nH\n\x02\x04\x03\x12\x04H\0u\x01\x1a<\x20D\
//     ocument\x20defines\x20the\x20metadata\x20about\x20a\x20source\x20file\
//     \x20on\x20disk.\n\n\n\n\x03\x04\x03\x01\x12\x03H\x08\x10\n\xa5\x02\n\x04\
//     \x04\x03\x02\0\x12\x03M\x02\x16\x1a\x97\x02\x20The\x20string\x20ID\x20fo\
//     r\x20the\x20programming\x20language\x20this\x20file\x20is\x20written\x20\
//     in.\n\x20The\x20`Language`\x20enum\x20contains\x20the\x20names\x20of\x20\
//     most\x20common\x20programming\x20languages.\n\x20This\x20field\x20is\x20\
//     typed\x20as\x20a\x20string\x20to\x20permit\x20any\x20programming\x20lang\
//     uage,\x20including\n\x20ones\x20that\x20are\x20not\x20specified\x20by\
//     \x20the\x20`Language`\x20enum.\n\n\x0c\n\x05\x04\x03\x02\0\x05\x12\x03M\
//     \x02\x08\n\x0c\n\x05\x04\x03\x02\0\x01\x12\x03M\t\x11\n\x0c\n\x05\x04\
//     \x03\x02\0\x03\x12\x03M\x14\x15\n\xb5\x03\n\x04\x04\x03\x02\x01\x12\x03W\
//     \x02\x1b\x1a\xa7\x03\x20(Required)\x20Unique\x20path\x20to\x20the\x20tex\
//     t\x20document.\n\n\x201.\x20The\x20path\x20must\x20be\x20relative\x20to\
//     \x20the\x20directory\x20supplied\x20in\x20the\x20associated\n\x20\x20\
//     \x20\x20`Metadata.project_root`.\n\x202.\x20The\x20path\x20must\x20not\
//     \x20begin\x20with\x20a\x20leading\x20'/'.\n\x203.\x20The\x20path\x20must\
//     \x20point\x20to\x20a\x20regular\x20file,\x20not\x20a\x20symbolic\x20link\
//     .\n\x204.\x20The\x20path\x20must\x20use\x20'/'\x20as\x20the\x20separator\
//     ,\x20including\x20on\x20Windows.\n\x205.\x20The\x20path\x20must\x20be\
//     \x20canonical;\x20it\x20cannot\x20include\x20empty\x20components\x20('//\
//     '),\n\x20\x20\x20\x20or\x20'.'\x20or\x20'..'.\n\n\x0c\n\x05\x04\x03\x02\
//     \x01\x05\x12\x03W\x02\x08\n\x0c\n\x05\x04\x03\x02\x01\x01\x12\x03W\t\x16\
//     \n\x0c\n\x05\x04\x03\x02\x01\x03\x12\x03W\x19\x1a\n4\n\x04\x04\x03\x02\
//     \x02\x12\x03Y\x02&\x1a'\x20Occurrences\x20that\x20appear\x20in\x20this\
//     \x20file.\n\n\x0c\n\x05\x04\x03\x02\x02\x04\x12\x03Y\x02\n\n\x0c\n\x05\
//     \x04\x03\x02\x02\x06\x12\x03Y\x0b\x15\n\x0c\n\x05\x04\x03\x02\x02\x01\
//     \x12\x03Y\x16!\n\x0c\n\x05\x04\x03\x02\x02\x03\x12\x03Y$%\n\xea\x01\n\
//     \x04\x04\x03\x02\x03\x12\x03_\x02)\x1a\xdc\x01\x20Symbols\x20that\x20are\
//     \x20\"defined\"\x20within\x20this\x20document.\n\n\x20This\x20should\x20\
//     include\x20symbols\x20which\x20technically\x20do\x20not\x20have\x20any\
//     \x20definition,\n\x20but\x20have\x20a\x20reference\x20and\x20are\x20defi\
//     ned\x20by\x20some\x20other\x20symbol\x20(see\n\x20Relationship.is_defini\
//     tion).\n\n\x0c\n\x05\x04\x03\x02\x03\x04\x12\x03_\x02\n\n\x0c\n\x05\x04\
//     \x03\x02\x03\x06\x12\x03_\x0b\x1c\n\x0c\n\x05\x04\x03\x02\x03\x01\x12\
//     \x03_\x1d$\n\x0c\n\x05\x04\x03\x02\x03\x03\x12\x03_'(\n\xf7\x03\n\x04\
//     \x04\x03\x02\x04\x12\x03h\x02\x12\x1a\xe9\x03\x20(optional)\x20Text\x20c\
//     ontents\x20of\x20the\x20this\x20document.\x20Indexers\x20are\x20not\x20e\
//     xpected\x20to\n\x20include\x20the\x20text\x20by\x20default.\x20It's\x20p\
//     referrable\x20that\x20clients\x20read\x20the\x20text\n\x20contents\x20fr\
//     om\x20the\x20file\x20system\x20by\x20resolving\x20the\x20absolute\x20pat\
//     h\x20from\x20joining\n\x20`Index.metadata.project_root`\x20and\x20`Docum\
//     ent.relative_path`.\x20This\x20field\x20was\n\x20introduced\x20to\x20sup\
//     port\x20`SymbolInformation.signature_documentation`,\x20but\x20it\n\x20c\
//     an\x20be\x20used\x20for\x20other\x20purposes\x20as\x20well,\x20for\x20ex\
//     ample\x20testing\x20or\x20when\x20working\n\x20with\x20virtual/in-memory\
//     \x20documents.\n\n\x0c\n\x05\x04\x03\x02\x04\x05\x12\x03h\x02\x08\n\x0c\
//     \n\x05\x04\x03\x02\x04\x01\x12\x03h\t\r\n\x0c\n\x05\x04\x03\x02\x04\x03\
//     \x12\x03h\x10\x11\n\xe7\x03\n\x04\x04\x03\x02\x05\x12\x03t\x02)\x1a\xd9\
//     \x03\x20Specifies\x20the\x20encoding\x20used\x20for\x20source\x20ranges\
//     \x20in\x20this\x20Document.\n\n\x20Usually,\x20this\x20will\x20match\x20\
//     the\x20type\x20used\x20to\x20index\x20the\x20string\x20type\n\x20in\x20t\
//     he\x20indexer's\x20implementation\x20language\x20in\x20O(1)\x20time.\n\
//     \x20-\x20For\x20an\x20indexer\x20implemented\x20in\x20JVM/.NET\x20langua\
//     ge\x20or\x20JavaScript/TypeScript,\n\x20\x20\x20use\x20UTF16CodeUnitOffs\
//     etFromLineStart.\n\x20-\x20For\x20an\x20indexer\x20implemented\x20in\x20\
//     Python,\n\x20\x20\x20use\x20UTF32CodeUnitOffsetFromLineStart.\n\x20-\x20\
//     For\x20an\x20indexer\x20implemented\x20in\x20Go,\x20Rust\x20or\x20C++,\n\
//     \x20\x20\x20use\x20UTF8ByteOffsetFromLineStart.\n\n\x0c\n\x05\x04\x03\
//     \x02\x05\x06\x12\x03t\x02\x12\n\x0c\n\x05\x04\x03\x02\x05\x01\x12\x03t\
//     \x13$\n\x0c\n\x05\x04\x03\x02\x05\x03\x12\x03t'(\nQ\n\x02\x05\x02\x12\
//     \x05x\0\x90\x01\x01\x1aD\x20Encoding\x20used\x20to\x20interpret\x20the\
//     \x20'character'\x20value\x20in\x20source\x20ranges.\n\n\n\n\x03\x05\x02\
//     \x01\x12\x03x\x05\x15\n\x93\x01\n\x04\x05\x02\x02\0\x12\x03{\x02\"\x1a\
//     \x85\x01\x20Default\x20value.\x20This\x20value\x20should\x20not\x20be\
//     \x20used\x20by\x20new\x20SCIP\x20indexers\n\x20so\x20that\x20a\x20consum\
//     er\x20can\x20process\x20the\x20SCIP\x20index\x20without\x20ambiguity.\n\
//     \n\x0c\n\x05\x05\x02\x02\0\x01\x12\x03{\x02\x1d\n\x0c\n\x05\x05\x02\x02\
//     \0\x02\x12\x03{\x20!\n\xf7\x01\n\x04\x05\x02\x02\x01\x12\x04\x82\x01\x02\
//     &\x1a\xe8\x01\x20The\x20'character'\x20value\x20is\x20interpreted\x20as\
//     \x20an\x20offset\x20in\x20terms\n\x20of\x20UTF-8\x20code\x20units\x20(i.\
//     e.\x20bytes).\n\n\x20Example:\x20For\x20the\x20string\x20\"\xf0\x9f\x9a\
//     \x80\x20Woo\"\x20in\x20UTF-8,\x20the\x20bytes\x20are\n\x20[240,\x20159,\
//     \x20154,\x20128,\x2032,\x2087,\x20111,\x20111],\x20so\x20the\x20offset\
//     \x20for\x20'W'\n\x20would\x20be\x205.\n\n\r\n\x05\x05\x02\x02\x01\x01\
//     \x12\x04\x82\x01\x02!\n\r\n\x05\x05\x02\x02\x01\x02\x12\x04\x82\x01$%\n\
//     \x82\x02\n\x04\x05\x02\x02\x02\x12\x04\x89\x01\x02'\x1a\xf3\x01\x20The\
//     \x20'character'\x20value\x20is\x20interpreted\x20as\x20an\x20offset\x20i\
//     n\x20terms\n\x20of\x20UTF-16\x20code\x20units\x20(each\x20is\x202\x20byt\
//     es).\n\n\x20Example:\x20For\x20the\x20string\x20\"\xf0\x9f\x9a\x80\x20Wo\
//     o\",\x20the\x20UTF-16\x20code\x20units\x20are\n\x20['\\ud83d',\x20'\\ude\
//     80',\x20'\x20',\x20'W',\x20'o',\x20'o'],\x20so\x20the\x20offset\x20for\
//     \x20'W'\n\x20would\x20be\x203.\n\n\r\n\x05\x05\x02\x02\x02\x01\x12\x04\
//     \x89\x01\x02\"\n\r\n\x05\x05\x02\x02\x02\x02\x12\x04\x89\x01%&\n\xf5\x01\
//     \n\x04\x05\x02\x02\x03\x12\x04\x8f\x01\x02'\x1a\xe6\x01\x20The\x20'chara\
//     cter'\x20value\x20is\x20interpreted\x20as\x20an\x20offset\x20in\x20terms\
//     \n\x20of\x20UTF-32\x20code\x20units\x20(each\x20is\x204\x20bytes).\n\n\
//     \x20Example:\x20For\x20the\x20string\x20\"\xf0\x9f\x9a\x80\x20Woo\",\x20\
//     the\x20UTF-32\x20code\x20units\x20are\n\x20['\xf0\x9f\x9a\x80',\x20'\x20\
//     ',\x20'W',\x20'o',\x20'o'],\x20so\x20the\x20offset\x20for\x20'W'\x20woul\
//     d\x20be\x202.\n\n\r\n\x05\x05\x02\x02\x03\x01\x12\x04\x8f\x01\x02\"\n\r\
//     \n\x05\x05\x02\x02\x03\x02\x12\x04\x8f\x01%&\n\xcc\x12\n\x02\x04\x04\x12\
//     \x06\xbc\x01\0\xc0\x01\x01\x1a\xbd\x12\x20Symbol\x20is\x20similar\x20to\
//     \x20a\x20URI,\x20it\x20identifies\x20a\x20class,\x20method,\x20or\x20a\
//     \x20local\n\x20variable.\x20`SymbolInformation`\x20contains\x20rich\x20m\
//     etadata\x20about\x20symbols\x20such\x20as\n\x20the\x20docstring.\n\n\x20\
//     Symbol\x20has\x20a\x20standardized\x20string\x20representation,\x20which\
//     \x20can\x20be\x20used\n\x20interchangeably\x20with\x20`Symbol`.\x20The\
//     \x20syntax\x20for\x20Symbol\x20is\x20the\x20following:\n\x20```\n\x20#\
//     \x20(<x>)+\x20stands\x20for\x20one\x20or\x20more\x20repetitions\x20of\
//     \x20<x>\n\x20#\x20(<x>)?\x20stands\x20for\x20zero\x20or\x20one\x20occurr\
//     ence\x20of\x20<x>\n\x20<symbol>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20::=\x20<scheme>\x20'\x20'\x20<package>\x20'\x20'\x20\
//     (<descriptor>)+\x20|\x20'local\x20'\x20<local-id>\n\x20<package>\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20<manager>\x20'\
//     \x20'\x20<package-name>\x20'\x20'\x20<version>\n\x20<scheme>\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20any\x20UTF-8,\x20\
//     escape\x20spaces\x20with\x20double\x20space.\x20Must\x20not\x20be\x20emp\
//     ty\x20nor\x20start\x20with\x20'local'\n\x20<manager>\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20any\x20UTF-8,\x20escape\x20sp\
//     aces\x20with\x20double\x20space.\x20Use\x20the\x20placeholder\x20'.'\x20\
//     to\x20indicate\x20an\x20empty\x20value\n\x20<package-name>\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20::=\x20same\x20as\x20above\n\x20<version>\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20same\x20as\
//     \x20above\n\x20<descriptor>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20:\
//     :=\x20<namespace>\x20|\x20<type>\x20|\x20<term>\x20|\x20<method>\x20|\
//     \x20<type-parameter>\x20|\x20<parameter>\x20|\x20<meta>\x20|\x20<macro>\
//     \n\x20<namespace>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20\
//     <name>\x20'/'\n\x20<type>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20::=\x20<name>\x20'#'\n\x20<term>\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20<name>\x20'.'\
//     \n\x20<meta>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20::=\x20<name>\x20':'\n\x20<macro>\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20<name>\x20'!'\n\x20<method>\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20<name\
//     >\x20'('\x20(<method-disambiguator>)?\x20').'\n\x20<type-parameter>\x20\
//     \x20\x20\x20\x20\x20\x20::=\x20'['\x20<name>\x20']'\n\x20<parameter>\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20::=\x20'('\x20<name>\x20')'\
//     \n\x20<name>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20::=\x20<identifier>\n\x20<method-disambiguator>\x20::=\x20<simpl\
//     e-identifier>\n\x20<identifier>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20::=\x20<simple-identifier>\x20|\x20<escaped-identifier>\n\x20<simple\
//     -identifier>\x20\x20\x20\x20::=\x20(<identifier-character>)+\n\x20<ident\
//     ifier-character>\x20::=\x20'_'\x20|\x20'+'\x20|\x20'-'\x20|\x20'$'\x20|\
//     \x20ASCII\x20letter\x20or\x20digit\n\x20<escaped-identifier>\x20\x20\x20\
//     ::=\x20'`'\x20(<escaped-character>)+\x20'`',\x20must\x20contain\x20at\
//     \x20least\x20one\x20non-<identifier-character>\n\x20<escaped-characters>\
//     \x20\x20\x20::=\x20any\x20UTF-8,\x20escape\x20backticks\x20with\x20doubl\
//     e\x20backtick.\n\x20<local-id>\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20::=\x20<simple-identifier>\n\x20```\n\n\x20The\x20list\x20of\
//     \x20descriptors\x20for\x20a\x20symbol\x20should\x20together\x20form\x20a\
//     \x20fully\n\x20qualified\x20name\x20for\x20the\x20symbol.\x20That\x20is,\
//     \x20it\x20should\x20serve\x20as\x20a\x20unique\n\x20identifier\x20across\
//     \x20the\x20package.\x20Typically,\x20it\x20will\x20include\x20one\x20des\
//     criptor\n\x20for\x20every\x20node\x20in\x20the\x20AST\x20(along\x20the\
//     \x20ancestry\x20path)\x20between\x20the\x20root\x20of\n\x20the\x20file\
//     \x20and\x20the\x20node\x20corresponding\x20to\x20the\x20symbol.\n\n\x20L\
//     ocal\x20symbols\x20MUST\x20only\x20be\x20used\x20for\x20entities\x20whic\
//     h\x20are\x20local\x20to\x20a\x20Document,\n\x20and\x20cannot\x20be\x20ac\
//     cessed\x20from\x20outside\x20the\x20Document.\n\n\x0b\n\x03\x04\x04\x01\
//     \x12\x04\xbc\x01\x08\x0e\n\x0c\n\x04\x04\x04\x02\0\x12\x04\xbd\x01\x02\
//     \x14\n\r\n\x05\x04\x04\x02\0\x05\x12\x04\xbd\x01\x02\x08\n\r\n\x05\x04\
//     \x04\x02\0\x01\x12\x04\xbd\x01\t\x0f\n\r\n\x05\x04\x04\x02\0\x03\x12\x04\
//     \xbd\x01\x12\x13\n\x0c\n\x04\x04\x04\x02\x01\x12\x04\xbe\x01\x02\x16\n\r\
//     \n\x05\x04\x04\x02\x01\x06\x12\x04\xbe\x01\x02\t\n\r\n\x05\x04\x04\x02\
//     \x01\x01\x12\x04\xbe\x01\n\x11\n\r\n\x05\x04\x04\x02\x01\x03\x12\x04\xbe\
//     \x01\x14\x15\n\x0c\n\x04\x04\x04\x02\x02\x12\x04\xbf\x01\x02&\n\r\n\x05\
//     \x04\x04\x02\x02\x04\x12\x04\xbf\x01\x02\n\n\r\n\x05\x04\x04\x02\x02\x06\
//     \x12\x04\xbf\x01\x0b\x15\n\r\n\x05\x04\x04\x02\x02\x01\x12\x04\xbf\x01\
//     \x16!\n\r\n\x05\x04\x04\x02\x02\x03\x12\x04\xbf\x01$%\nq\n\x02\x04\x05\
//     \x12\x06\xc5\x01\0\xc9\x01\x01\x1ac\x20Unit\x20of\x20packaging\x20and\
//     \x20distribution.\n\n\x20NOTE:\x20This\x20corresponds\x20to\x20a\x20modu\
//     le\x20in\x20Go\x20and\x20JVM\x20languages.\n\n\x0b\n\x03\x04\x05\x01\x12\
//     \x04\xc5\x01\x08\x0f\n\x0c\n\x04\x04\x05\x02\0\x12\x04\xc6\x01\x02\x15\n\
//     \r\n\x05\x04\x05\x02\0\x05\x12\x04\xc6\x01\x02\x08\n\r\n\x05\x04\x05\x02\
//     \0\x01\x12\x04\xc6\x01\t\x10\n\r\n\x05\x04\x05\x02\0\x03\x12\x04\xc6\x01\
//     \x13\x14\n\x0c\n\x04\x04\x05\x02\x01\x12\x04\xc7\x01\x02\x12\n\r\n\x05\
//     \x04\x05\x02\x01\x05\x12\x04\xc7\x01\x02\x08\n\r\n\x05\x04\x05\x02\x01\
//     \x01\x12\x04\xc7\x01\t\r\n\r\n\x05\x04\x05\x02\x01\x03\x12\x04\xc7\x01\
//     \x10\x11\n\x0c\n\x04\x04\x05\x02\x02\x12\x04\xc8\x01\x02\x15\n\r\n\x05\
//     \x04\x05\x02\x02\x05\x12\x04\xc8\x01\x02\x08\n\r\n\x05\x04\x05\x02\x02\
//     \x01\x12\x04\xc8\x01\t\x10\n\r\n\x05\x04\x05\x02\x02\x03\x12\x04\xc8\x01\
//     \x13\x14\n\x0c\n\x02\x04\x06\x12\x06\xcb\x01\0\xe2\x01\x01\n\x0b\n\x03\
//     \x04\x06\x01\x12\x04\xcb\x01\x08\x12\n\x0e\n\x04\x04\x06\x04\0\x12\x06\
//     \xcc\x01\x02\xde\x01\x03\n\r\n\x05\x04\x06\x04\0\x01\x12\x04\xcc\x01\x07\
//     \r\n\r\n\x05\x04\x06\x04\0\x03\x12\x04\xcd\x01\x04\x1e\n\x0e\n\x06\x04\
//     \x06\x04\0\x03\x02\x12\x04\xcd\x01\x04\x1e\n\x0e\n\x06\x04\x06\x04\0\x02\
//     \0\x12\x04\xce\x01\x04\x1a\n\x0f\n\x07\x04\x06\x04\0\x02\0\x01\x12\x04\
//     \xce\x01\x04\x15\n\x0f\n\x07\x04\x06\x04\0\x02\0\x02\x12\x04\xce\x01\x18\
//     \x19\n}\n\x06\x04\x06\x04\0\x02\x01\x12\x04\xd2\x01\x04\x12\x1am\x20Unit\
//     \x20of\x20code\x20abstraction\x20and/or\x20namespacing.\n\n\x20NOTE:\x20\
//     This\x20corresponds\x20to\x20a\x20package\x20in\x20Go\x20and\x20JVM\x20l\
//     anguages.\n\n\x0f\n\x07\x04\x06\x04\0\x02\x01\x01\x12\x04\xd2\x01\x04\r\
//     \n\x0f\n\x07\x04\x06\x04\0\x02\x01\x02\x12\x04\xd2\x01\x10\x11\n(\n\x06\
//     \x04\x06\x04\0\x02\x02\x12\x04\xd4\x01\x04\"\x1a\x18\x20Use\x20Namespace\
//     \x20instead.\n\n\x0f\n\x07\x04\x06\x04\0\x02\x02\x01\x12\x04\xd4\x01\x04\
//     \x0b\n\x0f\n\x07\x04\x06\x04\0\x02\x02\x02\x12\x04\xd4\x01\x0e\x0f\n\x0f\
//     \n\x07\x04\x06\x04\0\x02\x02\x03\x12\x04\xd4\x01\x10!\n\x10\n\x08\x04\
//     \x06\x04\0\x02\x02\x03\x01\x12\x04\xd4\x01\x11\x20\n\x0e\n\x06\x04\x06\
//     \x04\0\x02\x03\x12\x04\xd5\x01\x04\r\n\x0f\n\x07\x04\x06\x04\0\x02\x03\
//     \x01\x12\x04\xd5\x01\x04\x08\n\x0f\n\x07\x04\x06\x04\0\x02\x03\x02\x12\
//     \x04\xd5\x01\x0b\x0c\n\x0e\n\x06\x04\x06\x04\0\x02\x04\x12\x04\xd6\x01\
//     \x04\r\n\x0f\n\x07\x04\x06\x04\0\x02\x04\x01\x12\x04\xd6\x01\x04\x08\n\
//     \x0f\n\x07\x04\x06\x04\0\x02\x04\x02\x12\x04\xd6\x01\x0b\x0c\n\x0e\n\x06\
//     \x04\x06\x04\0\x02\x05\x12\x04\xd7\x01\x04\x0f\n\x0f\n\x07\x04\x06\x04\0\
//     \x02\x05\x01\x12\x04\xd7\x01\x04\n\n\x0f\n\x07\x04\x06\x04\0\x02\x05\x02\
//     \x12\x04\xd7\x01\r\x0e\n\x0e\n\x06\x04\x06\x04\0\x02\x06\x12\x04\xd8\x01\
//     \x04\x16\n\x0f\n\x07\x04\x06\x04\0\x02\x06\x01\x12\x04\xd8\x01\x04\x11\n\
//     \x0f\n\x07\x04\x06\x04\0\x02\x06\x02\x12\x04\xd8\x01\x14\x15\n\x0e\n\x06\
//     \x04\x06\x04\0\x02\x07\x12\x04\xd9\x01\x04\x12\n\x0f\n\x07\x04\x06\x04\0\
//     \x02\x07\x01\x12\x04\xd9\x01\x04\r\n\x0f\n\x07\x04\x06\x04\0\x02\x07\x02\
//     \x12\x04\xd9\x01\x10\x11\n.\n\x06\x04\x06\x04\0\x02\x08\x12\x04\xdb\x01\
//     \x04\r\x1a\x1e\x20Can\x20be\x20used\x20for\x20any\x20purpose.\n\n\x0f\n\
//     \x07\x04\x06\x04\0\x02\x08\x01\x12\x04\xdb\x01\x04\x08\n\x0f\n\x07\x04\
//     \x06\x04\0\x02\x08\x02\x12\x04\xdb\x01\x0b\x0c\n\x0e\n\x06\x04\x06\x04\0\
//     \x02\t\x12\x04\xdc\x01\x04\x0e\n\x0f\n\x07\x04\x06\x04\0\x02\t\x01\x12\
//     \x04\xdc\x01\x04\t\n\x0f\n\x07\x04\x06\x04\0\x02\t\x02\x12\x04\xdc\x01\
//     \x0c\r\n\x0e\n\x06\x04\x06\x04\0\x02\n\x12\x04\xdd\x01\x04\x0e\n\x0f\n\
//     \x07\x04\x06\x04\0\x02\n\x01\x12\x04\xdd\x01\x04\t\n\x0f\n\x07\x04\x06\
//     \x04\0\x02\n\x02\x12\x04\xdd\x01\x0c\r\n\x0c\n\x04\x04\x06\x02\0\x12\x04\
//     \xdf\x01\x02\x12\n\r\n\x05\x04\x06\x02\0\x05\x12\x04\xdf\x01\x02\x08\n\r\
//     \n\x05\x04\x06\x02\0\x01\x12\x04\xdf\x01\t\r\n\r\n\x05\x04\x06\x02\0\x03\
//     \x12\x04\xdf\x01\x10\x11\n\x0c\n\x04\x04\x06\x02\x01\x12\x04\xe0\x01\x02\
//     \x1b\n\r\n\x05\x04\x06\x02\x01\x05\x12\x04\xe0\x01\x02\x08\n\r\n\x05\x04\
//     \x06\x02\x01\x01\x12\x04\xe0\x01\t\x16\n\r\n\x05\x04\x06\x02\x01\x03\x12\
//     \x04\xe0\x01\x19\x1a\n\x0c\n\x04\x04\x06\x02\x02\x12\x04\xe1\x01\x02\x14\
//     \n\r\n\x05\x04\x06\x02\x02\x06\x12\x04\xe1\x01\x02\x08\n\r\n\x05\x04\x06\
//     \x02\x02\x01\x12\x04\xe1\x01\t\x0f\n\r\n\x05\x04\x06\x02\x02\x03\x12\x04\
//     \xe1\x01\x12\x13\n\x83\x01\n\x02\x04\x07\x12\x06\xe6\x01\0\xb6\x03\x01\
//     \x1au\x20SymbolInformation\x20defines\x20metadata\x20about\x20a\x20symbo\
//     l,\x20such\x20as\x20the\x20symbol's\n\x20docstring\x20or\x20what\x20pack\
//     age\x20it's\x20defined\x20it.\n\n\x0b\n\x03\x04\x07\x01\x12\x04\xe6\x01\
//     \x08\x19\n\xa0\x01\n\x04\x04\x07\x02\0\x12\x04\xe9\x01\x02\x14\x1a\x91\
//     \x01\x20Identifier\x20of\x20this\x20symbol,\x20which\x20can\x20be\x20ref\
//     erenced\x20from\x20`Occurence.symbol`.\n\x20The\x20string\x20must\x20be\
//     \x20formatted\x20according\x20to\x20the\x20grammar\x20in\x20`Symbol`.\n\
//     \n\r\n\x05\x04\x07\x02\0\x05\x12\x04\xe9\x01\x02\x08\n\r\n\x05\x04\x07\
//     \x02\0\x01\x12\x04\xe9\x01\t\x0f\n\r\n\x05\x04\x07\x02\0\x03\x12\x04\xe9\
//     \x01\x12\x13\n\xb4\x03\n\x04\x04\x07\x02\x01\x12\x04\xf0\x01\x02$\x1a\
//     \xa5\x03\x20(optional,\x20but\x20strongly\x20recommended)\x20The\x20mark\
//     down-formatted\x20documentation\n\x20for\x20this\x20symbol.\x20Use\x20`S\
//     ymbolInformation.signature_documentation`\x20to\n\x20document\x20the\x20\
//     method/class/type\x20signature\x20of\x20this\x20symbol.\n\x20Due\x20to\
//     \x20historical\x20reasons,\x20indexers\x20may\x20include\x20signature\
//     \x20documentation\x20in\n\x20this\x20field\x20by\x20rendering\x20markdow\
//     n\x20code\x20blocks.\x20New\x20indexers\x20should\x20only\n\x20include\
//     \x20non-code\x20documentation\x20in\x20this\x20field,\x20for\x20example\
//     \x20docstrings.\n\n\r\n\x05\x04\x07\x02\x01\x04\x12\x04\xf0\x01\x02\n\n\
//     \r\n\x05\x04\x07\x02\x01\x05\x12\x04\xf0\x01\x0b\x11\n\r\n\x05\x04\x07\
//     \x02\x01\x01\x12\x04\xf0\x01\x12\x1f\n\r\n\x05\x04\x07\x02\x01\x03\x12\
//     \x04\xf0\x01\"#\n^\n\x04\x04\x07\x02\x02\x12\x04\xf2\x01\x02*\x1aP\x20(o\
//     ptional)\x20Relationships\x20to\x20other\x20symbols\x20(e.g.,\x20impleme\
//     nts,\x20type\x20definition).\n\n\r\n\x05\x04\x07\x02\x02\x04\x12\x04\xf2\
//     \x01\x02\n\n\r\n\x05\x04\x07\x02\x02\x06\x12\x04\xf2\x01\x0b\x17\n\r\n\
//     \x05\x04\x07\x02\x02\x01\x12\x04\xf2\x01\x18%\n\r\n\x05\x04\x07\x02\x02\
//     \x03\x12\x04\xf2\x01()\n\xa4\x01\n\x04\x04\x07\x02\x03\x12\x04\xf6\x01\
//     \x02\x10\x1a\x95\x01\x20The\x20kind\x20of\x20this\x20symbol.\x20Use\x20t\
//     his\x20field\x20instead\x20of\n\x20`SymbolDescriptor.Suffix`\x20to\x20de\
//     termine\x20whether\x20something\x20is,\x20for\x20example,\x20a\n\x20clas\
//     s\x20or\x20a\x20method.\n\n\r\n\x05\x04\x07\x02\x03\x06\x12\x04\xf6\x01\
//     \x02\x06\n\r\n\x05\x04\x07\x02\x03\x01\x12\x04\xf6\x01\x07\x0b\n\r\n\x05\
//     \x04\x07\x02\x03\x03\x12\x04\xf6\x01\x0e\x0f\n\xf5\x05\n\x04\x04\x07\x04\
//     \0\x12\x06\x85\x02\x02\x92\x03\x03\x1a\xe4\x05\x20(optional)\x20Kind\x20\
//     represents\x20the\x20fine-grained\x20category\x20of\x20a\x20symbol,\x20s\
//     uitable\x20for\x20presenting\n\x20information\x20about\x20the\x20symbol'\
//     s\x20meaning\x20in\x20the\x20language.\n\n\x20For\x20example:\n\x20-\x20\
//     A\x20Java\x20method\x20would\x20have\x20the\x20kind\x20`Method`\x20while\
//     \x20a\x20Go\x20function\x20would\n\x20\x20\x20have\x20the\x20kind\x20`Fu\
//     nction`,\x20even\x20if\x20the\x20symbols\x20for\x20these\x20use\x20the\
//     \x20same\n\x20\x20\x20syntax\x20for\x20the\x20descriptor\x20`SymbolDescr\
//     iptor.Suffix.Method`.\n\x20-\x20A\x20Go\x20struct\x20has\x20the\x20symbo\
//     l\x20kind\x20`Struct`\x20while\x20a\x20Java\x20class\x20has\n\x20\x20\
//     \x20the\x20symbol\x20kind\x20`Class`\x20even\x20if\x20they\x20both\x20ha\
//     ve\x20the\x20same\x20descriptor:\n\x20\x20\x20`SymbolDescriptor.Suffix.T\
//     ype`.\n\n\x20Since\x20Kind\x20is\x20more\x20fine-grained\x20than\x20Suff\
//     ix:\n\x20-\x20If\x20two\x20symbols\x20have\x20the\x20same\x20Kind,\x20th\
//     ey\x20should\x20share\x20the\x20same\x20Suffix.\n\x20-\x20If\x20two\x20s\
//     ymbols\x20have\x20different\x20Suffixes,\x20they\x20should\x20have\x20di\
//     fferent\x20Kinds.\n\n\r\n\x05\x04\x07\x04\0\x01\x12\x04\x85\x02\x07\x0b\
//     \n\x0e\n\x06\x04\x07\x04\0\x02\0\x12\x04\x86\x02\x06\x1a\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\0\x01\x12\x04\x86\x02\x06\x15\n\x0f\n\x07\x04\x07\x04\0\
//     \x02\0\x02\x12\x04\x86\x02\x18\x19\nR\n\x06\x04\x07\x04\0\x02\x01\x12\
//     \x04\x88\x02\x06\x1a\x1aB\x20A\x20method\x20which\x20may\x20or\x20may\
//     \x20not\x20have\x20a\x20body.\x20For\x20Java,\x20Kotlin\x20etc.\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x01\x01\x12\x04\x88\x02\x06\x14\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x01\x02\x12\x04\x88\x02\x17\x19\n*\n\x06\x04\x07\x04\0\
//     \x02\x02\x12\x04\x8a\x02\x06\x14\x1a\x1a\x20For\x20Ruby's\x20attr_access\
//     or\n\n\x0f\n\x07\x04\x07\x04\0\x02\x02\x01\x12\x04\x8a\x02\x06\x0e\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x02\x02\x12\x04\x8a\x02\x11\x13\n\x0e\n\x06\x04\
//     \x07\x04\0\x02\x03\x12\x04\x8b\x02\x06\x10\n\x0f\n\x07\x04\x07\x04\0\x02\
//     \x03\x01\x12\x04\x8b\x02\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02\x03\x02\
//     \x12\x04\x8b\x02\x0e\x0f\n\x1b\n\x06\x04\x07\x04\0\x02\x04\x12\x04\x8d\
//     \x02\x06\x14\x1a\x0b\x20For\x20Alloy\n\n\x0f\n\x07\x04\x07\x04\0\x02\x04\
//     \x01\x12\x04\x8d\x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\x02\x04\x02\x12\
//     \x04\x8d\x02\x12\x13\n\x0e\n\x06\x04\x07\x04\0\x02\x05\x12\x04\x8e\x02\
//     \x06\x19\n\x0f\n\x07\x04\x07\x04\0\x02\x05\x01\x12\x04\x8e\x02\x06\x14\n\
//     \x0f\n\x07\x04\x07\x04\0\x02\x05\x02\x12\x04\x8e\x02\x17\x18\n\x19\n\x06\
//     \x04\x07\x04\0\x02\x06\x12\x04\x90\x02\x06\x14\x1a\t\x20For\x20C++\n\n\
//     \x0f\n\x07\x04\x07\x04\0\x02\x06\x01\x12\x04\x90\x02\x06\x0f\n\x0f\n\x07\
//     \x04\x07\x04\0\x02\x06\x02\x12\x04\x90\x02\x12\x13\n\x1a\n\x06\x04\x07\
//     \x04\0\x02\x07\x12\x04\x92\x02\x06\x10\x1a\n\x20For\x20Lean\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x07\x01\x12\x04\x92\x02\x06\x0b\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x07\x02\x12\x04\x92\x02\x0e\x0f\n\x0e\n\x06\x04\x07\x04\0\
//     \x02\x08\x12\x04\x93\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\0\x02\x08\x01\
//     \x12\x04\x93\x02\x06\r\n\x0f\n\x07\x04\x07\x04\0\x02\x08\x02\x12\x04\x93\
//     \x02\x10\x11\n\x0e\n\x06\x04\x07\x04\0\x02\t\x12\x04\x94\x02\x06\x10\n\
//     \x0f\n\x07\x04\x07\x04\0\x02\t\x01\x12\x04\x94\x02\x06\x0b\n\x0f\n\x07\
//     \x04\x07\x04\0\x02\t\x02\x12\x04\x94\x02\x0e\x0f\n\x0e\n\x06\x04\x07\x04\
//     \0\x02\n\x12\x04\x95\x02\x06\x13\n\x0f\n\x07\x04\x07\x04\0\x02\n\x01\x12\
//     \x04\x95\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\x02\n\x02\x12\x04\x95\x02\
//     \x11\x12\n\x0e\n\x06\x04\x07\x04\0\x02\x0b\x12\x04\x96\x02\x06\x16\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x0b\x01\x12\x04\x96\x02\x06\x11\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x0b\x02\x12\x04\x96\x02\x14\x15\n\x1e\n\x06\x04\x07\x04\0\
//     \x02\x0c\x12\x04\x98\x02\x06\x14\x1a\x0e\x20For\x20Solidity\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x0c\x01\x12\x04\x98\x02\x06\x0e\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x0c\x02\x12\x04\x98\x02\x11\x13\n\x1d\n\x06\x04\x07\x04\0\
//     \x02\r\x12\x04\x9a\x02\x06\x16\x1a\r\x20For\x20Haskell\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\r\x01\x12\x04\x9a\x02\x06\x10\n\x0f\n\x07\x04\x07\x04\0\
//     \x02\r\x02\x12\x04\x9a\x02\x13\x15\n\x1f\n\x06\x04\x07\x04\0\x02\x0e\x12\
//     \x04\x9c\x02\x06\x14\x1a\x0f\x20For\x20C#\x20and\x20F#\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x0e\x01\x12\x04\x9c\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\
//     \x02\x0e\x02\x12\x04\x9c\x02\x11\x13\n\x0e\n\x06\x04\x07\x04\0\x02\x0f\
//     \x12\x04\x9d\x02\x06\x10\n\x0f\n\x07\x04\x07\x04\0\x02\x0f\x01\x12\x04\
//     \x9d\x02\x06\n\n\x0f\n\x07\x04\x07\x04\0\x02\x0f\x02\x12\x04\x9d\x02\r\
//     \x0f\n\x0e\n\x06\x04\x07\x04\0\x02\x10\x12\x04\x9e\x02\x06\x16\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x10\x01\x12\x04\x9e\x02\x06\x10\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x10\x02\x12\x04\x9e\x02\x13\x15\n\x0e\n\x06\x04\x07\x04\0\
//     \x02\x11\x12\x04\x9f\x02\x06\x11\n\x0f\n\x07\x04\x07\x04\0\x02\x11\x01\
//     \x12\x04\x9f\x02\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02\x11\x02\x12\x04\
//     \x9f\x02\x0e\x10\n\x0e\n\x06\x04\x07\x04\0\x02\x12\x12\x04\xa0\x02\x06\
//     \x11\n\x0f\n\x07\x04\x07\x04\0\x02\x12\x01\x12\x04\xa0\x02\x06\x0b\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x12\x02\x12\x04\xa0\x02\x0e\x10\n\x1a\n\x06\x04\
//     \x07\x04\0\x02\x13\x12\x04\xa2\x02\x06\x15\x1a\n\x20For\x20Dart\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x13\x01\x12\x04\xa2\x02\x06\x0f\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x13\x02\x12\x04\xa2\x02\x12\x14\n\x1b\n\x06\x04\x07\x04\0\
//     \x02\x14\x12\x04\xa4\x02\x06\x10\x1a\x0b\x20For\x20Alloy\n\n\x0f\n\x07\
//     \x04\x07\x04\0\x02\x14\x01\x12\x04\xa4\x02\x06\n\n\x0f\n\x07\x04\x07\x04\
//     \0\x02\x14\x02\x12\x04\xa4\x02\r\x0f\n\x0e\n\x06\x04\x07\x04\0\x02\x15\
//     \x12\x04\xa5\x02\x06\x11\n\x0f\n\x07\x04\x07\x04\0\x02\x15\x01\x12\x04\
//     \xa5\x02\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02\x15\x02\x12\x04\xa5\x02\
//     \x0e\x10\n\x0e\n\x06\x04\x07\x04\0\x02\x16\x12\x04\xa6\x02\x06\x10\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x16\x01\x12\x04\xa6\x02\x06\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x16\x02\x12\x04\xa6\x02\r\x0f\n\x0e\n\x06\x04\x07\x04\0\
//     \x02\x17\x12\x04\xa7\x02\x06\x14\n\x0f\n\x07\x04\x07\x04\0\x02\x17\x01\
//     \x12\x04\xa7\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\x02\x17\x02\x12\x04\
//     \xa7\x02\x11\x13\n;\n\x06\x04\x07\x04\0\x02\x18\x12\x04\xa9\x02\x06\x12\
//     \x1a+\x20For\x20'get'\x20in\x20Swift,\x20'attr_reader'\x20in\x20Ruby\n\n\
//     \x0f\n\x07\x04\x07\x04\0\x02\x18\x01\x12\x04\xa9\x02\x06\x0c\n\x0f\n\x07\
//     \x04\x07\x04\0\x02\x18\x02\x12\x04\xa9\x02\x0f\x11\n\x1a\n\x06\x04\x07\
//     \x04\0\x02\x19\x12\x04\xab\x02\x06\x13\x1a\n\x20For\x20Raku\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x19\x01\x12\x04\xab\x02\x06\r\n\x0f\n\x07\x04\x07\
//     \x04\0\x02\x19\x02\x12\x04\xab\x02\x10\x12\n)\n\x06\x04\x07\x04\0\x02\
//     \x1a\x12\x04\xad\x02\x06\x14\x1a\x19\x20For\x20Purescript\x20and\x20Lean\
//     \n\n\x0f\n\x07\x04\x07\x04\0\x02\x1a\x01\x12\x04\xad\x02\x06\x0e\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x1a\x02\x12\x04\xad\x02\x11\x13\n\x0e\n\x06\x04\
//     \x07\x04\0\x02\x1b\x12\x04\xae\x02\x06\x15\n\x0f\n\x07\x04\x07\x04\0\x02\
//     \x1b\x01\x12\x04\xae\x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\x02\x1b\x02\
//     \x12\x04\xae\x02\x12\x14\n\x0e\n\x06\x04\x07\x04\0\x02\x1c\x12\x04\xaf\
//     \x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\x02\x1c\x01\x12\x04\xaf\x02\x06\t\
//     \n\x0f\n\x07\x04\x07\x04\0\x02\x1c\x02\x12\x04\xaf\x02\x0c\x0e\n\x1c\n\
//     \x06\x04\x07\x04\0\x02\x1d\x12\x04\xb1\x02\x06\x10\x1a\x0c\x20For\x20Rac\
//     ket\n\n\x0f\n\x07\x04\x07\x04\0\x02\x1d\x01\x12\x04\xb1\x02\x06\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x1d\x02\x12\x04\xb1\x02\r\x0f\n\x1a\n\x06\x04\
//     \x07\x04\0\x02\x1e\x12\x04\xb3\x02\x06\x11\x1a\n\x20For\x20Lean\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02\x1e\x01\x12\x04\xb3\x02\x06\x0b\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\x1e\x02\x12\x04\xb3\x02\x0e\x10\n\x1e\n\x06\x04\x07\x04\0\
//     \x02\x1f\x12\x04\xb5\x02\x06\x13\x1a\x0e\x20For\x20solidity\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02\x1f\x01\x12\x04\xb5\x02\x06\r\n\x0f\n\x07\x04\x07\
//     \x04\0\x02\x1f\x02\x12\x04\xb5\x02\x10\x12\n\x0e\n\x06\x04\x07\x04\0\x02\
//     \x20\x12\x04\xb6\x02\x06\x11\n\x0f\n\x07\x04\x07\x04\0\x02\x20\x01\x12\
//     \x04\xb6\x02\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02\x20\x02\x12\x04\xb6\
//     \x02\x0e\x10\n\x0e\n\x06\x04\x07\x04\0\x02!\x12\x04\xb7\x02\x06\x12\n\
//     \x0f\n\x07\x04\x07\x04\0\x02!\x01\x12\x04\xb7\x02\x06\x0c\n\x0f\n\x07\
//     \x04\x07\x04\0\x02!\x02\x12\x04\xb7\x02\x0f\x11\n\x1a\n\x06\x04\x07\x04\
//     \0\x02\"\x12\x04\xb9\x02\x06\x17\x1a\n\x20For\x20Ruby\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02\"\x01\x12\x04\xb9\x02\x06\x11\n\x0f\n\x07\x04\x07\x04\0\
//     \x02\"\x02\x12\x04\xb9\x02\x14\x16\n\x94\x01\n\x06\x04\x07\x04\0\x02#\
//     \x12\x04\xbc\x02\x06\x1a\x1a\x83\x01\x20Analogous\x20to\x20'ThisParamete\
//     r'\x20and\x20'SelfParameter',\x20but\x20for\x20languages\n\x20like\x20Go\
//     \x20where\x20the\x20receiver\x20doesn't\x20have\x20a\x20conventional\x20\
//     name.\n\n\x0f\n\x07\x04\x07\x04\0\x02#\x01\x12\x04\xbc\x02\x06\x14\n\x0f\
//     \n\x07\x04\x07\x04\0\x02#\x02\x12\x04\xbc\x02\x17\x19\n8\n\x06\x04\x07\
//     \x04\0\x02$\x12\x04\xbe\x02\x06\x1f\x1a(\x20Analogous\x20to\x20'Abstract\
//     Method',\x20for\x20Go.\n\n\x0f\n\x07\x04\x07\x04\0\x02$\x01\x12\x04\xbe\
//     \x02\x06\x19\n\x0f\n\x07\x04\x07\x04\0\x02$\x02\x12\x04\xbe\x02\x1c\x1e\
//     \n\x1e\n\x06\x04\x07\x04\0\x02%\x12\x04\xc0\x02\x06\x13\x1a\x0e\x20For\
//     \x20Protobuf\n\n\x0f\n\x07\x04\x07\x04\0\x02%\x01\x12\x04\xc0\x02\x06\r\
//     \n\x0f\n\x07\x04\x07\x04\0\x02%\x02\x12\x04\xc0\x02\x10\x12\n\x1a\n\x06\
//     \x04\x07\x04\0\x02&\x12\x04\xc2\x02\x06\x11\x1a\n\x20For\x20Dart\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02&\x01\x12\x04\xc2\x02\x06\x0b\n\x0f\n\x07\x04\
//     \x07\x04\0\x02&\x02\x12\x04\xc2\x02\x0e\x10\n\x1e\n\x06\x04\x07\x04\0\
//     \x02'\x12\x04\xc4\x02\x06\x14\x1a\x0e\x20For\x20Solidity\n\n\x0f\n\x07\
//     \x04\x07\x04\0\x02'\x01\x12\x04\xc4\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\
//     \0\x02'\x02\x12\x04\xc4\x02\x11\x13\n\x0e\n\x06\x04\x07\x04\0\x02(\x12\
//     \x04\xc5\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\0\x02(\x01\x12\x04\xc5\x02\
//     \x06\x0c\n\x0f\n\x07\x04\x07\x04\0\x02(\x02\x12\x04\xc5\x02\x0f\x11\n\
//     \x0e\n\x06\x04\x07\x04\0\x02)\x12\x04\xc6\x02\x06\x15\n\x0f\n\x07\x04\
//     \x07\x04\0\x02)\x01\x12\x04\xc6\x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\
//     \x02)\x02\x12\x04\xc6\x02\x12\x14\n\x0e\n\x06\x04\x07\x04\0\x02*\x12\x04\
//     \xc7\x02\x06\x10\n\x0f\n\x07\x04\x07\x04\0\x02*\x01\x12\x04\xc7\x02\x06\
//     \n\n\x0f\n\x07\x04\x07\x04\0\x02*\x02\x12\x04\xc7\x02\r\x0f\n\x0e\n\x06\
//     \x04\x07\x04\0\x02+\x12\x04\xc8\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\0\
//     \x02+\x01\x12\x04\xc8\x02\x06\x0c\n\x0f\n\x07\x04\x07\x04\0\x02+\x02\x12\
//     \x04\xc8\x02\x0f\x11\n\x0e\n\x06\x04\x07\x04\0\x02,\x12\x04\xc9\x02\x06\
//     \x12\n\x0f\n\x07\x04\x07\x04\0\x02,\x01\x12\x04\xc9\x02\x06\x0c\n\x0f\n\
//     \x07\x04\x07\x04\0\x02,\x02\x12\x04\xc9\x02\x0f\x11\n\x0e\n\x06\x04\x07\
//     \x04\0\x02-\x12\x04\xca\x02\x06\x14\n\x0f\n\x07\x04\x07\x04\0\x02-\x01\
//     \x12\x04\xca\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\x02-\x02\x12\x04\xca\
//     \x02\x11\x13\n\x0e\n\x06\x04\x07\x04\0\x02.\x12\x04\xcb\x02\x06\x13\n\
//     \x0f\n\x07\x04\x07\x04\0\x02.\x01\x12\x04\xcb\x02\x06\r\n\x0f\n\x07\x04\
//     \x07\x04\0\x02.\x02\x12\x04\xcb\x02\x10\x12\n\x0e\n\x06\x04\x07\x04\0\
//     \x02/\x12\x04\xcc\x02\x06\x19\n\x0f\n\x07\x04\x07\x04\0\x02/\x01\x12\x04\
//     \xcc\x02\x06\x13\n\x0f\n\x07\x04\x07\x04\0\x02/\x02\x12\x04\xcc\x02\x16\
//     \x18\n\x0e\n\x06\x04\x07\x04\0\x020\x12\x04\xcd\x02\x06\x15\n\x0f\n\x07\
//     \x04\x07\x04\0\x020\x01\x12\x04\xcd\x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\
//     \0\x020\x02\x12\x04\xcd\x02\x12\x14\n\x0e\n\x06\x04\x07\x04\0\x021\x12\
//     \x04\xce\x02\x06\x1a\n\x0f\n\x07\x04\x07\x04\0\x021\x01\x12\x04\xce\x02\
//     \x06\x14\n\x0f\n\x07\x04\x07\x04\0\x021\x02\x12\x04\xce\x02\x17\x19\n/\n\
//     \x06\x04\x07\x04\0\x022\x12\x04\xd0\x02\x06\x13\x1a\x1f\x20For\x20Haskel\
//     l's\x20PatternSynonyms\n\n\x0f\n\x07\x04\x07\x04\0\x022\x01\x12\x04\xd0\
//     \x02\x06\r\n\x0f\n\x07\x04\x07\x04\0\x022\x02\x12\x04\xd0\x02\x10\x12\n\
//     \x1b\n\x06\x04\x07\x04\0\x023\x12\x04\xd2\x02\x06\x15\x1a\x0b\x20For\x20\
//     Alloy\n\n\x0f\n\x07\x04\x07\x04\0\x023\x01\x12\x04\xd2\x02\x06\x0f\n\x0f\
//     \n\x07\x04\x07\x04\0\x023\x02\x12\x04\xd2\x02\x12\x14\n\x0e\n\x06\x04\
//     \x07\x04\0\x024\x12\x04\xd3\x02\x06\x14\n\x0f\n\x07\x04\x07\x04\0\x024\
//     \x01\x12\x04\xd3\x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\x024\x02\x12\x04\
//     \xd3\x02\x11\x13\nQ\n\x06\x04\x07\x04\0\x025\x12\x04\xd5\x02\x06\x14\x1a\
//     A\x20Analogous\x20to\x20'Trait'\x20and\x20'TypeClass',\x20for\x20Swift\
//     \x20and\x20Objective-C\n\n\x0f\n\x07\x04\x07\x04\0\x025\x01\x12\x04\xd5\
//     \x02\x06\x0e\n\x0f\n\x07\x04\x07\x04\0\x025\x02\x12\x04\xd5\x02\x11\x13\
//     \nK\n\x06\x04\x07\x04\0\x026\x12\x04\xd7\x02\x06\x1a\x1a;\x20Analogous\
//     \x20to\x20'AbstractMethod',\x20for\x20Swift\x20and\x20Objective-C.\n\n\
//     \x0f\n\x07\x04\x07\x04\0\x026\x01\x12\x04\xd7\x02\x06\x14\n\x0f\n\x07\
//     \x04\x07\x04\0\x026\x02\x12\x04\xd7\x02\x17\x19\n9\n\x06\x04\x07\x04\0\
//     \x027\x12\x04\xd9\x02\x06\x1d\x1a)\x20Analogous\x20to\x20'AbstractMethod\
//     ',\x20for\x20C++.\n\n\x0f\n\x07\x04\x07\x04\0\x027\x01\x12\x04\xd9\x02\
//     \x06\x17\n\x0f\n\x07\x04\x07\x04\0\x027\x02\x12\x04\xd9\x02\x1a\x1c\n\
//     \x1d\n\x06\x04\x07\x04\0\x028\x12\x04\xdb\x02\x06\x17\x1a\r\x20For\x20Ha\
//     skell\n\n\x0f\n\x07\x04\x07\x04\0\x028\x01\x12\x04\xdb\x02\x06\x11\n\x0f\
//     \n\x07\x04\x07\x04\0\x028\x02\x12\x04\xdb\x02\x14\x16\n4\n\x06\x04\x07\
//     \x04\0\x029\x12\x04\xdd\x02\x06\x19\x1a$\x20'self'\x20in\x20Python,\x20R\
//     ust,\x20Swift\x20etc.\n\n\x0f\n\x07\x04\x07\x04\0\x029\x01\x12\x04\xdd\
//     \x02\x06\x13\n\x0f\n\x07\x04\x07\x04\0\x029\x02\x12\x04\xdd\x02\x16\x18\
//     \n;\n\x06\x04\x07\x04\0\x02:\x12\x04\xdf\x02\x06\x12\x1a+\x20For\x20'set\
//     '\x20in\x20Swift,\x20'attr_writer'\x20in\x20Ruby\n\n\x0f\n\x07\x04\x07\
//     \x04\0\x02:\x01\x12\x04\xdf\x02\x06\x0c\n\x0f\n\x07\x04\x07\x04\0\x02:\
//     \x02\x12\x04\xdf\x02\x0f\x11\n3\n\x06\x04\x07\x04\0\x02;\x12\x04\xe1\x02\
//     \x06\x15\x1a#\x20For\x20Alloy,\x20analogous\x20to\x20'Struct'.\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02;\x01\x12\x04\xe1\x02\x06\x0f\n\x0f\n\x07\x04\x07\
//     \x04\0\x02;\x02\x12\x04\xe1\x02\x12\x14\n\x1a\n\x06\x04\x07\x04\0\x02<\
//     \x12\x04\xe3\x02\x06\x1a\x1a\n\x20For\x20Ruby\n\n\x0f\n\x07\x04\x07\x04\
//     \0\x02<\x01\x12\x04\xe3\x02\x06\x14\n\x0f\n\x07\x04\x07\x04\0\x02<\x02\
//     \x12\x04\xe3\x02\x17\x19\n8\n\x06\x04\x07\x04\0\x02=\x12\x04\xe5\x02\x06\
//     \x1b\x1a(\x20Analogous\x20to\x20'StaticMethod',\x20for\x20Ruby.\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02=\x01\x12\x04\xe5\x02\x06\x15\n\x0f\n\x07\x04\
//     \x07\x04\0\x02=\x02\x12\x04\xe5\x02\x18\x1a\n5\n\x06\x04\x07\x04\0\x02>\
//     \x12\x04\xe7\x02\x06\x1c\x1a%\x20Analogous\x20to\x20'StaticField',\x20fo\
//     r\x20C++\n\n\x0f\n\x07\x04\x07\x04\0\x02>\x01\x12\x04\xe7\x02\x06\x16\n\
//     \x0f\n\x07\x04\x07\x04\0\x02>\x02\x12\x04\xe7\x02\x19\x1b\n\x18\n\x06\
//     \x04\x07\x04\0\x02?\x12\x04\xe9\x02\x06\x17\x1a\x08\x20For\x20C#\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02?\x01\x12\x04\xe9\x02\x06\x11\n\x0f\n\x07\x04\
//     \x07\x04\0\x02?\x02\x12\x04\xe9\x02\x14\x16\n\x18\n\x06\x04\x07\x04\0\
//     \x02@\x12\x04\xeb\x02\x06\x17\x1a\x08\x20For\x20C#\n\n\x0f\n\x07\x04\x07\
//     \x04\0\x02@\x01\x12\x04\xeb\x02\x06\x11\n\x0f\n\x07\x04\x07\x04\0\x02@\
//     \x02\x12\x04\xeb\x02\x14\x16\n(\n\x06\x04\x07\x04\0\x02A\x12\x04\xed\x02\
//     \x06\x18\x1a\x18\x20For\x20Java,\x20C#,\x20C++\x20etc.\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02A\x01\x12\x04\xed\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\0\
//     \x02A\x02\x12\x04\xed\x02\x15\x17\n)\n\x06\x04\x07\x04\0\x02B\x12\x04\
//     \xef\x02\x06\x1a\x1a\x19\x20For\x20C#,\x20TypeScript\x20etc.\n\n\x0f\n\
//     \x07\x04\x07\x04\0\x02B\x01\x12\x04\xef\x02\x06\x14\n\x0f\n\x07\x04\x07\
//     \x04\0\x02B\x02\x12\x04\xef\x02\x17\x19\n\x1c\n\x06\x04\x07\x04\0\x02C\
//     \x12\x04\xf1\x02\x06\x1a\x1a\x0c\x20For\x20C,\x20C++\n\n\x0f\n\x07\x04\
//     \x07\x04\0\x02C\x01\x12\x04\xf1\x02\x06\x14\n\x0f\n\x07\x04\x07\x04\0\
//     \x02C\x02\x12\x04\xf1\x02\x17\x19\n\x0e\n\x06\x04\x07\x04\0\x02D\x12\x04\
//     \xf2\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\0\x02D\x01\x12\x04\xf2\x02\x06\
//     \x0c\n\x0f\n\x07\x04\x07\x04\0\x02D\x02\x12\x04\xf2\x02\x0f\x11\n\x0e\n\
//     \x06\x04\x07\x04\0\x02E\x12\x04\xf3\x02\x06\x12\n\x0f\n\x07\x04\x07\x04\
//     \0\x02E\x01\x12\x04\xf3\x02\x06\x0c\n\x0f\n\x07\x04\x07\x04\0\x02E\x02\
//     \x12\x04\xf3\x02\x0f\x11\n\x1b\n\x06\x04\x07\x04\0\x02F\x12\x04\xf5\x02\
//     \x06\x15\x1a\x0b\x20For\x20Swift\n\n\x0f\n\x07\x04\x07\x04\0\x02F\x01\
//     \x12\x04\xf5\x02\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\x02F\x02\x12\x04\xf5\
//     \x02\x12\x14\n\x1a\n\x06\x04\x07\x04\0\x02G\x12\x04\xf7\x02\x06\x12\x1a\
//     \n\x20For\x20Lean\n\n\x0f\n\x07\x04\x07\x04\0\x02G\x01\x12\x04\xf7\x02\
//     \x06\x0c\n\x0f\n\x07\x04\x07\x04\0\x02G\x02\x12\x04\xf7\x02\x0f\x11\n\
//     \x1a\n\x06\x04\x07\x04\0\x02H\x12\x04\xf9\x02\x06\x13\x1a\n\x20For\x20Le\
//     an\n\n\x0f\n\x07\x04\x07\x04\0\x02H\x01\x12\x04\xf9\x02\x06\r\n\x0f\n\
//     \x07\x04\x07\x04\0\x02H\x02\x12\x04\xf9\x02\x10\x12\nU\n\x06\x04\x07\x04\
//     \0\x02I\x12\x04\xfc\x02\x06\x19\x1aE\x20Method\x20receiver\x20for\x20lan\
//     guages\n\x20'this'\x20in\x20JavaScript,\x20C++,\x20Java\x20etc.\n\n\x0f\
//     \n\x07\x04\x07\x04\0\x02I\x01\x12\x04\xfc\x02\x06\x13\n\x0f\n\x07\x04\
//     \x07\x04\0\x02I\x02\x12\x04\xfc\x02\x16\x18\nO\n\x06\x04\x07\x04\0\x02J\
//     \x12\x04\xfe\x02\x06\x11\x1a?\x20Analogous\x20to\x20'Protocol'\x20and\
//     \x20'TypeClass',\x20for\x20Rust,\x20Scala\x20etc.\n\n\x0f\n\x07\x04\x07\
//     \x04\0\x02J\x01\x12\x04\xfe\x02\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02J\
//     \x02\x12\x04\xfe\x02\x0e\x10\nE\n\x06\x04\x07\x04\0\x02K\x12\x04\x80\x03\
//     \x06\x17\x1a5\x20Analogous\x20to\x20'AbstractMethod',\x20for\x20Rust,\
//     \x20Scala\x20etc.\n\n\x0f\n\x07\x04\x07\x04\0\x02K\x01\x12\x04\x80\x03\
//     \x06\x11\n\x0f\n\x07\x04\x07\x04\0\x02K\x02\x12\x04\x80\x03\x14\x16\n\
//     \x89\x01\n\x06\x04\x07\x04\0\x02L\x12\x04\x83\x03\x06\x10\x1ay\x20Data\
//     \x20type\x20definition\x20for\x20languages\x20like\x20OCaml\x20which\x20\
//     use\x20`type`\n\x20rather\x20than\x20separate\x20keywords\x20like\x20`st\
//     ruct`\x20and\x20`enum`.\n\n\x0f\n\x07\x04\x07\x04\0\x02L\x01\x12\x04\x83\
//     \x03\x06\n\n\x0f\n\x07\x04\x07\x04\0\x02L\x02\x12\x04\x83\x03\r\x0f\n\
//     \x0e\n\x06\x04\x07\x04\0\x02M\x12\x04\x84\x03\x06\x15\n\x0f\n\x07\x04\
//     \x07\x04\0\x02M\x01\x12\x04\x84\x03\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\
//     \x02M\x02\x12\x04\x84\x03\x12\x14\nS\n\x06\x04\x07\x04\0\x02N\x12\x04\
//     \x86\x03\x06\x15\x1aC\x20Analogous\x20to\x20'Trait'\x20and\x20'Protocol'\
//     ,\x20for\x20Haskell,\x20Purescript\x20etc.\n\n\x0f\n\x07\x04\x07\x04\0\
//     \x02N\x01\x12\x04\x86\x03\x06\x0f\n\x0f\n\x07\x04\x07\x04\0\x02N\x02\x12\
//     \x04\x86\x03\x12\x14\nM\n\x06\x04\x07\x04\0\x02O\x12\x04\x88\x03\x06\x1b\
//     \x1a=\x20Analogous\x20to\x20'AbstractMethod',\x20for\x20Haskell,\x20Pure\
//     script\x20etc.\n\n\x0f\n\x07\x04\x07\x04\0\x02O\x01\x12\x04\x88\x03\x06\
//     \x15\n\x0f\n\x07\x04\x07\x04\0\x02O\x02\x12\x04\x88\x03\x18\x1a\n\x1d\n\
//     \x06\x04\x07\x04\0\x02P\x12\x04\x8a\x03\x06\x16\x1a\r\x20For\x20Haskell\
//     \n\n\x0f\n\x07\x04\x07\x04\0\x02P\x01\x12\x04\x8a\x03\x06\x10\n\x0f\n\
//     \x07\x04\x07\x04\0\x02P\x02\x12\x04\x8a\x03\x13\x15\n\x0e\n\x06\x04\x07\
//     \x04\0\x02Q\x12\x04\x8b\x03\x06\x19\n\x0f\n\x07\x04\x07\x04\0\x02Q\x01\
//     \x12\x04\x8b\x03\x06\x13\n\x0f\n\x07\x04\x07\x04\0\x02Q\x02\x12\x04\x8b\
//     \x03\x16\x18\n(\n\x06\x04\x07\x04\0\x02R\x12\x04\x8d\x03\x06\x11\x1a\x18\
//     \x20For\x20C,\x20C++,\x20Capn\x20Proto\n\n\x0f\n\x07\x04\x07\x04\0\x02R\
//     \x01\x12\x04\x8d\x03\x06\x0b\n\x0f\n\x07\x04\x07\x04\0\x02R\x02\x12\x04\
//     \x8d\x03\x0e\x10\n\x0e\n\x06\x04\x07\x04\0\x02S\x12\x04\x8e\x03\x06\x11\
//     \n\x0f\n\x07\x04\x07\x04\0\x02S\x01\x12\x04\x8e\x03\x06\x0b\n\x0f\n\x07\
//     \x04\x07\x04\0\x02S\x02\x12\x04\x8e\x03\x0e\x10\n[\n\x06\x04\x07\x04\0\
//     \x02T\x12\x04\x8f\x03\x06\x14\"K\x20Next\x20=\x2086;\n\x20Feel\x20free\
//     \x20to\x20open\x20a\x20PR\x20proposing\x20new\x20language-specific\x20ki\
//     nds.\n\n\x0f\n\x07\x04\x07\x04\0\x02T\x01\x12\x04\x8f\x03\x06\x0e\n\x0f\
//     \n\x07\x04\x07\x04\0\x02T\x02\x12\x04\x8f\x03\x11\x13\n\xf3\x03\n\x04\
//     \x04\x07\x02\x04\x12\x04\x9c\x03\x02\x1a\x1a\xe4\x03\x20(optional)\x20Th\
//     e\x20name\x20of\x20this\x20symbol\x20as\x20it\x20should\x20be\x20display\
//     ed\x20to\x20the\x20user.\n\x20For\x20example,\x20the\x20symbol\x20\"com/\
//     example/MyClass#myMethod(+1).\"\x20should\x20have\x20the\n\x20display\
//     \x20name\x20\"myMethod\".\x20The\x20`symbol`\x20field\x20is\x20not\x20a\
//     \x20reliable\x20source\x20of\n\x20the\x20display\x20name\x20for\x20sever\
//     al\x20reasons:\n\n\x20-\x20Local\x20symbols\x20don't\x20encode\x20the\
//     \x20name.\n\x20-\x20Some\x20languages\x20have\x20case-insensitive\x20nam\
//     es,\x20so\x20the\x20symbol\x20is\x20all-lowercase.\n\x20-\x20The\x20symb\
//     ol\x20may\x20encode\x20names\x20with\x20special\x20characters\x20that\
//     \x20should\x20not\x20be\n\x20\x20\x20displayed\x20to\x20the\x20user.\n\n\
//     \r\n\x05\x04\x07\x02\x04\x05\x12\x04\x9c\x03\x02\x08\n\r\n\x05\x04\x07\
//     \x02\x04\x01\x12\x04\x9c\x03\t\x15\n\r\n\x05\x04\x07\x02\x04\x03\x12\x04\
//     \x9c\x03\x18\x19\n\xc4\x03\n\x04\x04\x07\x02\x05\x12\x04\xa3\x03\x02'\
//     \x1a\xb5\x03\x20(optional)\x20The\x20signature\x20of\x20this\x20symbol\
//     \x20as\x20it's\x20displayed\x20in\x20API\n\x20documentation\x20or\x20in\
//     \x20hover\x20tooltips.\x20For\x20example,\x20a\x20Java\x20method\x20that\
//     \x20adds\n\x20two\x20numbers\x20this\x20would\x20have\x20`Document.langu\
//     age\x20=\x20\"java\"`\x20and\x20`Document.text\n\x20=\x20\"void\x20add(i\
//     nt\x20a,\x20int\x20b)\".\x20The\x20`language`\x20and\x20`text`\x20fields\
//     \x20are\x20required\n\x20while\x20other\x20fields\x20such\x20as\x20`Docu\
//     mentation.occurrences`\x20can\x20be\x20optionally\n\x20included\x20to\
//     \x20support\x20hyperlinking\x20referenced\x20symbols\x20in\x20the\x20sig\
//     nature.\n\n\r\n\x05\x04\x07\x02\x05\x06\x12\x04\xa3\x03\x02\n\n\r\n\x05\
//     \x04\x07\x02\x05\x01\x12\x04\xa3\x03\x0b\"\n\r\n\x05\x04\x07\x02\x05\x03\
//     \x12\x04\xa3\x03%&\n\xc9\x08\n\x04\x04\x07\x02\x06\x12\x04\xb5\x03\x02\
//     \x1e\x1a\xba\x08\x20(optional)\x20The\x20enclosing\x20symbol\x20if\x20th\
//     is\x20is\x20a\x20local\x20symbol.\x20\x20For\x20non-local\n\x20symbols,\
//     \x20the\x20enclosing\x20symbol\x20should\x20be\x20parsed\x20from\x20the\
//     \x20`symbol`\x20field\n\x20using\x20the\x20`Descriptor`\x20grammar.\n\n\
//     \x20The\x20primary\x20use-case\x20for\x20this\x20field\x20is\x20to\x20al\
//     low\x20local\x20symbol\x20to\x20be\x20displayed\n\x20in\x20a\x20symbol\
//     \x20hierarchy\x20for\x20API\x20documentation.\x20It's\x20OK\x20to\x20lea\
//     ve\x20this\x20field\n\x20empty\x20for\x20local\x20variables\x20since\x20\
//     local\x20variables\x20usually\x20don't\x20belong\x20in\x20API\n\x20docum\
//     entation.\x20However,\x20in\x20the\x20situation\x20that\x20you\x20wish\
//     \x20to\x20include\x20a\x20local\n\x20symbol\x20in\x20the\x20hierarchy,\
//     \x20then\x20you\x20can\x20use\x20`enclosing_symbol`\x20to\x20locate\x20t\
//     he\n\x20\"parent\"\x20or\x20\"owner\"\x20of\x20this\x20local\x20symbol.\
//     \x20For\x20example,\x20a\x20Java\x20indexer\x20may\n\x20choose\x20to\x20\
//     use\x20local\x20symbols\x20for\x20private\x20class\x20fields\x20while\
//     \x20providing\x20an\n\x20`enclosing_symbol`\x20to\x20reference\x20the\
//     \x20enclosing\x20class\x20to\x20allow\x20the\x20field\x20to\n\x20be\x20p\
//     art\x20of\x20the\x20class\x20documentation\x20hierarchy.\x20From\x20the\
//     \x20perspective\x20of\x20an\n\x20author\x20of\x20an\x20indexer,\x20the\
//     \x20decision\x20to\x20use\x20a\x20local\x20symbol\x20or\x20global\x20sym\
//     bol\n\x20should\x20exclusively\x20be\x20determined\x20whether\x20the\x20\
//     local\x20symbol\x20is\x20accessible\n\x20outside\x20the\x20document,\x20\
//     not\x20by\x20the\x20capability\x20to\x20find\x20the\x20enclosing\n\x20sy\
//     mbol.\n\n\r\n\x05\x04\x07\x02\x06\x05\x12\x04\xb5\x03\x02\x08\n\r\n\x05\
//     \x04\x07\x02\x06\x01\x12\x04\xb5\x03\t\x19\n\r\n\x05\x04\x07\x02\x06\x03\
//     \x12\x04\xb5\x03\x1c\x1d\n\x0c\n\x02\x04\x08\x12\x06\xb9\x03\0\xf1\x03\
//     \x01\n\x0b\n\x03\x04\x08\x01\x12\x04\xb9\x03\x08\x14\n\x0c\n\x04\x04\x08\
//     \x02\0\x12\x04\xba\x03\x02\x14\n\r\n\x05\x04\x08\x02\0\x05\x12\x04\xba\
//     \x03\x02\x08\n\r\n\x05\x04\x08\x02\0\x01\x12\x04\xba\x03\t\x0f\n\r\n\x05\
//     \x04\x08\x02\0\x03\x12\x04\xba\x03\x12\x13\n\xde\x08\n\x04\x04\x08\x02\
//     \x01\x12\x04\xd3\x03\x02\x18\x1a\xcf\x08\x20When\x20resolving\x20\"Find\
//     \x20references\",\x20this\x20field\x20documents\x20what\x20other\x20symb\
//     ols\n\x20should\x20be\x20included\x20together\x20with\x20this\x20symbol.\
//     \x20For\x20example,\x20consider\x20the\n\x20following\x20TypeScript\x20c\
//     ode\x20that\x20defines\x20two\x20symbols\x20`Animal#sound()`\x20and\n\
//     \x20`Dog#sound()`:\n\x20```ts\n\x20interface\x20Animal\x20{\n\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^^^\x20definition\x20Animal#\n\
//     \x20\x20\x20sound():\x20string\n\x20\x20\x20^^^^^\x20definition\x20Anima\
//     l#sound()\n\x20}\n\x20class\x20Dog\x20implements\x20Animal\x20{\n\x20\
//     \x20\x20\x20\x20\x20\x20^^^\x20definition\x20Dog#,\x20relationships\x20=\
//     \x20[{symbol:\x20\"Animal#\",\x20is_implementation:\x20true}]\n\x20\x20\
//     \x20public\x20sound():\x20string\x20{\x20return\x20\"woof\"\x20}\n\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^^\x20definition\x20Dog#sound(),\
//     \x20references_symbols\x20=\x20Animal#sound(),\x20relationships\x20=\x20\
//     [{symbol:\x20\"Animal#sound()\",\x20is_implementation:true,\x20is_refere\
//     nce:\x20true}]\n\x20}\n\x20const\x20animal:\x20Animal\x20=\x20new\x20Dog\
//     ()\n\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^^^\
//     \x20reference\x20Animal#\n\x20console.log(animal.sound())\n\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^\
//     ^\x20reference\x20Animal#sound()\n\x20```\n\x20Doing\x20\"Find\x20refere\
//     nces\"\x20on\x20the\x20symbol\x20`Animal#sound()`\x20should\x20return\n\
//     \x20references\x20to\x20the\x20`Dog#sound()`\x20method\x20as\x20well.\
//     \x20Vice-versa,\x20doing\x20\"Find\n\x20references\"\x20on\x20the\x20`Do\
//     g#sound()`\x20method\x20should\x20include\x20references\x20to\x20the\n\
//     \x20`Animal#sound()`\x20method\x20as\x20well.\n\n\r\n\x05\x04\x08\x02\
//     \x01\x05\x12\x04\xd3\x03\x02\x06\n\r\n\x05\x04\x08\x02\x01\x01\x12\x04\
//     \xd3\x03\x07\x13\n\r\n\x05\x04\x08\x02\x01\x03\x12\x04\xd3\x03\x16\x17\n\
//     \xee\x03\n\x04\x04\x08\x02\x02\x12\x04\xdc\x03\x02\x1d\x1a\xdf\x03\x20Si\
//     milar\x20to\x20`is_reference`\x20but\x20for\x20\"Find\x20implementations\
//     \".\n\x20It's\x20common\x20for\x20`is_implementation`\x20and\x20`is_refe\
//     rence`\x20to\x20both\x20be\x20true\x20but\n\x20it's\x20not\x20always\x20\
//     the\x20case.\n\x20In\x20the\x20TypeScript\x20example\x20above,\x20observ\
//     e\x20that\x20`Dog#`\x20has\x20an\n\x20`is_implementation`\x20relationshi\
//     p\x20with\x20`\"Animal#\"`\x20but\x20not\x20`is_reference`.\n\x20This\
//     \x20is\x20because\x20\"Find\x20references\"\x20on\x20the\x20\"Animal#\"\
//     \x20symbol\x20should\x20not\x20return\n\x20\"Dog#\".\x20We\x20only\x20wa\
//     nt\x20\"Dog#\"\x20to\x20return\x20as\x20a\x20result\x20for\x20\"Find\n\
//     \x20implementations\"\x20on\x20the\x20\"Animal#\"\x20symbol.\n\n\r\n\x05\
//     \x04\x08\x02\x02\x05\x12\x04\xdc\x03\x02\x06\n\r\n\x05\x04\x08\x02\x02\
//     \x01\x12\x04\xdc\x03\x07\x18\n\r\n\x05\x04\x08\x02\x02\x03\x12\x04\xdc\
//     \x03\x1b\x1c\nP\n\x04\x04\x08\x02\x03\x12\x04\xde\x03\x02\x1e\x1aB\x20Si\
//     milar\x20to\x20`references_symbols`\x20but\x20for\x20\"Go\x20to\x20type\
//     \x20definition\".\n\n\r\n\x05\x04\x08\x02\x03\x05\x12\x04\xde\x03\x02\
//     \x06\n\r\n\x05\x04\x08\x02\x03\x01\x12\x04\xde\x03\x07\x19\n\r\n\x05\x04\
//     \x08\x02\x03\x03\x12\x04\xde\x03\x1c\x1d\n\xa7\x07\n\x04\x04\x08\x02\x04\
//     \x12\x04\xef\x03\x02\x19\x1a\xd5\x06\x20Allows\x20overriding\x20the\x20b\
//     ehavior\x20of\x20\"Go\x20to\x20definition\"\x20and\x20\"Find\x20referenc\
//     es\"\n\x20for\x20symbols\x20which\x20do\x20not\x20have\x20a\x20definitio\
//     n\x20of\x20their\x20own\x20or\x20could\n\x20potentially\x20have\x20multi\
//     ple\x20definitions.\n\n\x20For\x20example,\x20in\x20a\x20language\x20wit\
//     h\x20single\x20inheritance\x20and\x20no\x20field\x20overriding,\n\x20inh\
//     erited\x20fields\x20can\x20reuse\x20the\x20same\x20symbol\x20as\x20the\
//     \x20ancestor\x20which\x20declares\n\x20the\x20field.\x20In\x20such\x20a\
//     \x20situation,\x20is_definition\x20is\x20not\x20needed.\n\n\x20On\x20the\
//     \x20other\x20hand,\x20in\x20languages\x20with\x20single\x20inheritance\
//     \x20and\x20some\x20form\n\x20of\x20mixins,\x20you\x20can\x20use\x20is_de\
//     finition\x20to\x20relate\x20the\x20symbol\x20to\x20the\n\x20matching\x20\
//     symbol\x20in\x20ancestor\x20classes,\x20and\x20is_reference\x20to\x20rel\
//     ate\x20the\n\x20symbol\x20to\x20the\x20matching\x20symbol\x20in\x20mixin\
//     s.\n\n\x20NOTE:\x20At\x20the\x20moment,\x20due\x20to\x20limitations\x20o\
//     f\x20the\x20SCIP\x20to\x20LSIF\x20conversion,\n\x20only\x20global\x20sym\
//     bols\x20in\x20an\x20index\x20are\x20allowed\x20to\x20use\x20is_definitio\
//     n.\n\x20The\x20relationship\x20may\x20not\x20get\x20recorded\x20if\x20ei\
//     ther\x20symbol\x20is\x20local.\n\"A\x20Update\x20registerInverseRelation\
//     ships\x20on\x20adding\x20a\x20new\x20field\x20here.\n\n\r\n\x05\x04\x08\
//     \x02\x04\x05\x12\x04\xef\x03\x02\x06\n\r\n\x05\x04\x08\x02\x04\x01\x12\
//     \x04\xef\x03\x07\x14\n\r\n\x05\x04\x08\x02\x04\x03\x12\x04\xef\x03\x17\
//     \x18\n\x88\x03\n\x02\x05\x03\x12\x06\xf8\x03\0\x8e\x04\x01\x1a\xf9\x02\
//     \x20SymbolRole\x20declares\x20what\x20\"role\"\x20a\x20symbol\x20has\x20\
//     in\x20an\x20occurrence.\x20A\x20role\x20is\n\x20encoded\x20as\x20a\x20bi\
//     tset\x20where\x20each\x20bit\x20represents\x20a\x20different\x20role.\
//     \x20For\x20example,\n\x20to\x20determine\x20if\x20the\x20`Import`\x20rol\
//     e\x20is\x20set,\x20test\x20whether\x20the\x20second\x20bit\x20of\x20the\
//     \n\x20enum\x20value\x20is\x20defined.\x20In\x20pseudocode,\x20this\x20ca\
//     n\x20be\x20implemented\x20with\x20the\n\x20logic:\x20`const\x20isImportR\
//     ole\x20=\x20(role.value\x20&\x20SymbolRole.Import.value)\x20>\x200`.\n\n\
//     \x0b\n\x03\x05\x03\x01\x12\x04\xf8\x03\x05\x0f\nv\n\x04\x05\x03\x02\0\
//     \x12\x04\xfb\x03\x02\x1c\x1ah\x20This\x20case\x20is\x20not\x20meant\x20t\
//     o\x20be\x20used;\x20it\x20only\x20exists\x20to\x20avoid\x20an\x20error\n\
//     \x20from\x20the\x20Protobuf\x20code\x20generator.\n\n\r\n\x05\x05\x03\
//     \x02\0\x01\x12\x04\xfb\x03\x02\x17\n\r\n\x05\x05\x03\x02\0\x02\x12\x04\
//     \xfb\x03\x1a\x1b\nT\n\x04\x05\x03\x02\x01\x12\x04\xfd\x03\x02\x13\x1aF\
//     \x20Is\x20the\x20symbol\x20defined\x20here?\x20If\x20not,\x20then\x20thi\
//     s\x20is\x20a\x20symbol\x20reference.\n\n\r\n\x05\x05\x03\x02\x01\x01\x12\
//     \x04\xfd\x03\x02\x0c\n\r\n\x05\x05\x03\x02\x01\x02\x12\x04\xfd\x03\x0f\
//     \x12\n,\n\x04\x05\x03\x02\x02\x12\x04\xff\x03\x02\x0f\x1a\x1e\x20Is\x20t\
//     he\x20symbol\x20imported\x20here?\n\n\r\n\x05\x05\x03\x02\x02\x01\x12\
//     \x04\xff\x03\x02\x08\n\r\n\x05\x05\x03\x02\x02\x02\x12\x04\xff\x03\x0b\
//     \x0e\n+\n\x04\x05\x03\x02\x03\x12\x04\x81\x04\x02\x14\x1a\x1d\x20Is\x20t\
//     he\x20symbol\x20written\x20here?\n\n\r\n\x05\x05\x03\x02\x03\x01\x12\x04\
//     \x81\x04\x02\r\n\r\n\x05\x05\x03\x02\x03\x02\x12\x04\x81\x04\x10\x13\n(\
//     \n\x04\x05\x03\x02\x04\x12\x04\x83\x04\x02\x13\x1a\x1a\x20Is\x20the\x20s\
//     ymbol\x20read\x20here?\n\n\r\n\x05\x05\x03\x02\x04\x01\x12\x04\x83\x04\
//     \x02\x0c\n\r\n\x05\x05\x03\x02\x04\x02\x12\x04\x83\x04\x0f\x12\n0\n\x04\
//     \x05\x03\x02\x05\x12\x04\x85\x04\x02\x13\x1a\"\x20Is\x20the\x20symbol\
//     \x20in\x20generated\x20code?\n\n\r\n\x05\x05\x03\x02\x05\x01\x12\x04\x85\
//     \x04\x02\x0b\n\r\n\x05\x05\x03\x02\x05\x02\x12\x04\x85\x04\x0e\x12\n+\n\
//     \x04\x05\x03\x02\x06\x12\x04\x87\x04\x02\x0e\x1a\x1d\x20Is\x20the\x20sym\
//     bol\x20in\x20test\x20code?\n\n\r\n\x05\x05\x03\x02\x06\x01\x12\x04\x87\
//     \x04\x02\x06\n\r\n\x05\x05\x03\x02\x06\x02\x12\x04\x87\x04\t\r\n\xed\x01\
//     \n\x04\x05\x03\x02\x07\x12\x04\x8d\x04\x02\x1b\x1a\xde\x01\x20Is\x20this\
//     \x20a\x20signature\x20for\x20a\x20symbol\x20that\x20is\x20defined\x20els\
//     ewhere?\n\n\x20Applies\x20to\x20forward\x20declarations\x20for\x20langua\
//     ges\x20like\x20C,\x20C++\n\x20and\x20Objective-C,\x20as\x20well\x20as\
//     \x20`val`\x20declarations\x20in\x20interface\n\x20files\x20in\x20languag\
//     es\x20like\x20SML\x20and\x20OCaml.\n\n\r\n\x05\x05\x03\x02\x07\x01\x12\
//     \x04\x8d\x04\x02\x13\n\r\n\x05\x05\x03\x02\x07\x02\x12\x04\x8d\x04\x16\
//     \x1a\n\x0c\n\x02\x05\x04\x12\x06\x90\x04\0\xed\x04\x01\n\x0b\n\x03\x05\
//     \x04\x01\x12\x04\x90\x04\x05\x0f\n\x0b\n\x03\x05\x04\x03\x12\x04\x91\x04\
//     \x02\x1c\n\x0c\n\x04\x05\x04\x03\x02\x12\x04\x91\x04\x02\x1c\n\x0c\n\x04\
//     \x05\x04\x02\0\x12\x04\x93\x04\x02\x1c\n\r\n\x05\x05\x04\x02\0\x01\x12\
//     \x04\x93\x04\x02\x17\n\r\n\x05\x05\x04\x02\0\x02\x12\x04\x93\x04\x1a\x1b\
//     \n;\n\x04\x05\x04\x02\x01\x12\x04\x96\x04\x02\x0e\x1a-\x20Comment,\x20in\
//     cluding\x20comment\x20markers\x20and\x20text\n\n\r\n\x05\x05\x04\x02\x01\
//     \x01\x12\x04\x96\x04\x02\t\n\r\n\x05\x05\x04\x02\x01\x02\x12\x04\x96\x04\
//     \x0c\r\n\x1b\n\x04\x05\x04\x02\x02\x12\x04\x99\x04\x02\x1b\x1a\r\x20`;`\
//     \x20`.`\x20`,`\n\n\r\n\x05\x05\x04\x02\x02\x01\x12\x04\x99\x04\x02\x16\n\
//     \r\n\x05\x05\x04\x02\x02\x02\x12\x04\x99\x04\x19\x1a\n2\n\x04\x05\x04\
//     \x02\x03\x12\x04\x9b\x04\x02\x19\x1a$\x20(),\x20{},\x20[]\x20when\x20use\
//     d\x20syntactically\n\n\r\n\x05\x05\x04\x02\x03\x01\x12\x04\x9b\x04\x02\
//     \x14\n\r\n\x05\x05\x04\x02\x03\x02\x12\x04\x9b\x04\x17\x18\n5\n\x04\x05\
//     \x04\x02\x04\x12\x04\x9e\x04\x02\x0e\x1a'\x20`if`,\x20`else`,\x20`return\
//     `,\x20`class`,\x20etc.\n\n\r\n\x05\x05\x04\x02\x04\x01\x12\x04\x9e\x04\
//     \x02\t\n\r\n\x05\x05\x04\x02\x04\x02\x12\x04\x9e\x04\x0c\r\n\x0c\n\x04\
//     \x05\x04\x02\x05\x12\x04\x9f\x04\x02*\n\r\n\x05\x05\x04\x02\x05\x01\x12\
//     \x04\x9f\x04\x02\x13\n\r\n\x05\x05\x04\x02\x05\x02\x12\x04\x9f\x04\x16\
//     \x17\n\r\n\x05\x05\x04\x02\x05\x03\x12\x04\x9f\x04\x18)\n\x0e\n\x06\x05\
//     \x04\x02\x05\x03\x01\x12\x04\x9f\x04\x19(\n\x1e\n\x04\x05\x04\x02\x06\
//     \x12\x04\xa2\x04\x02\x19\x1a\x10\x20`+`,\x20`*`,\x20etc.\n\n\r\n\x05\x05\
//     \x04\x02\x06\x01\x12\x04\xa2\x04\x02\x14\n\r\n\x05\x05\x04\x02\x06\x02\
//     \x12\x04\xa2\x04\x17\x18\nX\n\x04\x05\x04\x02\x07\x12\x04\xa5\x04\x02\
//     \x11\x1aJ\x20non-specific\x20catch-all\x20for\x20any\x20identifier\x20no\
//     t\x20better\x20described\x20elsewhere\n\n\r\n\x05\x05\x04\x02\x07\x01\
//     \x12\x04\xa5\x04\x02\x0c\n\r\n\x05\x05\x04\x02\x07\x02\x12\x04\xa5\x04\
//     \x0f\x10\nN\n\x04\x05\x04\x02\x08\x12\x04\xa7\x04\x02\x18\x1a@\x20Identi\
//     fiers\x20builtin\x20to\x20the\x20language:\x20`min`,\x20`print`\x20in\
//     \x20Python.\n\n\r\n\x05\x05\x04\x02\x08\x01\x12\x04\xa7\x04\x02\x13\n\r\
//     \n\x05\x05\x04\x02\x08\x02\x12\x04\xa7\x04\x16\x17\n[\n\x04\x05\x04\x02\
//     \t\x12\x04\xa9\x04\x02\x15\x1aM\x20Identifiers\x20representing\x20`null`\
//     -like\x20values:\x20`None`\x20in\x20Python,\x20`nil`\x20in\x20Go.\n\n\r\
//     \n\x05\x05\x04\x02\t\x01\x12\x04\xa9\x04\x02\x10\n\r\n\x05\x05\x04\x02\t\
//     \x02\x12\x04\xa9\x04\x13\x14\n.\n\x04\x05\x04\x02\n\x12\x04\xab\x04\x02\
//     \x19\x1a\x20\x20`xyz`\x20in\x20`const\x20xyz\x20=\x20\"hello\"`\n\n\r\n\
//     \x05\x05\x04\x02\n\x01\x12\x04\xab\x04\x02\x14\n\r\n\x05\x05\x04\x02\n\
//     \x02\x12\x04\xab\x04\x17\x18\n'\n\x04\x05\x04\x02\x0b\x12\x04\xad\x04\
//     \x02\x1f\x1a\x19\x20`var\x20X\x20=\x20\"hello\"`\x20in\x20Go\n\n\r\n\x05\
//     \x05\x04\x02\x0b\x01\x12\x04\xad\x04\x02\x19\n\r\n\x05\x05\x04\x02\x0b\
//     \x02\x12\x04\xad\x04\x1c\x1e\n3\n\x04\x05\x04\x02\x0c\x12\x04\xaf\x04\
//     \x02\x1b\x1a%\x20Parameter\x20definition\x20and\x20references\n\n\r\n\
//     \x05\x05\x04\x02\x0c\x01\x12\x04\xaf\x04\x02\x15\n\r\n\x05\x05\x04\x02\
//     \x0c\x02\x12\x04\xaf\x04\x18\x1a\nX\n\x04\x05\x04\x02\r\x12\x04\xb1\x04\
//     \x02\x17\x1aJ\x20Identifiers\x20for\x20variable\x20definitions\x20and\
//     \x20references\x20within\x20a\x20local\x20scope\n\n\r\n\x05\x05\x04\x02\
//     \r\x01\x12\x04\xb1\x04\x02\x11\n\r\n\x05\x05\x04\x02\r\x02\x12\x04\xb1\
//     \x04\x14\x16\nK\n\x04\x05\x04\x02\x0e\x12\x04\xb3\x04\x02\x1a\x1a=\x20Id\
//     entifiers\x20that\x20shadow\x20other\x20identifiers\x20in\x20an\x20outer\
//     \x20scope\n\n\r\n\x05\x05\x04\x02\x0e\x01\x12\x04\xb3\x04\x02\x14\n\r\n\
//     \x05\x05\x04\x02\x0e\x02\x12\x04\xb3\x04\x17\x19\n\xcd\x01\n\x04\x05\x04\
//     \x02\x0f\x12\x04\xb8\x04\x02\x1b\x1a\xbe\x01\x20Identifier\x20representi\
//     ng\x20a\x20unit\x20of\x20code\x20abstraction\x20and/or\x20namespacing.\n\
//     \n\x20NOTE:\x20This\x20corresponds\x20to\x20a\x20package\x20in\x20Go\x20\
//     and\x20JVM\x20languages,\n\x20and\x20a\x20module\x20in\x20languages\x20l\
//     ike\x20Python\x20and\x20JavaScript.\n\n\r\n\x05\x05\x04\x02\x0f\x01\x12\
//     \x04\xb8\x04\x02\x15\n\r\n\x05\x05\x04\x02\x0f\x02\x12\x04\xb8\x04\x18\
//     \x1a\n\x0c\n\x04\x05\x04\x02\x10\x12\x04\xb9\x04\x02*\n\r\n\x05\x05\x04\
//     \x02\x10\x01\x12\x04\xb9\x04\x02\x12\n\r\n\x05\x05\x04\x02\x10\x02\x12\
//     \x04\xb9\x04\x15\x17\n\r\n\x05\x05\x04\x02\x10\x03\x12\x04\xb9\x04\x18)\
//     \n\x0e\n\x06\x05\x04\x02\x10\x03\x01\x12\x04\xb9\x04\x19(\n4\n\x04\x05\
//     \x04\x02\x11\x12\x04\xbc\x04\x02\x1a\x1a&\x20Function\x20references,\x20\
//     including\x20calls\n\n\r\n\x05\x05\x04\x02\x11\x01\x12\x04\xbc\x04\x02\
//     \x14\n\r\n\x05\x05\x04\x02\x11\x02\x12\x04\xbc\x04\x17\x19\n(\n\x04\x05\
//     \x04\x02\x12\x12\x04\xbe\x04\x02$\x1a\x1a\x20Function\x20definition\x20o\
//     nly\n\n\r\n\x05\x05\x04\x02\x12\x01\x12\x04\xbe\x04\x02\x1e\n\r\n\x05\
//     \x05\x04\x02\x12\x02\x12\x04\xbe\x04!#\n7\n\x04\x05\x04\x02\x13\x12\x04\
//     \xc1\x04\x02\x17\x1a)\x20Macro\x20references,\x20including\x20invocation\
//     s\n\n\r\n\x05\x05\x04\x02\x13\x01\x12\x04\xc1\x04\x02\x11\n\r\n\x05\x05\
//     \x04\x02\x13\x02\x12\x04\xc1\x04\x14\x16\n%\n\x04\x05\x04\x02\x14\x12\
//     \x04\xc3\x04\x02!\x1a\x17\x20Macro\x20definition\x20only\n\n\r\n\x05\x05\
//     \x04\x02\x14\x01\x12\x04\xc3\x04\x02\x1b\n\r\n\x05\x05\x04\x02\x14\x02\
//     \x12\x04\xc3\x04\x1e\x20\n!\n\x04\x05\x04\x02\x15\x12\x04\xc6\x04\x02\
//     \x16\x1a\x13\x20non-builtin\x20types\n\n\r\n\x05\x05\x04\x02\x15\x01\x12\
//     \x04\xc6\x04\x02\x10\n\r\n\x05\x05\x04\x02\x15\x02\x12\x04\xc6\x04\x13\
//     \x15\nK\n\x04\x05\x04\x02\x16\x12\x04\xc8\x04\x02\x1d\x1a=\x20builtin\
//     \x20types\x20only,\x20such\x20as\x20`str`\x20for\x20Python\x20or\x20`int\
//     `\x20in\x20Go\n\n\r\n\x05\x05\x04\x02\x16\x01\x12\x04\xc8\x04\x02\x17\n\
//     \r\n\x05\x05\x04\x02\x16\x02\x12\x04\xc8\x04\x1a\x1c\n7\n\x04\x05\x04\
//     \x02\x17\x12\x04\xcb\x04\x02\x1b\x1a)\x20Python\x20decorators,\x20c-like\
//     \x20__attribute__\n\n\r\n\x05\x05\x04\x02\x17\x01\x12\x04\xcb\x04\x02\
//     \x15\n\r\n\x05\x05\x04\x02\x17\x02\x12\x04\xcb\x04\x18\x1a\n\x14\n\x04\
//     \x05\x04\x02\x18\x12\x04\xce\x04\x02\x13\x1a\x06\x20`\\b`\n\n\r\n\x05\
//     \x05\x04\x02\x18\x01\x12\x04\xce\x04\x02\r\n\r\n\x05\x05\x04\x02\x18\x02\
//     \x12\x04\xce\x04\x10\x12\n\x18\n\x04\x05\x04\x02\x19\x12\x04\xd0\x04\x02\
//     \x15\x1a\n\x20`*`,\x20`+`\n\n\r\n\x05\x05\x04\x02\x19\x01\x12\x04\xd0\
//     \x04\x02\x0f\n\r\n\x05\x05\x04\x02\x19\x02\x12\x04\xd0\x04\x12\x14\n\x13\
//     \n\x04\x05\x04\x02\x1a\x12\x04\xd2\x04\x02\x15\x1a\x05\x20`.`\n\n\r\n\
//     \x05\x05\x04\x02\x1a\x01\x12\x04\xd2\x04\x02\x0f\n\r\n\x05\x05\x04\x02\
//     \x1a\x02\x12\x04\xd2\x04\x12\x14\n\"\n\x04\x05\x04\x02\x1b\x12\x04\xd4\
//     \x04\x02\x16\x1a\x14\x20`(`,\x20`)`,\x20`[`,\x20`]`\n\n\r\n\x05\x05\x04\
//     \x02\x1b\x01\x12\x04\xd4\x04\x02\x10\n\r\n\x05\x05\x04\x02\x1b\x02\x12\
//     \x04\xd4\x04\x13\x15\n\x18\n\x04\x05\x04\x02\x1c\x12\x04\xd6\x04\x02\x11\
//     \x1a\n\x20`|`,\x20`-`\n\n\r\n\x05\x05\x04\x02\x1c\x01\x12\x04\xd6\x04\
//     \x02\x0b\n\r\n\x05\x05\x04\x02\x1c\x02\x12\x04\xd6\x04\x0e\x10\n0\n\x04\
//     \x05\x04\x02\x1d\x12\x04\xd9\x04\x02\x15\x1a\"\x20Literal\x20strings:\
//     \x20\"Hello,\x20world!\"\n\n\r\n\x05\x05\x04\x02\x1d\x01\x12\x04\xd9\x04\
//     \x02\x0f\n\r\n\x05\x05\x04\x02\x1d\x02\x12\x04\xd9\x04\x12\x14\n-\n\x04\
//     \x05\x04\x02\x1e\x12\x04\xdb\x04\x02\x1b\x1a\x1f\x20non-regex\x20escapes\
//     :\x20\"\\t\",\x20\"\\n\"\n\n\r\n\x05\x05\x04\x02\x1e\x01\x12\x04\xdb\x04\
//     \x02\x15\n\r\n\x05\x05\x04\x02\x1e\x02\x12\x04\xdb\x04\x18\x1a\n_\n\x04\
//     \x05\x04\x02\x1f\x12\x04\xdd\x04\x02\x1c\x1aQ\x20datetimes\x20within\x20\
//     strings,\x20special\x20words\x20within\x20a\x20string,\x20`{}`\x20in\x20\
//     format\x20strings\n\n\r\n\x05\x05\x04\x02\x1f\x01\x12\x04\xdd\x04\x02\
//     \x16\n\r\n\x05\x05\x04\x02\x1f\x02\x12\x04\xdd\x04\x19\x1b\nG\n\x04\x05\
//     \x04\x02\x20\x12\x04\xdf\x04\x02\x18\x1a9\x20\"key\"\x20in\x20{\x20\"key\
//     \":\x20\"value\"\x20},\x20useful\x20for\x20example\x20in\x20JSON\n\n\r\n\
//     \x05\x05\x04\x02\x20\x01\x12\x04\xdf\x04\x02\x12\n\r\n\x05\x05\x04\x02\
//     \x20\x02\x12\x04\xdf\x04\x15\x17\nV\n\x04\x05\x04\x02!\x12\x04\xe1\x04\
//     \x02\x18\x1aH\x20'c'\x20or\x20similar,\x20in\x20languages\x20that\x20dif\
//     ferentiate\x20strings\x20and\x20characters\n\n\r\n\x05\x05\x04\x02!\x01\
//     \x12\x04\xe1\x04\x02\x12\n\r\n\x05\x05\x04\x02!\x02\x12\x04\xe1\x04\x15\
//     \x17\n9\n\x04\x05\x04\x02\"\x12\x04\xe3\x04\x02\x16\x1a+\x20Literal\x20n\
//     umbers,\x20both\x20floats\x20and\x20integers\n\n\r\n\x05\x05\x04\x02\"\
//     \x01\x12\x04\xe3\x04\x02\x10\n\r\n\x05\x05\x04\x02\"\x02\x12\x04\xe3\x04\
//     \x13\x15\n\x1f\n\x04\x05\x04\x02#\x12\x04\xe5\x04\x02\x16\x1a\x11\x20`tr\
//     ue`,\x20`false`\n\n\r\n\x05\x05\x04\x02#\x01\x12\x04\xe5\x04\x02\x10\n\r\
//     \n\x05\x05\x04\x02#\x02\x12\x04\xe5\x04\x13\x15\n&\n\x04\x05\x04\x02$\
//     \x12\x04\xe8\x04\x02\x0b\x1a\x18\x20Used\x20for\x20XML-like\x20tags\n\n\
//     \r\n\x05\x05\x04\x02$\x01\x12\x04\xe8\x04\x02\x05\n\r\n\x05\x05\x04\x02$\
//     \x02\x12\x04\xe8\x04\x08\n\n/\n\x04\x05\x04\x02%\x12\x04\xea\x04\x02\x14\
//     \x1a!\x20Attribute\x20name\x20in\x20XML-like\x20tags\n\n\r\n\x05\x05\x04\
//     \x02%\x01\x12\x04\xea\x04\x02\x0e\n\r\n\x05\x05\x04\x02%\x02\x12\x04\xea\
//     \x04\x11\x13\n,\n\x04\x05\x04\x02&\x12\x04\xec\x04\x02\x14\x1a\x1e\x20De\
//     limiters\x20for\x20XML-like\x20tags\n\n\r\n\x05\x05\x04\x02&\x01\x12\x04\
//     \xec\x04\x02\x0e\n\r\n\x05\x05\x04\x02&\x02\x12\x04\xec\x04\x11\x13\n\
//     \xf9\x01\n\x02\x04\t\x12\x06\xf4\x04\0\xd5\x05\x01\x1a\xea\x01\x20Occurr\
//     ence\x20associates\x20a\x20source\x20position\x20with\x20a\x20symbol\x20\
//     and/or\x20highlighting\n\x20information.\n\n\x20If\x20possible,\x20index\
//     ers\x20should\x20try\x20to\x20bundle\x20logically\x20related\x20informat\
//     ion\n\x20across\x20occurrences\x20into\x20a\x20single\x20occurrence\x20t\
//     o\x20reduce\x20payload\x20sizes.\n\n\x0b\n\x03\x04\t\x01\x12\x04\xf4\x04\
//     \x08\x12\n\xe8\x08\n\x04\x04\t\x02\0\x12\x04\x8c\x05\x02\x1b\x1a\xd9\x08\
//     \x20Half-open\x20[start,\x20end)\x20range\x20of\x20this\x20occurrence.\
//     \x20Must\x20be\x20exactly\x20three\x20or\x20four\n\x20elements:\n\n\x20-\
//     \x20Four\x20elements:\x20`[startLine,\x20startCharacter,\x20endLine,\x20\
//     endCharacter]`\n\x20-\x20Three\x20elements:\x20`[startLine,\x20startChar\
//     acter,\x20endCharacter]`.\x20The\x20end\x20line\n\x20\x20\x20is\x20infer\
//     red\x20to\x20have\x20the\x20same\x20value\x20as\x20the\x20start\x20line.\
//     \n\n\x20It\x20is\x20allowed\x20for\x20the\x20range\x20to\x20be\x20empty\
//     \x20(i.e.\x20start==end).\n\n\x20Line\x20numbers\x20and\x20characters\
//     \x20are\x20always\x200-based.\x20Make\x20sure\x20to\x20increment\x20the\
//     \n\x20line/character\x20values\x20before\x20displaying\x20them\x20in\x20\
//     an\x20editor-like\x20UI\x20because\n\x20editors\x20conventionally\x20use\
//     \x201-based\x20numbers.\n\n\x20The\x20'character'\x20value\x20is\x20inte\
//     rpreted\x20based\x20on\x20the\x20PositionEncoding\x20for\n\x20the\x20Doc\
//     ument.\n\n\x20Historical\x20note:\x20the\x20original\x20draft\x20of\x20t\
//     his\x20schema\x20had\x20a\x20`Range`\x20message\n\x20type\x20with\x20`st\
//     art`\x20and\x20`end`\x20fields\x20of\x20type\x20`Position`,\x20mirroring\
//     \x20LSP.\n\x20Benchmarks\x20revealed\x20that\x20this\x20encoding\x20was\
//     \x20inefficient\x20and\x20that\x20we\x20could\n\x20reduce\x20the\x20tota\
//     l\x20payload\x20size\x20of\x20an\x20index\x20by\x2050%\x20by\x20using\
//     \x20`repeated\x20int32`\n\x20instead.\x20The\x20`repeated\x20int32`\x20e\
//     ncoding\x20is\x20admittedly\x20more\x20embarrassing\x20to\n\x20work\x20w\
//     ith\x20in\x20some\x20programming\x20languages\x20but\x20we\x20hope\x20th\
//     e\x20performance\n\x20improvements\x20make\x20up\x20for\x20it.\n\n\r\n\
//     \x05\x04\t\x02\0\x04\x12\x04\x8c\x05\x02\n\n\r\n\x05\x04\t\x02\0\x05\x12\
//     \x04\x8c\x05\x0b\x10\n\r\n\x05\x04\t\x02\0\x01\x12\x04\x8c\x05\x11\x16\n\
//     \r\n\x05\x04\t\x02\0\x03\x12\x04\x8c\x05\x19\x1a\n\x8a\x01\n\x04\x04\t\
//     \x02\x01\x12\x04\x8f\x05\x02\x14\x1a|\x20(optional)\x20The\x20symbol\x20\
//     that\x20appears\x20at\x20this\x20position.\x20See\n\x20`SymbolInformatio\
//     n.symbol`\x20for\x20how\x20to\x20format\x20symbols\x20as\x20strings.\n\n\
//     \r\n\x05\x04\t\x02\x01\x05\x12\x04\x8f\x05\x02\x08\n\r\n\x05\x04\t\x02\
//     \x01\x01\x12\x04\x8f\x05\t\x0f\n\r\n\x05\x04\t\x02\x01\x03\x12\x04\x8f\
//     \x05\x12\x13\n\x97\x01\n\x04\x04\t\x02\x02\x12\x04\x92\x05\x02\x19\x1a\
//     \x88\x01\x20(optional)\x20Bitset\x20containing\x20`SymbolRole`s\x20in\
//     \x20this\x20occurrence.\n\x20See\x20`SymbolRole`'s\x20documentation\x20f\
//     or\x20how\x20to\x20read\x20and\x20write\x20this\x20field.\n\n\r\n\x05\
//     \x04\t\x02\x02\x05\x12\x04\x92\x05\x02\x07\n\r\n\x05\x04\t\x02\x02\x01\
//     \x12\x04\x92\x05\x08\x14\n\r\n\x05\x04\t\x02\x02\x03\x12\x04\x92\x05\x17\
//     \x18\n\xf1\x03\n\x04\x04\t\x02\x03\x12\x04\x9b\x05\x02-\x1a\xe2\x03\x20(\
//     optional)\x20CommonMark-formatted\x20documentation\x20for\x20this\x20spe\
//     cific\x20range.\x20If\n\x20empty,\x20the\x20`Symbol.documentation`\x20fi\
//     eld\x20is\x20used\x20instead.\x20One\x20example\n\x20where\x20this\x20fi\
//     eld\x20might\x20be\x20useful\x20is\x20when\x20the\x20symbol\x20represent\
//     s\x20a\x20generic\n\x20function\x20(with\x20abstract\x20type\x20paramete\
//     rs\x20such\x20as\x20`List<T>`)\x20and\x20at\x20this\n\x20occurrence\x20w\
//     e\x20know\x20the\x20exact\x20values\x20(such\x20as\x20`List<String>`).\n\
//     \n\x20This\x20field\x20can\x20also\x20be\x20used\x20for\x20dynamically\
//     \x20or\x20gradually\x20typed\x20languages,\n\x20which\x20commonly\x20all\
//     ow\x20for\x20type-changing\x20assignment.\n\n\r\n\x05\x04\t\x02\x03\x04\
//     \x12\x04\x9b\x05\x02\n\n\r\n\x05\x04\t\x02\x03\x05\x12\x04\x9b\x05\x0b\
//     \x11\n\r\n\x05\x04\t\x02\x03\x01\x12\x04\x9b\x05\x12(\n\r\n\x05\x04\t\
//     \x02\x03\x03\x12\x04\x9b\x05+,\nX\n\x04\x04\t\x02\x04\x12\x04\x9d\x05\
//     \x02\x1d\x1aJ\x20(optional)\x20What\x20syntax\x20highlighting\x20class\
//     \x20should\x20be\x20used\x20for\x20this\x20range?\n\n\r\n\x05\x04\t\x02\
//     \x04\x06\x12\x04\x9d\x05\x02\x0c\n\r\n\x05\x04\t\x02\x04\x01\x12\x04\x9d\
//     \x05\r\x18\n\r\n\x05\x04\t\x02\x04\x03\x12\x04\x9d\x05\x1b\x1c\nW\n\x04\
//     \x04\t\x02\x05\x12\x04\x9f\x05\x02&\x1aI\x20(optional)\x20Diagnostics\
//     \x20that\x20have\x20been\x20reported\x20for\x20this\x20specific\x20range\
//     .\n\n\r\n\x05\x04\t\x02\x05\x04\x12\x04\x9f\x05\x02\n\n\r\n\x05\x04\t\
//     \x02\x05\x06\x12\x04\x9f\x05\x0b\x15\n\r\n\x05\x04\t\x02\x05\x01\x12\x04\
//     \x9f\x05\x16!\n\r\n\x05\x04\t\x02\x05\x03\x12\x04\x9f\x05$%\n\xb7\x0e\n\
//     \x04\x04\t\x02\x06\x12\x04\xd4\x05\x02%\x1a\xa8\x0e\x20(optional)\x20Usi\
//     ng\x20the\x20same\x20encoding\x20as\x20the\x20sibling\x20`range`\x20fiel\
//     d,\x20half-open\n\x20source\x20range\x20of\x20the\x20nearest\x20non-triv\
//     ial\x20enclosing\x20AST\x20node.\x20This\x20range\x20must\n\x20enclose\
//     \x20the\x20`range`\x20field.\x20Example\x20applications\x20that\x20make\
//     \x20use\x20of\x20the\n\x20enclosing_range\x20field:\n\n\x20-\x20Call\x20\
//     hierarchies:\x20to\x20determine\x20what\x20symbols\x20are\x20references\
//     \x20from\x20the\x20body\n\x20\x20\x20of\x20a\x20function\n\x20-\x20Symbo\
//     l\x20outline:\x20to\x20display\x20breadcrumbs\x20from\x20the\x20cursor\
//     \x20position\x20to\x20the\n\x20\x20\x20root\x20of\x20the\x20file\n\x20-\
//     \x20Expand\x20selection:\x20to\x20select\x20the\x20nearest\x20enclosing\
//     \x20AST\x20node.\n\x20-\x20Highlight\x20range:\x20to\x20indicate\x20the\
//     \x20AST\x20expression\x20that\x20is\x20associated\x20with\x20a\n\x20\x20\
//     \x20hover\x20popover\n\n\x20For\x20definition\x20occurrences,\x20the\x20\
//     enclosing\x20range\x20should\x20indicate\x20the\n\x20start/end\x20bounds\
//     \x20of\x20the\x20entire\x20definition\x20AST\x20node,\x20including\n\x20\
//     documentation.\n\x20```\n\x20const\x20n\x20=\x203\n\x20\x20\x20\x20\x20\
//     \x20\x20^\x20range\n\x20^^^^^^^^^^^\x20enclosing_range\n\n\x20/**\x20Par\
//     ses\x20the\x20string\x20into\x20something\x20*/\n\x20^\x20enclosing_rang\
//     e\x20start\x20--------------------------------------|\n\x20function\x20p\
//     arse(input\x20string):\x20string\x20{\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20|\n\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^^\x20range\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20|\n\x20\x20\x20\x20\x20return\x20input.slice(n)\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20|\n\x20}\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20|\n\x20^\x20enclosing_range\x20end\x20<-------------\
//     --------------------------|\n\x20```\n\n\x20Any\x20attributes/decorators\
//     /attached\x20macros\x20should\x20also\x20be\x20part\x20of\x20the\n\x20en\
//     closing\x20range.\n\n\x20```python\n\x20@cache\n\x20^\x20enclosing_range\
//     \x20start---------------------|\n\x20def\x20factorial(n):\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20|\n\x20\x20\x20\x20\x20return\x20n\x20*\x20facto\
//     rial(n-1)\x20if\x20n\x20else\x201\x20\x20\x20|\n\x20<\x20enclosing_range\
//     \x20end-----------------------|\n\n\x20```\n\n\x20For\x20reference\x20oc\
//     currences,\x20the\x20enclosing\x20range\x20should\x20indicate\x20the\x20\
//     start/end\n\x20bounds\x20of\x20the\x20parent\x20expression.\n\x20```\n\
//     \x20const\x20a\x20=\x20a.b\n\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
//     \x20\x20^\x20range\n\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20^^^\x20e\
//     nclosing_range\n\x20const\x20b\x20=\x20a.b(41).f(42).g(43)\n\x20\x20\x20\
//     \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20^\x20ran\
//     ge\n\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20^^^^^^^^^^^^^\x20enclosi\
//     ng_range\n\x20```\n\n\r\n\x05\x04\t\x02\x06\x04\x12\x04\xd4\x05\x02\n\n\
//     \r\n\x05\x04\t\x02\x06\x05\x12\x04\xd4\x05\x0b\x10\n\r\n\x05\x04\t\x02\
//     \x06\x01\x12\x04\xd4\x05\x11\x20\n\r\n\x05\x04\t\x02\x06\x03\x12\x04\xd4\
//     \x05#$\nw\n\x02\x04\n\x12\x06\xd9\x05\0\xe4\x05\x01\x1ai\x20Represents\
//     \x20a\x20diagnostic,\x20such\x20as\x20a\x20compiler\x20error\x20or\x20wa\
//     rning,\x20which\x20should\x20be\n\x20reported\x20for\x20a\x20document.\n\
//     \n\x0b\n\x03\x04\n\x01\x12\x04\xd9\x05\x08\x12\nW\n\x04\x04\n\x02\0\x12\
//     \x04\xdb\x05\x02\x18\x1aI\x20Should\x20this\x20diagnostic\x20be\x20repor\
//     ted\x20as\x20an\x20error,\x20warning,\x20info,\x20or\x20hint?\n\n\r\n\
//     \x05\x04\n\x02\0\x06\x12\x04\xdb\x05\x02\n\n\r\n\x05\x04\n\x02\0\x01\x12\
//     \x04\xdb\x05\x0b\x13\n\r\n\x05\x04\n\x02\0\x03\x12\x04\xdb\x05\x16\x17\n\
//     ]\n\x04\x04\n\x02\x01\x12\x04\xdd\x05\x02\x12\x1aO\x20(optional)\x20Code\
//     \x20of\x20this\x20diagnostic,\x20which\x20might\x20appear\x20in\x20the\
//     \x20user\x20interface.\n\n\r\n\x05\x04\n\x02\x01\x05\x12\x04\xdd\x05\x02\
//     \x08\n\r\n\x05\x04\n\x02\x01\x01\x12\x04\xdd\x05\t\r\n\r\n\x05\x04\n\x02\
//     \x01\x03\x12\x04\xdd\x05\x10\x11\n+\n\x04\x04\n\x02\x02\x12\x04\xdf\x05\
//     \x02\x15\x1a\x1d\x20Message\x20of\x20this\x20diagnostic.\n\n\r\n\x05\x04\
//     \n\x02\x02\x05\x12\x04\xdf\x05\x02\x08\n\r\n\x05\x04\n\x02\x02\x01\x12\
//     \x04\xdf\x05\t\x10\n\r\n\x05\x04\n\x02\x02\x03\x12\x04\xdf\x05\x13\x14\n\
//     ~\n\x04\x04\n\x02\x03\x12\x04\xe2\x05\x02\x14\x1ap\x20(optional)\x20Huma\
//     n-readable\x20string\x20describing\x20the\x20source\x20of\x20this\x20dia\
//     gnostic,\x20e.g.\n\x20'typescript'\x20or\x20'super\x20lint'.\n\n\r\n\x05\
//     \x04\n\x02\x03\x05\x12\x04\xe2\x05\x02\x08\n\r\n\x05\x04\n\x02\x03\x01\
//     \x12\x04\xe2\x05\t\x0f\n\r\n\x05\x04\n\x02\x03\x03\x12\x04\xe2\x05\x12\
//     \x13\n\x0c\n\x04\x04\n\x02\x04\x12\x04\xe3\x05\x02\"\n\r\n\x05\x04\n\x02\
//     \x04\x04\x12\x04\xe3\x05\x02\n\n\r\n\x05\x04\n\x02\x04\x06\x12\x04\xe3\
//     \x05\x0b\x18\n\r\n\x05\x04\n\x02\x04\x01\x12\x04\xe3\x05\x19\x1d\n\r\n\
//     \x05\x04\n\x02\x04\x03\x12\x04\xe3\x05\x20!\n\x0c\n\x02\x05\x05\x12\x06\
//     \xe6\x05\0\xec\x05\x01\n\x0b\n\x03\x05\x05\x01\x12\x04\xe6\x05\x05\r\n\
//     \x0c\n\x04\x05\x05\x02\0\x12\x04\xe7\x05\x02\x1a\n\r\n\x05\x05\x05\x02\0\
//     \x01\x12\x04\xe7\x05\x02\x15\n\r\n\x05\x05\x05\x02\0\x02\x12\x04\xe7\x05\
//     \x18\x19\n\x0c\n\x04\x05\x05\x02\x01\x12\x04\xe8\x05\x02\x0c\n\r\n\x05\
//     \x05\x05\x02\x01\x01\x12\x04\xe8\x05\x02\x07\n\r\n\x05\x05\x05\x02\x01\
//     \x02\x12\x04\xe8\x05\n\x0b\n\x0c\n\x04\x05\x05\x02\x02\x12\x04\xe9\x05\
//     \x02\x0e\n\r\n\x05\x05\x05\x02\x02\x01\x12\x04\xe9\x05\x02\t\n\r\n\x05\
//     \x05\x05\x02\x02\x02\x12\x04\xe9\x05\x0c\r\n\x0c\n\x04\x05\x05\x02\x03\
//     \x12\x04\xea\x05\x02\x12\n\r\n\x05\x05\x05\x02\x03\x01\x12\x04\xea\x05\
//     \x02\r\n\r\n\x05\x05\x05\x02\x03\x02\x12\x04\xea\x05\x10\x11\n\x0c\n\x04\
//     \x05\x05\x02\x04\x12\x04\xeb\x05\x02\x0b\n\r\n\x05\x05\x05\x02\x04\x01\
//     \x12\x04\xeb\x05\x02\x06\n\r\n\x05\x05\x05\x02\x04\x02\x12\x04\xeb\x05\t\
//     \n\n\x0c\n\x02\x05\x06\x12\x06\xee\x05\0\xf2\x05\x01\n\x0b\n\x03\x05\x06\
//     \x01\x12\x04\xee\x05\x05\x12\n\x0c\n\x04\x05\x06\x02\0\x12\x04\xef\x05\
//     \x02\x1f\n\r\n\x05\x05\x06\x02\0\x01\x12\x04\xef\x05\x02\x1a\n\r\n\x05\
//     \x05\x06\x02\0\x02\x12\x04\xef\x05\x1d\x1e\n\x0c\n\x04\x05\x06\x02\x01\
//     \x12\x04\xf0\x05\x02\x12\n\r\n\x05\x05\x06\x02\x01\x01\x12\x04\xf0\x05\
//     \x02\r\n\r\n\x05\x05\x06\x02\x01\x02\x12\x04\xf0\x05\x10\x11\n\x0c\n\x04\
//     \x05\x06\x02\x02\x12\x04\xf1\x05\x02\x11\n\r\n\x05\x05\x06\x02\x02\x01\
//     \x12\x04\xf1\x05\x02\x0c\n\r\n\x05\x05\x06\x02\x02\x02\x12\x04\xf1\x05\
//     \x0f\x10\n\xd0\x03\n\x02\x05\x07\x12\x06\xfa\x05\0\xf0\x06\x01\x1a\xc1\
//     \x03\x20Language\x20standardises\x20names\x20of\x20common\x20programming\
//     \x20languages\x20that\x20can\x20be\x20used\n\x20for\x20the\x20`Document.\
//     language`\x20field.\x20The\x20primary\x20purpose\x20of\x20this\x20enum\
//     \x20is\x20to\n\x20prevent\x20a\x20situation\x20where\x20we\x20have\x20a\
//     \x20single\x20programming\x20language\x20ends\x20up\x20with\n\x20multipl\
//     e\x20string\x20representations.\x20For\x20example,\x20the\x20C++\x20lang\
//     uage\x20uses\x20the\x20name\n\x20\"CPP\"\x20in\x20this\x20enum\x20and\
//     \x20other\x20names\x20such\x20as\x20\"cpp\"\x20are\x20incompatible.\n\
//     \x20Feel\x20free\x20to\x20send\x20a\x20pull-request\x20to\x20add\x20miss\
//     ing\x20programming\x20languages.\n\n\x0b\n\x03\x05\x07\x01\x12\x04\xfa\
//     \x05\x05\r\n\x0c\n\x04\x05\x07\x02\0\x12\x04\xfb\x05\x02\x1a\n\r\n\x05\
//     \x05\x07\x02\0\x01\x12\x04\xfb\x05\x02\x15\n\r\n\x05\x05\x07\x02\0\x02\
//     \x12\x04\xfb\x05\x18\x19\n\x0c\n\x04\x05\x07\x02\x01\x12\x04\xfc\x05\x02\
//     \x0c\n\r\n\x05\x05\x07\x02\x01\x01\x12\x04\xfc\x05\x02\x06\n\r\n\x05\x05\
//     \x07\x02\x01\x02\x12\x04\xfc\x05\t\x0b\n\x0c\n\x04\x05\x07\x02\x02\x12\
//     \x04\xfd\x05\x02\x0c\n\r\n\x05\x05\x07\x02\x02\x01\x12\x04\xfd\x05\x02\
//     \x06\n\r\n\x05\x05\x07\x02\x02\x02\x12\x04\xfd\x05\t\x0b\n\x0c\n\x04\x05\
//     \x07\x02\x03\x12\x04\xfe\x05\x02\x0b\n\r\n\x05\x05\x07\x02\x03\x01\x12\
//     \x04\xfe\x05\x02\x05\n\r\n\x05\x05\x07\x02\x03\x02\x12\x04\xfe\x05\x08\n\
//     \n\x0c\n\x04\x05\x07\x02\x04\x12\x04\xff\x05\x02\x0b\n\r\n\x05\x05\x07\
//     \x02\x04\x01\x12\x04\xff\x05\x02\x05\n\r\n\x05\x05\x07\x02\x04\x02\x12\
//     \x04\xff\x05\x08\n\n\x0c\n\x04\x05\x07\x02\x05\x12\x04\x80\x06\x02\x0c\n\
//     \r\n\x05\x05\x07\x02\x05\x01\x12\x04\x80\x06\x02\x06\n\r\n\x05\x05\x07\
//     \x02\x05\x02\x12\x04\x80\x06\t\x0b\n\x0c\n\x04\x05\x07\x02\x06\x12\x04\
//     \x81\x06\x02\x10\n\r\n\x05\x05\x07\x02\x06\x01\x12\x04\x81\x06\x02\n\n\r\
//     \n\x05\x05\x07\x02\x06\x02\x12\x04\x81\x06\r\x0f\n\x0c\n\x04\x05\x07\x02\
//     \x07\x12\x04\x82\x06\x02\x10\n\r\n\x05\x05\x07\x02\x07\x01\x12\x04\x82\
//     \x06\x02\n\n\r\n\x05\x05\x07\x02\x07\x02\x12\x04\x82\x06\r\x0f\n\x0c\n\
//     \x04\x05\x07\x02\x08\x12\x04\x83\x06\x02\x0b\n\r\n\x05\x05\x07\x02\x08\
//     \x01\x12\x04\x83\x06\x02\x05\n\r\n\x05\x05\x07\x02\x08\x02\x12\x04\x83\
//     \x06\x08\n\n\x0c\n\x04\x05\x07\x02\t\x12\x04\x84\x06\x02\x0b\n\r\n\x05\
//     \x05\x07\x02\t\x01\x12\x04\x84\x06\x02\x05\n\r\n\x05\x05\x07\x02\t\x02\
//     \x12\x04\x84\x06\x08\n\n\x0c\n\x04\x05\x07\x02\n\x12\x04\x85\x06\x02\x0e\
//     \n\r\n\x05\x05\x07\x02\n\x01\x12\x04\x85\x06\x02\x08\n\r\n\x05\x05\x07\
//     \x02\n\x02\x12\x04\x85\x06\x0b\r\n\x0c\n\x04\x05\x07\x02\x0b\x12\x04\x86\
//     \x06\x02\t\n\r\n\x05\x05\x07\x02\x0b\x01\x12\x04\x86\x06\x02\x03\n\r\n\
//     \x05\x05\x07\x02\x0b\x02\x12\x04\x86\x06\x06\x08\n\x0c\n\x04\x05\x07\x02\
//     \x0c\x12\x04\x87\x06\x02\r\n\r\n\x05\x05\x07\x02\x0c\x01\x12\x04\x87\x06\
//     \x02\x07\n\r\n\x05\x05\x07\x02\x0c\x02\x12\x04\x87\x06\n\x0c\nH\n\x04\
//     \x05\x07\x02\r\x12\x04\x88\x06\x02\x0b\":\x20C++\x20(the\x20name\x20\"CP\
//     P\"\x20was\x20chosen\x20for\x20consistency\x20with\x20LSP)\n\n\r\n\x05\
//     \x05\x07\x02\r\x01\x12\x04\x88\x06\x02\x05\n\r\n\x05\x05\x07\x02\r\x02\
//     \x12\x04\x88\x06\x08\n\n\x0c\n\x04\x05\x07\x02\x0e\x12\x04\x89\x06\x02\
//     \x0b\n\r\n\x05\x05\x07\x02\x0e\x01\x12\x04\x89\x06\x02\x05\n\r\n\x05\x05\
//     \x07\x02\x0e\x02\x12\x04\x89\x06\x08\n\n\x0c\n\x04\x05\x07\x02\x0f\x12\
//     \x04\x8a\x06\x02\r\n\r\n\x05\x05\x07\x02\x0f\x01\x12\x04\x8a\x06\x02\x08\
//     \n\r\n\x05\x05\x07\x02\x0f\x02\x12\x04\x8a\x06\x0b\x0c\n\x0c\n\x04\x05\
//     \x07\x02\x10\x12\x04\x8b\x06\x02\x0e\n\r\n\x05\x05\x07\x02\x10\x01\x12\
//     \x04\x8b\x06\x02\t\n\r\n\x05\x05\x07\x02\x10\x02\x12\x04\x8b\x06\x0c\r\n\
//     \x0c\n\x04\x05\x07\x02\x11\x12\x04\x8c\x06\x02\x14\n\r\n\x05\x05\x07\x02\
//     \x11\x01\x12\x04\x8c\x06\x02\x0e\n\r\n\x05\x05\x07\x02\x11\x02\x12\x04\
//     \x8c\x06\x11\x13\n\x0c\n\x04\x05\x07\x02\x12\x12\x04\x8d\x06\x02\x11\n\r\
//     \n\x05\x05\x07\x02\x12\x01\x12\x04\x8d\x06\x02\x0c\n\r\n\x05\x05\x07\x02\
//     \x12\x02\x12\x04\x8d\x06\x0f\x10\n\x0c\n\x04\x05\x07\x02\x13\x12\x04\x8e\
//     \x06\x02\x0b\n\r\n\x05\x05\x07\x02\x13\x01\x12\x04\x8e\x06\x02\x05\n\r\n\
//     \x05\x05\x07\x02\x13\x02\x12\x04\x8e\x06\x08\n\n\x0c\n\x04\x05\x07\x02\
//     \x14\x12\x04\x8f\x06\x02\x0c\n\r\n\x05\x05\x07\x02\x14\x01\x12\x04\x8f\
//     \x06\x02\x06\n\r\n\x05\x05\x07\x02\x14\x02\x12\x04\x8f\x06\t\x0b\n\x0c\n\
//     \x04\x05\x07\x02\x15\x12\x04\x90\x06\x02\x0b\n\r\n\x05\x05\x07\x02\x15\
//     \x01\x12\x04\x90\x06\x02\x06\n\r\n\x05\x05\x07\x02\x15\x02\x12\x04\x90\
//     \x06\t\n\n\x0c\n\x04\x05\x07\x02\x16\x12\x04\x91\x06\x02\x0e\n\r\n\x05\
//     \x05\x07\x02\x16\x01\x12\x04\x91\x06\x02\x08\n\r\n\x05\x05\x07\x02\x16\
//     \x02\x12\x04\x91\x06\x0b\r\n\x0c\n\x04\x05\x07\x02\x17\x12\x04\x92\x06\
//     \x02\x0c\n\r\n\x05\x05\x07\x02\x17\x01\x12\x04\x92\x06\x02\x06\n\r\n\x05\
//     \x05\x07\x02\x17\x02\x12\x04\x92\x06\t\x0b\n\x0c\n\x04\x05\x07\x02\x18\
//     \x12\x04\x93\x06\x02\x12\n\r\n\x05\x05\x07\x02\x18\x01\x12\x04\x93\x06\
//     \x02\x0c\n\r\n\x05\x05\x07\x02\x18\x02\x12\x04\x93\x06\x0f\x11\n\x0c\n\
//     \x04\x05\x07\x02\x19\x12\x04\x94\x06\x02\x0e\n\r\n\x05\x05\x07\x02\x19\
//     \x01\x12\x04\x94\x06\x02\x08\n\r\n\x05\x05\x07\x02\x19\x02\x12\x04\x94\
//     \x06\x0b\r\n\x0c\n\x04\x05\x07\x02\x1a\x12\x04\x95\x06\x02\x0e\n\r\n\x05\
//     \x05\x07\x02\x1a\x01\x12\x04\x95\x06\x02\x08\n\r\n\x05\x05\x07\x02\x1a\
//     \x02\x12\x04\x95\x06\x0b\r\n\x0c\n\x04\x05\x07\x02\x1b\x12\x04\x96\x06\
//     \x02\x0e\n\r\n\x05\x05\x07\x02\x1b\x01\x12\x04\x96\x06\x02\x08\n\r\n\x05\
//     \x05\x07\x02\x1b\x02\x12\x04\x96\x06\x0b\r\n\x0c\n\x04\x05\x07\x02\x1c\
//     \x12\x04\x97\x06\x02\x0e\n\r\n\x05\x05\x07\x02\x1c\x01\x12\x04\x97\x06\
//     \x02\x08\n\r\n\x05\x05\x07\x02\x1c\x02\x12\x04\x97\x06\x0b\r\n\x0c\n\x04\
//     \x05\x07\x02\x1d\x12\x04\x98\x06\x02\x0c\n\r\n\x05\x05\x07\x02\x1d\x01\
//     \x12\x04\x98\x06\x02\x06\n\r\n\x05\x05\x07\x02\x1d\x02\x12\x04\x98\x06\t\
//     \x0b\n\x0c\n\x04\x05\x07\x02\x1e\x12\x04\x99\x06\x02\x0c\n\r\n\x05\x05\
//     \x07\x02\x1e\x01\x12\x04\x99\x06\x02\x06\n\r\n\x05\x05\x07\x02\x1e\x02\
//     \x12\x04\x99\x06\t\x0b\n\x0c\n\x04\x05\x07\x02\x1f\x12\x04\x9a\x06\x02\
//     \x0f\n\r\n\x05\x05\x07\x02\x1f\x01\x12\x04\x9a\x06\x02\t\n\r\n\x05\x05\
//     \x07\x02\x1f\x02\x12\x04\x9a\x06\x0c\x0e\n\x0c\n\x04\x05\x07\x02\x20\x12\
//     \x04\x9b\x06\x02\x12\n\r\n\x05\x05\x07\x02\x20\x01\x12\x04\x9b\x06\x02\
//     \x0c\n\r\n\x05\x05\x07\x02\x20\x02\x12\x04\x9b\x06\x0f\x11\n\x0c\n\x04\
//     \x05\x07\x02!\x12\x04\x9c\x06\x02\x12\n\r\n\x05\x05\x07\x02!\x01\x12\x04\
//     \x9c\x06\x02\x0c\n\r\n\x05\x05\x07\x02!\x02\x12\x04\x9c\x06\x0f\x11\n\
//     \x0c\n\x04\x05\x07\x02\"\x12\x04\x9d\x06\x02\x12\n\r\n\x05\x05\x07\x02\"\
//     \x01\x12\x04\x9d\x06\x02\x0c\n\r\n\x05\x05\x07\x02\"\x02\x12\x04\x9d\x06\
//     \x0f\x11\n\x0c\n\x04\x05\x07\x02#\x12\x04\x9e\x06\x02\n\n\r\n\x05\x05\
//     \x07\x02#\x01\x12\x04\x9e\x06\x02\x04\n\r\n\x05\x05\x07\x02#\x02\x12\x04\
//     \x9e\x06\x07\t\n\x0c\n\x04\x05\x07\x02$\x12\x04\x9f\x06\x02\x0f\n\r\n\
//     \x05\x05\x07\x02$\x01\x12\x04\x9f\x06\x02\t\n\r\n\x05\x05\x07\x02$\x02\
//     \x12\x04\x9f\x06\x0c\x0e\n\x0c\n\x04\x05\x07\x02%\x12\x04\xa0\x06\x02\r\
//     \n\r\n\x05\x05\x07\x02%\x01\x12\x04\xa0\x06\x02\x08\n\r\n\x05\x05\x07\
//     \x02%\x02\x12\x04\xa0\x06\x0b\x0c\n\x0c\n\x04\x05\x07\x02&\x12\x04\xa1\
//     \x06\x02\x0c\n\r\n\x05\x05\x07\x02&\x01\x12\x04\xa1\x06\x02\x06\n\r\n\
//     \x05\x05\x07\x02&\x02\x12\x04\xa1\x06\t\x0b\n\x0c\n\x04\x05\x07\x02'\x12\
//     \x04\xa2\x06\x02\x0c\n\r\n\x05\x05\x07\x02'\x01\x12\x04\xa2\x06\x02\x06\
//     \n\r\n\x05\x05\x07\x02'\x02\x12\x04\xa2\x06\t\x0b\n\x0c\n\x04\x05\x07\
//     \x02(\x12\x04\xa3\x06\x02\x12\n\r\n\x05\x05\x07\x02(\x01\x12\x04\xa3\x06\
//     \x02\x0c\n\r\n\x05\x05\x07\x02(\x02\x12\x04\xa3\x06\x0f\x11\n\x0c\n\x04\
//     \x05\x07\x02)\x12\x04\xa4\x06\x02\x0f\n\r\n\x05\x05\x07\x02)\x01\x12\x04\
//     \xa4\x06\x02\t\n\r\n\x05\x05\x07\x02)\x02\x12\x04\xa4\x06\x0c\x0e\n\x0c\
//     \n\x04\x05\x07\x02*\x12\x04\xa5\x06\x02\r\n\r\n\x05\x05\x07\x02*\x01\x12\
//     \x04\xa5\x06\x02\x07\n\r\n\x05\x05\x07\x02*\x02\x12\x04\xa5\x06\n\x0c\n\
//     \x0c\n\x04\x05\x07\x02+\x12\x04\xa6\x06\x02\x0b\n\r\n\x05\x05\x07\x02+\
//     \x01\x12\x04\xa6\x06\x02\x05\n\r\n\x05\x05\x07\x02+\x02\x12\x04\xa6\x06\
//     \x08\n\n\x0c\n\x04\x05\x07\x02,\x12\x04\xa7\x06\x02\t\n\r\n\x05\x05\x07\
//     \x02,\x01\x12\x04\xa7\x06\x02\x03\n\r\n\x05\x05\x07\x02,\x02\x12\x04\xa7\
//     \x06\x06\x08\n\x0c\n\x04\x05\x07\x02-\x12\x04\xa8\x06\x02\x0c\n\r\n\x05\
//     \x05\x07\x02-\x01\x12\x04\xa8\x06\x02\x06\n\r\n\x05\x05\x07\x02-\x02\x12\
//     \x04\xa8\x06\t\x0b\n\x0c\n\x04\x05\x07\x02.\x12\x04\xa9\x06\x02\x0b\n\r\
//     \n\x05\x05\x07\x02.\x01\x12\x04\xa9\x06\x02\x06\n\r\n\x05\x05\x07\x02.\
//     \x02\x12\x04\xa9\x06\t\n\n\x0c\n\x04\x05\x07\x02/\x12\x04\xaa\x06\x02\
//     \x12\n\r\n\x05\x05\x07\x02/\x01\x12\x04\xaa\x06\x02\x0c\n\r\n\x05\x05\
//     \x07\x02/\x02\x12\x04\xaa\x06\x0f\x11\n\x0c\n\x04\x05\x07\x020\x12\x04\
//     \xab\x06\x02\x17\n\r\n\x05\x05\x07\x020\x01\x12\x04\xab\x06\x02\x11\n\r\
//     \n\x05\x05\x07\x020\x02\x12\x04\xab\x06\x14\x16\n\x0c\n\x04\x05\x07\x021\
//     \x12\x04\xac\x06\x02\x0f\n\r\n\x05\x05\x07\x021\x01\x12\x04\xac\x06\x02\
//     \t\n\r\n\x05\x05\x07\x021\x02\x12\x04\xac\x06\x0c\x0e\n\x0c\n\x04\x05\
//     \x07\x022\x12\x04\xad\x06\x02\x0e\n\r\n\x05\x05\x07\x022\x01\x12\x04\xad\
//     \x06\x02\x07\n\r\n\x05\x05\x07\x022\x02\x12\x04\xad\x06\x0b\r\n\x0c\n\
//     \x04\x05\x07\x023\x12\x04\xae\x06\x02\x11\n\r\n\x05\x05\x07\x023\x01\x12\
//     \x04\xae\x06\x02\n\n\r\n\x05\x05\x07\x023\x02\x12\x04\xae\x06\r\x10\n\
//     \x0c\n\x04\x05\x07\x024\x12\x04\xaf\x06\x02\r\n\r\n\x05\x05\x07\x024\x01\
//     \x12\x04\xaf\x06\x02\x08\n\r\n\x05\x05\x07\x024\x02\x12\x04\xaf\x06\x0b\
//     \x0c\n\x0c\n\x04\x05\x07\x025\x12\x04\xb0\x06\x02\r\n\r\n\x05\x05\x07\
//     \x025\x01\x12\x04\xb0\x06\x02\x07\n\r\n\x05\x05\x07\x025\x02\x12\x04\xb0\
//     \x06\n\x0c\n\x0c\n\x04\x05\x07\x026\x12\x04\xb1\x06\x02\x0c\n\r\n\x05\
//     \x05\x07\x026\x01\x12\x04\xb1\x06\x02\x06\n\r\n\x05\x05\x07\x026\x02\x12\
//     \x04\xb1\x06\t\x0b\n\x0c\n\x04\x05\x07\x027\x12\x04\xb2\x06\x02\x0c\n\r\
//     \n\x05\x05\x07\x027\x01\x12\x04\xb2\x06\x02\x06\n\r\n\x05\x05\x07\x027\
//     \x02\x12\x04\xb2\x06\t\x0b\n\x0c\n\x04\x05\x07\x028\x12\x04\xb3\x06\x02\
//     \x0b\n\r\n\x05\x05\x07\x028\x01\x12\x04\xb3\x06\x02\x05\n\r\n\x05\x05\
//     \x07\x028\x02\x12\x04\xb3\x06\x08\n\n\x0c\n\x04\x05\x07\x029\x12\x04\xb4\
//     \x06\x02\r\n\r\n\x05\x05\x07\x029\x01\x12\x04\xb4\x06\x02\x06\n\r\n\x05\
//     \x05\x07\x029\x02\x12\x04\xb4\x06\t\x0c\n\x0c\n\x04\x05\x07\x02:\x12\x04\
//     \xb5\x06\x02\x10\n\r\n\x05\x05\x07\x02:\x01\x12\x04\xb5\x06\x02\n\n\r\n\
//     \x05\x05\x07\x02:\x02\x12\x04\xb5\x06\r\x0f\n\x0c\n\x04\x05\x07\x02;\x12\
//     \x04\xb6\x06\x02\x10\n\r\n\x05\x05\x07\x02;\x01\x12\x04\xb6\x06\x02\n\n\
//     \r\n\x05\x05\x07\x02;\x02\x12\x04\xb6\x06\r\x0f\n\x0c\n\x04\x05\x07\x02<\
//     \x12\x04\xb7\x06\x02\x0e\n\r\n\x05\x05\x07\x02<\x01\x12\x04\xb7\x06\x02\
//     \x08\n\r\n\x05\x05\x07\x02<\x02\x12\x04\xb7\x06\x0b\r\n(\n\x04\x05\x07\
//     \x02=\x12\x04\xb8\x06\x02\x0f\"\x1a\x20https://nickel-lang.org/\n\n\r\n\
//     \x05\x05\x07\x02=\x01\x12\x04\xb8\x06\x02\x08\n\r\n\x05\x05\x07\x02=\x02\
//     \x12\x04\xb8\x06\x0b\x0e\n\x0c\n\x04\x05\x07\x02>\x12\x04\xb9\x06\x02\
//     \x0b\n\r\n\x05\x05\x07\x02>\x01\x12\x04\xb9\x06\x02\x05\n\r\n\x05\x05\
//     \x07\x02>\x02\x12\x04\xb9\x06\x08\n\n\x0c\n\x04\x05\x07\x02?\x12\x04\xba\
//     \x06\x02\r\n\r\n\x05\x05\x07\x02?\x01\x12\x04\xba\x06\x02\x07\n\r\n\x05\
//     \x05\x07\x02?\x02\x12\x04\xba\x06\n\x0c\n\x0c\n\x04\x05\x07\x02@\x12\x04\
//     \xbb\x06\x02\x13\n\r\n\x05\x05\x07\x02@\x01\x12\x04\xbb\x06\x02\r\n\r\n\
//     \x05\x05\x07\x02@\x02\x12\x04\xbb\x06\x10\x12\n\x0c\n\x04\x05\x07\x02A\
//     \x12\x04\xbc\x06\x02\x15\n\r\n\x05\x05\x07\x02A\x01\x12\x04\xbc\x06\x02\
//     \x0f\n\r\n\x05\x05\x07\x02A\x02\x12\x04\xbc\x06\x12\x14\n\x0c\n\x04\x05\
//     \x07\x02B\x12\x04\xbd\x06\x02\x0e\n\r\n\x05\x05\x07\x02B\x01\x12\x04\xbd\
//     \x06\x02\x08\n\r\n\x05\x05\x07\x02B\x02\x12\x04\xbd\x06\x0b\r\n\x0c\n\
//     \x04\x05\x07\x02C\x12\x04\xbe\x06\x02\x0b\n\r\n\x05\x05\x07\x02C\x01\x12\
//     \x04\xbe\x06\x02\x05\n\r\n\x05\x05\x07\x02C\x02\x12\x04\xbe\x06\x08\n\n\
//     \x0c\n\x04\x05\x07\x02D\x12\x04\xbf\x06\x02\r\n\r\n\x05\x05\x07\x02D\x01\
//     \x12\x04\xbf\x06\x02\x07\n\r\n\x05\x05\x07\x02D\x02\x12\x04\xbf\x06\n\
//     \x0c\n\x0c\n\x04\x05\x07\x02E\x12\x04\xc0\x06\x02\x0c\n\r\n\x05\x05\x07\
//     \x02E\x01\x12\x04\xc0\x06\x02\x06\n\r\n\x05\x05\x07\x02E\x02\x12\x04\xc0\
//     \x06\t\x0b\n\x0c\n\x04\x05\x07\x02F\x12\x04\xc1\x06\x02\x12\n\r\n\x05\
//     \x05\x07\x02F\x01\x12\x04\xc1\x06\x02\x0c\n\r\n\x05\x05\x07\x02F\x02\x12\
//     \x04\xc1\x06\x0f\x11\n\x0c\n\x04\x05\x07\x02G\x12\x04\xc2\x06\x02\x0e\n\
//     \r\n\x05\x05\x07\x02G\x01\x12\x04\xc2\x06\x02\x08\n\r\n\x05\x05\x07\x02G\
//     \x02\x12\x04\xc2\x06\x0b\r\n\x0c\n\x04\x05\x07\x02H\x12\x04\xc3\x06\x02\
//     \x11\n\r\n\x05\x05\x07\x02H\x01\x12\x04\xc3\x06\x02\n\n\r\n\x05\x05\x07\
//     \x02H\x02\x12\x04\xc3\x06\r\x10\n\x0c\n\x04\x05\x07\x02I\x12\x04\xc4\x06\
//     \x02\x0e\n\r\n\x05\x05\x07\x02I\x01\x12\x04\xc4\x06\x02\x08\n\r\n\x05\
//     \x05\x07\x02I\x02\x12\x04\xc4\x06\x0b\r\n\x0c\n\x04\x05\x07\x02J\x12\x04\
//     \xc5\x06\x02\t\n\r\n\x05\x05\x07\x02J\x01\x12\x04\xc5\x06\x02\x03\n\r\n\
//     \x05\x05\x07\x02J\x02\x12\x04\xc5\x06\x06\x08\n\x0c\n\x04\x05\x07\x02K\
//     \x12\x04\xc6\x06\x02\x0e\n\r\n\x05\x05\x07\x02K\x01\x12\x04\xc6\x06\x02\
//     \x08\n\r\n\x05\x05\x07\x02K\x02\x12\x04\xc6\x06\x0b\r\n\x0c\n\x04\x05\
//     \x07\x02L\x12\x04\xc7\x06\x02\x0c\n\r\n\x05\x05\x07\x02L\x01\x12\x04\xc7\
//     \x06\x02\x06\n\r\n\x05\x05\x07\x02L\x02\x12\x04\xc7\x06\t\x0b\n\x0c\n\
//     \x04\x05\x07\x02M\x12\x04\xc8\x06\x02\r\n\r\n\x05\x05\x07\x02M\x01\x12\
//     \x04\xc8\x06\x02\x07\n\r\n\x05\x05\x07\x02M\x02\x12\x04\xc8\x06\n\x0c\n2\
//     \n\x04\x05\x07\x02N\x12\x04\xc9\x06\x02\x0e\"$\x20Internal\x20language\
//     \x20for\x20testing\x20SCIP\n\n\r\n\x05\x05\x07\x02N\x01\x12\x04\xc9\x06\
//     \x02\x07\n\r\n\x05\x05\x07\x02N\x02\x12\x04\xc9\x06\n\r\n\x0c\n\x04\x05\
//     \x07\x02O\x12\x04\xca\x06\x02\x0c\n\r\n\x05\x05\x07\x02O\x01\x12\x04\xca\
//     \x06\x02\x06\n\r\n\x05\x05\x07\x02O\x02\x12\x04\xca\x06\t\x0b\n\x0c\n\
//     \x04\x05\x07\x02P\x12\x04\xcb\x06\x02\x0c\n\r\n\x05\x05\x07\x02P\x01\x12\
//     \x04\xcb\x06\x02\x06\n\r\n\x05\x05\x07\x02P\x02\x12\x04\xcb\x06\t\x0b\n\
//     \x0c\n\x04\x05\x07\x02Q\x12\x04\xcc\x06\x02\x0c\n\r\n\x05\x05\x07\x02Q\
//     \x01\x12\x04\xcc\x06\x02\x06\n\r\n\x05\x05\x07\x02Q\x02\x12\x04\xcc\x06\
//     \t\x0b\n\x0c\n\x04\x05\x07\x02R\x12\x04\xcd\x06\x02\x0b\n\r\n\x05\x05\
//     \x07\x02R\x01\x12\x04\xcd\x06\x02\x05\n\r\n\x05\x05\x07\x02R\x02\x12\x04\
//     \xcd\x06\x08\n\n\x0c\n\x04\x05\x07\x02S\x12\x04\xce\x06\x02\x0c\n\r\n\
//     \x05\x05\x07\x02S\x01\x12\x04\xce\x06\x02\x06\n\r\n\x05\x05\x07\x02S\x02\
//     \x12\x04\xce\x06\t\x0b\n\x0c\n\x04\x05\x07\x02T\x12\x04\xcf\x06\x02\x0b\
//     \n\r\n\x05\x05\x07\x02T\x01\x12\x04\xcf\x06\x02\x05\n\r\n\x05\x05\x07\
//     \x02T\x02\x12\x04\xcf\x06\x08\n\n\x0c\n\x04\x05\x07\x02U\x12\x04\xd0\x06\
//     \x02\x0b\n\r\n\x05\x05\x07\x02U\x01\x12\x04\xd0\x06\x02\x05\n\r\n\x05\
//     \x05\x07\x02U\x02\x12\x04\xd0\x06\x08\n\n\x0c\n\x04\x05\x07\x02V\x12\x04\
//     \xd1\x06\x02\x0c\n\r\n\x05\x05\x07\x02V\x01\x12\x04\xd1\x06\x02\x06\n\r\
//     \n\x05\x05\x07\x02V\x02\x12\x04\xd1\x06\t\x0b\n\x0c\n\x04\x05\x07\x02W\
//     \x12\x04\xd2\x06\x02\x0c\n\r\n\x05\x05\x07\x02W\x01\x12\x04\xd2\x06\x02\
//     \x07\n\r\n\x05\x05\x07\x02W\x02\x12\x04\xd2\x06\n\x0b\n\x0c\n\x04\x05\
//     \x07\x02X\x12\x04\xd3\x06\x02\x0e\n\r\n\x05\x05\x07\x02X\x01\x12\x04\xd3\
//     \x06\x02\x08\n\r\n\x05\x05\x07\x02X\x02\x12\x04\xd3\x06\x0b\r\n\x14\n\
//     \x04\x05\x07\x02Y\x12\x04\xd4\x06\x02\x13\"\x06\x20Bash\n\n\r\n\x05\x05\
//     \x07\x02Y\x01\x12\x04\xd4\x06\x02\r\n\r\n\x05\x05\x07\x02Y\x02\x12\x04\
//     \xd4\x06\x10\x12\n\x0c\n\x04\x05\x07\x02Z\x12\x04\xd5\x06\x02\x0f\n\r\n\
//     \x05\x05\x07\x02Z\x01\x12\x04\xd5\x06\x02\t\n\r\n\x05\x05\x07\x02Z\x02\
//     \x12\x04\xd5\x06\x0c\x0e\n\x0c\n\x04\x05\x07\x02[\x12\x04\xd6\x06\x02\
//     \x0e\n\r\n\x05\x05\x07\x02[\x01\x12\x04\xd6\x06\x02\x07\n\r\n\x05\x05\
//     \x07\x02[\x02\x12\x04\xd6\x06\n\r\n\x0c\n\x04\x05\x07\x02\\\x12\x04\xd7\
//     \x06\x02\x10\n\r\n\x05\x05\x07\x02\\\x01\x12\x04\xd7\x06\x02\n\n\r\n\x05\
//     \x05\x07\x02\\\x02\x12\x04\xd7\x06\r\x0f\n\x0c\n\x04\x05\x07\x02]\x12\
//     \x04\xd8\x06\x02\x0f\n\r\n\x05\x05\x07\x02]\x01\x12\x04\xd8\x06\x02\x08\
//     \n\r\n\x05\x05\x07\x02]\x02\x12\x04\xd8\x06\x0b\x0e\n\x0c\n\x04\x05\x07\
//     \x02^\x12\x04\xd9\x06\x02\x0c\n\r\n\x05\x05\x07\x02^\x01\x12\x04\xd9\x06\
//     \x02\x07\n\r\n\x05\x05\x07\x02^\x02\x12\x04\xd9\x06\n\x0b\n\x0c\n\x04\
//     \x05\x07\x02_\x12\x04\xda\x06\x02\x0c\n\r\n\x05\x05\x07\x02_\x01\x12\x04\
//     \xda\x06\x02\x05\n\r\n\x05\x05\x07\x02_\x02\x12\x04\xda\x06\x08\x0b\n\
//     \x0c\n\x04\x05\x07\x02`\x12\x04\xdb\x06\x02\x0c\n\r\n\x05\x05\x07\x02`\
//     \x01\x12\x04\xdb\x06\x02\x06\n\r\n\x05\x05\x07\x02`\x02\x12\x04\xdb\x06\
//     \t\x0b\n\x0c\n\x04\x05\x07\x02a\x12\x04\xdc\x06\x02\x0b\n\r\n\x05\x05\
//     \x07\x02a\x01\x12\x04\xdc\x06\x02\x05\n\r\n\x05\x05\x07\x02a\x02\x12\x04\
//     \xdc\x06\x08\n\n\x0c\n\x04\x05\x07\x02b\x12\x04\xdd\x06\x02\x0f\n\r\n\
//     \x05\x05\x07\x02b\x01\x12\x04\xdd\x06\x02\x08\n\r\n\x05\x05\x07\x02b\x02\
//     \x12\x04\xdd\x06\x0b\x0e\n\x0c\n\x04\x05\x07\x02c\x12\x04\xde\x06\x02\
//     \x12\n\r\n\x05\x05\x07\x02c\x01\x12\x04\xde\x06\x02\x0c\n\r\n\x05\x05\
//     \x07\x02c\x02\x12\x04\xde\x06\x0f\x11\n\x0c\n\x04\x05\x07\x02d\x12\x04\
//     \xdf\x06\x02\x17\n\r\n\x05\x05\x07\x02d\x01\x12\x04\xdf\x06\x02\x11\n\r\
//     \n\x05\x05\x07\x02d\x02\x12\x04\xdf\x06\x14\x16\n\x0c\n\x04\x05\x07\x02e\
//     \x12\x04\xe0\x06\x02\x10\n\r\n\x05\x05\x07\x02e\x01\x12\x04\xe0\x06\x02\
//     \t\n\r\n\x05\x05\x07\x02e\x02\x12\x04\xe0\x06\x0c\x0f\n\x0c\n\x04\x05\
//     \x07\x02f\x12\x04\xe1\x06\x02\r\n\r\n\x05\x05\x07\x02f\x01\x12\x04\xe1\
//     \x06\x02\x06\n\r\n\x05\x05\x07\x02f\x02\x12\x04\xe1\x06\t\x0c\n\x0c\n\
//     \x04\x05\x07\x02g\x12\x04\xe2\x06\x02\x13\n\r\n\x05\x05\x07\x02g\x01\x12\
//     \x04\xe2\x06\x02\r\n\r\n\x05\x05\x07\x02g\x02\x12\x04\xe2\x06\x10\x12\n\
//     \x0c\n\x04\x05\x07\x02h\x12\x04\xe3\x06\x02\x0b\n\r\n\x05\x05\x07\x02h\
//     \x01\x12\x04\xe3\x06\x02\x05\n\r\n\x05\x05\x07\x02h\x02\x12\x04\xe3\x06\
//     \x08\n\n\x0c\n\x04\x05\x07\x02i\x12\x04\xe4\x06\x02\x0f\n\r\n\x05\x05\
//     \x07\x02i\x01\x12\x04\xe4\x06\x02\t\n\r\n\x05\x05\x07\x02i\x02\x12\x04\
//     \xe4\x06\x0c\x0e\n\x0c\n\x04\x05\x07\x02j\x12\x04\xe5\x06\x02\x0b\n\r\n\
//     \x05\x05\x07\x02j\x01\x12\x04\xe5\x06\x02\x05\n\r\n\x05\x05\x07\x02j\x02\
//     \x12\x04\xe5\x06\x08\n\n\x0c\n\x04\x05\x07\x02k\x12\x04\xe6\x06\x02\x0b\
//     \n\r\n\x05\x05\x07\x02k\x01\x12\x04\xe6\x06\x02\x05\n\r\n\x05\x05\x07\
//     \x02k\x02\x12\x04\xe6\x06\x08\n\n\x0c\n\x04\x05\x07\x02l\x12\x04\xe7\x06\
//     \x02\x0c\n\r\n\x05\x05\x07\x02l\x01\x12\x04\xe7\x06\x02\x06\n\r\n\x05\
//     \x05\x07\x02l\x02\x12\x04\xe7\x06\t\x0b\n\x93\x03\n\x04\x05\x07\x02m\x12\
//     \x04\xe8\x06\x02\x0b\"\x84\x03\x20NextLanguage\x20=\x20111;\n\x20Steps\
//     \x20add\x20a\x20new\x20language:\n\x201.\x20Copy-paste\x20the\x20\"NextL\
//     anguage\x20=\x20N\"\x20line\x20above\n\x202.\x20Increment\x20\"NextLangu\
//     age\x20=\x20N\"\x20to\x20\"NextLanguage\x20=\x20N+1\"\n\x203.\x20Replace\
//     \x20\"NextLanguage\x20=\x20N\"\x20with\x20the\x20name\x20of\x20the\x20ne\
//     w\x20language.\n\x204.\x20Move\x20the\x20new\x20language\x20to\x20the\
//     \x20correct\x20line\x20above\x20using\x20alphabetical\x20order\n\x205.\
//     \x20(optional)\x20Add\x20a\x20brief\x20comment\x20behind\x20the\x20langu\
//     age\x20if\x20the\x20name\x20is\x20not\x20self-explanatory\n\n\r\n\x05\
//     \x05\x07\x02m\x01\x12\x04\xe8\x06\x02\x05\n\r\n\x05\x05\x07\x02m\x02\x12\
//     \x04\xe8\x06\x08\nb\x06proto3\
// ";

// /// `FileDescriptorProto` object which was a source for this generated file
// fn file_descriptor_proto() -> &'static ::protobuf::descriptor::FileDescriptorProto {
//     static file_descriptor_proto_lazy: ::protobuf::rt::Lazy<::protobuf::descriptor::FileDescriptorProto> = ::protobuf::rt::Lazy::new();
//     file_descriptor_proto_lazy.get(|| {
//         ::protobuf::Message::parse_from_bytes(file_descriptor_proto_data).unwrap()
//     })
// }

// /// `FileDescriptor` object which allows dynamic access to files
// pub fn file_descriptor() -> &'static ::protobuf::reflect::FileDescriptor {
//     static generated_file_descriptor_lazy: ::protobuf::rt::Lazy<::protobuf::reflect::GeneratedFileDescriptor> = ::protobuf::rt::Lazy::new();
//     static file_descriptor: ::protobuf::rt::Lazy<::protobuf::reflect::FileDescriptor> = ::protobuf::rt::Lazy::new();
//     file_descriptor.get(|| {
//         let generated_file_descriptor = generated_file_descriptor_lazy.get(|| {
//             let mut deps = ::std::vec::Vec::with_capacity(0);
//             let mut messages = ::std::vec::Vec::with_capacity(11);
//             messages.push(Index::generated_message_descriptor_data());
//             messages.push(Metadata::generated_message_descriptor_data());
//             messages.push(ToolInfo::generated_message_descriptor_data());
//             messages.push(Document::generated_message_descriptor_data());
//             messages.push(Symbol::generated_message_descriptor_data());
//             messages.push(Package::generated_message_descriptor_data());
//             messages.push(Descriptor::generated_message_descriptor_data());
//             messages.push(SymbolInformation::generated_message_descriptor_data());
//             messages.push(Relationship::generated_message_descriptor_data());
//             messages.push(Occurrence::generated_message_descriptor_data());
//             messages.push(Diagnostic::generated_message_descriptor_data());
//             let mut enums = ::std::vec::Vec::with_capacity(10);
//             enums.push(ProtocolVersion::generated_enum_descriptor_data());
//             enums.push(TextEncoding::generated_enum_descriptor_data());
//             enums.push(PositionEncoding::generated_enum_descriptor_data());
//             enums.push(SymbolRole::generated_enum_descriptor_data());
//             enums.push(SyntaxKind::generated_enum_descriptor_data());
//             enums.push(Severity::generated_enum_descriptor_data());
//             enums.push(DiagnosticTag::generated_enum_descriptor_data());
//             enums.push(Language::generated_enum_descriptor_data());
//             enums.push(descriptor::Suffix::generated_enum_descriptor_data());
//             enums.push(symbol_information::Kind::generated_enum_descriptor_data());
//             ::protobuf::reflect::GeneratedFileDescriptor::new_generated(
//                 file_descriptor_proto(),
//                 deps,
//                 messages,
//                 enums,
//             )
//         });
//         ::protobuf::reflect::FileDescriptor::new_generated_2(generated_file_descriptor)
//     })
// }
