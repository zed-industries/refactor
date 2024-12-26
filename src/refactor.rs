use anyhow::{Context as _, Result};
use clap::Parser as ClapParser;
use protobuf::Message;
use scip::types::{
    symbol_information::Kind, DiagnosticTag, PositionEncoding, ProtocolVersion, Severity,
    SymbolRole, SyntaxKind, TextEncoding,
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
    edits: HashMap<RelativePath, Vec<Edit>>,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub metadata: Arc<Metadata>,
    pub documents: HashMap<RelativePath, Document>,
    pub symbols: HashMap<GlobalSymbol, SymbolInformation>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativePath(Arc<Path>);

#[derive(Debug, Clone)]
pub struct Document {
    pub relative_path: RelativePath,
    pub language: Arc<str>,
    pub occurrences: Vec<Occurrence>,
    pub locals: HashMap<LocalSymbol, SymbolInformation>,
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

impl Occurrence {
    fn has_role(&self, role: SymbolRole) -> bool {
        self.symbol_roles & (role as i32) != 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Local(LocalSymbol),
    Global(GlobalSymbol),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalSymbol(Arc<str>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalSymbol(Arc<str>);

#[derive(Debug, Clone)]
pub struct SymbolInformation {
    pub symbol: Symbol,
    pub documentation: Vec<Arc<str>>,
    pub relationships: Vec<Relationship>,
    pub kind: Kind,
    pub display_name: Arc<str>,
    pub signature_documentation: Option<Arc<SignatureDocumentation>>,
    pub enclosing_symbol: Arc<str>,
}

// In SCIP this is represented as a Document, but it's cleaner to have a separate type.
#[derive(Debug, Clone)]
pub struct SignatureDocumentation {
    pub language: Arc<str>,
    pub occurrences: Vec<Occurrence>,
    pub locals: HashMap<LocalSymbol, SymbolInformation>,
    pub text: Arc<str>,
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
            let relative_path = RelativePath(
                file_path
                    .strip_prefix(&self.index_folder)
                    .unwrap_or(file_path)
                    .into(),
            );
            let mut file = File::open(file_path)?;
            let mut source = String::new();
            file.read_to_string(&mut source)?;

            let tree = self.parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            self.track_callers(&relative_path, &source, root_node)?;
            self.process_functions(&relative_path, &source, root_node);
            // self.process_imports(relative_path, &source, root_node);
        }

        self.trace_window_context_callers();

        Ok(())
    }

    fn track_callers(
        &mut self,
        relative_path: &RelativePath,
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
        relative_path: &RelativePath,
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
            match callee {
                Symbol::Global(symbol) if symbol.0.contains("WindowContext#") => {
                    for caller in callers {
                        if self
                            .transitive_window_context_callers
                            .insert(caller.clone())
                        {
                            to_process.push(caller.clone());
                        }
                    }
                }
                _ => {}
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
        relative_path: &RelativePath,
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
        relative_path: &RelativePath,
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
        relative_path: &RelativePath,
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
        relative_path: &RelativePath,
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
        relative_path: &RelativePath,
    ) -> Result<&Occurrence> {
        let line = point.row;
        let column = point.column;
        let document = self
            .index
            .documents
            .get(relative_path)
            .ok_or_else(|| anyhow::anyhow!("Document not found: {:?}", relative_path))?;

        let occurrence_index = document
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
                    document.relative_path,
                    line,
                    column
                )
            })?;

        Ok(&document.occurrences[occurrence_index])
    }

    pub fn display_dry_run_results(&self) {
        for (path, edits) in &self.edits {
            let path = &self.index_folder.join(path.0.clone());
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
        relative_path: &RelativePath,
        node: &tree_sitter::Node,
        replacement: &str,
    ) {
        self.record_edit(relative_path, node.byte_range(), replacement);
    }

    fn record_insertion_before_node(
        &mut self,
        relative_path: &RelativePath,
        node: &tree_sitter::Node,
        insertion: &str,
    ) {
        let start = node.start_byte();
        self.record_edit(relative_path, start..start, insertion);
    }

    fn record_insertion_after_node(
        &mut self,
        relative_path: &RelativePath,
        node: &tree_sitter::Node,
        insertion: &str,
    ) {
        let end = node.end_byte();
        self.record_edit(relative_path, end..end, insertion);
    }

    fn record_edit(
        &mut self,
        relative_path: &RelativePath,
        byte_range: Range<usize>,
        replacement: &str,
    ) {
        self.edits
            .entry(relative_path.clone())
            .or_default()
            .push(Edit {
                byte_range,
                replacement: replacement.to_string(),
            });
    }

    pub fn apply_edits(&mut self) -> Result<()> {
        let mut changed_files = 0;
        for (path, edits) in self.edits.iter_mut() {
            let path = &self.index_folder.join(path.0.clone());
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

// TODO: Complain about insertion of duplicates in all use of insert below
impl From<scip::types::Index> for Index {
    fn from(index: scip::types::Index) -> Self {
        let mut documents = HashMap::new();
        let mut symbols = HashMap::new();
        for document in index.documents.into_iter() {
            let mut locals = HashMap::new();
            for symbol_info in document.symbols {
                let converted_symbol: SymbolInformation = symbol_info.into();
                match converted_symbol.symbol.clone() {
                    Symbol::Local(local) => {
                        locals.insert(local, converted_symbol);
                    }
                    Symbol::Global(global) => {
                        symbols.insert(global, converted_symbol);
                    }
                }
            }

            let relative_path = RelativePath(Path::new(&document.relative_path).into());
            documents.insert(
                relative_path.clone(),
                Document {
                    relative_path,
                    language: document.language.into(),
                    occurrences: document
                        .occurrences
                        .into_iter()
                        .map(Occurrence::from)
                        .collect(),
                    locals,
                    position_encoding: document.position_encoding.enum_value().unwrap(),
                },
            );
        }
        for symbol_info in index.external_symbols.into_iter() {
            let converted_symbol: SymbolInformation = symbol_info.into();
            match converted_symbol.symbol.clone() {
                // TODO: Complain about local case
                Symbol::Local(_) => {}
                Symbol::Global(global) => {
                    symbols.insert(global, converted_symbol);
                }
            }
        }

        Index {
            metadata: Arc::new(Metadata::from(*index.metadata.0.unwrap())),
            documents,
            symbols,
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

impl From<scip::types::Occurrence> for Occurrence {
    fn from(occurrence: scip::types::Occurrence) -> Self {
        Occurrence {
            range: occurrence.range,
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

impl From<String> for Symbol {
    fn from(symbol: String) -> Self {
        if symbol.starts_with("local ") {
            Symbol::Local(LocalSymbol(symbol.into()))
        } else {
            Symbol::Global(GlobalSymbol(symbol.into()))
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
                .map(|doc| Arc::new(SignatureDocumentation::from(*doc))),
            enclosing_symbol: symbol_info.enclosing_symbol.into(),
        }
    }
}

impl From<scip::types::Document> for SignatureDocumentation {
    fn from(document: scip::types::Document) -> Self {
        let mut locals = HashMap::new();
        for symbol_info in document.symbols.into_iter() {
            let converted_symbol: SymbolInformation = symbol_info.into();
            match converted_symbol.symbol.clone() {
                Symbol::Local(local_symbol) => {
                    locals.insert(local_symbol, converted_symbol);
                }
                // TODO: complain for this case.
                Symbol::Global(_) => {}
            }
        }
        SignatureDocumentation {
            language: document.language.into(),
            occurrences: document
                .occurrences
                .into_iter()
                .map(Occurrence::from)
                .collect(),
            locals,
            text: document.text.into(),
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
