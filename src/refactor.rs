use anyhow::{Context as _, Result};
use clap::Parser as ClapParser;
use protobuf::Message;
use scip::types::Index;
use std::{collections::HashMap, fs::File, io::Read, path::PathBuf};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Parser, Query, QueryCursor};

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the directory or file to refactor
    #[clap(value_parser)]
    path: String,

    /// Perform a dry run (show potential changes without applying them)
    #[clap(long, action)]
    dry_run: bool,
}

struct Refactor {
    parser: Parser,
    index: Index,
    window_methods: HashMap<&'static str, bool>,
    path: PathBuf,
    edits: HashMap<PathBuf, Vec<Edit>>,
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

        let index_path = path.join("index.scip");
        let mut file = File::open(&index_path)
            .context(format!("Failed to open index file: {:?}", index_path))?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)
            .context("Failed to read index file")?;

        let mut index =
            Index::parse_from_bytes(&buffer).context("Failed to parse index from bytes")?;
        index
            .documents
            .sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
        for doc in &mut index.documents {
            doc.occurrences.sort_by_key(|a| a.range[0]);
        }

        Ok(Self {
            parser,
            index,
            window_methods: Self::build_window_methods(),
            path,
            edits: HashMap::new(),
        })
    }

    pub fn process(&mut self) -> Result<()> {
        for entry in walkdir::WalkDir::new(&self.path)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_type().is_file() && e.path().extension() == Some(std::ffi::OsStr::new("rs"))
            })
        {
            let file_path = entry.path();
            let mut file = File::open(file_path)?;
            let mut source = String::new();
            file.read_to_string(&mut source)?;

            let tree = self.parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            self.process_functions(&source, root_node);
            self.process_imports(&source);
        }

        Ok(())
    }

    fn process_functions(&mut self, source: &str, root_node: tree_sitter::Node) {
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

            for capture in match_.captures {
                match capture.index {
                    i if i == function_name_index => {}
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

                    self.edits.entry(self.path.clone()).or_default().push(Edit {
                        start,
                        end,
                        replacement: new_param.to_string(),
                    });
                }
            } else {
                if let (Some(start), Some(end)) = (target_param_start, target_param_end) {
                    self.edits.entry(self.path.clone()).or_default().push(Edit {
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
            let object_text = &source[object.node.byte_range()];
            let method_name = &source[method.node.byte_range()];

            // If this a method call on the context itself, decide whether to call it on window or cx.
            if object_text == param_name {
                if let Some(&needs_cx) = self.window_methods.get(method_name) {
                    self.edits.entry(self.path.clone()).or_default().push(Edit {
                        start: object.node.start_byte(),
                        end: object.node.end_byte(),
                        replacement: "window".to_string(),
                    });

                    if needs_cx {
                        let arg_end = arg.node.end_byte();
                        self.edits.entry(self.path.clone()).or_default().push(Edit {
                            start: arg_end,
                            end: arg_end,
                            replacement: ", cx".to_string(),
                        });
                    }
                } else {
                    let arg_start = arg.node.start_byte();
                    self.edits.entry(self.path.clone()).or_default().push(Edit {
                        start: arg_start,
                        end: arg_start,
                        replacement: "window, ".to_string(),
                    });
                }
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

    fn display_dry_run_results(&self) {
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
                let _context_end = source[edit.end..]
                    .find('\n')
                    .map_or(source.len(), |i| edit.end + i);

                println!("Lines {}-{}:", start_line, end_line);
                println!("- {}", &source[context_start..edit.end]);
                println!(
                    "+ {}{}",
                    &source[context_start..edit.start],
                    edit.replacement
                );
                println!();
            }

            println!("---\n");
        }
    }

    fn apply_edits(&mut self) -> Result<()> {
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
        }
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

fn main() -> Result<()> {
    let args = Args::parse();

    let path = std::fs::canonicalize(&args.path)?;
    let _relative_path = path.strip_prefix(std::env::current_dir()?).unwrap_or(&path);

    let refactor = Refactor::new(path);
    let mut refactor = refactor?;
    refactor.process()?;

    if args.dry_run {
        refactor.display_dry_run_results();
    } else {
        // TODO: Implement actual file modifications
        println!("Refactoring complete. File modifications not implemented yet.");
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
