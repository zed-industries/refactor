use clap::Parser as ClapParser;
use std::{collections::HashMap, fs, io::Write};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Parser, Query, QueryCursor};
use walkdir::WalkDir;

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

fn main() {
    let args = Args::parse();

    let path = std::fs::canonicalize(&args.path).expect("Failed to canonicalize path");
    let relative_path = path
        .strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(&path);

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_rust::LANGUAGE.into())
        .expect("Error loading Rust grammar");

    for entry in WalkDir::new(relative_path)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.path().extension().map_or(false, |ext| ext == "rs") {
            let source = fs::read_to_string(entry.path()).unwrap();
            let tree = parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            let mut edits = Vec::new();

            // Process imports
            process_imports(&source, &mut parser, &mut edits);

            process_functions(&source, root_node, &mut edits);

            // Sort edits in reverse order of their start positions
            edits.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));
            // Deduplicate edits with identical ranges
            edits.dedup_by(|(start1, end1, _), (start2, end2, _)| start1 == start2 && end1 == end2);

            let mut last_end = 0;
            for (start, end, _) in edits.iter() {
                if *start < last_end {
                    println!("Warning: Overlapping edits detected!");
                    break;
                }
                last_end = *end;
            }

            if args.dry_run {
                display_dry_run_results(entry.path(), &source, &edits);
            } else {
                // Apply edits in reverse order
                let mut updated_source = source.clone();
                for (start, end, replacement) in edits.iter() {
                    let line_number = updated_source[..*start].lines().count();
                    println!(
                        "Line {}: Replacing '{}' with '{}'",
                        line_number,
                        &updated_source[*start..*end],
                        replacement
                    );
                    updated_source.replace_range(*start..*end, replacement);
                }

                // Write the updated content back to the file
                let mut file = fs::File::create(entry.path()).unwrap();
                file.write_all(updated_source.as_bytes()).unwrap();

                println!("Updated file: {}", entry.path().display());
            }
        }
    }
}

fn process_functions(
    source: &str,
    root_node: tree_sitter::Node,
    edits: &mut Vec<(usize, usize, String)>,
) {
    let function_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), function_query())
        .expect("Failed to create function query");
    let call_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), call_query())
        .expect("Failed to create call query");
    let window_methods = get_window_methods();

    let mut function_query_cursor = QueryCursor::new();
    let mut matches = function_query_cursor.matches(&function_query, root_node, source.as_bytes());

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

                edits.push((start, end, new_param.to_string()));
            }
        } else {
            if let (Some(start), Some(end)) = (target_param_start, target_param_end) {
                edits.push((
                    start,
                    end,
                    "window: &mut Window, cx: &mut AppContext".to_string(),
                ));

                if let Some(body_node) = function_body_node {
                    let mut call_cursor = QueryCursor::new();
                    let mut calls = call_cursor.matches(&call_query, body_node, source.as_bytes());

                    while let Some(call) = calls.next() {
                        let func = call.captures.iter().find(|c| {
                            c.index == call_query.capture_index_for_name("func").unwrap()
                        });
                        let object = call.captures.iter().find(|c| {
                            c.index == call_query.capture_index_for_name("object").unwrap()
                        });
                        let method = call.captures.iter().find(|c| {
                            c.index == call_query.capture_index_for_name("method").unwrap()
                        });
                        let args = call.captures.iter().find(|c| {
                            c.index == call_query.capture_index_for_name("args").unwrap()
                        });

                        if let Some(args) = args {
                            if let (Some(object), Some(method)) = (object, method) {
                                // Handle object method calls
                                let object_text = &source[object.node.byte_range()];
                                let method_name = &source[method.node.byte_range()];

                                if object_text == param_name {
                                    if let Some(&needs_cx) = window_methods.get(method_name) {
                                        edits.push((
                                            object.node.start_byte(),
                                            object.node.end_byte(),
                                            "window".to_string(),
                                        ));

                                        if needs_cx {
                                            add_cx_to_args(source, args, edits);
                                        }
                                    }
                                }
                            } else if let Some(func) = func {
                                // Handle regular function calls
                                let func_name = &source[func.node.byte_range()];
                                if func_name == param_name {
                                    replace_args_with_window_and_cx(source, args, edits);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn get_window_methods() -> HashMap<&'static str, bool> {
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

fn display_dry_run_results(path: &std::path::Path, source: &str, edits: &[(usize, usize, String)]) {
    println!("Potential changes for file: {}", path.display());
    for (start, end, replacement) in edits.iter() {
        let start_line = source[..*start].lines().count();
        let end_line = source[..*end].lines().count();
        let context_start = source[..*start].rfind('\n').map_or(0, |i| i + 1);
        let _context_end = source[*end..].find('\n').map_or(source.len(), |i| *end + i);

        println!("Lines {}-{}:", start_line, end_line);
        println!("- {}", &source[context_start..*end]);
        println!("+ {}{}", &source[context_start..*start], replacement);
        println!();
    }
}

fn function_query() -> &'static str {
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

fn call_query() -> &'static str {
    r#"
        (call_expression
            function: [
                (identifier) @func
                (field_expression
                    value: (identifier) @object
                    field: (field_identifier) @method
                )
                (scoped_identifier
                    path: (_) @path
                    name: (identifier) @func
                )
            ]
            arguments: (arguments) @args
        )
    "#
}

fn add_cx_to_args(
    source: &str,
    args: &tree_sitter::QueryCapture,
    edits: &mut Vec<(usize, usize, String)>,
) {
    let args_start = args.node.start_byte();
    let args_end = args.node.end_byte();
    let existing_args = &source[args_start..args_end];

    if existing_args.trim() == "()" {
        // No existing arguments
        edits.push((args_start + 1, args_end - 1, "cx".to_string()));
    } else {
        // Existing arguments present
        edits.push((args_end - 1, args_end - 1, ", cx".to_string()));
    }
}

fn replace_args_with_window_and_cx(
    source: &str,
    args: &tree_sitter::QueryCapture,
    edits: &mut Vec<(usize, usize, String)>,
) {
    let args_start = args.node.start_byte();
    let args_end = args.node.end_byte();
    let existing_args = &source[args_start..args_end];

    if existing_args.trim() == "()" {
        // No existing arguments
        edits.push((args_start + 1, args_end - 1, "window, cx".to_string()));
    } else {
        // Existing arguments present
        edits.push((args_start + 1, args_start + 1, "window, cx, ".to_string()));
    }
}

fn process_imports(source: &str, parser: &mut Parser, edits: &mut Vec<(usize, usize, String)>) {
    let tree = parser.parse(source, None).unwrap();
    let root_node = tree.root_node();

    let import_query = Query::new(
        &tree_sitter_rust::LANGUAGE.into(),
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
        "#,
    )
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
            edits.push((start, end, "{Window, AppContext}".to_string()));
        } else if !window_imported {
            // Only add Window
            edits.push((start, end, "Window".to_string()));
        } else if !app_context_imported {
            // Only add AppContext
            edits.push((start, end, "AppContext".to_string()));
        }
        // If both are already imported, we don't need to do anything
    }
}
