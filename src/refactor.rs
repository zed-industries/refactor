use std::collections::HashSet;
use std::fs;
use std::io::Write;
use streaming_iterator::StreamingIterator;
use tree_sitter::QueryMatch;
use tree_sitter::{Language, Parser, Query, QueryCursor};
use walkdir::WalkDir;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    let language: Language = tree_sitter_rust::LANGUAGE.into();

    let mut parser = Parser::new();
    parser
        .set_language(&language)
        .expect("Error loading Rust grammar");

    let function_query = Query::new(&language, get_function_query_string())
        .expect("Failed to create function query");

    let method_call_query = Query::new(&language, get_method_call_query_string())
        .expect("Failed to create method call query");

    let window_methods: HashSet<&str> = [
        "window_handle",
        "refresh",
        "notify",
        "remove_window",
        "focused",
        "focus",
        "blur",
        "disable_focus",
        "text_system",
        "text_style",
        "is_maximized",
        "request_decorations",
        "start_window_resize",
        "window_bounds",
        "dispatch_action",
        "defer",
        "observe",
        "subscribe",
        "observe_release",
        "activate_window",
        "minimize_window",
        "toggle_fullscreen",
        "invalidate_character_coordinates",
        "prompt",
        "observe_global",
        "dispatch_keystroke",
        "keystroke_text_for",
        "dispatch_event",
        "has_pending_keystrokes",
        "pending_input_keystrokes",
        "bounds",
        "is_fullscreen",
        "appearance",
        "viewport_size",
        "is_window_active",
        "is_window_hovered",
        "zoom_window",
        "show_window_menu",
        "start_window_move",
        "set_client_inset",
        "window_decorations",
        "window_controls",
        "set_window_title",
        "set_app_id",
        "set_background_appearance",
        "set_window_edited",
        "display",
        "show_character_palette",
        "scale_factor",
        "rem_size",
        "set_rem_size",
        "with_rem_size",
        "line_height",
        "prevent_default",
        "default_prevented",
        "is_action_available",
        "mouse_position",
        "modifiers",
        "gpu_specs",
    ]
    .iter()
    .cloned()
    .collect();

    let path = std::fs::canonicalize(&args[1]).expect("Failed to canonicalize path");
    let relative_path = path
        .strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(&path);

    for entry in WalkDir::new(relative_path)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.path().extension().map_or(false, |ext| ext == "rs") {
            let mut source = fs::read_to_string(entry.path()).unwrap();
            let tree = parser.parse(&source, None).unwrap();
            let root_node = tree.root_node();

            let mut edits = Vec::new();

            // Process imports
            process_imports(&source, &mut parser, &mut edits);

            let mut function_query_cursor = QueryCursor::new();
            let mut matches =
                function_query_cursor.matches(&function_query, root_node, source.as_bytes());

            while let Some(match_) = matches.next() {
                process_function_match(
                    match_,
                    &source,
                    &method_call_query,
                    &window_methods,
                    &mut edits,
                );
            }

            // Sort edits in reverse order of their start positions
            edits.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));

            // Apply edits in reverse order
            for (start, end, replacement) in edits.into_iter() {
                source.replace_range(start..end, &replacement);
            }

            // Write the updated content back to the file
            let mut file = fs::File::create(entry.path()).unwrap();
            file.write_all(source.as_bytes()).unwrap();

            println!("Updated file: {}", entry.path().display());
        }
    }
}

fn get_function_query_string() -> &'static str {
    r#"
        [
          (function_item
            name: (identifier) @function_name
            parameters: (parameters
              (parameter
                pattern: (identifier) @param_name
                type: (reference_type
                  (mutable_specifier)
                  type: [
                    (type_identifier) @param_type
                    (scoped_type_identifier
                      name: (type_identifier) @param_type)
                  ]
                )
              ) @target_param
              (#eq? @param_type "WindowContext")
            )
            body: (block) @function_body
          )
          (function_signature_item
            name: (identifier) @function_name
            parameters: (parameters
              (parameter
                pattern: (identifier) @param_name
                type: (reference_type
                  (mutable_specifier)
                  type: [
                    (type_identifier) @param_type
                    (scoped_type_identifier
                      name: (type_identifier) @param_type)
                  ]
                )
              ) @target_param
              (#eq? @param_type "WindowContext")
            )
          )
          (function_type
            parameters: (parameters
              (reference_type
                (mutable_specifier)
                type: [
                  (type_identifier) @param_type
                  (scoped_type_identifier
                    name: (type_identifier) @param_type)
                ]
              ) @target_param
              (#eq? @param_type "WindowContext")
            )
          )
        ]
    "#
}

fn get_method_call_query_string() -> &'static str {
    r#"
        (call_expression
            function: (field_expression
                value: (identifier) @object
                field: (field_identifier) @method
            )
        )
    "#
}

fn get_import_query_string() -> &'static str {
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
      (#eq? @import_name "WindowContext")
    )
    "#
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
        let mut path = "";
        let mut import_name = "";
        let mut import_start = 0;
        let mut import_end = 0;

        for capture in match_.captures {
            match import_query.capture_names()[capture.index as usize] {
                "path" => path = &source[capture.node.byte_range()],
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

fn process_function_match(
    match_: &QueryMatch,
    source: &str,
    method_call_query: &Query,
    window_methods: &HashSet<&str>,
    edits: &mut Vec<(usize, usize, String)>,
) {
    let mut param_name = "";
    let mut target_param_start = 0;
    let mut target_param_end = 0;
    let mut function_body_node = None;

    for capture in match_.captures {
        match capture.index as usize {
            1 => param_name = &source[capture.node.byte_range()],
            3 => {
                target_param_start = capture.node.start_byte();
                target_param_end = capture.node.end_byte();
            }
            4 => {
                function_body_node = Some(capture.node);
            }
            _ => {}
        }
    }

    // Check if it's a function type
    let is_function_type = match_
        .captures
        .iter()
        .any(|c| c.node.kind() == "function_type");

    if is_function_type {
        // Replace the WindowContext parameter for function types
        edits.push((
            target_param_start,
            target_param_end,
            "&mut Window, &mut AppContext".to_string(),
        ));
    } else {
        // Replace the WindowContext parameter for regular functions
        edits.push((
            target_param_start,
            target_param_end,
            "window: &mut Window, cx: &mut AppContext".to_string(),
        ));

        // Update the function body
        if let Some(body_node) = function_body_node {
            let mut method_call_cursor = QueryCursor::new();
            let mut method_calls =
                method_call_cursor.matches(method_call_query, body_node, source.as_bytes());

            while let Some(method_call) = method_calls.next() {
                for capture in method_call.captures {
                    if capture.index == 0 {
                        if &source[capture.node.byte_range()] == param_name {
                            let method_node = method_call.captures[1].node;
                            let method_name = &source[method_node.byte_range()];
                            let new_object = if window_methods.contains(method_name) {
                                "window"
                            } else {
                                "cx"
                            };
                            edits.push((
                                capture.node.start_byte(),
                                capture.node.end_byte(),
                                new_object.to_string(),
                            ));
                        }
                    }
                }
            }
        }
    }
}
