use clap::Parser as ClapParser;
use std::{collections::HashSet, fs, io::Write};
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
    let method_call_query = Query::new(&tree_sitter_rust::LANGUAGE.into(), method_call_query())
        .expect("Failed to create method call query");
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
    println!("Function query capture indices:");
    println!("  function_name: {}", function_name_index);
    println!("  param_name: {}", param_name_index);
    println!("  param_type: {}", param_type_index);
    println!("  target_param: {}", target_param_index);
    println!("  function_body: {}", function_body_index);

    let object_index = method_call_query.capture_index_for_name("object").unwrap();
    let method_index = method_call_query.capture_index_for_name("method").unwrap();
    println!("Method call query capture indices:");
    println!("  object: {}", object_index);
    println!("  method: {}", method_index);

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
                    let mut method_call_cursor = QueryCursor::new();
                    let mut method_calls = method_call_cursor.matches(
                        &method_call_query,
                        body_node,
                        source.as_bytes(),
                    );

                    while let Some(method_call) = method_calls.next() {
                        let object = method_call
                            .captures
                            .iter()
                            .find(|c| c.index == object_index);
                        let method = method_call
                            .captures
                            .iter()
                            .find(|c| c.index == method_index);

                        if let (Some(object), Some(method)) = (object, method) {
                            if &source[object.node.byte_range()] == param_name {
                                let method_name = &source[method.node.byte_range()];
                                let new_object = if window_methods.contains(method_name) {
                                    "window"
                                } else {
                                    "cx"
                                };

                                edits.push((
                                    object.node.start_byte(),
                                    object.node.end_byte(),
                                    new_object.to_string(),
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
}

fn get_window_methods() -> HashSet<&'static str> {
    [
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
    .collect()
}

fn display_dry_run_results(path: &std::path::Path, source: &str, edits: &[(usize, usize, String)]) {
    return;
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

fn method_call_query() -> &'static str {
    r#"
        (call_expression
            function: (field_expression
                value: (identifier) @object
                field: (field_identifier) @method
            )
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
