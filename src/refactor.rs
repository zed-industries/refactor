use anyhow::{bail, Context as _, Result};
use clap::Parser as ClapParser;
use file_editor::*;
use scip_index::*;
use std::{collections::BTreeSet, iter, path::PathBuf, sync::Arc};
use streaming_iterator::StreamingIterator as _;
use tree_sitter::{Node, Parser, Query, QueryCursor};

mod file_editor;
mod scip_index;

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

pub fn main() -> Result<()> {
    let args = Args::parse();
    let mut refactor = Refactor::new(std::fs::canonicalize(&args.path)?)?;
    refactor.process();
    if args.dry {
        refactor.editor.display_dry_run_results();
    } else {
        refactor.editor.apply_edits()?;
    }
    Ok(())
}

struct Refactor {
    focus_path: PathBuf,
    editor: FileEditor,
    index: Index,
    use_list_query: Query,
}

impl Refactor {
    fn new(path: PathBuf) -> Result<Self> {
        let language = tree_sitter_rust::LANGUAGE.into();
        let mut parser = Parser::new();
        parser
            .set_language(&language)
            .context("Failed to set language for parser")?;

        let index_path = Index::find(&path)?;
        let root_folder = index_path.parent().unwrap().to_path_buf();
        let index = Index::load(&index_path)?;
        let focus_path = path
            .strip_prefix(&root_folder)
            .context("Failed to relativize path")?
            .to_path_buf();

        let editor = FileEditor::new(parser, root_folder.clone());

        let use_list_query = Query::new(&language, "(use_list) @use_list").unwrap();

        Ok(Self {
            focus_path,
            editor,
            index,
            use_list_query,
        })
    }

    fn process(&mut self) {
        println!(
            "Processing files starting with focus path: {:?}",
            self.focus_path
        );

        let process_document = |relative_path: &RelativePath| {
            relative_path.0.starts_with(&self.focus_path)
                && !relative_path.0.starts_with("crates/gpui/src")
        };

        let mut fns_taking_window = BTreeSet::new();
        let mut fns_taking_window_fn = BTreeSet::new();

        // Pass 1: Update signatures and detect which methods need to take a Window
        for (relative_path, document) in &self.index.documents {
            if !process_document(relative_path) {
                continue;
            }

            let file = self.editor.file(relative_path).unwrap();

            process_imports(file, &self.use_list_query);

            document.occurrences.iter().for_each(|occurrence| {
                if occurrence.symbol.text().ends_with("WindowContext#") {
                    if let Err(err) = process_window_context_mention(
                        file,
                        document,
                        occurrence,
                        &mut fns_taking_window,
                        &mut fns_taking_window_fn,
                    ) {
                        eprintln!("Error processing window context mention: {}", err);
                    }
                }
                if occurrence.symbol.text().ends_with("ViewContext#") {
                    if let Err(err) = process_view_context_mention(
                        file,
                        document,
                        occurrence,
                        &mut fns_taking_window,
                        &mut fns_taking_window_fn,
                    ) {
                        eprintln!("Error processing view context mention: {}", err);
                    }
                }
                if occurrence.symbol.text().ends_with("view/View#") {
                    process_view_mention(file, occurrence);
                }
            });
        }

        // Pass 2: Update fn calls to thread through a window
        for (relative_path, document) in &self.index.documents {
            if !process_document(relative_path) {
                continue;
            }

            let file = self.editor.file(relative_path).unwrap();

            document.occurrences.iter().for_each(|occurrence| {
                if let Symbol::Global(global_symbol) = &occurrence.symbol {
                    if let Some(replace_cx) = takes_window_arg(global_symbol, &fns_taking_window) {
                        if let Err(err) =
                            add_window_argument_before_cx(file, occurrence, replace_cx)
                        {
                            eprintln!("Error adding window parameter: {}", err);
                        }
                    }

                    if takes_window_fn(global_symbol, &fns_taking_window_fn) {
                        if let Err(err) = add_window_fn_param(file, occurrence) {
                            eprintln!("Error adding fn window parameter: {}", err);
                        }
                    }

                    if let Some(takes_cx) = relocated_to_window(global_symbol) {
                        if let Err(err) =
                            update_window_call(file, occurrence, takes_cx, &fns_taking_window_fn)
                        {
                            eprintln!("Error moving call to window: {}", err);
                        }
                    }

                    let (move_to_model_cx, new_name) = relocated_to_model_context(global_symbol);
                    if move_to_model_cx {
                        if let Err(err) = update_model_context_call(file, occurrence, new_name) {
                            eprintln!("Error updating model context call: {}", err);
                        }
                    }
                }
            });
        }
    }
}

fn process_window_context_mention(
    file: &mut File,
    document: &Document,
    occurrence: &Occurrence,
    fns_taking_window: &mut BTreeSet<GlobalSymbol>,
    fns_taking_window_fn: &mut BTreeSet<GlobalSymbol>,
) -> Result<()> {
    let Ok(node) = file.find_node(&occurrence.range) else {
        bail!("No node found at occurrence range");
    };

    let mut inside_function_type = false;
    for ancestor in node_ancestors(node) {
        match ancestor.kind() {
            // Replace "cx: &mut WindowContext" with "window: &mut Window, model: &mut AppContext"
            "parameter" if !inside_function_type => {
                let parameter_name =
                    file.node_text(ancestor.child_by_field_name("pattern").unwrap());
                let leading_underscore = if parameter_name.starts_with('_') {
                    "_"
                } else {
                    ""
                };

                file.record_edit(
                    ancestor.byte_range(),
                    format!("{leading_underscore}window: &mut Window, {parameter_name}: &mut AppContext")
                );
            }
            "function_type" => {
                // Inside an fn
                inside_function_type = true;
                file.record_edit(node.byte_range(), format!("Window, &mut AppContext"));
            }
            // Record this method so we can update calls to it in a second pass.
            "function_item" => {
                let name_node = ancestor.child_by_field_name("name").unwrap();
                let fn_occurrence = document.find_occurrence(&name_node.start_position())?;
                let fn_symbol = fn_occurrence.symbol.to_global().unwrap();

                if inside_function_type {
                    fns_taking_window_fn.insert(fn_symbol);
                } else {
                    fns_taking_window.insert(fn_symbol);
                }

                break;
            }
            _ => {}
        }
    }
    Ok(())
}

fn process_view_context_mention(
    file: &mut File,
    document: &Document,
    occurrence: &Occurrence,
    fns_taking_window: &mut BTreeSet<GlobalSymbol>,
    fns_taking_window_fn: &mut BTreeSet<GlobalSymbol>,
) -> Result<()> {
    let Ok(node) = file.find_node(&occurrence.range) else {
        bail!("No node found at occurrence range");
    };

    let mut generics = None;
    let mut inside_function_type = false;
    for ancestor in node_ancestors(node) {
        match ancestor.kind() {
            // Capture the <T> from ViewContext<T> for possible use later in a later loop iteration.
            "generic_type" => {
                generics = file.node_text(ancestor).strip_prefix("ViewContext");
            }
            // If we're inside a function type, we rewrite the function type parameter.
            "function_type" => {
                // Inside an fn
                inside_function_type = true;
                file.record_edit(node.byte_range(), format!("Window, &mut ModelContext"));
            }
            // Replace "cx: &mut ViewContext<T>" with "window: &mut Window, model: &mut ModelContext<T>"
            "parameter" if !inside_function_type => {
                if let Some(generics) = generics {
                    let parameter_name =
                        file.node_text(ancestor.child_by_field_name("pattern").unwrap());
                    let leading_underscore = if parameter_name.starts_with('_') {
                        "_"
                    } else {
                        ""
                    };

                    file.record_edit(
                        ancestor.byte_range(),
                        format!("{leading_underscore}window: &mut Window, {parameter_name}: &mut ModelContext{generics}")
                    );
                } else {
                    file.record_error(
                        ancestor.range().start_point.row,
                        "Missing generics types".to_string(),
                    );
                }
            }
            // Record this method so we can update calls to it in a second pass.
            "function_item" => {
                let name_node = ancestor.child_by_field_name("name").unwrap();
                let occurence = document.find_occurrence(&name_node.start_position())?;
                let fn_symbol = occurence.symbol.to_global().unwrap();

                if inside_function_type {
                    fns_taking_window_fn.insert(fn_symbol);
                } else {
                    fns_taking_window.insert(fn_symbol);
                }
            }
            _ => {}
        }
    }

    Ok(())
}

fn process_view_mention(file: &mut File, occurrence: &Occurrence) {
    let Ok(node) = file.find_node(&occurrence.range) else {
        return;
    };

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "generic_type" {
            let generics = file.node_text(ancestor).strip_prefix("View").unwrap();
            file.record_edit(ancestor.byte_range(), format!("Model{}", generics));
            break;
        }
    }
}

fn add_window_argument_before_cx(
    file: &mut File,
    occurrence: &Occurrence,
    replace_cx: bool,
) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "call_expression" {
            let arguments = ancestor.child_by_field_name("arguments").unwrap();
            let mut cursor = arguments.walk();
            for child in arguments.named_children(&mut cursor) {
                if file.node_text(child) == "cx" {
                    if replace_cx {
                        file.record_edit(child.byte_range(), "window".to_string());
                    } else {
                        file.record_edit(child.byte_range(), "window, cx".to_string());
                    }
                }
            }
            break;
        }
    }

    Ok(())
}

fn add_window_fn_param(file: &mut File, occurrence: &Occurrence) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    for ancestor in [node].into_iter().chain(node_ancestors(node)) {
        if ancestor.kind() == "call_expression" {
            let arguments = ancestor.child_by_field_name("arguments").unwrap();
            let mut cursor = arguments.walk();
            for child in arguments.named_children(&mut cursor) {
                if child.kind() == "closure_expression" {
                    let parameters = child.child_by_field_name("parameters").unwrap();
                    let last_parameter = parameters.child(parameters.child_count() - 2).unwrap();
                    let replacement = if file.node_text(last_parameter) == "_" {
                        "_, "
                    } else if file.node_text(last_parameter).starts_with("_") {
                        "_window, "
                    } else {
                        "window, "
                    };
                    file.record_edit(
                        last_parameter.start_byte()..last_parameter.start_byte(),
                        replacement.to_string(),
                    );
                }
            }
            break;
        }
    }

    Ok(())
}

fn update_window_call(
    file: &mut File,
    occurrence: &Occurrence,
    takes_cx: bool,
    fns_taking_window_fn: &BTreeSet<GlobalSymbol>,
) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "field_expression" {
            let receiver = ancestor.child_by_field_name("value").unwrap();
            file.record_edit(receiver.byte_range(), "window".to_string());
        }

        if ancestor.kind() == "call_expression" {
            if takes_cx {
                let arguments = ancestor.child_by_field_name("arguments").unwrap();
                let last_child = arguments.child(arguments.child_count() - 1).unwrap();

                if takes_window_fn(
                    &occurrence.symbol.to_global().unwrap(),
                    &fns_taking_window_fn,
                ) || last_child.prev_sibling().map_or(false, |prev_child| {
                    prev_child.kind() == "closure_expression"
                }) {
                    let insert_at = if arguments.named_child_count() == 1 {
                        arguments.child(0).unwrap().end_byte()
                    } else {
                        arguments
                            .child(arguments.child_count() - 3) // The comma before the closure
                            .unwrap()
                            .end_byte()
                    };
                    file.record_edit(insert_at..insert_at, "cx, ".to_string());
                } else {
                    let replacement = if arguments.named_child_count() == 0 {
                        "cx".to_string()
                    } else if last_child
                        .prev_sibling()
                        .map_or(false, |prev_child| prev_child.kind() == ",")
                    {
                        "cx".to_string()
                    } else {
                        ", cx".to_string()
                    };

                    file.record_edit(
                        last_child.start_byte()..last_child.start_byte(),
                        replacement,
                    );
                }
            }

            // Rewrite window.with_foo(|cx| {}) to window.with_foo(|window| {})
            if occurrence.symbol.text().contains("]with_") {
                println!(
                    "Rewriting with_foo closure argument from cx to window for: {:?}",
                    occurrence.symbol.text()
                );
                let arguments = ancestor.child_by_field_name("arguments").unwrap();
                let mut cursor = arguments.walk();
                cursor.goto_first_child();
                'outer: loop {
                    println!("Node kind: {}", cursor.node().kind());
                    if cursor.node().kind() == "closure_expression" {
                        println!(
                            "Rewriting with_foo closure argument from cx to window for: {:?}",
                            occurrence.symbol.text()
                        );
                        cursor.reset(cursor.node().child_by_field_name("parameters").unwrap());
                        cursor.goto_first_child();

                        loop {
                            if file.node_text(cursor.node()) == "cx" {
                                file.record_edit(cursor.node().byte_range(), "window".to_string());
                                break 'outer;
                            }
                            if !cursor.goto_next_sibling() {
                                break;
                            }
                        }
                    }
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }

            break;
        }
    }

    Ok(())
}

fn update_model_context_call(
    file: &mut File,
    occurrence: &Occurrence,
    new_name: Option<&'static str>,
) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "field_expression" {
            if let Some(new_name) = new_name {
                let field_name = ancestor.child_by_field_name("field").unwrap();
                file.record_edit(field_name.byte_range(), new_name.to_string());
            }
        }

        if ancestor.kind() == "call_expression" {
            let arguments = ancestor.child_by_field_name("arguments").unwrap();
            if arguments.named_child_count() == 0 {
                file.record_edit(arguments.byte_range(), "(window)".to_string());
            } else if arguments.named_child_count() == 1 {
                let insert_at = arguments.child(0).unwrap().end_byte();
                file.record_edit(insert_at..insert_at, "window, ".to_string());
            } else {
                let insert_at = arguments
                    .named_child(arguments.named_child_count() - 1)
                    .unwrap()
                    .start_byte();
                file.record_edit(insert_at..insert_at, "window, ".to_string());
            }

            break;
        }
    }

    Ok(())
}

fn relocated_to_model_context(symbol: &GlobalSymbol) -> (bool, Option<&'static str>) {
    if let Some(new_name) = match symbol.0.as_ref() {
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]defer()." => {
            Some("defer_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe()." => {
            Some("observe_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]subscribe()." => {
            Some("subscribe_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_release()." => {
            Some("subscribe_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_release()." => {
            Some("observe_release_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_global()." => {
            Some("observe_global_in")
        }
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]spawn()." => {
            Some("spawn_in")
        }
        _ => None,
    } {
        return (true, Some(new_name));
    }

    let not_renamed = [
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_next_frame().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_bounds().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_activation().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_appearance().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_keystrokes().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_pending_input().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_in().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_blur().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_lost().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_out().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]focus_self()."
    ];

    if not_renamed.contains(&symbol.0.as_ref()) {
        (true, None)
    } else {
        (false, None)
    }
}

fn takes_window_arg(
    symbol: &GlobalSymbol,
    fns_with_window_param: &BTreeSet<GlobalSymbol>,
) -> Option<bool> {
    let replace_cx = [
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]contains().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]dispatch_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]focus().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]is_focused().",
    ];
    let preserve_cx = [
        "rust-analyzer cargo gpui 0.1.0 text_system/line/impl#[ShapedLine]paint().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]contains().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]contains_focused().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]dispatch_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]focus().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]is_focused().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[FocusHandle]within_focused().",
    ];

    if replace_cx.contains(&symbol.0.as_ref()) {
        Some(true)
    } else if preserve_cx.contains(&symbol.0.as_ref()) {
        Some(false)
    } else if symbol.0.ends_with("[Element]request_layout().")
        || symbol.0.ends_with("[Element]prepaint().")
        || symbol.0.ends_with("[Element]paint().")
    {
        Some(false)
    } else if fns_with_window_param.contains(symbol) {
        Some(false)
    } else {
        None
    }
}

fn takes_window_fn(symbol: &GlobalSymbol, fns_taking_window_fn: &BTreeSet<GlobalSymbol>) -> bool {
    let window_fn_symbols = [
        "rust-analyzer cargo gpui 0.1.0 app/async_context/impl#[AsyncWindowContext]update().",
        "rust-analyzer cargo gpui 0.1.0 app/impl#[AppContext]observe_keystrokes().",
        "rust-analyzer cargo gpui 0.1.0 app/impl#[AppContext]open_window().",
        "rust-analyzer cargo gpui 0.1.0 elements/canvas/canvas().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#block_mouse_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#can_drop().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#capture_action().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#capture_any_mouse_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#capture_any_mouse_up().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#capture_key_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#capture_key_up().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#drag_over().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#occlude().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_action().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_any_mouse_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_boxed_action().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_drag_move().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_drop().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_key_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_key_up().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_modifiers_changed().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_mouse_down().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_mouse_down_out().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_mouse_move().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_mouse_up().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_mouse_up_out().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/InteractiveElement#on_scroll_wheel().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/StatefulInteractiveElement#on_click().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/StatefulInteractiveElement#on_drag().",
        "rust-analyzer cargo gpui 0.1.0 elements/div/StatefulInteractiveElement#on_hover().",
        "rust-analyzer cargo gpui 0.1.0 elements/uniform_list/uniform_list().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[AnyWindowHandle]update().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]defer().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]focus_self().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]listener().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_global().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_keystrokes().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_keystrokes().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_pending_input().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_release().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_activation().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_activation().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_appearance().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_appearance().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_bounds().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]observe_window_bounds().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_blur().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_in().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_lost().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_focus_out().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_next_frame().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]on_release().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]subscribe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][Context]update_window().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][Context]update_window().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]new_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]defer().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]defer().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]handler_for().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]listener_for().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe_global().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe_release().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_focus_in().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_focus_out().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_key_event().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_modifiers_changed().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_mouse_event().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_next_frame().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_window_should_close().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_measured_layout().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]subscribe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowHandle<V>`]update().",
    ];
    window_fn_symbols.contains(&symbol.0.as_ref()) || fns_taking_window_fn.contains(&symbol)
}

fn relocated_to_window(symbol: &GlobalSymbol) -> Option<bool> {
    let with_cx = [
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'_, V>`][VisualContext]dismiss_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'_, V>`][VisualContext]focus_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'_, V>`][VisualContext]new_view().",
        "rust-analyzer cargo gpui 0.1.0 VisualContext#new_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'_, V>`][VisualContext]replace_root_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'_, V>`][VisualContext]update_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]dismiss_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]focus_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]new_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]replace_root_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'_>`][VisualContext]update_view().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]appearance_changed().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]defer().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_action().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_keystroke_observers().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]display().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]focused().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]handle_input().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]is_action_available().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]notify().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe_global().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]observe_release().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_window_should_close().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_svg().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_svg().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]prompt().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_layout().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]spawn().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]subscribe().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]to_async().",
        "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]use_asset().",
    ];

    let without_cx = [
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]activate_window().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]remove_window().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]all_bindings_for_input().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]appearance().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]available_actions().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]bindings_for_action().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]bindings_for_action_in().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]blur().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]bounds().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]bounds_changed().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]build_custom_prompt().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]clear_pending_keystrokes().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]complete_frame().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]compute_layout().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]content_mask().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]context_stack().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]default_prevented().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]defer_draw().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]disable_focus().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_action_on_node().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_key_down_up_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_key_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_keystroke().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_modifiers_changed_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]dispatch_mouse_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]draw().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]draw_roots().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]drop_image().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]element_offset().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]element_opacity().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]finish_dispatch_key_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]focus().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]gpu_specs().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]handle_input().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]handler_for().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]has_pending_keystrokes().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]inner_window_bounds().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]insert_hitbox().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]invalidate_character_coordinates().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]is_fullscreen().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]is_maximized().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]is_window_active().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]is_window_hovered().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]keystroke_text_for().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]layout_bounds().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]line_height().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]listener_for().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]minimize_window().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]modifiers().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]mouse_position().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_action().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_focus_in().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_focus_out().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_key_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_modifiers_changed().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_mouse_event().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]on_next_frame().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_deferred_draws().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_emoji().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_emoji().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_glyph().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_glyph().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_image().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_image().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_index().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_layer().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_layer().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_path().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_path().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_quad().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_quad().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_shadows().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_shadows().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_strikethrough().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_strikethrough().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_surface().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_surface().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_underline().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]paint_underline().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]parent_view_id().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]pending_input_changed().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]pending_input_keystrokes().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]prepaint_deferred_draws().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]prepaint_index().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]prepaint_tooltip().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]present().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]prevent_default().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]refresh().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]rem_size().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]replay_pending_input().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_animation_frame().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_autoscroll().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_decorations().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]request_measured_layout().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]reset_cursor_style().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]reuse_paint().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]reuse_prepaint().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]scale_factor().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_app_id().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_background_appearance().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_client_inset().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_cursor_style().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_focus_handle().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_key_context().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_rem_size().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_tooltip().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_view_id().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_window_edited().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]set_window_title().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]show_character_palette().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]show_window_menu().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]start_window_move().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]start_window_resize().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]take_autoscroll().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]text_style().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]text_system().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]toggle_fullscreen().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]transact().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]viewport_size().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]window_bounds().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]window_controls().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]window_decorations().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]window_handle().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_absolute_element_offset().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_content_mask().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_element_namespace().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_element_offset().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_element_opacity().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_element_state().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_optional_element_state().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_rem_size().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]with_text_style().",
            "rust-analyzer cargo gpui 0.1.0 window/impl#[`WindowContext<'a>`]zoom_window().",
    ];

    if with_cx.contains(&symbol.0.as_ref()) {
        Some(true)
    } else if without_cx.contains(&symbol.0.as_ref()) {
        Some(false)
    } else {
        None
    }
}

fn process_imports(file: &mut File, use_list_query: &Query) {
    let mut query_cursor = QueryCursor::new();
    let mut matches =
        query_cursor.matches(&use_list_query, file.tree.root_node(), file.text.as_bytes());

    while let Some(m) = matches.next() {
        let capture = m.captures.iter().next().unwrap();

        let mut imported_view_context = false;
        let mut imported_window_context = false;
        let mut imported_model_context = false;
        let mut imported_app_context = false;
        let mut imported_window = false;
        let mut imported_view = false;
        let mut imported_model = false;

        let mut cursor = capture.node.walk();
        cursor.goto_first_child();
        loop {
            let child_text = file.node_text(cursor.node());
            if child_text == "ViewContext" {
                imported_view_context = true;
                let mut deletion_range = cursor.node().byte_range();
                if cursor.goto_next_sibling() && file.node_text(cursor.node()) == "," {
                    deletion_range.end = cursor.node().byte_range().end;
                }
                file.record_edit(deletion_range, "".to_string());
            } else if child_text == "WindowContext" {
                imported_window_context = true;
                let mut deletion_range = cursor.node().byte_range();
                if cursor.goto_next_sibling() && file.node_text(cursor.node()) == "," {
                    deletion_range.end = cursor.node().byte_range().end;
                }
                file.record_edit(deletion_range, "".to_string());
            } else if child_text == "ModelContext" {
                imported_model_context = true;
            } else if child_text == "AppContext" {
                imported_app_context = true;
            } else if child_text == "Window" {
                imported_window = true;
            } else if child_text == "View" {
                imported_view = true;
                let mut deletion_range = cursor.node().byte_range();
                if cursor.goto_next_sibling() && file.node_text(cursor.node()) == "," {
                    deletion_range.end = cursor.node().byte_range().end;
                }
                file.record_edit(deletion_range, "".to_string());
            } else if child_text == "Model" {
                imported_model = true;
            }

            if !cursor.goto_next_sibling() {
                break;
            };
        }

        let mut cursor = capture.node.walk();
        cursor.goto_first_child();

        if file.node_text(cursor.node()) == "{" {
            let insert_at = cursor.node().byte_range().end;

            if imported_view_context {
                if !imported_window {
                    file.record_edit(insert_at..insert_at, "Window, ".to_string());
                    imported_window = true;
                }
                if !imported_model_context {
                    file.record_edit(insert_at..insert_at, "ModelContext, ".to_string());
                }
            }

            if imported_window_context {
                if !imported_window {
                    file.record_edit(insert_at..insert_at, "Window, ".to_string());
                }
                if !imported_app_context {
                    file.record_edit(insert_at..insert_at, "AppContext, ".to_string());
                }
            }

            if imported_view && !imported_model {
                file.record_edit(insert_at..insert_at, "Model, ".to_string());
            }
        } else {
            println!("Unsupported import format");
        }
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
