use anyhow::{Context as _, Result};
use clap::Parser as ClapParser;
use refactor::{calls_query::*, functions_query::*, imports_query::*, scip_index::*};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::File,
    io::Read,
    ops::Range,
    path::PathBuf,
};
use tree_sitter::Parser;

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

        let index_path = Index::find(path)?;
        let index_folder = index_path.parent().unwrap().to_path_buf();
        let index = Index::load(&index_path)?;

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
                .unwrap_or(file_path)
                .to_path_buf()
                .into();
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

        let function_occurrence = match self
            .index
            .find_occurrence(&name_node.start_position(), relative_path)
        {
            Ok(occurrence) => occurrence,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        };

        let parent_symbol = function_occurrence.symbol.clone();

        for_each_call(body_node, source, |call| {
            if let Some(method) = call.method {
                match self
                    .index
                    .find_occurrence(&method.start_position(), relative_path)
                {
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
