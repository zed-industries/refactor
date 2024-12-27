use anyhow::{anyhow, Context as _, Result};
use clap::Parser as ClapParser;
use itertools::Itertools;
use refactor::{file_editor::*, flow_analysis::*, query_functions::*, scip_index::*};
use std::{
    collections::{HashMap, HashSet},
    io::Read,
    iter,
    ops::Range,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};
use tree_sitter::{Node, Parser, Point};

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

fn main() -> Result<()> {
    let args = Args::parse();
    let mut refactor = Refactor::new(&std::fs::canonicalize(&args.path)?)?;
    refactor.process();
    if args.dry {
        refactor.editor.display_dry_run_results();
    } else {
        refactor.editor.apply_edits()?;
    }
    Ok(())
}

struct Refactor {
    editor: FileEditor,
    index: Index,
    root_folder: PathBuf,
    flow_analysis_input: Vec<(PlaceId, Place<PlaceId, Requirements>)>,
}

impl Refactor {
    fn new(path: &PathBuf) -> Result<Self> {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .context("Failed to set language for parser")?;

        let index_path = Index::find(path)?;
        let root_folder = index_path.parent().unwrap().to_path_buf();
        let index = Index::load(&index_path)?;

        let editor = FileEditor::new(parser, root_folder.clone());

        Ok(Self {
            editor,
            index,
            root_folder,
            flow_analysis_input: Vec::new(),
        })
    }

    fn process(&mut self) {
        let locals = self.all_window_context_locals();

        // display_locals(&signature_to_locals)

        for local_info in locals.into_iter() {
            let signature: String = local_info.type_signature.to_string();
            if signature != "&mut WindowContext"
                && signature != "&mut WindowContext<'a>"
                && signature != "&mut WindowContext<'_>"
                && signature != "&WindowContext<'_>"
            {
                println!("ERROR: Unexpected type_signature in {local_info:?}");
                continue;
            }

            match self.analyze_local_variable(&local_info) {
                Ok(()) => {}
                Err(e) => {
                    let file = match self.editor.file(&local_info.id.relative_path) {
                        Ok(it) => it,
                        Err(e) => {
                            eprintln!("ERROR: {e:?}");
                            continue;
                        }
                    };
                    file.record_error(local_info.definition.range.start.row, format!("{e}"));
                }
            }
        }

        /*
        self.flow_analysis_input
            .iter()
            .chunk_by(|(id, _)| id)
            .into_iter()
            .map(|(id, places)| Place::lattice_merge(places.map(|(_, place)| place)));
            */
    }

    fn all_window_context_locals(&self) -> Vec<LocalInfo> {
        let mut results: Vec<LocalInfo> = Vec::new();
        for document in self.index.documents.values() {
            if document.relative_path.0
                == PathBuf::from_str("crates/gpui/src/window.rs")
                    .unwrap()
                    .into()
            {
                continue;
            }
            for (symbol, symbol_info) in document.locals.iter() {
                let local_info = match LocalInfo::compute(
                    document,
                    symbol,
                    symbol_info,
                    is_window_context_signature,
                ) {
                    Ok(Some(local_info)) => local_info,
                    Ok(None) => continue,
                    Err(e) => {
                        eprintln!("ERROR: {e:?}");
                        continue;
                    }
                };
                results.push(local_info)
            }
        }
        results
    }

    fn analyze_local_variable(&mut self, local_info: &LocalInfo) -> Result<()> {
        let place_id = PlaceId::LocalVariable(local_info.id.clone());
        self.analyze_local_variable_definition(&local_info, &place_id)?;
        self.analyze_local_variable_references(&local_info, &place_id)?;

        Ok(())
    }

    fn analyze_local_variable_definition(
        &mut self,
        local_info: &LocalInfo,
        place_id: &PlaceId,
    ) -> Result<Option<ParameterAnalysisResult>> {
        let relative_path = &local_info.id.relative_path;
        let definition = &local_info.definition;
        let file = self.editor.file(relative_path)?;
        let document = self.index.document(relative_path)?;
        let node = file.find_node(&definition.range)?;

        let parent = node.parent().unwrap().parent().unwrap();
        let (result, requirements) = match parent.grammar_name() {
            "parameters" => {
                let match_ = function_match_containing_node(&file, parent)?;
                let result = ParameterAnalysisResult {
                    parameter: match_.target_param.range(),
                    parameter_type: match_.param_type.range(),
                    parameter_name: node.range(),
                };
                let function_name = match_
                    .name
                    .ok_or_else(|| anyhow!("Expected function to have a name?!?"))?;
                let function_symbol = document
                    .find_occurrence(&function_name.start_position())?
                    .symbol
                    .clone();
                let function_place_id = match function_symbol {
                    Symbol::Local(symbol) => {
                        return Err(anyhow!(
                            "Expected function to have a global symbol, instead got {symbol:?}"
                        ))
                    }
                    Symbol::Global(symbol) => PlaceId::FunctionSignature(symbol),
                };
                self.flow_analysis_input.push((
                    function_place_id,
                    Place {
                        initial_analysis: Requirements::default(),
                        flows_from: vec![place_id.clone()],
                    },
                ));
                (Some(result), Requirements::default())
            }
            "closure_expression" | "closure_parameters" => (
                None,
                Requirements {
                    window: true,
                    app_context: true,
                },
            ),
            "token_tree" => {
                // Can't refactor within a macro invocation.
                return Ok(None);
            }
            _ => {
                let display_name = local_info.symbol_info.display_name.clone();
                return Err(anyhow!(
                    "Can't handle {} ({:?}) being defined within {:?}",
                    display_name,
                    definition.range,
                    node_ancestors(node).collect_vec(),
                ));
            }
        };

        self.flow_analysis_input.push((
            place_id.clone(),
            Place {
                initial_analysis: requirements,
                flows_from: Vec::new(),
            },
        ));

        Ok(result)
    }

    fn analyze_local_variable_references(
        &mut self,
        local_info: &LocalInfo,
        place_id: &PlaceId,
    ) -> Result<()> {
        let relative_path = &local_info.id.relative_path;
        let file = self.editor.file(relative_path)?;
        let document = self.index.document(relative_path)?;

        for reference in local_info.references.iter() {
            let node = file.find_node(&reference.range)?.clone();
            let parent0 = node.parent().unwrap();
            let parent1 = parent0.parent().unwrap();
            let parent2 = parent1.parent().unwrap();
            match (
                node.grammar_name(),
                parent0.grammar_name(),
                parent1.grammar_name(),
                parent2.grammar_name(),
            ) {
                ("identifier", "arguments", "call_expression", _) => {}
                ("identifier", "field_expression", "call_expression", _) => {}
                ("identifier", "field_expression", "generic_function", "call_expression") => {}
                ("identifier", "token_tree", _, _) => {}
                _ => {
                    file.record_info(
                        reference.range.start.row,
                        format!(
                            "{:?} {:?}",
                            node.grammar_name(),
                            node_ancestors(node).collect_vec()
                        ),
                    );
                }
            }
        }

        Ok(())
    }

    fn display_locals(&self, locals: Vec<LocalInfo>) {
        for (signature, infos) in &locals
            .into_iter()
            .chunk_by(|local_info| local_info.type_signature.clone())
        {
            println!();
            println!("Locals with signature `{signature}`:");
            for info in infos {
                println!(
                    "  {} at {}",
                    info.symbol_info.display_name,
                    info.definition.range_string(&info.id.relative_path)
                );
                for reference in info.references.iter() {
                    println!("    {}", reference.range_string(&info.id.relative_path));
                }
            }
        }
    }
}

fn is_window_context_signature(signature: &str) -> bool {
    signature.contains("WindowContext")
    && !signature.contains("Fn")
    // TODO: handle AsyncWindowContext?
    && !signature.contains("AsyncWindowContext")
}

fn node_and_ancestor_names(mut node: Node) -> Vec<String> {
    let mut names = Vec::new();
    loop {
        names.push(node.grammar_name().to_owned());
        node = match node.parent() {
            Some(node) => node,
            None => return names,
        }
    }
}

#[derive(Debug, Clone)]
struct LocalInfo {
    id: LocalId,
    type_signature: Arc<str>,
    symbol_info: SymbolInformation,
    definition: Occurrence,
    references: Vec<Occurrence>,
}

impl LocalInfo {
    fn compute(
        document: &Document,
        symbol: &LocalSymbol,
        symbol_info: &SymbolInformation,
        signature_predicate: impl FnOnce(&str) -> bool,
    ) -> Result<Option<LocalInfo>> {
        let Some(signature) = symbol_info.signature_documentation.as_ref() else {
            return Ok(None);
        };
        let signature = &signature.text;
        if !signature_predicate(signature) {
            return Ok(None);
        }
        let type_signature = match signature.find(|c| c == ':') {
            Some(colon_position) => signature[colon_position + 2..].to_owned(),
            None => {
                return Err(anyhow!("{signature} does not contain ':'"));
            }
        };

        let (definition, references) = document.find_local_definition_and_references(symbol)?;
        Ok(Some(LocalInfo {
            id: LocalId {
                relative_path: document.relative_path.clone(),
                symbol: symbol.clone(),
            },
            type_signature: type_signature.into(),
            symbol_info: symbol_info.clone(),
            definition: definition.clone(),
            references: references.into_iter().map(|occ| occ.clone()).collect(),
        }))
    }
}

struct ParameterAnalysisResult {
    parameter: tree_sitter::Range,
    parameter_type: tree_sitter::Range,
    parameter_name: tree_sitter::Range,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum PlaceId {
    LocalVariable(LocalId),
    FunctionSignature(GlobalSymbol),
}

// TODO: cause tracking?
#[derive(Debug, Clone, Default)]
struct Requirements {
    window: bool,
    app_context: bool,
}

impl LatticeMerge for Requirements {
    fn lattice_merge(iterator: impl Iterator<Item = Self>) -> Self {
        let mut window = false;
        let mut app_context = false;
        for requirements in iterator {
            window |= requirements.window;
            app_context |= requirements.app_context;
        }
        Requirements {
            window,
            app_context,
        }
    }
}

fn function_match_containing_node<'a>(file: &'a File, node: Node<'a>) -> Result<FunctionMatch<'a>> {
    let Some(function_node) =
        node_ancestors(node).find(|node| node.grammar_name() == "function_item")
    else {
        return Err(anyhow!(
            "Expected to find function_item in ancestors. Got: {:?}",
            node_ancestors(node).collect_vec()
        ));
    };
    let mut result = Err(anyhow!("Function query returned no results."));
    for_each_function_that_has_window_context_param(function_node, file.text(), |match_| {
        if result.is_ok() {
            result = Err(anyhow!("Function query returned multiple results."));
        }
        result = Ok(match_);
    });
    result
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
