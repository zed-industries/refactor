use anyhow::{anyhow, Context as _, Result};
use clap::Parser as ClapParser;
use refactor::{file_editor::*, scip_index::*};
use std::{collections::BTreeSet, iter, path::PathBuf, sync::Arc};
use streaming_iterator::StreamingIterator as _;
use tree_sitter::{Node, Parser, Query, QueryCursor};

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

        let mut fns_with_window_param = BTreeSet::new();

        // Pass 1: Update signatures and detect which methods need to take a Window
        for (relative_path, document) in &self.index.documents {
            if !process_document(relative_path) {
                continue;
            }

            println!("Processing document: {:?}", relative_path);
            let file = self.editor.file(relative_path).unwrap();

            process_imports(file, &self.use_list_query);

            document.occurrences.iter().for_each(|occurrence| {
                if occurrence.symbol.text().ends_with("ViewContext#") {
                    process_view_context_mention(
                        file,
                        document,
                        occurrence,
                        &mut fns_with_window_param,
                    );
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
                    if fns_with_window_param.contains(global_symbol) {
                        if let Err(err) = add_window_argument(file, occurrence) {
                            eprintln!("Error adding window parameter: {}", err);
                        }
                    }

                    if takes_window_fn(global_symbol) {
                        println!(
                            "Adding window parameter to function call: {:?}",
                            occurrence.symbol
                        );

                        if let Err(err) = add_window_fn_param(file, occurrence) {
                            eprintln!("Error adding window parameter: {}", err);
                        }
                    }
                }
            });
        }

        // println!("ViewContext methods: {:?}", fns_with_window_param);
    }
}

fn process_view_context_mention(
    file: &mut File,
    document: &Document,
    occurrence: &Occurrence,
    window_methods: &mut BTreeSet<GlobalSymbol>,
) {
    let Ok(node) = file.find_node(&occurrence.range) else {
        return;
    };

    let mut generics = None;
    for ancestor in node_ancestors(node) {
        match ancestor.kind() {
            // Capture the <T> from ViewContext<T> for possible use later in a later loop iteration.
            "generic_type" => {
                generics = file.node_text(ancestor).strip_prefix("ViewContext");
            }
            // Replace "cx: &mut ViewContext<T>" with "window: &mut Window, model: &mut ModelContext<T>"
            "parameter" => {
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
                match document.find_occurrence(&name_node.start_position()) {
                    Ok(occurrence) => {
                        window_methods.insert(occurrence.symbol.to_global().unwrap());
                    }
                    Err(error) => {
                        println!("No occurrence found for function name: {}", error);
                    }
                }
            }
            _ => {}
        }
    }
}

fn add_window_argument(file: &mut File, occurrence: &Occurrence) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "call_expression" {
            let arguments = ancestor.child_by_field_name("arguments").unwrap();
            let mut cursor = arguments.walk();
            for child in arguments.named_children(&mut cursor) {
                if file.node_text(child) == "cx" {
                    file.record_edit(child.byte_range(), "window, cx".to_string());
                }
            }
            break;
        }
    }

    Ok(())
}

fn add_window_fn_param(file: &mut File, occurrence: &Occurrence) -> Result<()> {
    let node = file.find_node(&occurrence.range)?;

    println!(
        "Occurrence symbol: {:?}\nLine: {:?}",
        occurrence.symbol,
        file.line(node.range().start_point.row).unwrap()
    );

    for ancestor in node_ancestors(node) {
        if ancestor.kind() == "call_expression" {
            let arguments = ancestor.child_by_field_name("arguments").unwrap();
            let mut cursor = arguments.walk();
            for child in arguments.named_children(&mut cursor) {
                if child.kind() == "closure_expression" {
                    println!("Found function_expression: {:?}", file.node_text(child));
                    let parameters = child.child_by_field_name("parameters").unwrap();
                    let mut cursor = parameters.walk();

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
                    // file.record_edit(child.byte_range(), "window, cx".to_string());
                }
            }
            break;
        }
    }

    Ok(())
}

fn takes_window_fn(symbol: &GlobalSymbol) -> bool {
    ["rust-analyzer cargo gpui 0.1.0 window/impl#[`ViewContext<'a, V>`]listener()."]
        .contains(&symbol.0.as_ref())
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
        } else {
            println!("Unsupported import format");
        }
    }
}

// #[derive(Debug, Clone)]
// struct LocalInfo {
//     id: LocalId,
//     name: Arc<str>,
//     type_signature: Arc<str>,
//     definition: Occurrence,
//     references: Vec<Occurrence>,
// }

// impl LocalInfo {
//     fn compute(
//         document: &Document,
//         symbol: &LocalSymbol,
//         symbol_info: &SymbolInformation,
//         signature_predicate: impl FnOnce(&str) -> bool,
//     ) -> Result<Option<LocalInfo>> {
//         let Some(signature) = symbol_info.signature_documentation.as_ref() else {
//             return Ok(None);
//         };
//         let signature = &signature.text;
//         if !signature_predicate(signature) {
//             return Ok(None);
//         }
//         let type_signature = extract_type_signature(signature)?;

//         let (definition, references) = document.find_local_definition_and_references(symbol)?;
//         Ok(Some(LocalInfo {
//             id: LocalId {
//                 relative_path: document.relative_path.clone(),
//                 symbol: symbol.clone(),
//             },
//             name: symbol_info.display_name.clone(),
//             type_signature: type_signature.into(),
//             definition: definition.clone(),
//             references: references.into_iter().map(|occ| occ.clone()).collect(),
//         }))
//     }
// }

fn node_ancestors(mut node: Node) -> impl Iterator<Item = Node> {
    iter::from_fn(move || {
        let result = node.parent();
        if let Some(parent) = result {
            node = parent;
        }
        result
    })
}

// fn extract_type_signature(typed_identifier: &str) -> Result<String> {
//     match typed_identifier.find(|c| c == ':') {
//         Some(colon_position) => Ok(typed_identifier[colon_position + 2..].to_owned()),
//         None => Err(anyhow!("{typed_identifier} does not contain ':'")),
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Index {
//     pub metadata: Arc<Metadata>,
//     pub documents: HashMap<RelativePath, Document>,
//     pub symbols: HashMap<GlobalSymbol, SymbolInformation>,
// }

// #[derive(Debug, Clone)]
// pub struct Document {
//     pub relative_path: RelativePath,
//     pub language: Arc<str>,
//     pub occurrences: Vec<Occurrence>,
//     pub locals: HashMap<LocalSymbol, SymbolInformation>,
//     pub position_encoding: PositionEncoding,
// }

// // TODO: hide field
// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct RelativePath(pub Arc<Path>);

// impl fmt::Display for RelativePath {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         self.0.display().fmt(f)
//     }
// }

// impl From<PathBuf> for RelativePath {
//     fn from(path: PathBuf) -> RelativePath {
//         RelativePath(path.into())
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub enum Symbol {
//     Local(LocalSymbol),
//     Global(GlobalSymbol),
// }

// impl Symbol {
//     pub fn text(&self) -> &str {
//         match self {
//             Symbol::Local(LocalSymbol(text)) => text,
//             Symbol::Global(GlobalSymbol(text)) => text,
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct LocalSymbol(Arc<str>);

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct GlobalSymbol(pub Arc<str>);

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct LocalId {
//     pub relative_path: RelativePath,
//     pub symbol: LocalSymbol,
// }

// #[derive(Debug, Clone)]
// pub struct Occurrence {
//     pub range: Range<Point>,
//     pub symbol: Symbol,
//     pub symbol_roles: i32,
//     pub override_documentation: Vec<Arc<str>>,
//     pub syntax_kind: SyntaxKind,
//     pub diagnostics: Vec<Diagnostic>,
//     pub enclosing_range: Vec<i32>,
// }

// #[derive(Debug, Clone)]
// pub struct SymbolInformation {
//     pub symbol: Symbol,
//     pub documentation: Vec<Arc<str>>,
//     pub relationships: Vec<Relationship>,
//     pub kind: Kind,
//     pub display_name: Arc<str>,
//     pub signature_documentation: Option<Arc<SignatureDocumentation>>,
//     pub enclosing_symbol: Option<GlobalSymbol>,
// }

// // In SCIP this is represented as a Document, but it's cleaner to have a separate type.
// #[derive(Debug, Clone)]
// pub struct SignatureDocumentation {
//     pub language: Arc<str>,
//     pub occurrences: Vec<Occurrence>,
//     pub locals: HashMap<LocalSymbol, SymbolInformation>,
//     pub text: Arc<str>,
// }

// #[derive(Debug, Clone)]
// pub struct Metadata {
//     pub version: ProtocolVersion,
//     pub tool_info: Arc<ToolInfo>,
//     pub project_root: Arc<str>,
//     pub text_document_encoding: TextEncoding,
// }

// #[derive(Debug, Clone)]
// pub struct ToolInfo {
//     pub name: Arc<str>,
//     pub version: Arc<str>,
//     pub arguments: Vec<Arc<str>>,
// }

// #[derive(Debug, Clone)]
// pub struct Relationship {
//     pub symbol: Symbol,
//     pub is_reference: bool,
//     pub is_implementation: bool,
//     pub is_type_definition: bool,
//     pub is_definition: bool,
// }

// #[derive(Debug, Clone)]
// pub struct Diagnostic {
//     pub severity: Severity,
//     pub code: Arc<str>,
//     pub message: Arc<str>,
//     pub source: Arc<str>,
//     pub tags: Vec<DiagnosticTag>,
// }

// impl Index {
//     pub fn find(path: &PathBuf) -> Result<PathBuf> {
//         let mut current_dir = path.clone();
//         let mut index_path = None;
//         while let Some(parent) = current_dir.parent() {
//             let potential_index_path = current_dir.join("index.scip");
//             if potential_index_path.exists() {
//                 index_path = Some(potential_index_path);
//                 break;
//             }
//             current_dir = parent.to_path_buf();
//         }
//         index_path.context("Failed to find index.scip in any ancestor directory")
//     }

//     pub fn load(index_path: &PathBuf) -> Result<Index> {
//         let mut file = File::open(&index_path)
//             .context(format!("Failed to open index file: {:?}", index_path))?;
//         let mut buffer = Vec::new();
//         file.read_to_end(&mut buffer)
//             .context("Failed to read index file")?;

//         let index = scip::types::Index::parse_from_bytes(&buffer)
//             .context("Failed to parse index from bytes")?;
//         println!("Loaded {} SCIP documents", index.documents.len());

//         Ok(index.into())
//     }

//     pub fn document(&self, relative_path: &RelativePath) -> Result<&Document> {
//         self.documents
//             .get(relative_path)
//             .ok_or_else(|| anyhow::anyhow!("Document not found: {:?}", relative_path))
//     }
// }

// impl Document {
//     pub fn lookup_symbol<'a>(
//         &'a self,
//         symbol: &Symbol,
//         index: &'a Index,
//     ) -> Option<&'a SymbolInformation> {
//         match symbol {
//             Symbol::Global(global) => index.symbols.get(global),
//             Symbol::Local(local) => self.locals.get(local),
//         }
//     }

//     pub fn lookup_point<'a>(
//         &'a self,
//         point: &Point,
//         index: &'a Index,
//     ) -> Result<&'a SymbolInformation> {
//         let symbol = self.find_occurrence(point)?.symbol.clone();
//         match self.lookup_symbol(&symbol, &index) {
//             Some(symbol_info) => Ok(symbol_info),
//             None => Err(anyhow::anyhow!("Missing symbol info for {symbol:?}")),
//         }
//     }
//     pub fn lookup_node<'a>(
//         &'a self,
//         node: tree_sitter::Node,
//         index: &'a Index,
//     ) -> Result<&'a SymbolInformation> {
//         self.lookup_point(&node.start_position(), index)
//     }

//     pub fn find_occurrence(&self, point: &Point) -> Result<&Occurrence> {
//         let occurrence_index = self
//             .occurrences
//             .binary_search_by_key(point, |occ| occ.range.start)
//             .map_err(|_| {
//                 anyhow::anyhow!(
//                     "Occurrence not found at {:?}:{}:{}",
//                     self.relative_path,
//                     point.row,
//                     point.column,
//                 )
//             })?;

//         Ok(&self.occurrences[occurrence_index])
//     }

//     pub fn find_local_definition_and_references(
//         &self,
//         local: &LocalSymbol,
//     ) -> Result<(&Occurrence, Vec<&Occurrence>)> {
//         let target = Symbol::Local(local.clone());
//         let mut definition = None;
//         let mut references = Vec::new();
//         for occ in self.occurrences.iter() {
//             if occ.symbol == target {
//                 if occ.has_role(SymbolRole::Definition) {
//                     definition = Some(occ);
//                 } else {
//                     references.push(occ);
//                 }
//             }
//         }
//         definition
//             .ok_or_else(|| {
//                 anyhow::anyhow!(
//                     "Local definition for `{}` not found in {:?}",
//                     local.0.clone(),
//                     self.relative_path
//                 )
//             })
//             .map(|definition| (definition, references))
//     }
// }

// impl Occurrence {
//     pub fn has_role(&self, role: SymbolRole) -> bool {
//         self.symbol_roles & (role as i32) != 0
//     }

//     pub fn range_string(&self, relative_path: &RelativePath) -> String {
//         format!("{}:{}-{}", relative_path, self.range.start, self.range.end)
//     }
// }

// impl SymbolInformation {
//     pub fn signature(&self) -> String {
//         self.signature_documentation
//             .clone()
//             .unwrap()
//             .text
//             .to_string()
//     }
// }

// // TODO: Complain about insertion of duplicates in all use of insert below
// impl From<scip::types::Index> for Index {
//     fn from(index: scip::types::Index) -> Self {
//         let mut documents = HashMap::new();
//         let mut symbols = HashMap::new();
//         for document in index.documents.into_iter() {
//             let mut locals = HashMap::new();
//             for symbol_info in document.symbols {
//                 let converted_symbol: SymbolInformation = symbol_info.into();
//                 match converted_symbol.symbol.clone() {
//                     Symbol::Local(local) => {
//                         locals.insert(local, converted_symbol);
//                     }
//                     Symbol::Global(global) => {
//                         symbols.insert(global, converted_symbol);
//                     }
//                 }
//             }

//             let relative_path = RelativePath(Path::new(&document.relative_path).into());

//             let mut occurrences = document
//                 .occurrences
//                 .into_iter()
//                 .map(Occurrence::from)
//                 .collect::<Vec<_>>();
//             occurrences.sort_by_key(|occ| occ.range.start);

//             documents.insert(
//                 relative_path.clone(),
//                 Document {
//                     relative_path,
//                     language: document.language.into(),
//                     occurrences,
//                     locals,
//                     position_encoding: document.position_encoding.enum_value().unwrap(),
//                 },
//             );
//         }
//         for symbol_info in index.external_symbols.into_iter() {
//             let converted_symbol: SymbolInformation = symbol_info.into();
//             match converted_symbol.symbol.clone() {
//                 // TODO: Complain about local case
//                 Symbol::Local(_) => {}
//                 Symbol::Global(global) => {
//                     symbols.insert(global, converted_symbol);
//                 }
//             }
//         }

//         Index {
//             metadata: Arc::new(Metadata::from(*index.metadata.0.unwrap())),
//             documents,
//             symbols,
//         }
//     }
// }

// impl From<scip::types::Metadata> for Metadata {
//     fn from(metadata: scip::types::Metadata) -> Self {
//         Metadata {
//             version: metadata.version.enum_value().unwrap(),
//             tool_info: Arc::new(ToolInfo::from(*metadata.tool_info.0.unwrap())),
//             project_root: metadata.project_root.into(),
//             text_document_encoding: metadata.text_document_encoding.enum_value().unwrap(),
//         }
//     }
// }

// impl From<scip::types::ToolInfo> for ToolInfo {
//     fn from(tool_info: scip::types::ToolInfo) -> Self {
//         ToolInfo {
//             name: tool_info.name.into(),
//             version: tool_info.version.into(),
//             arguments: tool_info.arguments.into_iter().map(Into::into).collect(),
//         }
//     }
// }

// impl From<scip::types::Occurrence> for Occurrence {
//     fn from(occ: scip::types::Occurrence) -> Self {
//         let range = match occ.range.len() {
//             3 => Range {
//                 start: Point::new(occ.range[0] as usize, occ.range[1] as usize),
//                 end: Point::new(occ.range[0] as usize, occ.range[2] as usize),
//             },
//             4 => Range {
//                 start: Point::new(occ.range[0] as usize, occ.range[1] as usize),
//                 end: Point::new(occ.range[2] as usize, occ.range[3] as usize),
//             },
//             l => panic!("SCIP Occurrence.range has {l} values when 3 or 4 are expected."),
//         };
//         Occurrence {
//             range,
//             symbol: occ.symbol.into(),
//             symbol_roles: occ.symbol_roles,
//             override_documentation: occ
//                 .override_documentation
//                 .into_iter()
//                 .map(Into::into)
//                 .collect(),
//             syntax_kind: occ.syntax_kind.enum_value().unwrap(),
//             diagnostics: occ.diagnostics.into_iter().map(Diagnostic::from).collect(),
//             enclosing_range: occ.enclosing_range,
//         }
//     }
// }

// impl From<String> for Symbol {
//     fn from(symbol: String) -> Self {
//         if symbol.starts_with("local ") {
//             Symbol::Local(LocalSymbol(symbol.into()))
//         } else {
//             Symbol::Global(GlobalSymbol(symbol.into()))
//         }
//     }
// }

// impl From<scip::types::SymbolInformation> for SymbolInformation {
//     fn from(symbol_info: scip::types::SymbolInformation) -> Self {
//         SymbolInformation {
//             symbol: symbol_info.symbol.into(),
//             documentation: symbol_info
//                 .documentation
//                 .into_iter()
//                 .map(Into::into)
//                 .collect(),
//             relationships: symbol_info
//                 .relationships
//                 .into_iter()
//                 .map(Relationship::from)
//                 .collect(),
//             kind: symbol_info.kind.enum_value().unwrap(),
//             display_name: symbol_info.display_name.into(),
//             signature_documentation: symbol_info
//                 .signature_documentation
//                 .0
//                 .map(|doc| Arc::new(SignatureDocumentation::from(*doc))),
//             enclosing_symbol: if symbol_info.enclosing_symbol.is_empty() {
//                 None
//             } else {
//                 match Symbol::from(symbol_info.enclosing_symbol) {
//                     Symbol::Global(symbol) => symbol.into(),
//                     Symbol::Local(_) => panic!(
//                         "Encountered local symbol in SymbolInformation.enclosing_symbol, \
//                     which is invalid."
//                     ),
//                 }
//             },
//         }
//     }
// }

// impl From<scip::types::Document> for SignatureDocumentation {
//     fn from(document: scip::types::Document) -> Self {
//         let mut locals = HashMap::new();
//         for symbol_info in document.symbols.into_iter() {
//             let converted_symbol: SymbolInformation = symbol_info.into();
//             match converted_symbol.symbol.clone() {
//                 Symbol::Local(local_symbol) => {
//                     locals.insert(local_symbol, converted_symbol);
//                 }
//                 // TODO: complain for this case.
//                 Symbol::Global(_) => {}
//             }
//         }
//         SignatureDocumentation {
//             language: document.language.into(),
//             occurrences: document
//                 .occurrences
//                 .into_iter()
//                 .map(Occurrence::from)
//                 .collect(),
//             locals,
//             text: document.text.into(),
//         }
//     }
// }

// impl From<scip::types::Relationship> for Relationship {
//     fn from(relationship: scip::types::Relationship) -> Self {
//         Relationship {
//             symbol: relationship.symbol.into(),
//             is_reference: relationship.is_reference,
//             is_implementation: relationship.is_implementation,
//             is_type_definition: relationship.is_type_definition,
//             is_definition: relationship.is_definition,
//         }
//     }
// }

// impl From<scip::types::Diagnostic> for Diagnostic {
//     fn from(diagnostic: scip::types::Diagnostic) -> Self {
//         Diagnostic {
//             severity: diagnostic.severity.enum_value().unwrap(),
//             code: diagnostic.code.into(),
//             message: diagnostic.message.into(),
//             source: diagnostic.source.into(),
//             tags: diagnostic
//                 .tags
//                 .iter()
//                 .map(|tag| tag.enum_value().unwrap())
//                 .collect(),
//         }
//     }
// }
