use anyhow::{anyhow, Context as _, Result};
use clap::Parser as ClapParser;
use refactor::{file_editor::*, scip_index::*};
use std::{iter, path::PathBuf, sync::Arc};
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
        let focused_documents = self.index.documents.iter().filter(|(relative_path, _)| {
            relative_path.0.starts_with(&self.focus_path)
                && !relative_path.0.starts_with("crates/gpui/src")
        });

        for (relative_path, document) in focused_documents {
            println!("Processing document: {:?}", relative_path);
            let file = self.editor.file(relative_path).unwrap();
            process_imports(file, document, &self.use_list_query);
            process_context_locals(file, document);
        }
    }
}

fn process_imports(file: &mut File, document: &Document, use_list_query: &Query) {
    let mut query_cursor = QueryCursor::new();
    let import_ix = use_list_query.capture_index_for_name("import");
    let nested_import_ix = use_list_query.capture_index_for_name("nested_import");
    let mut matches =
        query_cursor.matches(&use_list_query, file.tree.root_node(), file.text.as_bytes());

    while let Some(m) = matches.next() {
        let capture = m.captures.iter().next().unwrap();

        let mut imported_view_context = false;
        let mut imported_window_context = false;
        let mut imported_model_context = false;
        let mut imported_app_context = false;
        let mut imported_window = false;

        println!("Capture node text: {}", file.node_text(capture.node));

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

        if dbg!(file.node_text(cursor.node()) == "{") {
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

fn process_context_locals(file: &mut File, document: &Document) {
    for (local, symbol_info) in document.locals.iter() {
        match LocalInfo::compute(document, local, symbol_info, |s| {
            s.contains("ViewContext") || s.contains("WindowContext")
        }) {
            Ok(Some(local_info)) => match process_context_local(file, local_info) {
                Ok(_) => {}
                Err(err) => println!("Error processing context local: {}", err),
            },
            Ok(None) => {} // Skip
            Err(err) => println!("Error computing LocalInfo: {}", err),
        }
    }
}

fn process_context_local(file: &mut File, local_info: LocalInfo) -> Result<()> {
    // Process the local_info if needed
    //
    println!("Local: {}", local_info.name);
    println!(
        "{}",
        file.line(local_info.definition.range.start.row).unwrap()
    );

    let context_local_name = local_info.name;
    let leading_underscore = if context_local_name.starts_with("_") {
        "_"
    } else {
        ""
    };

    println!("Ancestors:");
    let definition_node = file.find_node(&local_info.definition.range)?;
    let ancestors = node_ancestors(definition_node);

    for ancestor in ancestors {
        println!("  {}", ancestor.kind());

        match ancestor.kind() {
            "closure_parameters" => {
                let insert_at = definition_node.byte_range().start;
                file.record_edit(
                    insert_at..insert_at,
                    format!("{leading_underscore}window, "),
                );

                break;
            }
            "parameter" => {
                let new_context_type;
                let window_type;

                let type_signature = file.node_text(ancestor.child_by_field_name("type").unwrap());

                if type_signature.contains("&ViewContext<Self>") {
                    window_type = "&Window".to_string();
                    new_context_type = format!("&ModelContext<Self>");
                } else if type_signature.contains("&mut ViewContext<Self>") {
                    window_type = "&mut Window".to_string();
                    new_context_type = format!("&mut ModelContext<Self>");
                } else if let Some(generics) = type_signature.strip_prefix("&ViewContext") {
                    window_type = "&Window".to_string();
                    new_context_type = format!("&ModelContext{generics}");
                } else if let Some(generics) = type_signature.strip_prefix("&mut ViewContext") {
                    window_type = "&mut Window".to_string();
                    new_context_type = format!("&mut ModelContext{generics}");
                } else if type_signature.contains("&WindowContext") {
                    window_type = "&mut Window".to_string();
                    new_context_type = "&AppContext".to_string();
                } else if type_signature.contains("&mut WindowContext") {
                    window_type = "&mut Window".to_string();
                    new_context_type = "&mut AppContext".to_string();
                } else {
                    anyhow::bail!("Unsupported context type: {}", type_signature)
                };

                file.record_edit(
                    ancestor.byte_range(),
                    format!(
                        "{leading_underscore}window: {window_type}, {context_local_name}: {new_context_type}"
                    ),
                );

                break;
            }
            _ => {}
        }
    }

    Ok(())
}

fn process_window_context_mention(file: &mut File, occurrence: &Occurrence) -> Result<()> {
    println!("WindowContext Mention");
    println!("{}", file.line(occurrence.range.start.row).unwrap());

    let node = file.find_node(&occurrence.range)?;

    let ancestors = node_ancestors(node);
    for ancestor in ancestors {
        println!("  Ancestor: {}", ancestor.kind());
        match ancestor.grammar_name() {
            "parameter" => {
                println!("  Parameter: {}", file.node_text(ancestor));
            }
            _ => {}
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
struct LocalInfo {
    id: LocalId,
    name: Arc<str>,
    type_signature: Arc<str>,
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
        let type_signature = extract_type_signature(signature)?;

        let (definition, references) = document.find_local_definition_and_references(symbol)?;
        Ok(Some(LocalInfo {
            id: LocalId {
                relative_path: document.relative_path.clone(),
                symbol: symbol.clone(),
            },
            name: symbol_info.display_name.clone(),
            type_signature: type_signature.into(),
            definition: definition.clone(),
            references: references.into_iter().map(|occ| occ.clone()).collect(),
        }))
    }
}

fn process_view_context_mention(file: &mut File, occurrence: &Occurrence) {
    println!("{:?}", file.line(occurrence.range.start.row).unwrap());
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

fn extract_type_signature(typed_identifier: &str) -> Result<String> {
    match typed_identifier.find(|c| c == ':') {
        Some(colon_position) => Ok(typed_identifier[colon_position + 2..].to_owned()),
        None => Err(anyhow!("{typed_identifier} does not contain ':'")),
    }
}

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
