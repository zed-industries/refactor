use anyhow::{Context as _, Result};
use protobuf::Message;
use scip::types::{
    symbol_information::Kind, DiagnosticTag, PositionEncoding, ProtocolVersion, Severity,
    SymbolRole, SyntaxKind, TextEncoding,
};
use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct Index {
    pub metadata: Arc<Metadata>,
    pub documents: HashMap<RelativePath, Document>,
    pub symbols: HashMap<GlobalSymbol, SymbolInformation>,
}

#[derive(Debug, Clone)]
pub struct Document {
    pub relative_path: RelativePath,
    pub language: Arc<str>,
    pub occurrences: Vec<Occurrence>,
    pub locals: HashMap<LocalSymbol, SymbolInformation>,
    pub position_encoding: PositionEncoding,
}

// TODO: hide field
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativePath(pub Arc<Path>);

impl From<PathBuf> for RelativePath {
    fn from(path: PathBuf) -> RelativePath {
        RelativePath(path.into())
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
pub struct GlobalSymbol(pub Arc<str>);

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

impl Index {
    pub fn find(path: &PathBuf) -> Result<PathBuf> {
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
        index_path.context("Failed to find index.scip in any ancestor directory")
    }

    pub fn load(index_path: &PathBuf) -> Result<Index> {
        let mut file = File::open(&index_path)
            .context(format!("Failed to open index file: {:?}", index_path))?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)
            .context("Failed to read index file")?;

        let index = scip::types::Index::parse_from_bytes(&buffer)
            .context("Failed to parse index from bytes")?;
        println!("Loaded {} SCIP documents", index.documents.len());

        Ok(index.into())
    }

    pub fn document(&self, relative_path: &RelativePath) -> Result<&Document> {
        self.documents
            .get(relative_path)
            .ok_or_else(|| anyhow::anyhow!("Document not found: {:?}", relative_path))
    }
}

impl Document {
    pub fn find_occurrence(&self, point: &tree_sitter::Point) -> Result<&Occurrence> {
        let line = point.row;
        let column = point.column;

        let occurrence_index = self
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
                    self.relative_path,
                    line,
                    column
                )
            })?;

        Ok(&self.occurrences[occurrence_index])
    }

    pub fn find_local_definition(&self, local: LocalSymbol) -> Result<&Occurrence> {
        self.occurrences
            .iter()
            .find(|occ| {
                occ.has_role(SymbolRole::Definition) && occ.symbol == Symbol::Local(local.clone())
            })
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Local definition for `{}` not found in {:?}",
                    local.0.clone(),
                    self.relative_path
                )
            })
    }
}

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

            let mut occurrences = document
                .occurrences
                .into_iter()
                .map(Occurrence::from)
                .collect::<Vec<_>>();
            occurrences.sort_by_key(|occ| (occ.range[0], occ.range[1]));

            let relative_path = RelativePath(Path::new(&document.relative_path).into());
            documents.insert(
                relative_path.clone(),
                Document {
                    relative_path,
                    language: document.language.into(),
                    occurrences,
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
