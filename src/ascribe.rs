use anyhow::Result;
use lsp_types::{
    lsif::{self, Edge, Vertex},
    NumberOrString,
};
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    path: String,
}

struct LsifIndex {
    vertices: BTreeMap<i32, Vertex>,
    edges: BTreeMap<i32, Edge>,
    documents: BTreeMap<i32, Document>,
}

struct Document {
    path: Arc<Path>,
}

impl LsifIndex {
    fn new(path: PathBuf) -> Result<Self> {
        let mut this = LsifIndex {
            vertices: BTreeMap::new(),
            edges: BTreeMap::new(),
            documents: BTreeMap::new(),
        };

        this.populate_graph_from_json(path)?;
        this.populate_documents();

        Ok(this)
    }

    fn populate_graph_from_json(&mut self, path: PathBuf) -> Result<()> {
        let line_count = BufReader::new(File::open(&path)?).lines().count();

        let file = File::open(&path)?;
        let reader = BufReader::new(file);

        for (i, line) in reader.lines().enumerate() {
            let line = line?;
            let entry: lsif::Entry = serde_json::from_str(&line)?;

            let id = parse_id(entry.id);
            match entry.data {
                lsif::Element::Vertex(vertex) => {
                    self.vertices.insert(id, vertex);
                }
                lsif::Element::Edge(edge) => {
                    self.edges.insert(id, edge);
                }
            }

            print_progress(i + 1, line_count, "Parsing JSON");
        }

        Ok(())
    }

    fn populate_documents(&mut self) {
        let total = self.vertices.len();
        for (i, (id, vertex)) in self.vertices.iter().enumerate() {
            if let Vertex::Document(document) = vertex {
                self.documents.insert(
                    *id,
                    Document {
                        path: Arc::from(PathBuf::from(document.uri.path().as_str())),
                    },
                );
            }
            print_progress(i + 1, total, "Populating documents");
        }
        println!();
    }
}

fn parse_id(id: NumberOrString) -> i32 {
    match id {
        NumberOrString::Number(n) => n,
        NumberOrString::String(s) => s.parse::<i32>().unwrap(),
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let lsif_index = LsifIndex::new(PathBuf::from(cli.path.clone()))?;
    println!("\nNumber of vertices: {}", lsif_index.vertices.len());
    println!("Number of edges: {}", lsif_index.edges.len());

    Ok(())
}

fn print_progress(current: usize, total: usize, message: &str) {
    let percentage = (current as f32 / total as f32) * 100.0;
    print!("\r{}: [{:>3.0}%]", message, percentage);
    std::io::stdout().flush().unwrap();
}
