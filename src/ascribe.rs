use anyhow::Result;
use heed::{byteorder::BigEndian, types::I32, EnvOpenOptions};
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
    lsif_path: String,
}

struct LsifGraph {
    env: heed::Env,
    vertices: heed::Database<I32<BigEndian>, heed::types::SerdeJson<Vertex>>,
    edges: heed::Database<I32<BigEndian>, heed::types::SerdeJson<Edge>>,
}

struct LsifIndex {
    graph: LsifGraph,
    documents: BTreeMap<i32, Document>,
}

struct Document {
    path: Arc<Path>,
}

impl LsifIndex {
    fn new(path: PathBuf) -> Result<Self> {
        let graph = LsifGraph::from_json(&path)?;

        let mut this = LsifIndex {
            graph,
            documents: BTreeMap::new(),
        };

        this.populate_documents()?;

        Ok(this)
    }

    fn populate_documents(&mut self) -> Result<()> {
        let rtxn = self.graph.env.read_txn()?;
        let total = self.graph.vertices.len(&rtxn)?;
        for (i, item) in self.graph.vertices.iter(&rtxn)?.enumerate() {
            let (id, vertex) = item?;
            if let Vertex::Document(document) = vertex {
                self.documents.insert(
                    id,
                    Document {
                        path: Arc::from(PathBuf::from(document.uri.path().as_str())),
                    },
                );
            }
            print_progress(i + 1, total as usize, "Populating documents");
        }
        println!();
        Ok(())
    }
}

impl LsifGraph {
    fn from_json(path: &Path) -> Result<Self> {
        let db_path = path.with_extension("db");
        std::fs::create_dir_all(&db_path)?;
        println!("Using database path: {:?}", db_path);
        let env = unsafe {
            EnvOpenOptions::new()
                .max_dbs(128)
                .map_size(10 * 1024 * 1024 * 1024) // 10 GB
                .open(db_path)?
        };
        let mut wtxn = env.write_txn()?;
        let vertices = env.create_database(&mut wtxn, Some("vertices"))?;
        let edges = env.create_database(&mut wtxn, Some("edges"))?;
        let timestamp: heed::Database<
            I32<BigEndian>,
            heed::types::SerdeJson<std::time::SystemTime>,
        > = env.create_database(&mut wtxn, Some("timestamp"))?;
        wtxn.commit()?;

        let graph = LsifGraph {
            env,
            vertices,
            edges,
        };

        let lsif_modified = path.metadata()?.modified()?;
        let db_timestamp = timestamp.get(&graph.env.read_txn()?, &0)?;

        if let Some(db_time) = db_timestamp {
            if db_time >= lsif_modified {
                println!("Database is up to date. Skipping population.");
                return Ok(graph);
            }
        }

        if db_timestamp.is_some() {
            println!("Database is outdated. Clearing and repopulating...");
            let mut wtxn = graph.env.write_txn()?;
            graph.vertices.clear(&mut wtxn)?;
            graph.edges.clear(&mut wtxn)?;
            wtxn.commit()?;
        } else {
            println!("Populating database for the first time...");
        }

        let line_count = BufReader::new(File::open(path)?).lines().count();
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let mut wtxn = graph.env.write_txn()?;

        for (i, line) in reader.lines().enumerate() {
            let line = line?;
            let entry: lsif::Entry = serde_json::from_str(&line)?;

            let id = parse_id(entry.id);
            match entry.data {
                lsif::Element::Vertex(vertex) => {
                    graph.vertices.put(&mut wtxn, &id, &vertex)?;
                }
                lsif::Element::Edge(edge) => {
                    graph.edges.put(&mut wtxn, &id, &edge)?;
                }
            }

            print_progress(i + 1, line_count, "Parsing LSIF graph from JSON");
        }
        println!();

        timestamp.put(&mut wtxn, &0, &lsif_modified)?;
        wtxn.commit()?;

        Ok(graph)
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

    let lsif_path = std::fs::canonicalize(cli.lsif_path)?;
    let lsif_index = LsifIndex::new(lsif_path)?;
    let rtxn = lsif_index.graph.env.read_txn()?;
    println!(
        "\nNumber of vertices: {}",
        lsif_index.graph.vertices.len(&rtxn)?
    );
    println!("Number of edges: {}", lsif_index.graph.edges.len(&rtxn)?);

    Ok(())
}

fn print_progress(current: usize, total: usize, message: &str) {
    let percentage = (current as f32 / total as f32) * 100.0;
    print!("\r{}: [{:>3.0}%]", message, percentage);
    std::io::stdout().flush().unwrap();
}
