use protobuf::Message;
use scip::types::Index;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Get the file path from command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path-to-scip-index>", args[0]);
        std::process::exit(1);
    }

    // Canonicalize the path
    let path = PathBuf::from(&args[1]).canonicalize()?;
    println!("Loading SCIP index from: {}", path.display());

    // Read the file
    let mut file = File::open(&path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // Parse the SCIP index
    let index = Index::parse_from_bytes(&buffer)?;

    // Pretty print the index
    pretty_print_index(&index);

    Ok(())
}

fn pretty_print_index(index: &Index) {
    println!("SCIP Index:");

    // Print metadata
    println!("  Metadata:");
    println!("    Version: {:?}", index.metadata.version);
    println!("    Tool Info:");
    println!("      Name: {}", index.metadata.tool_info.name);
    println!("      Version: {}", index.metadata.tool_info.version);
    println!("      Arguments: {:?}", index.metadata.tool_info.arguments);
    println!("    Project Root: {}", index.metadata.project_root);
    println!(
        "    Text Document Encoding: {:?}",
        index.metadata.text_document_encoding
    );

    // Print documents
    println!("  Documents: {}", index.documents.len());
    for doc in index.documents.iter() {
        println!("    {}", doc.relative_path);

        // Print all occurrences
        println!("      Occurrences:");
        for occurrence in doc.occurrences.iter() {
            if occurrence.symbol_roles == 1 && !occurrence.symbol.starts_with("local") {
                println!("        {:?},", occurrence.symbol);
            }
        }
    }

    // // Print external symbols
    // println!("  External Symbols: {}", index.external_symbols.len());
    // for (i, symbol) in index.external_symbols.iter().enumerate() {
    //     println!("    Symbol {}:", i);
    //     println!("      Name: {}", symbol.symbol);
    //     println!("      Documentation: {} lines", symbol.documentation.len());
    //     println!("      Relationships: {}", symbol.relationships.len());
    // }
}
