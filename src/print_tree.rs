use std::fs;
use tree_sitter::{Node, Parser, TreeCursor};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path to Rust file>", args[0]);
        std::process::exit(1);
    }

    let filepath = &args[1];
    let filepath = fs::canonicalize(&args[1]).expect("Failed to canonicalize path");
    let source_code = fs::read_to_string(filepath).expect("Unable to read file");

    let mut parser = Parser::new();
    let language = tree_sitter_rust::LANGUAGE.into();
    parser
        .set_language(&language)
        .expect("Error loading Rust grammar");

    let tree = parser.parse(&source_code, None).unwrap();
    let root_node = tree.root_node();

    print_tree(&root_node, &source_code, 0);
}

fn print_tree(node: &Node, source: &str, depth: usize) {
    let indent = "  ".repeat(depth);
    let node_text = &source[node.byte_range()];
    let preview = if node_text.contains('\n') {
        node_text.lines().next().unwrap_or("").trim()
    } else {
        node_text.trim()
    };

    println!(
        "{}{}:{} [{}] - [{}] {}",
        indent,
        node.kind(),
        if node.is_named() { " (named)" } else { "" },
        node.start_position(),
        node.end_position(),
        if !preview.is_empty() {
            format!("\"{}\"", preview)
        } else {
            String::new()
        }
    );

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        print_tree(&child, source, depth + 1);
    }
}
