use crate::scip_index::RelativePath;
use anyhow::{anyhow, Context as _, Result};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fs,
    io::Read,
    ops::Range,
    panic::Location,
    path::PathBuf,
};
use tree_sitter::{Node, Parser, Point, Tree};

pub struct FileEditor {
    pub parser: Parser,
    root_folder: PathBuf,
    files: HashMap<RelativePath, File>,
}

pub struct File {
    pub relative_path: RelativePath,
    tree: Tree,
    text: String,
    edits: RefCell<Vec<Edit>>,
}

pub struct Edit {
    pub byte_range: Range<usize>,
    pub replacement: String,
}

impl FileEditor {
    pub fn new(parser: Parser, root_folder: PathBuf) -> Self {
        FileEditor {
            parser,
            root_folder,
            files: HashMap::new(),
        }
    }

    pub fn file(&mut self, relative_path: &RelativePath) -> Result<&mut File> {
        let full_path = self.full_path(&relative_path);
        match self.files.entry(relative_path.clone()) {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                let mut text = String::new();
                fs::File::open(full_path)?.read_to_string(&mut text)?;
                Ok(entry.insert(File {
                    relative_path: relative_path.clone(),
                    tree: self.parser.parse(&text, None).unwrap(),
                    text,
                    edits: RefCell::new(Vec::new()),
                }))
            }
        }
    }

    pub fn full_path(&self, relative_path: &RelativePath) -> PathBuf {
        self.root_folder.join(relative_path.0.clone())
    }

    pub fn apply_edits(&mut self) -> Result<()> {
        let mut changed_files = 0;
        for (path, file) in self.files.iter_mut() {
            let mut edits = file.edits.borrow_mut();
            if edits.is_empty() {
                continue;
            }
            // TODO: This could be made a lot faster, but it probably doesn't matter. Currently it
            // copies a suffix of the code for every edit.
            //
            // TODO: complain about overlapping edits.
            let mut new_text = file.text.clone();
            edits.sort_by(|a, b| a.byte_range.start.cmp(&b.byte_range.start));
            // Reverse after sort instead of sorting with a reverse comparison preserves insert
            // order for insertions at the same point.
            edits.reverse();
            for edit in edits.iter() {
                new_text.replace_range(edit.byte_range.clone(), &edit.replacement);
            }

            let full_path = &self.root_folder.join(path.0.clone());
            std::fs::write(full_path, new_text)
                .with_context(|| format!("Failed to write file {:?}", path))?;
            changed_files += 1;
        }
        println!("Changed {} files", changed_files);
        Ok(())
    }

    pub fn display_dry_run_results(&self) {
        for (path, file) in &self.files {
            let full_path = &self.root_folder.join(path.0.clone());
            println!("File: {:?}", full_path);
            println!("---");

            let text = file.text.clone();
            for edit in file.edits.borrow().iter() {
                let range = &edit.byte_range;
                let start_line = text[..range.start].lines().count();
                let end_line = text[..range.end].lines().count();
                let context_start = text[..range.start].rfind('\n').map_or(0, |i| i + 1);
                let context_end = text[range.end..]
                    .find('\n')
                    .map_or(text.len(), |i| range.end + i);

                println!("Lines {}-{}:", start_line, end_line);
                println!("- {}", &text[context_start..context_end]);
                println!(
                    "+ {}{}{}",
                    &text[context_start..range.start],
                    edit.replacement,
                    &text[range.end..context_end]
                );
                println!();
            }

            println!("---\n");
        }
    }
}

impl File {
    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    pub fn node_text(&self, node: Node) -> &str {
        &self.text[node.byte_range()]
    }

    pub fn find_node(&self, range: &Range<Point>) -> Result<Node> {
        let mut cursor = self.tree.walk();
        // Descend to the first leaf that touches the start of the range, and if the range is
        // non-empty, extends beyond the start.
        while cursor.goto_first_child_for_point(range.start).is_some() {
            if !range.is_empty() && cursor.node().end_position() == range.start {
                cursor.goto_next_sibling();
            }
        }

        // Ascend to the smallest ancestor that strictly contains the range.
        loop {
            let node = cursor.node();
            let node_range = node.range();
            if node_range.start_point <= range.start && node_range.end_point >= range.end {
                if node_range.start_point == range.start && node_range.end_point == range.end {
                    return Ok(node);
                } else {
                    return Err(anyhow!(
                        "Mismatch between SCIP index and TreeSitter parse for {}. \
                        Expected to find node at {}-{}, but containing ancestor is {}-{}.",
                        self.relative_path,
                        range.start,
                        range.end,
                        node_range.start_point,
                        node_range.end_point,
                    ));
                }
            }
            if !cursor.goto_parent() {
                return Err(anyhow!(
                    "Mismatch between SCIP index and TreeSitter parse for {}. \
                    Expected to find node at {}-{}, but ran out of ancestor nodes.",
                    self.relative_path,
                    range.start,
                    range.end,
                ));
            }
        }
    }

    pub fn record_node_replacement(&self, node: Node, replacement: String) {
        self.record_edit(node.byte_range(), replacement.as_str());
    }

    pub fn record_insertion_before_node(&self, node: Node, insertion: String) {
        let start = node.start_byte();
        self.record_edit(start..start, insertion.as_str());
    }

    pub fn record_insertion_after_node(&self, node: Node, insertion: String) {
        let end = node.end_byte();
        self.record_edit(end..end, insertion.as_str());
    }

    pub fn record_info(&self, row: usize, message: String) {
        self.record_insertion_before_row(row, format!("// REFACTOR: {message}\n").as_str());
    }

    #[track_caller]
    pub fn record_error(&self, row: usize, message: String) {
        let caller = Location::caller();
        self.record_insertion_before_row(
            row,
            format!("// REFACTOR ERROR: {message}\n// {caller}\n").as_str(),
        );
    }

    pub fn record_insertion_before_row(&self, row: usize, insertion: &str) {
        // TODO: There must be a more efficient way.
        let mut current_row = 0;
        let Some((start, _)) = self.text.char_indices().find(|(_, char)| {
            if current_row == row {
                return true;
            }
            if *char == '\n' {
                current_row += 1;
            }
            false
        }) else {
            panic!("Failed to find line {} in {}", row, self.relative_path);
        };
        self.record_edit(start..start, insertion)
    }

    pub fn record_edit(&self, byte_range: Range<usize>, replacement: &str) {
        self.edits.borrow_mut().push(Edit {
            byte_range,
            replacement: replacement.to_string(),
        });
    }
}
