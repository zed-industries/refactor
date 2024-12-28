use std::sync::LazyLock;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Query, QueryCursor, QueryMatch};

pub fn for_each_import<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(ImportMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&IMPORTS_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(ImportMatch::from_match(match_));
    }
}

pub struct ImportMatch<'a> {
    pub path: Option<tree_sitter::Node<'a>>,
    pub import_name: tree_sitter::Node<'a>,
}

static IMPORTS_QUERY: LazyLock<ImportsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), IMPORTS_QUERY_STR)
        .expect("Failed to create imports query");
    ImportsQuery {
        path: query.capture_index_for_name("path").unwrap(),
        import_name: query.capture_index_for_name("import_name").unwrap(),
        query,
    }
});

struct ImportsQuery {
    query: Query,
    path: u32,
    import_name: u32,
}

impl<'a> ImportMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &IMPORTS_QUERY;
        let mut path = None;
        let mut import_name = None;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if i == query.path {
                path = node;
            } else if i == query.import_name {
                import_name = node;
            }
        }

        ImportMatch {
            path,
            import_name: import_name.unwrap(),
        }
    }
}

const IMPORTS_QUERY_STR: &str = r#"
    (use_declaration
        argument: [
        (scoped_identifier
            path: (_) @path
            name: (identifier) @import_name
        )
        (scoped_use_list
            path: (_) @path
            list: (use_list
            (identifier) @import_name
            )
        )
        (use_list
            (identifier) @import_name
        )
        ]
        (#match? @import_name "^(WindowContext|Window|AppContext)$")
    )
    "#;
