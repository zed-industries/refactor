use std::sync::LazyLock;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Query, QueryCursor, QueryMatch};

pub fn for_each_call<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(CallMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&CALLS_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(CallMatch::from_match(match_));
    }
}

pub struct CallMatch<'a> {
    pub method: Option<tree_sitter::Node<'a>>,
    pub object: Option<tree_sitter::Node<'a>>,
    pub args: tree_sitter::Node<'a>,
}

impl<'a> CallMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &CALLS_QUERY;
        let mut method = None;
        let mut object = None;
        let mut args = None;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if i == query.method {
                method = node;
            } else if i == query.object {
                object = node;
            } else if i == query.args {
                args = node;
            }
        }

        CallMatch {
            method,
            object,
            args: args.unwrap(),
        }
    }
}

static CALLS_QUERY: LazyLock<CallsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), CALLS_QUERY_STR)
        .expect("Failed to create calls query");
    CallsQuery {
        method: query.capture_index_for_name("method").unwrap(),
        object: query.capture_index_for_name("object").unwrap(),
        args: query.capture_index_for_name("args").unwrap(),
        query,
    }
});

struct CallsQuery {
    query: Query,
    method: u32,
    object: u32,
    args: u32,
}

const CALLS_QUERY_STR: &str = r#"
    (call_expression
        function: [
            (identifier) @method
            (scoped_identifier) @object
            (field_expression
                value: [
                    (identifier) @object
                    (field_expression) @object
                    (self) @object
                    (call_expression)
                ]
                field: [
                    (field_identifier) @method
                    (integer_literal) @field_index
                ]
            )
        ]
        arguments: (arguments) @args
    )
    "#;
