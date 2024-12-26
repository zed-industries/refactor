use std::sync::LazyLock;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Query, QueryCursor, QueryMatch};

pub fn for_each_function_that_has_window_context_param<'a>(
    root_node: tree_sitter::Node<'a>,
    source: &'a str,
    mut f: impl FnMut(FunctionMatch<'a>) -> (),
) {
    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&FUNCTION_QUERY.query, root_node, source.as_bytes());
    while let Some(match_) = matches.next() {
        if match_.captures.is_empty() {
            continue;
        }
        f(FunctionMatch::from_match(match_));
    }
}

pub struct FunctionMatch<'a> {
    pub name: Option<tree_sitter::Node<'a>>,
    pub body: Option<tree_sitter::Node<'a>>,
    pub param_name: Option<tree_sitter::Node<'a>>,
    pub param_type: tree_sitter::Node<'a>,
    pub target_param: tree_sitter::Node<'a>,
    pub is_function_type: bool,
}

impl<'a> FunctionMatch<'a> {
    fn from_match(match_: &QueryMatch<'_, 'a>) -> Self {
        let query = &FUNCTION_QUERY;
        let mut name = None;
        let mut body = None;
        let mut param_name = None;
        let mut param_type = None;
        let mut target_param = None;
        let mut is_function_type = false;

        for capture in match_.captures {
            let i = capture.index;
            let node = Some(capture.node);
            if capture.node.kind() == "function_type" {
                is_function_type = true;
            }
            if i == query.name {
                name = node;
            } else if i == query.body {
                body = node;
            } else if i == query.param_name {
                param_name = node;
            } else if i == query.param_type {
                param_type = node;
            } else if i == query.target_param {
                target_param = node;
            }
        }

        FunctionMatch {
            name,
            body,
            param_name,
            param_type: param_type.unwrap(),
            target_param: target_param.unwrap(),
            is_function_type,
        }
    }
}

static FUNCTION_QUERY: LazyLock<FunctionsQuery> = LazyLock::new(|| {
    let query = Query::new(&tree_sitter_rust::LANGUAGE.into(), FUNCTION_QUERY_STR)
        .expect("Failed to create functions query");
    FunctionsQuery {
        name: query.capture_index_for_name("function_name").unwrap(),
        body: query.capture_index_for_name("function_body").unwrap(),
        param_name: query.capture_index_for_name("param_name").unwrap(),
        param_type: query.capture_index_for_name("param_type").unwrap(),
        target_param: query.capture_index_for_name("target_param").unwrap(),
        query,
    }
});

struct FunctionsQuery {
    query: Query,
    name: u32,
    body: u32,
    param_name: u32,
    param_type: u32,
    target_param: u32,
}

const FUNCTION_QUERY_STR: &str = r#"
    [
        (function_item
        name: (identifier) @function_name
        parameters: (parameters
            (parameter
            pattern: (identifier) @param_name
            type: (reference_type
                (mutable_specifier)?
                type: (type_identifier) @param_type
            )
            ) @target_param
        )
        body: (block) @function_body
        )
        (#eq? @param_type "WindowContext")
        (function_signature_item
        name: (identifier) @function_name
        parameters: (parameters
            (parameter
            pattern: (identifier) @param_name
            type: (reference_type
                (mutable_specifier)?
                type: (type_identifier) @param_type
            )
            ) @target_param
        )
        )
        (#eq? @param_type "WindowContext")
        (function_type
        parameters: (parameters
            (reference_type
            (mutable_specifier)?
            type: (generic_type
                type: (type_identifier) @param_type
                type_arguments: (type_arguments
                (lifetime)?
                )
            )
            ) @target_param
        )
        )
        (#eq? @param_type "WindowContext")
        (impl_item
        body: (declaration_list
            (function_item
            name: (identifier) @function_name
            parameters: (parameters
                (self_parameter)?
                (parameter
                type: (reference_type
                    (mutable_specifier)?
                    type: (type_identifier) @param_type
                )
                ) @target_param
            )
            body: (block) @function_body
            )
        )
        )
        (#eq? @param_type "WindowContext")
    ]
"#;
