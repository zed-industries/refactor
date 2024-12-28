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
)
