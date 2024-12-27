; Capture entire function items and their names in one pattern
(function_item
    parameters: (parameters
    (parameter
        type: [
        (reference_type
            type: [
            (type_identifier) @function.param.type
            (scoped_type_identifier
                name: (type_identifier) @function.param.type
            )
            ]
        )
        ]
    ) @parameter)
)

; Function signature parameters
(function_signature_item
    parameters: (parameters
    (parameter
        type: (reference_type
        type: [(type_identifier) @function_sig.param.type (scoped_type_identifier name: (type_identifier) @function_sig.param.type)]))))

; Closure parameters
(closure_expression
    parameters: (closure_parameters
    (parameter
        type: (reference_type
        type: [(type_identifier) @closure.param.type (scoped_type_identifier name: (type_identifier) @closure.param.type)]))))
