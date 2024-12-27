; Capture entire function items and their names in one pattern
(function_item
    name: (identifier) @function.name
    parameters: (parameters
        (parameter
            pattern: ((identifier) @parameter.name)
            type:
                (reference_type
                    type: [
                        (type_identifier) @parameter.type
                        (scoped_type_identifier
                            name: (type_identifier) @parameter.type
                        )
                    ]
                )
        ) @parameter
    )
)

; Function signature parameters
(function_signature_item
    name: (identifier) @function.name
    parameters: (parameters
        (parameter
            pattern: ((identifier) @parameter.name)
            type: (reference_type
                type: [
                    (type_identifier) @parameter.type
                    (scoped_type_identifier
                        name: (type_identifier) @parameter.type
                    )
                ]
            )
        ) @parameter
    )
)

; Closure parameters
; (closure_expression
;     parameters: (closure_parameters
;         (parameter
;             type: (reference_type
;                 type: [
;                     (type_identifier) @closure.param.type
;                     (scoped_type_identifier
;                         name: (type_identifier) @closure.param.type
;                     )
;                 ]
;             )
;         ) @parameter
;     )
; ) @closure
