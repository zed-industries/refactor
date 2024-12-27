(function_item
  name: (identifier) @function.name
  (parameters
    (parameter
      [
        ; (pattern: (identifier) @param.name
        ; type: (bounded_type (abstract_type
        ;     trait: (function_type
        ;     parameters: (parameters
        ;         (reference_type
        ;         type: (type_identifier) @param.type))))))
        type: [
          (function_type
            parameters: (parameters
              (reference_type
                type: (_) @param.type)))
          (generic_type
            type_arguments: (type_arguments
              (dynamic_type
                trait: (function_type
                  parameters: (parameters
                    (reference_type
                      type: (_) @param.type))))))
          (reference_type
            type: (dynamic_type
              trait: (function_type
                parameters: (parameters
                  (reference_type
                    type: (_) @param.type)))))
          (abstract_type
            trait: (function_type
              parameters: (parameters
                (reference_type
                  type: (_) @param.type))))
          (bounded_type (abstract_type
            trait: (function_type
              parameters: (parameters
                (reference_type
                  type: (_) @param.type)))))
        ]
      ]))
  (where_clause
    (where_predicate
      bounds: (trait_bounds
        [
          (function_type
            parameters: (parameters
              (reference_type
                type: (_) @param.type)))
          (higher_ranked_trait_bound
            type: (function_type
              parameters: (parameters
                (reference_type
                  type: (_) @param.type))))
        ])))
)
