(function_type
  (parameters
      (reference_type
        type:
          [(scoped_type_identifier
             name: (type_identifier) @parameter.type)
           (type_identifier) @parameter.type
           (generic_type
             type: [(type_identifier) @parameter.type
                    (scoped_type_identifier
                      name: (type_identifier) @parameter.type)])]) @parameter)
)
