  $ cat >test.ml <<'EOF'
  > module ERROR_locate_from_inside_function_literal_used_as_non_function = struct
  >   let problem = `Problem
  >   let () = fun () -> problem
  > EOF

  $ $MERLIN single dump -what typedtree -filename test.ml < test.ml 
  {
    "class": "return",
    "value": "[
    structure_item (test.ml[1,0+0]..test.ml[3,104+28])
      Tstr_module
      ERROR_locate_from_inside_function_literal_used_as_non_function/276
        module_expr (test.ml[1,0+72]..test.ml[3,104+28])
          Tmod_structure
          [
            structure_item (test.ml[2,79+2]..test.ml[2,79+24])
              Tstr_value Nonrec
              [
                <def>
                  pattern (test.ml[2,79+6]..test.ml[2,79+13])
                    Tpat_var \"problem/274\"
                    value_mode Global, uniqueness:?, Many
                  expression (test.ml[2,79+16]..test.ml[2,79+24])
                    Texp_variant \"Problem\"
                    None
              ]
            structure_item (test.ml[3,104+2]..test.ml[3,104+28])
              Tstr_value Nonrec
              [
                <def>
                  pattern (test.ml[3,104+6]..test.ml[3,104+8])
                    Tpat_construct \"()\"
                    []
                    None
                  expression (test.ml[3,104+11]..test.ml[3,104+28])
                    Texp_function
                    region true
                    alloc_mode locality:?, uniqueness:?, linearity:?
                    []
                    Tfunction_body
                      expression (test.ml[3,104+11]..test.ml[3,104+28]) ghost
                        attribute \"merlin.incorrect\"
                          []
                        attribute \"merlin.saved-parts\"
                          [
                            structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pstr_eval
                              expression (_none_[0,0+-1]..[0,0+-1]) ghost
                                Pexp_constant PConst_int (1,None)
                          ]
                        Texp_ident \"*type-error*/275\"
              ]
          ]
  ]
  
  
  ",
    "notifications": []
  }
