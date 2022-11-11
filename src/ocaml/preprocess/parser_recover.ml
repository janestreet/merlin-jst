open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc Ast_helper.hole_txt !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc []
  let default_module_type () = Mty.signature ~loc:!default_loc []

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!+"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONLOCAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LOCAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GLOBAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_comprehension_clause_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_or_global_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_42_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_local_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_local_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_index_mod -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_include_and_functor_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_comprehension_tail_RBRACKET_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_comprehension_tail_BARRBRACKET_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_comprehension_clause -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_any_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;1;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;1;2;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;1;1;1;2;1;2;3;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;2;1;1;1;2;3;1;2;3;1;1;1;1;1;2;1;2;3;1;4;1;2;1;2;3;1;2;1;1;2;1;2;2;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;3;4;3;4;5;1;2;1;1;3;2;3;2;1;2;3;4;4;1;2;3;4;5;6;5;5;2;3;4;5;4;4;3;3;1;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;3;4;5;3;4;2;1;2;3;4;1;1;1;1;2;1;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;1;1;1;2;3;1;2;3;4;5;6;3;4;5;1;2;1;2;3;4;1;2;3;4;5;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;6;7;8;3;2;3;4;5;6;7;2;3;4;2;1;1;1;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;1;1;2;3;1;1;2;3;4;1;1;2;8;9;10;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;7;8;9;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;2;3;4;5;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;2;3;4;1;1;2;3;1;1;2;1;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;3;4;5;1;2;1;1;1;1;2;3;3;4;1;1;1;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;4;1;2;3;4;5;5;3;2;1;2;3;4;5;4;5;1;1;6;7;3;3;1;5;3;4;4;5;4;1;2;3;4;5;5;3;3;5;3;4;5;3;1;2;3;1;1;2;3;4;5;1;4;5;1;2;3;3;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;3;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;5;2;2;5;6;3;4;5;2;1;2;3;4;1;2;1;2;3;1;5;1;2;3;4;5;6;7;8;3;4;5;3;5;6;3;2;2;2;3;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;4;5;6;7;6;6;3;4;5;6;5;5;3;4;5;6;1;7;1;2;3;2;2;3;2;2;3;4;5;4;2;3;2;3;2;3;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NONLOCAL -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LOCAL -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_HASH -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_GLOBAL -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 615] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 137] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 301 :: r6 in
  let r8 = [R 718] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 42] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 199] in
  let r13 = [R 43] in
  let r14 = [R 534] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 44] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 152] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = R 301 :: r23 in
  let r25 = [R 682] in
  let r26 = [R 370] in
  let r27 = [R 133] in
  let r28 = Sub (r1) :: r27 in
  let r29 = R 301 :: r28 in
  let r30 = [R 339] in
  let r31 = Sub (r1) :: r30 in
  let r32 = S (T T_MINUSGREATER) :: r31 in
  let r33 = S (N N_pattern) :: r32 in
  let r34 = [R 578] in
  let r35 = Sub (r33) :: r34 in
  let r36 = [R 149] in
  let r37 = Sub (r35) :: r36 in
  let r38 = S (T T_WITH) :: r37 in
  let r39 = Sub (r1) :: r38 in
  let r40 = R 301 :: r39 in
  let r41 = [R 201] in
  let r42 = S (T T_UNDERSCORE) :: r25 in
  let r43 = [R 672] in
  let r44 = [R 667] in
  let r45 = S (T T_END) :: r44 in
  let r46 = R 318 :: r45 in
  let r47 = R 69 :: r46 in
  let r48 = R 301 :: r47 in
  let r49 = [R 67] in
  let r50 = S (T T_RPAREN) :: r49 in
  let r51 = [R 704] in
  let r52 = [R 643] in
  let r53 = [R 641] in
  let r54 = [R 111] in
  let r55 = [R 700] in
  let r56 = S (T T_RPAREN) :: r55 in
  let r57 = [R 469] in
  let r58 = S (T T_AMPERAMPER) :: r57 in
  let r59 = [R 852] in
  let r60 = S (T T_RPAREN) :: r59 in
  let r61 = Sub (r58) :: r60 in
  let r62 = [R 392] in
  let r63 = S (T T_UNDERSCORE) :: r62 in
  let r64 = [R 702] in
  let r65 = S (T T_RPAREN) :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = R 301 :: r66 in
  let r68 = [R 703] in
  let r69 = S (T T_RPAREN) :: r68 in
  let r70 = [R 358] in
  let r71 = [R 620] in
  let r72 = R 309 :: r71 in
  let r73 = [R 394] in
  let r74 = S (T T_END) :: r73 in
  let r75 = Sub (r72) :: r74 in
  let r76 = [R 853] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 25] in
  let r79 = S (T T_UNDERSCORE) :: r78 in
  let r80 = [R 826] in
  let r81 = Sub (r79) :: r80 in
  let r82 = [R 213] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 17] in
  let r85 = Sub (r83) :: r84 in
  let r86 = [R 127] in
  let r87 = Sub (r85) :: r86 in
  let r88 = [R 539] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 861] in
  let r91 = R 307 :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = S (T T_COLON) :: r92 in
  let r94 = Sub (r77) :: r93 in
  let r95 = R 301 :: r94 in
  let r96 = [R 443] in
  let r97 = S (T T_RPAREN) :: r96 in
  let r98 = R 235 :: r97 in
  let r99 = [R 236] in
  let r100 = [R 445] in
  let r101 = S (T T_RBRACKET) :: r100 in
  let r102 = [R 447] in
  let r103 = S (T T_RBRACE) :: r102 in
  let r104 = [R 231] in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = [R 24] in
  let r107 = Sub (r105) :: r106 in
  let r108 = [R 576] in
  let r109 = [R 492] in
  let r110 = S (T T_COLON) :: r109 in
  let r111 = [R 23] in
  let r112 = S (T T_RPAREN) :: r111 in
  let r113 = S (N N_module_type) :: r112 in
  let r114 = R 301 :: r113 in
  let r115 = R 198 :: r114 in
  let r116 = [R 396] in
  let r117 = S (N N_module_expr) :: r116 in
  let r118 = R 301 :: r117 in
  let r119 = S (T T_OF) :: r118 in
  let r120 = [R 382] in
  let r121 = S (T T_END) :: r120 in
  let r122 = S (N N_structure) :: r121 in
  let r123 = [R 356] in
  let r124 = S (T T_LIDENT) :: r123 in
  let r125 = [R 833] in
  let r126 = Sub (r124) :: r125 in
  let r127 = [R 112] in
  let r128 = S (T T_FALSE) :: r127 in
  let r129 = [R 116] in
  let r130 = Sub (r128) :: r129 in
  let r131 = [R 225] in
  let r132 = R 301 :: r131 in
  let r133 = R 218 :: r132 in
  let r134 = Sub (r130) :: r133 in
  let r135 = [R 559] in
  let r136 = Sub (r134) :: r135 in
  let r137 = [R 801] in
  let r138 = R 307 :: r137 in
  let r139 = Sub (r136) :: r138 in
  let r140 = R 545 :: r139 in
  let r141 = S (T T_PLUSEQ) :: r140 in
  let r142 = Sub (r126) :: r141 in
  let r143 = R 835 :: r142 in
  let r144 = R 301 :: r143 in
  let r145 = [R 228] in
  let r146 = R 307 :: r145 in
  let r147 = R 568 :: r146 in
  let r148 = R 831 :: r147 in
  let r149 = S (T T_LIDENT) :: r148 in
  let r150 = R 835 :: r149 in
  let r151 = R 301 :: r150 in
  let r152 = R 198 :: r151 in
  let r153 = [R 802] in
  let r154 = R 307 :: r153 in
  let r155 = Sub (r136) :: r154 in
  let r156 = R 545 :: r155 in
  let r157 = S (T T_PLUSEQ) :: r156 in
  let r158 = Sub (r126) :: r157 in
  let r159 = [R 229] in
  let r160 = R 307 :: r159 in
  let r161 = R 568 :: r160 in
  let r162 = R 831 :: r161 in
  let r163 = S (T T_LIDENT) :: r162 in
  let r164 = R 835 :: r163 in
  let r165 = [R 839] in
  let r166 = S (T T_UNDERSCORE) :: r165 in
  let r167 = [R 834] in
  let r168 = Sub (r166) :: r167 in
  let r169 = R 840 :: r168 in
  let r170 = [R 591] in
  let r171 = Sub (r169) :: r170 in
  let r172 = [R 837] in
  let r173 = S (T T_RPAREN) :: r172 in
  let r174 = [R 838] in
  let r175 = [R 592] in
  let r176 = [R 428] in
  let r177 = S (T T_DOTDOT) :: r176 in
  let r178 = [R 832] in
  let r179 = [R 429] in
  let r180 = [R 115] in
  let r181 = S (T T_RPAREN) :: r180 in
  let r182 = [R 790] in
  let r183 = Sub (r81) :: r182 in
  let r184 = S (T T_MINUSGREATER) :: r183 in
  let r185 = [R 30] in
  let r186 = [R 541] in
  let r187 = Sub (r85) :: r186 in
  let r188 = [R 346] in
  let r189 = R 301 :: r188 in
  let r190 = Sub (r187) :: r189 in
  let r191 = [R 784] in
  let r192 = Sub (r81) :: r191 in
  let r193 = S (T T_MINUSGREATER) :: r192 in
  let r194 = Sub (r81) :: r193 in
  let r195 = [R 786] in
  let r196 = Sub (r81) :: r195 in
  let r197 = S (T T_MINUSGREATER) :: r196 in
  let r198 = [R 200] in
  let r199 = S (T T_RBRACKET) :: r198 in
  let r200 = Sub (r15) :: r199 in
  let r201 = [R 313] in
  let r202 = [R 436] in
  let r203 = R 307 :: r202 in
  let r204 = S (N N_module_expr) :: r203 in
  let r205 = R 301 :: r204 in
  let r206 = [R 437] in
  let r207 = R 307 :: r206 in
  let r208 = S (N N_module_expr) :: r207 in
  let r209 = R 301 :: r208 in
  let r210 = [R 494] in
  let r211 = S (T T_RPAREN) :: r210 in
  let r212 = [R 495] in
  let r213 = S (T T_RPAREN) :: r212 in
  let r214 = S (N N_expr) :: r213 in
  let r215 = [R 368] in
  let r216 = S (T T_LIDENT) :: r215 in
  let r217 = [R 66] in
  let r218 = Sub (r216) :: r217 in
  let r219 = [R 664] in
  let r220 = Sub (r218) :: r219 in
  let r221 = R 301 :: r220 in
  let r222 = [R 369] in
  let r223 = S (T T_LIDENT) :: r222 in
  let r224 = [R 371] in
  let r225 = [R 376] in
  let r226 = [R 302] in
  let r227 = [R 132] in
  let r228 = Sub (r35) :: r227 in
  let r229 = S (T T_WITH) :: r228 in
  let r230 = Sub (r1) :: r229 in
  let r231 = R 301 :: r230 in
  let r232 = [R 148] in
  let r233 = Sub (r35) :: r232 in
  let r234 = S (T T_WITH) :: r233 in
  let r235 = Sub (r1) :: r234 in
  let r236 = R 301 :: r235 in
  let r237 = [R 651] in
  let r238 = S (T T_RPAREN) :: r237 in
  let r239 = [R 687] in
  let r240 = [R 197] in
  let r241 = [R 185] in
  let r242 = [R 268] in
  let r243 = Sub (r77) :: r242 in
  let r244 = [R 336] in
  let r245 = R 307 :: r244 in
  let r246 = Sub (r243) :: r245 in
  let r247 = R 552 :: r246 in
  let r248 = R 301 :: r247 in
  let r249 = [R 333] in
  let r250 = Sub (r1) :: r249 in
  let r251 = S (T T_EQUAL) :: r250 in
  let r252 = [R 277] in
  let r253 = Sub (r251) :: r252 in
  let r254 = [R 262] in
  let r255 = [R 245] in
  let r256 = S (T T_LIDENT) :: r255 in
  let r257 = [R 260] in
  let r258 = S (T T_RPAREN) :: r257 in
  let r259 = [R 261] in
  let r260 = S (T T_RPAREN) :: r259 in
  let r261 = [R 246] in
  let r262 = [R 602] in
  let r263 = Sub (r87) :: r262 in
  let r264 = [R 587] in
  let r265 = Sub (r263) :: r264 in
  let r266 = [R 39] in
  let r267 = S (T T_RBRACKET) :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = [R 38] in
  let r270 = [R 37] in
  let r271 = S (T T_RBRACKET) :: r270 in
  let r272 = [R 417] in
  let r273 = Sub (r105) :: r272 in
  let r274 = S (T T_BACKQUOTE) :: r273 in
  let r275 = [R 814] in
  let r276 = R 301 :: r275 in
  let r277 = Sub (r274) :: r276 in
  let r278 = [R 34] in
  let r279 = S (T T_RBRACKET) :: r278 in
  let r280 = [R 95] in
  let r281 = Sub (r124) :: r280 in
  let r282 = [R 31] in
  let r283 = [R 359] in
  let r284 = S (T T_UIDENT) :: r283 in
  let r285 = S (T T_DOT) :: r284 in
  let r286 = [R 357] in
  let r287 = S (T T_LIDENT) :: r286 in
  let r288 = S (T T_UIDENT) :: r70 in
  let r289 = [R 374] in
  let r290 = Sub (r288) :: r289 in
  let r291 = [R 375] in
  let r292 = S (T T_RPAREN) :: r291 in
  let r293 = [R 35] in
  let r294 = S (T T_RBRACKET) :: r293 in
  let r295 = [R 788] in
  let r296 = [R 789] in
  let r297 = [R 791] in
  let r298 = [R 599] in
  let r299 = [R 32] in
  let r300 = [R 600] in
  let r301 = [R 780] in
  let r302 = Sub (r81) :: r301 in
  let r303 = S (T T_MINUSGREATER) :: r302 in
  let r304 = [R 782] in
  let r305 = Sub (r81) :: r304 in
  let r306 = S (T T_MINUSGREATER) :: r305 in
  let r307 = [R 783] in
  let r308 = [R 781] in
  let r309 = [R 588] in
  let r310 = [R 581] in
  let r311 = Sub (r85) :: r310 in
  let r312 = [R 813] in
  let r313 = R 301 :: r312 in
  let r314 = Sub (r311) :: r313 in
  let r315 = [R 582] in
  let r316 = [R 18] in
  let r317 = Sub (r105) :: r316 in
  let r318 = [R 36] in
  let r319 = S (T T_RBRACKET) :: r318 in
  let r320 = Sub (r265) :: r319 in
  let r321 = [R 574] in
  let r322 = Sub (r274) :: r321 in
  let r323 = [R 40] in
  let r324 = S (T T_RBRACKET) :: r323 in
  let r325 = [R 256] in
  let r326 = [R 254] in
  let r327 = S (T T_RPAREN) :: r326 in
  let r328 = R 487 :: r327 in
  let r329 = [R 255] in
  let r330 = S (T T_RPAREN) :: r329 in
  let r331 = R 487 :: r330 in
  let r332 = [R 488] in
  let r333 = [R 286] in
  let r334 = Sub (r77) :: r333 in
  let r335 = [R 289] in
  let r336 = Sub (r334) :: r335 in
  let r337 = [R 183] in
  let r338 = Sub (r1) :: r337 in
  let r339 = S (T T_IN) :: r338 in
  let r340 = [R 648] in
  let r341 = [R 110] in
  let r342 = [R 609] in
  let r343 = S (N N_pattern) :: r342 in
  let r344 = [R 646] in
  let r345 = S (T T_RBRACKET) :: r344 in
  let r346 = [R 247] in
  let r347 = Sub (r216) :: r346 in
  let r348 = [R 327] in
  let r349 = R 485 :: r348 in
  let r350 = R 479 :: r349 in
  let r351 = Sub (r347) :: r350 in
  let r352 = [R 645] in
  let r353 = S (T T_RBRACE) :: r352 in
  let r354 = [R 480] in
  let r355 = [R 486] in
  let r356 = S (T T_UNDERSCORE) :: r51 in
  let r357 = [R 699] in
  let r358 = Sub (r356) :: r357 in
  let r359 = [R 525] in
  let r360 = Sub (r358) :: r359 in
  let r361 = R 301 :: r360 in
  let r362 = [R 106] in
  let r363 = [R 709] in
  let r364 = S (T T_INT) :: r362 in
  let r365 = [R 640] in
  let r366 = Sub (r364) :: r365 in
  let r367 = [R 706] in
  let r368 = [R 711] in
  let r369 = S (T T_RBRACKET) :: r368 in
  let r370 = S (T T_LBRACKET) :: r369 in
  let r371 = [R 712] in
  let r372 = [R 516] in
  let r373 = S (N N_pattern) :: r372 in
  let r374 = R 301 :: r373 in
  let r375 = [R 517] in
  let r376 = [R 510] in
  let r377 = [R 524] in
  let r378 = [R 522] in
  let r379 = [R 418] in
  let r380 = S (T T_LIDENT) :: r379 in
  let r381 = [R 523] in
  let r382 = Sub (r358) :: r381 in
  let r383 = S (T T_RPAREN) :: r382 in
  let r384 = [R 120] in
  let r385 = [R 119] in
  let r386 = S (T T_RPAREN) :: r385 in
  let r387 = [R 518] in
  let r388 = [R 714] in
  let r389 = S (T T_RPAREN) :: r388 in
  let r390 = [R 515] in
  let r391 = [R 513] in
  let r392 = [R 118] in
  let r393 = S (T T_RPAREN) :: r392 in
  let r394 = [R 713] in
  let r395 = [R 329] in
  let r396 = [R 647] in
  let r397 = [R 533] in
  let r398 = S (T T_UNDERSCORE) :: r397 in
  let r399 = [R 259] in
  let r400 = [R 257] in
  let r401 = S (T T_RPAREN) :: r400 in
  let r402 = R 487 :: r401 in
  let r403 = [R 258] in
  let r404 = S (T T_RPAREN) :: r403 in
  let r405 = R 487 :: r404 in
  let r406 = [R 284] in
  let r407 = [R 771] in
  let r408 = Sub (r1) :: r407 in
  let r409 = S (T T_EQUAL) :: r408 in
  let r410 = [R 207] in
  let r411 = Sub (r409) :: r410 in
  let r412 = [R 773] in
  let r413 = Sub (r411) :: r412 in
  let r414 = S (T T_RPAREN) :: r413 in
  let r415 = Sub (r380) :: r414 in
  let r416 = [R 263] in
  let r417 = [R 264] in
  let r418 = S (T T_RPAREN) :: r417 in
  let r419 = S (N N_pattern) :: r418 in
  let r420 = [R 143] in
  let r421 = Sub (r1) :: r420 in
  let r422 = S (T T_IN) :: r421 in
  let r423 = S (N N_module_expr) :: r422 in
  let r424 = R 301 :: r423 in
  let r425 = R 198 :: r424 in
  let r426 = [R 278] in
  let r427 = R 307 :: r426 in
  let r428 = Sub (r243) :: r427 in
  let r429 = R 552 :: r428 in
  let r430 = R 301 :: r429 in
  let r431 = R 198 :: r430 in
  let r432 = [R 144] in
  let r433 = Sub (r1) :: r432 in
  let r434 = S (T T_IN) :: r433 in
  let r435 = S (N N_module_expr) :: r434 in
  let r436 = R 301 :: r435 in
  let r437 = [R 383] in
  let r438 = S (N N_module_expr) :: r437 in
  let r439 = S (T T_MINUSGREATER) :: r438 in
  let r440 = S (N N_functor_args) :: r439 in
  let r441 = [R 215] in
  let r442 = [R 216] in
  let r443 = S (T T_RPAREN) :: r442 in
  let r444 = S (N N_module_type) :: r443 in
  let r445 = [R 397] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = [R 395] in
  let r448 = S (N N_module_type) :: r447 in
  let r449 = S (T T_MINUSGREATER) :: r448 in
  let r450 = S (N N_functor_args) :: r449 in
  let r451 = [R 366] in
  let r452 = Sub (r105) :: r451 in
  let r453 = [R 405] in
  let r454 = Sub (r452) :: r453 in
  let r455 = [R 874] in
  let r456 = S (N N_module_type) :: r455 in
  let r457 = S (T T_EQUAL) :: r456 in
  let r458 = Sub (r454) :: r457 in
  let r459 = S (T T_TYPE) :: r458 in
  let r460 = S (T T_MODULE) :: r459 in
  let r461 = [R 585] in
  let r462 = Sub (r460) :: r461 in
  let r463 = [R 401] in
  let r464 = [R 871] in
  let r465 = Sub (r85) :: r464 in
  let r466 = S (T T_COLONEQUAL) :: r465 in
  let r467 = Sub (r347) :: r466 in
  let r468 = [R 870] in
  let r469 = R 568 :: r468 in
  let r470 = [R 569] in
  let r471 = Sub (r87) :: r470 in
  let r472 = S (T T_EQUAL) :: r471 in
  let r473 = [R 367] in
  let r474 = Sub (r105) :: r473 in
  let r475 = [R 875] in
  let r476 = [R 400] in
  let r477 = [R 872] in
  let r478 = Sub (r290) :: r477 in
  let r479 = S (T T_UIDENT) :: r224 in
  let r480 = [R 873] in
  let r481 = [R 586] in
  let r482 = [R 388] in
  let r483 = [R 493] in
  let r484 = S (T T_RPAREN) :: r483 in
  let r485 = [R 603] in
  let r486 = S (N N_expr) :: r485 in
  let r487 = [R 690] in
  let r488 = S (T T_RBRACKET) :: r487 in
  let r489 = [R 675] in
  let r490 = [R 606] in
  let r491 = R 481 :: r490 in
  let r492 = [R 482] in
  let r493 = [R 612] in
  let r494 = R 481 :: r493 in
  let r495 = R 489 :: r494 in
  let r496 = Sub (r347) :: r495 in
  let r497 = [R 554] in
  let r498 = Sub (r496) :: r497 in
  let r499 = [R 684] in
  let r500 = S (T T_RBRACE) :: r499 in
  let r501 = [R 650] in
  let r502 = [R 649] in
  let r503 = S (T T_GREATERDOT) :: r502 in
  let r504 = [R 155] in
  let r505 = Sub (r42) :: r504 in
  let r506 = R 301 :: r505 in
  let r507 = [R 663] in
  let r508 = S (T T_END) :: r507 in
  let r509 = R 301 :: r508 in
  let r510 = [R 151] in
  let r511 = S (N N_expr) :: r510 in
  let r512 = S (T T_THEN) :: r511 in
  let r513 = Sub (r1) :: r512 in
  let r514 = R 301 :: r513 in
  let r515 = [R 145] in
  let r516 = Sub (r35) :: r515 in
  let r517 = R 301 :: r516 in
  let r518 = [R 579] in
  let r519 = [R 340] in
  let r520 = Sub (r1) :: r519 in
  let r521 = S (T T_MINUSGREATER) :: r520 in
  let r522 = [R 265] in
  let r523 = Sub (r358) :: r522 in
  let r524 = [R 209] in
  let r525 = Sub (r1) :: r524 in
  let r526 = S (T T_MINUSGREATER) :: r525 in
  let r527 = [R 146] in
  let r528 = Sub (r526) :: r527 in
  let r529 = Sub (r523) :: r528 in
  let r530 = R 301 :: r529 in
  let r531 = [R 266] in
  let r532 = S (T T_RPAREN) :: r531 in
  let r533 = S (N N_let_pattern) :: r532 in
  let r534 = [R 147] in
  let r535 = Sub (r526) :: r534 in
  let r536 = S (T T_RPAREN) :: r535 in
  let r537 = [R 139] in
  let r538 = S (T T_DONE) :: r537 in
  let r539 = Sub (r1) :: r538 in
  let r540 = S (T T_DO) :: r539 in
  let r541 = Sub (r1) :: r540 in
  let r542 = S (T T_IN) :: r541 in
  let r543 = S (N N_pattern) :: r542 in
  let r544 = R 301 :: r543 in
  let r545 = [R 130] in
  let r546 = S (T T_DOWNTO) :: r545 in
  let r547 = [R 153] in
  let r548 = S (T T_DONE) :: r547 in
  let r549 = Sub (r1) :: r548 in
  let r550 = S (T T_DO) :: r549 in
  let r551 = Sub (r1) :: r550 in
  let r552 = Sub (r546) :: r551 in
  let r553 = Sub (r1) :: r552 in
  let r554 = S (T T_EQUAL) :: r553 in
  let r555 = S (N N_pattern) :: r554 in
  let r556 = R 301 :: r555 in
  let r557 = [R 673] in
  let r558 = [R 683] in
  let r559 = S (T T_RPAREN) :: r558 in
  let r560 = S (T T_LPAREN) :: r559 in
  let r561 = S (T T_DOT) :: r560 in
  let r562 = [R 697] in
  let r563 = S (T T_RPAREN) :: r562 in
  let r564 = S (N N_module_type) :: r563 in
  let r565 = S (T T_COLON) :: r564 in
  let r566 = S (N N_module_expr) :: r565 in
  let r567 = R 301 :: r566 in
  let r568 = [R 287] in
  let r569 = Sub (r1) :: r568 in
  let r570 = S (T T_EQUAL) :: r569 in
  let r571 = [R 154] in
  let r572 = Sub (r42) :: r571 in
  let r573 = R 301 :: r572 in
  let r574 = [R 680] in
  let r575 = [R 656] in
  let r576 = S (T T_RPAREN) :: r575 in
  let r577 = Sub (r486) :: r576 in
  let r578 = S (T T_LPAREN) :: r577 in
  let r579 = [R 180] in
  let r580 = [R 250] in
  let r581 = [R 828] in
  let r582 = Sub (r87) :: r581 in
  let r583 = S (T T_COLON) :: r582 in
  let r584 = [R 251] in
  let r585 = S (T T_RPAREN) :: r584 in
  let r586 = Sub (r583) :: r585 in
  let r587 = [R 830] in
  let r588 = [R 829] in
  let r589 = [R 252] in
  let r590 = [R 253] in
  let r591 = [R 679] in
  let r592 = [R 653] in
  let r593 = S (T T_RPAREN) :: r592 in
  let r594 = Sub (r1) :: r593 in
  let r595 = S (T T_LPAREN) :: r594 in
  let r596 = [R 597] in
  let r597 = [R 131] in
  let r598 = Sub (r1) :: r597 in
  let r599 = [R 182] in
  let r600 = Sub (r1) :: r599 in
  let r601 = [R 170] in
  let r602 = [R 164] in
  let r603 = [R 181] in
  let r604 = [R 618] in
  let r605 = Sub (r1) :: r604 in
  let r606 = [R 167] in
  let r607 = [R 171] in
  let r608 = [R 163] in
  let r609 = [R 166] in
  let r610 = [R 165] in
  let r611 = [R 175] in
  let r612 = [R 169] in
  let r613 = [R 168] in
  let r614 = [R 173] in
  let r615 = [R 162] in
  let r616 = [R 161] in
  let r617 = [R 184] in
  let r618 = [R 160] in
  let r619 = [R 174] in
  let r620 = [R 172] in
  let r621 = [R 176] in
  let r622 = [R 177] in
  let r623 = [R 178] in
  let r624 = [R 598] in
  let r625 = [R 179] in
  let r626 = [R 19] in
  let r627 = R 307 :: r626 in
  let r628 = Sub (r243) :: r627 in
  let r629 = [R 274] in
  let r630 = Sub (r1) :: r629 in
  let r631 = S (T T_EQUAL) :: r630 in
  let r632 = Sub (r87) :: r631 in
  let r633 = S (T T_DOT) :: r632 in
  let r634 = [R 272] in
  let r635 = Sub (r1) :: r634 in
  let r636 = S (T T_EQUAL) :: r635 in
  let r637 = Sub (r87) :: r636 in
  let r638 = [R 577] in
  let r639 = [R 270] in
  let r640 = Sub (r1) :: r639 in
  let r641 = [R 772] in
  let r642 = [R 208] in
  let r643 = Sub (r1) :: r642 in
  let r644 = [R 276] in
  let r645 = Sub (r1) :: r644 in
  let r646 = S (T T_EQUAL) :: r645 in
  let r647 = [R 275] in
  let r648 = Sub (r1) :: r647 in
  let r649 = [R 520] in
  let r650 = [R 526] in
  let r651 = [R 531] in
  let r652 = [R 529] in
  let r653 = [R 519] in
  let r654 = [R 543] in
  let r655 = S (T T_RBRACKET) :: r654 in
  let r656 = Sub (r15) :: r655 in
  let r657 = [R 537] in
  let r658 = [R 538] in
  let r659 = [R 377] in
  let r660 = S (N N_module_expr) :: r659 in
  let r661 = S (T T_EQUAL) :: r660 in
  let r662 = [R 804] in
  let r663 = R 307 :: r662 in
  let r664 = Sub (r661) :: r663 in
  let r665 = Sub (r63) :: r664 in
  let r666 = R 301 :: r665 in
  let r667 = [R 403] in
  let r668 = R 307 :: r667 in
  let r669 = R 483 :: r668 in
  let r670 = Sub (r105) :: r669 in
  let r671 = R 301 :: r670 in
  let r672 = R 198 :: r671 in
  let r673 = [R 484] in
  let r674 = [R 308] in
  let r675 = [R 805] in
  let r676 = R 297 :: r675 in
  let r677 = R 307 :: r676 in
  let r678 = Sub (r661) :: r677 in
  let r679 = [R 378] in
  let r680 = S (N N_module_expr) :: r679 in
  let r681 = S (T T_EQUAL) :: r680 in
  let r682 = [R 298] in
  let r683 = R 297 :: r682 in
  let r684 = R 307 :: r683 in
  let r685 = Sub (r661) :: r684 in
  let r686 = Sub (r63) :: r685 in
  let r687 = [R 379] in
  let r688 = [R 238] in
  let r689 = S (T T_RBRACKET) :: r688 in
  let r690 = Sub (r15) :: r689 in
  let r691 = [R 204] in
  let r692 = S (T T_RBRACKET) :: r691 in
  let r693 = Sub (r15) :: r692 in
  let r694 = [R 420] in
  let r695 = S (T T_STRING) :: r694 in
  let r696 = [R 544] in
  let r697 = R 307 :: r696 in
  let r698 = Sub (r695) :: r697 in
  let r699 = S (T T_EQUAL) :: r698 in
  let r700 = Sub (r89) :: r699 in
  let r701 = S (T T_COLON) :: r700 in
  let r702 = Sub (r77) :: r701 in
  let r703 = R 301 :: r702 in
  let r704 = [R 540] in
  let r705 = Sub (r87) :: r704 in
  let r706 = Sub (r128) :: r384 in
  let r707 = [R 770] in
  let r708 = R 307 :: r707 in
  let r709 = R 301 :: r708 in
  let r710 = Sub (r706) :: r709 in
  let r711 = S (T T_EQUAL) :: r710 in
  let r712 = Sub (r130) :: r711 in
  let r713 = R 301 :: r712 in
  let r714 = [R 619] in
  let r715 = R 307 :: r714 in
  let r716 = R 301 :: r715 in
  let r717 = R 218 :: r716 in
  let r718 = Sub (r130) :: r717 in
  let r719 = R 301 :: r718 in
  let r720 = R 198 :: r719 in
  let r721 = [R 122] in
  let r722 = Sub (r79) :: r721 in
  let r723 = [R 219] in
  let r724 = [R 240] in
  let r725 = R 301 :: r724 in
  let r726 = Sub (r187) :: r725 in
  let r727 = S (T T_COLON) :: r726 in
  let r728 = S (T T_LIDENT) :: r727 in
  let r729 = R 408 :: r728 in
  let r730 = [R 242] in
  let r731 = Sub (r729) :: r730 in
  let r732 = [R 124] in
  let r733 = S (T T_RBRACE) :: r732 in
  let r734 = [R 241] in
  let r735 = R 301 :: r734 in
  let r736 = S (T T_SEMI) :: r735 in
  let r737 = R 301 :: r736 in
  let r738 = Sub (r187) :: r737 in
  let r739 = S (T T_COLON) :: r738 in
  let r740 = [R 542] in
  let r741 = Sub (r85) :: r740 in
  let r742 = [R 123] in
  let r743 = Sub (r79) :: r742 in
  let r744 = S (T T_COLONCOLON) :: r393 in
  let r745 = [R 222] in
  let r746 = [R 223] in
  let r747 = Sub (r79) :: r746 in
  let r748 = [R 221] in
  let r749 = Sub (r79) :: r748 in
  let r750 = [R 220] in
  let r751 = Sub (r79) :: r750 in
  let r752 = [R 535] in
  let r753 = [R 565] in
  let r754 = Sub (r134) :: r753 in
  let r755 = [R 627] in
  let r756 = R 307 :: r755 in
  let r757 = Sub (r754) :: r756 in
  let r758 = R 545 :: r757 in
  let r759 = S (T T_PLUSEQ) :: r758 in
  let r760 = Sub (r126) :: r759 in
  let r761 = R 835 :: r760 in
  let r762 = R 301 :: r761 in
  let r763 = [R 628] in
  let r764 = R 307 :: r763 in
  let r765 = Sub (r754) :: r764 in
  let r766 = R 545 :: r765 in
  let r767 = S (T T_PLUSEQ) :: r766 in
  let r768 = Sub (r126) :: r767 in
  let r769 = [R 227] in
  let r770 = R 307 :: r769 in
  let r771 = R 568 :: r770 in
  let r772 = [R 432] in
  let r773 = S (T T_RBRACE) :: r772 in
  let r774 = [R 224] in
  let r775 = R 301 :: r774 in
  let r776 = R 218 :: r775 in
  let r777 = Sub (r130) :: r776 in
  let r778 = [R 430] in
  let r779 = [R 431] in
  let r780 = [R 435] in
  let r781 = S (T T_RBRACE) :: r780 in
  let r782 = [R 434] in
  let r783 = S (T T_RBRACE) :: r782 in
  let r784 = [R 226] in
  let r785 = R 307 :: r784 in
  let r786 = R 568 :: r785 in
  let r787 = [R 310] in
  let r788 = [R 438] in
  let r789 = R 307 :: r788 in
  let r790 = Sub (r290) :: r789 in
  let r791 = R 301 :: r790 in
  let r792 = [R 439] in
  let r793 = R 307 :: r792 in
  let r794 = Sub (r290) :: r793 in
  let r795 = R 301 :: r794 in
  let r796 = [R 380] in
  let r797 = S (N N_module_type) :: r796 in
  let r798 = S (T T_COLON) :: r797 in
  let r799 = [R 630] in
  let r800 = R 307 :: r799 in
  let r801 = Sub (r798) :: r800 in
  let r802 = Sub (r63) :: r801 in
  let r803 = R 301 :: r802 in
  let r804 = [R 404] in
  let r805 = R 307 :: r804 in
  let r806 = S (N N_module_type) :: r805 in
  let r807 = S (T T_COLONEQUAL) :: r806 in
  let r808 = Sub (r105) :: r807 in
  let r809 = R 301 :: r808 in
  let r810 = [R 393] in
  let r811 = R 307 :: r810 in
  let r812 = [R 633] in
  let r813 = R 299 :: r812 in
  let r814 = R 307 :: r813 in
  let r815 = S (N N_module_type) :: r814 in
  let r816 = S (T T_COLON) :: r815 in
  let r817 = [R 300] in
  let r818 = R 299 :: r817 in
  let r819 = R 307 :: r818 in
  let r820 = S (N N_module_type) :: r819 in
  let r821 = S (T T_COLON) :: r820 in
  let r822 = Sub (r63) :: r821 in
  let r823 = S (T T_UIDENT) :: r26 in
  let r824 = Sub (r823) :: r225 in
  let r825 = [R 631] in
  let r826 = R 307 :: r825 in
  let r827 = [R 381] in
  let r828 = S (T T_QUOTED_STRING_EXPR) :: r41 in
  let r829 = [R 80] in
  let r830 = Sub (r828) :: r829 in
  let r831 = [R 90] in
  let r832 = Sub (r830) :: r831 in
  let r833 = [R 638] in
  let r834 = R 293 :: r833 in
  let r835 = R 307 :: r834 in
  let r836 = Sub (r832) :: r835 in
  let r837 = S (T T_COLON) :: r836 in
  let r838 = S (T T_LIDENT) :: r837 in
  let r839 = R 205 :: r838 in
  let r840 = R 862 :: r839 in
  let r841 = R 301 :: r840 in
  let r842 = [R 94] in
  let r843 = R 295 :: r842 in
  let r844 = R 307 :: r843 in
  let r845 = Sub (r830) :: r844 in
  let r846 = S (T T_EQUAL) :: r845 in
  let r847 = S (T T_LIDENT) :: r846 in
  let r848 = R 205 :: r847 in
  let r849 = R 862 :: r848 in
  let r850 = R 301 :: r849 in
  let r851 = [R 206] in
  let r852 = S (T T_RBRACKET) :: r851 in
  let r853 = [R 81] in
  let r854 = S (T T_END) :: r853 in
  let r855 = R 316 :: r854 in
  let r856 = R 71 :: r855 in
  let r857 = [R 70] in
  let r858 = S (T T_RPAREN) :: r857 in
  let r859 = [R 73] in
  let r860 = R 307 :: r859 in
  let r861 = Sub (r87) :: r860 in
  let r862 = S (T T_COLON) :: r861 in
  let r863 = S (T T_LIDENT) :: r862 in
  let r864 = R 412 :: r863 in
  let r865 = [R 74] in
  let r866 = R 307 :: r865 in
  let r867 = Sub (r89) :: r866 in
  let r868 = S (T T_COLON) :: r867 in
  let r869 = S (T T_LIDENT) :: r868 in
  let r870 = R 547 :: r869 in
  let r871 = [R 72] in
  let r872 = R 307 :: r871 in
  let r873 = Sub (r830) :: r872 in
  let r874 = [R 83] in
  let r875 = Sub (r830) :: r874 in
  let r876 = S (T T_IN) :: r875 in
  let r877 = Sub (r824) :: r876 in
  let r878 = R 301 :: r877 in
  let r879 = [R 84] in
  let r880 = Sub (r830) :: r879 in
  let r881 = S (T T_IN) :: r880 in
  let r882 = Sub (r824) :: r881 in
  let r883 = [R 589] in
  let r884 = Sub (r87) :: r883 in
  let r885 = [R 79] in
  let r886 = Sub (r281) :: r885 in
  let r887 = S (T T_RBRACKET) :: r886 in
  let r888 = Sub (r884) :: r887 in
  let r889 = [R 590] in
  let r890 = [R 121] in
  let r891 = Sub (r87) :: r890 in
  let r892 = S (T T_EQUAL) :: r891 in
  let r893 = Sub (r87) :: r892 in
  let r894 = [R 75] in
  let r895 = R 307 :: r894 in
  let r896 = Sub (r893) :: r895 in
  let r897 = [R 76] in
  let r898 = [R 317] in
  let r899 = [R 296] in
  let r900 = R 295 :: r899 in
  let r901 = R 307 :: r900 in
  let r902 = Sub (r830) :: r901 in
  let r903 = S (T T_EQUAL) :: r902 in
  let r904 = S (T T_LIDENT) :: r903 in
  let r905 = R 205 :: r904 in
  let r906 = R 862 :: r905 in
  let r907 = [R 92] in
  let r908 = Sub (r832) :: r907 in
  let r909 = S (T T_MINUSGREATER) :: r908 in
  let r910 = Sub (r81) :: r909 in
  let r911 = [R 93] in
  let r912 = Sub (r832) :: r911 in
  let r913 = [R 91] in
  let r914 = Sub (r832) :: r913 in
  let r915 = S (T T_MINUSGREATER) :: r914 in
  let r916 = [R 294] in
  let r917 = R 293 :: r916 in
  let r918 = R 307 :: r917 in
  let r919 = Sub (r832) :: r918 in
  let r920 = S (T T_COLON) :: r919 in
  let r921 = S (T T_LIDENT) :: r920 in
  let r922 = R 205 :: r921 in
  let r923 = R 862 :: r922 in
  let r924 = [R 311] in
  let r925 = [R 621] in
  let r926 = [R 637] in
  let r927 = R 307 :: r926 in
  let r928 = S (N N_module_type) :: r927 in
  let r929 = R 301 :: r928 in
  let r930 = [R 625] in
  let r931 = [R 304] in
  let r932 = R 303 :: r931 in
  let r933 = R 307 :: r932 in
  let r934 = R 568 :: r933 in
  let r935 = R 831 :: r934 in
  let r936 = S (T T_LIDENT) :: r935 in
  let r937 = R 835 :: r936 in
  let r938 = [R 626] in
  let r939 = [R 306] in
  let r940 = R 305 :: r939 in
  let r941 = R 307 :: r940 in
  let r942 = R 568 :: r941 in
  let r943 = Sub (r177) :: r942 in
  let r944 = S (T T_COLONEQUAL) :: r943 in
  let r945 = S (T T_LIDENT) :: r944 in
  let r946 = R 835 :: r945 in
  let r947 = [R 52] in
  let r948 = Sub (r828) :: r947 in
  let r949 = [R 61] in
  let r950 = Sub (r948) :: r949 in
  let r951 = S (T T_EQUAL) :: r950 in
  let r952 = [R 808] in
  let r953 = R 291 :: r952 in
  let r954 = R 307 :: r953 in
  let r955 = Sub (r951) :: r954 in
  let r956 = S (T T_LIDENT) :: r955 in
  let r957 = R 205 :: r956 in
  let r958 = R 862 :: r957 in
  let r959 = R 301 :: r958 in
  let r960 = [R 89] in
  let r961 = S (T T_END) :: r960 in
  let r962 = R 318 :: r961 in
  let r963 = R 69 :: r962 in
  let r964 = [R 857] in
  let r965 = Sub (r1) :: r964 in
  let r966 = S (T T_EQUAL) :: r965 in
  let r967 = S (T T_LIDENT) :: r966 in
  let r968 = R 406 :: r967 in
  let r969 = R 301 :: r968 in
  let r970 = [R 55] in
  let r971 = R 307 :: r970 in
  let r972 = [R 858] in
  let r973 = Sub (r1) :: r972 in
  let r974 = S (T T_EQUAL) :: r973 in
  let r975 = S (T T_LIDENT) :: r974 in
  let r976 = R 406 :: r975 in
  let r977 = [R 860] in
  let r978 = Sub (r1) :: r977 in
  let r979 = [R 856] in
  let r980 = Sub (r87) :: r979 in
  let r981 = S (T T_COLON) :: r980 in
  let r982 = [R 859] in
  let r983 = Sub (r1) :: r982 in
  let r984 = [R 350] in
  let r985 = Sub (r409) :: r984 in
  let r986 = S (T T_LIDENT) :: r985 in
  let r987 = R 545 :: r986 in
  let r988 = R 301 :: r987 in
  let r989 = [R 56] in
  let r990 = R 307 :: r989 in
  let r991 = [R 351] in
  let r992 = Sub (r409) :: r991 in
  let r993 = S (T T_LIDENT) :: r992 in
  let r994 = R 545 :: r993 in
  let r995 = [R 353] in
  let r996 = Sub (r1) :: r995 in
  let r997 = S (T T_EQUAL) :: r996 in
  let r998 = [R 355] in
  let r999 = Sub (r1) :: r998 in
  let r1000 = S (T T_EQUAL) :: r999 in
  let r1001 = Sub (r87) :: r1000 in
  let r1002 = S (T T_DOT) :: r1001 in
  let r1003 = [R 349] in
  let r1004 = Sub (r89) :: r1003 in
  let r1005 = S (T T_COLON) :: r1004 in
  let r1006 = [R 352] in
  let r1007 = Sub (r1) :: r1006 in
  let r1008 = S (T T_EQUAL) :: r1007 in
  let r1009 = [R 354] in
  let r1010 = Sub (r1) :: r1009 in
  let r1011 = S (T T_EQUAL) :: r1010 in
  let r1012 = Sub (r87) :: r1011 in
  let r1013 = S (T T_DOT) :: r1012 in
  let r1014 = [R 58] in
  let r1015 = R 307 :: r1014 in
  let r1016 = Sub (r1) :: r1015 in
  let r1017 = [R 53] in
  let r1018 = R 307 :: r1017 in
  let r1019 = R 477 :: r1018 in
  let r1020 = Sub (r948) :: r1019 in
  let r1021 = [R 54] in
  let r1022 = R 307 :: r1021 in
  let r1023 = R 477 :: r1022 in
  let r1024 = Sub (r948) :: r1023 in
  let r1025 = [R 85] in
  let r1026 = S (T T_RPAREN) :: r1025 in
  let r1027 = [R 48] in
  let r1028 = Sub (r948) :: r1027 in
  let r1029 = S (T T_IN) :: r1028 in
  let r1030 = Sub (r824) :: r1029 in
  let r1031 = R 301 :: r1030 in
  let r1032 = [R 281] in
  let r1033 = R 307 :: r1032 in
  let r1034 = Sub (r243) :: r1033 in
  let r1035 = R 552 :: r1034 in
  let r1036 = R 301 :: r1035 in
  let r1037 = [R 49] in
  let r1038 = Sub (r948) :: r1037 in
  let r1039 = S (T T_IN) :: r1038 in
  let r1040 = Sub (r824) :: r1039 in
  let r1041 = [R 87] in
  let r1042 = Sub (r218) :: r1041 in
  let r1043 = S (T T_RBRACKET) :: r1042 in
  let r1044 = [R 64] in
  let r1045 = Sub (r948) :: r1044 in
  let r1046 = S (T T_MINUSGREATER) :: r1045 in
  let r1047 = Sub (r523) :: r1046 in
  let r1048 = [R 46] in
  let r1049 = Sub (r1047) :: r1048 in
  let r1050 = [R 47] in
  let r1051 = Sub (r948) :: r1050 in
  let r1052 = [R 249] in
  let r1053 = [R 280] in
  let r1054 = R 307 :: r1053 in
  let r1055 = Sub (r243) :: r1054 in
  let r1056 = [R 88] in
  let r1057 = S (T T_RPAREN) :: r1056 in
  let r1058 = [R 478] in
  let r1059 = [R 57] in
  let r1060 = R 307 :: r1059 in
  let r1061 = Sub (r893) :: r1060 in
  let r1062 = [R 59] in
  let r1063 = [R 319] in
  let r1064 = [R 62] in
  let r1065 = Sub (r948) :: r1064 in
  let r1066 = S (T T_EQUAL) :: r1065 in
  let r1067 = [R 63] in
  let r1068 = [R 292] in
  let r1069 = R 291 :: r1068 in
  let r1070 = R 307 :: r1069 in
  let r1071 = Sub (r951) :: r1070 in
  let r1072 = S (T T_LIDENT) :: r1071 in
  let r1073 = R 205 :: r1072 in
  let r1074 = R 862 :: r1073 in
  let r1075 = [R 315] in
  let r1076 = [R 796] in
  let r1077 = [R 810] in
  let r1078 = R 307 :: r1077 in
  let r1079 = S (N N_module_expr) :: r1078 in
  let r1080 = R 301 :: r1079 in
  let r1081 = [R 800] in
  let r1082 = [R 793] in
  let r1083 = R 312 :: r1082 in
  let r1084 = [R 655] in
  let r1085 = S (T T_RBRACKET) :: r1084 in
  let r1086 = Sub (r1) :: r1085 in
  let r1087 = [R 654] in
  let r1088 = S (T T_RBRACE) :: r1087 in
  let r1089 = Sub (r1) :: r1088 in
  let r1090 = [R 657] in
  let r1091 = S (T T_RPAREN) :: r1090 in
  let r1092 = Sub (r486) :: r1091 in
  let r1093 = S (T T_LPAREN) :: r1092 in
  let r1094 = [R 661] in
  let r1095 = S (T T_RBRACKET) :: r1094 in
  let r1096 = Sub (r486) :: r1095 in
  let r1097 = [R 659] in
  let r1098 = S (T T_RBRACE) :: r1097 in
  let r1099 = Sub (r486) :: r1098 in
  let r1100 = [R 190] in
  let r1101 = [R 660] in
  let r1102 = S (T T_RBRACKET) :: r1101 in
  let r1103 = Sub (r486) :: r1102 in
  let r1104 = [R 194] in
  let r1105 = [R 658] in
  let r1106 = S (T T_RBRACE) :: r1105 in
  let r1107 = Sub (r486) :: r1106 in
  let r1108 = [R 192] in
  let r1109 = [R 187] in
  let r1110 = [R 189] in
  let r1111 = [R 188] in
  let r1112 = [R 191] in
  let r1113 = [R 195] in
  let r1114 = [R 193] in
  let r1115 = [R 186] in
  let r1116 = [R 288] in
  let r1117 = Sub (r1) :: r1116 in
  let r1118 = [R 290] in
  let r1119 = [R 677] in
  let r1120 = [R 689] in
  let r1121 = [R 688] in
  let r1122 = [R 97] in
  let r1123 = S (N N_expr) :: r1122 in
  let r1124 = S (T T_IN) :: r1123 in
  let r1125 = S (N N_pattern) :: r1124 in
  let r1126 = R 301 :: r1125 in
  let r1127 = R 198 :: r1126 in
  let r1128 = [R 583] in
  let r1129 = Sub (r1127) :: r1128 in
  let r1130 = [R 98] in
  let r1131 = S (T T_BARRBRACKET) :: r1130 in
  let r1132 = [R 99] in
  let r1133 = S (T T_BARRBRACKET) :: r1132 in
  let r1134 = [R 584] in
  let r1135 = [R 96] in
  let r1136 = S (N N_expr) :: r1135 in
  let r1137 = Sub (r546) :: r1136 in
  let r1138 = [R 696] in
  let r1139 = [R 695] in
  let r1140 = [R 102] in
  let r1141 = S (T T_RBRACKET) :: r1140 in
  let r1142 = [R 103] in
  let r1143 = S (T T_RBRACKET) :: r1142 in
  let r1144 = S (T T_LIDENT) :: r491 in
  let r1145 = [R 678] in
  let r1146 = S (T T_GREATERRBRACE) :: r1145 in
  let r1147 = [R 685] in
  let r1148 = S (T T_RBRACE) :: r1147 in
  let r1149 = [R 555] in
  let r1150 = Sub (r496) :: r1149 in
  let r1151 = [R 138] in
  let r1152 = S (T T_DONE) :: r1151 in
  let r1153 = Sub (r1) :: r1152 in
  let r1154 = S (T T_DO) :: r1153 in
  let r1155 = Sub (r1) :: r1154 in
  let r1156 = Sub (r546) :: r1155 in
  let r1157 = [R 212] in
  let r1158 = Sub (r526) :: r1157 in
  let r1159 = S (T T_RPAREN) :: r1158 in
  let r1160 = [R 210] in
  let r1161 = Sub (r1) :: r1160 in
  let r1162 = S (T T_MINUSGREATER) :: r1161 in
  let r1163 = [R 211] in
  let r1164 = [R 580] in
  let r1165 = [R 150] in
  let r1166 = [R 662] in
  let r1167 = [R 674] in
  let r1168 = [R 141] in
  let r1169 = Sub (r1) :: r1168 in
  let r1170 = S (T T_IN) :: r1169 in
  let r1171 = Sub (r661) :: r1170 in
  let r1172 = Sub (r63) :: r1171 in
  let r1173 = R 301 :: r1172 in
  let r1174 = [R 142] in
  let r1175 = Sub (r1) :: r1174 in
  let r1176 = S (T T_IN) :: r1175 in
  let r1177 = R 301 :: r1176 in
  let r1178 = R 218 :: r1177 in
  let r1179 = Sub (r130) :: r1178 in
  let r1180 = R 301 :: r1179 in
  let r1181 = [R 331] in
  let r1182 = Sub (r251) :: r1181 in
  let r1183 = [R 335] in
  let r1184 = Sub (r1182) :: r1183 in
  let r1185 = S (T T_RPAREN) :: r1184 in
  let r1186 = Sub (r380) :: r1185 in
  let r1187 = [R 332] in
  let r1188 = Sub (r1) :: r1187 in
  let r1189 = [R 334] in
  let r1190 = [R 273] in
  let r1191 = Sub (r1) :: r1190 in
  let r1192 = S (T T_EQUAL) :: r1191 in
  let r1193 = Sub (r87) :: r1192 in
  let r1194 = [R 271] in
  let r1195 = Sub (r1) :: r1194 in
  let r1196 = [R 686] in
  let r1197 = [R 665] in
  let r1198 = S (T T_RPAREN) :: r1197 in
  let r1199 = S (N N_module_expr) :: r1198 in
  let r1200 = R 301 :: r1199 in
  let r1201 = [R 666] in
  let r1202 = S (T T_RPAREN) :: r1201 in
  let r1203 = [R 652] in
  let r1204 = [R 498] in
  let r1205 = S (T T_RPAREN) :: r1204 in
  let r1206 = [R 496] in
  let r1207 = S (T T_RPAREN) :: r1206 in
  let r1208 = [R 497] in
  let r1209 = S (T T_RPAREN) :: r1208 in
  let r1210 = [R 314] in
  let r1211 = R 312 :: r1210 in
  let r1212 = [R 787] in
  let r1213 = [R 785] in
  let r1214 = [R 344] in
  let r1215 = [R 29] in
  let r1216 = [R 28] in
  let r1217 = Sub (r126) :: r1216 in
  let r1218 = [R 33] in
  let r1219 = [R 595] in
  let r1220 = [R 22] in
  let r1221 = [R 596] in
  let r1222 = [R 433] in
  let r1223 = S (T T_RBRACE) :: r1222 in
  let r1224 = [R 202] in
  let r1225 = R 301 :: r1224 in
  let r1226 = [R 203] in
  let r1227 = R 301 :: r1226 in
  let r1228 = [R 68] in
  let r1229 = S (T T_RPAREN) :: r1228 in
  let r1230 = [R 134] in
  let r1231 = [R 136] in
  let r1232 = [R 135] in
  let r1233 = [R 232] in
  let r1234 = [R 237] in
  let r1235 = [R 361] in
  let r1236 = [R 364] in
  let r1237 = S (T T_RPAREN) :: r1236 in
  let r1238 = S (T T_COLONCOLON) :: r1237 in
  let r1239 = S (T T_LPAREN) :: r1238 in
  let r1240 = [R 499] in
  let r1241 = [R 500] in
  let r1242 = [R 501] in
  let r1243 = [R 502] in
  let r1244 = [R 503] in
  let r1245 = [R 504] in
  let r1246 = [R 505] in
  let r1247 = [R 506] in
  let r1248 = [R 507] in
  let r1249 = [R 508] in
  let r1250 = [R 509] in
  let r1251 = [R 815] in
  let r1252 = [R 824] in
  let r1253 = [R 321] in
  let r1254 = [R 822] in
  let r1255 = S (T T_SEMISEMI) :: r1254 in
  let r1256 = [R 823] in
  let r1257 = [R 323] in
  let r1258 = [R 326] in
  let r1259 = [R 325] in
  let r1260 = [R 324] in
  let r1261 = R 322 :: r1260 in
  let r1262 = [R 851] in
  let r1263 = S (T T_EOF) :: r1262 in
  let r1264 = R 322 :: r1263 in
  let r1265 = [R 850] in
  function
  | 0 | 1880 | 1884 | 1902 | 1906 | 1910 | 1914 | 1918 | 1922 | 1926 | 1930 | 1934 | 1938 | 1944 | 1964 -> Nothing
  | 1879 -> One ([R 0])
  | 1883 -> One ([R 1])
  | 1889 -> One ([R 2])
  | 1903 -> One ([R 3])
  | 1907 -> One ([R 4])
  | 1913 -> One ([R 5])
  | 1915 -> One ([R 6])
  | 1919 -> One ([R 7])
  | 1923 -> One ([R 8])
  | 1927 -> One ([R 9])
  | 1931 -> One ([R 10])
  | 1937 -> One ([R 11])
  | 1941 -> One ([R 12])
  | 1954 -> One ([R 13])
  | 1974 -> One ([R 14])
  | 218 -> One ([R 15])
  | 217 -> One ([R 16])
  | 1897 -> One ([R 20])
  | 1899 -> One ([R 21])
  | 287 -> One ([R 26])
  | 302 -> One ([R 27])
  | 298 -> One ([R 41])
  | 1326 -> One ([R 45])
  | 1335 -> One ([R 50])
  | 1330 -> One ([R 51])
  | 1371 -> One ([R 60])
  | 1338 -> One ([R 65])
  | 1119 -> One ([R 77])
  | 1099 -> One ([R 78])
  | 1101 -> One ([R 82])
  | 1333 -> One ([R 86])
  | 1557 -> One ([R 100])
  | 1542 -> One ([R 101])
  | 1572 -> One ([R 104])
  | 1570 -> One ([R 105])
  | 395 -> One ([R 107])
  | 75 -> One ([R 108])
  | 393 -> One ([R 109])
  | 74 -> One ([R 113])
  | 202 | 868 -> One ([R 114])
  | 902 -> One ([R 117])
  | 936 -> One ([R 125])
  | 940 -> One ([R 126])
  | 346 -> One ([R 128])
  | 1552 -> One ([R 129])
  | 659 -> One ([R 140])
  | 1509 -> One ([R 156])
  | 682 -> One ([R 157])
  | 704 -> One ([R 158])
  | 685 -> One ([R 159])
  | 702 -> One ([R 196])
  | 1 -> One (R 198 :: r7)
  | 63 -> One (R 198 :: r24)
  | 68 -> One (R 198 :: r29)
  | 71 -> One (R 198 :: r40)
  | 78 -> One (R 198 :: r48)
  | 98 -> One (R 198 :: r67)
  | 109 -> One (R 198 :: r95)
  | 219 -> One (R 198 :: r205)
  | 220 -> One (R 198 :: r209)
  | 226 -> One (R 198 :: r221)
  | 241 -> One (R 198 :: r231)
  | 244 -> One (R 198 :: r236)
  | 253 -> One (R 198 :: r248)
  | 387 -> One (R 198 :: r361)
  | 410 -> One (R 198 :: r374)
  | 501 -> One (R 198 :: r436)
  | 592 -> One (R 198 :: r506)
  | 595 -> One (R 198 :: r509)
  | 598 -> One (R 198 :: r514)
  | 601 -> One (R 198 :: r517)
  | 607 -> One (R 198 :: r530)
  | 615 -> One (R 198 :: r544)
  | 620 -> One (R 198 :: r556)
  | 636 -> One (R 198 :: r567)
  | 650 -> One (R 198 :: r573)
  | 809 -> One (R 198 :: r666)
  | 850 -> One (R 198 :: r703)
  | 992 -> One (R 198 :: r791)
  | 993 -> One (R 198 :: r795)
  | 1002 -> One (R 198 :: r803)
  | 1043 -> One (R 198 :: r841)
  | 1044 -> One (R 198 :: r850)
  | 1180 -> One (R 198 :: r929)
  | 1212 -> One (R 198 :: r959)
  | 1412 -> One (R 198 :: r1080)
  | 1668 -> One (R 198 :: r1173)
  | 1675 -> One (R 198 :: r1180)
  | 1726 -> One (R 198 :: r1200)
  | 325 -> One ([R 214])
  | 513 -> One ([R 217])
  | 155 -> One ([R 230])
  | 848 -> One ([R 233])
  | 849 -> One ([R 234])
  | 133 -> One (R 235 :: r101)
  | 137 -> One (R 235 :: r103)
  | 216 -> One ([R 239])
  | 892 -> One ([R 243])
  | 893 -> One ([R 244])
  | 1329 -> One ([R 248])
  | 801 -> One ([R 267])
  | 772 -> One ([R 269])
  | 1409 -> One ([R 279])
  | 1336 -> One ([R 282])
  | 477 -> One ([R 283])
  | 1685 -> One ([R 285])
  | 107 -> One (R 301 :: r75)
  | 173 -> One (R 301 :: r122)
  | 224 -> One (R 301 :: r214)
  | 237 -> One (R 301 :: r226)
  | 504 -> One (R 301 :: r440)
  | 511 -> One (R 301 :: r450)
  | 752 -> One (R 301 :: r628)
  | 832 -> One (R 301 :: r686)
  | 1021 -> One (R 301 :: r822)
  | 1055 -> One (R 301 :: r856)
  | 1061 -> One (R 301 :: r864)
  | 1072 -> One (R 301 :: r870)
  | 1083 -> One (R 301 :: r873)
  | 1087 -> One (R 301 :: r882)
  | 1108 -> One (R 301 :: r896)
  | 1124 -> One (R 301 :: r906)
  | 1159 -> One (R 301 :: r923)
  | 1186 -> One (R 301 :: r937)
  | 1196 -> One (R 301 :: r946)
  | 1223 -> One (R 301 :: r963)
  | 1227 -> One (R 301 :: r976)
  | 1256 -> One (R 301 :: r994)
  | 1295 -> One (R 301 :: r1016)
  | 1299 -> One (R 301 :: r1020)
  | 1300 -> One (R 301 :: r1024)
  | 1311 -> One (R 301 :: r1040)
  | 1319 -> One (R 301 :: r1049)
  | 1363 -> One (R 301 :: r1061)
  | 1383 -> One (R 301 :: r1074)
  | 1781 -> One (R 301 :: r1214)
  | 1185 -> One (R 303 :: r930)
  | 1417 -> One (R 303 :: r1081)
  | 1195 -> One (R 305 :: r938)
  | 817 -> One (R 307 :: r674)
  | 1117 -> One (R 307 :: r897)
  | 1178 -> One (R 307 :: r925)
  | 1369 -> One (R 307 :: r1062)
  | 1410 -> One (R 307 :: r1076)
  | 1422 -> One (R 307 :: r1083)
  | 1761 -> One (R 307 :: r1211)
  | 1959 -> One (R 307 :: r1255)
  | 1970 -> One (R 307 :: r1261)
  | 1975 -> One (R 307 :: r1264)
  | 991 -> One (R 309 :: r787)
  | 1170 -> One (R 309 :: r924)
  | 215 -> One (R 312 :: r201)
  | 1393 -> One (R 312 :: r1075)
  | 1120 -> One (R 316 :: r898)
  | 1372 -> One (R 318 :: r1063)
  | 1957 -> One (R 320 :: r1253)
  | 1965 -> One (R 322 :: r1257)
  | 1966 -> One (R 322 :: r1258)
  | 1967 -> One (R 322 :: r1259)
  | 464 -> One ([R 328])
  | 468 -> One ([R 330])
  | 693 -> One ([R 337])
  | 1406 -> One ([R 338])
  | 1630 -> One ([R 341])
  | 1784 -> One ([R 342])
  | 1787 -> One ([R 343])
  | 1786 -> One ([R 345])
  | 1785 -> One ([R 347])
  | 1783 -> One ([R 348])
  | 1898 -> One ([R 360])
  | 1888 -> One ([R 362])
  | 1896 -> One ([R 363])
  | 1895 -> One ([R 365])
  | 627 -> One ([R 372])
  | 1593 -> One ([R 373])
  | 569 -> One ([R 384])
  | 579 -> One ([R 385])
  | 580 -> One ([R 386])
  | 578 -> One ([R 387])
  | 581 -> One ([R 389])
  | 172 -> One ([R 390])
  | 102 | 1012 -> One ([R 391])
  | 540 -> One ([R 398])
  | 517 -> One ([R 399])
  | 547 -> One ([R 402])
  | 1229 | 1242 -> One ([R 407])
  | 877 -> One ([R 409])
  | 878 -> One ([R 410])
  | 876 -> One ([R 411])
  | 1065 -> One ([R 413])
  | 1063 -> One ([R 414])
  | 1066 -> One ([R 415])
  | 1064 -> One ([R 416])
  | 428 -> One ([R 419])
  | 861 -> One ([R 421])
  | 948 -> One ([R 422])
  | 1807 -> One ([R 423])
  | 964 -> One ([R 424])
  | 1808 -> One ([R 425])
  | 963 -> One ([R 426])
  | 955 -> One ([R 427])
  | 92 | 248 -> One ([R 440])
  | 116 | 645 -> One ([R 441])
  | 144 -> One ([R 442])
  | 132 -> One ([R 444])
  | 136 -> One ([R 446])
  | 140 -> One ([R 448])
  | 123 -> One ([R 449])
  | 143 | 1529 -> One ([R 450])
  | 122 -> One ([R 451])
  | 121 -> One ([R 452])
  | 120 -> One ([R 453])
  | 119 -> One ([R 454])
  | 118 -> One ([R 455])
  | 95 | 113 | 635 -> One ([R 456])
  | 94 | 634 -> One ([R 457])
  | 93 -> One ([R 458])
  | 115 | 434 | 644 -> One ([R 459])
  | 114 | 643 -> One ([R 460])
  | 90 -> One ([R 461])
  | 96 -> One ([R 462])
  | 125 -> One ([R 463])
  | 117 -> One ([R 464])
  | 124 -> One ([R 465])
  | 97 -> One ([R 466])
  | 142 -> One ([R 467])
  | 145 -> One ([R 468])
  | 141 -> One ([R 470])
  | 333 -> One ([R 471])
  | 332 -> One (R 472 :: r314)
  | 265 -> One (R 473 :: r268)
  | 266 -> One ([R 474])
  | 465 -> One (R 475 :: r395)
  | 466 -> One ([R 476])
  | 1582 -> One ([R 490])
  | 161 -> One ([R 491])
  | 420 -> One ([R 511])
  | 414 -> One ([R 512])
  | 415 -> One ([R 514])
  | 413 | 646 -> One ([R 521])
  | 796 -> One ([R 527])
  | 797 -> One ([R 528])
  | 798 -> One ([R 530])
  | 486 -> One ([R 532])
  | 1211 -> One ([R 536])
  | 970 | 1276 -> One ([R 546])
  | 1076 -> One ([R 548])
  | 1074 -> One ([R 549])
  | 1077 -> One ([R 550])
  | 1075 -> One ([R 551])
  | 1345 -> One (R 552 :: r1055)
  | 256 -> One ([R 553])
  | 946 -> One ([R 556])
  | 947 -> One ([R 557])
  | 942 -> One ([R 558])
  | 1824 -> One ([R 560])
  | 1823 -> One ([R 561])
  | 1825 -> One ([R 562])
  | 1820 -> One ([R 563])
  | 1821 -> One ([R 564])
  | 976 -> One ([R 566])
  | 974 -> One ([R 567])
  | 562 -> One ([R 570])
  | 514 -> One ([R 571])
  | 1332 -> One ([R 572])
  | 1331 -> One ([R 573])
  | 361 -> One ([R 575])
  | 324 -> One ([R 601])
  | 1448 -> One ([R 604])
  | 1449 -> One ([R 605])
  | 1653 -> One ([R 607])
  | 1654 -> One ([R 608])
  | 459 -> One ([R 610])
  | 460 -> One ([R 611])
  | 1585 -> One ([R 613])
  | 1586 -> One ([R 614])
  | 707 -> One ([R 616])
  | 711 -> One ([R 617])
  | 1206 -> One ([R 622])
  | 1169 -> One ([R 623])
  | 1172 -> One ([R 624])
  | 1171 -> One ([R 629])
  | 1176 -> One ([R 632])
  | 1175 -> One ([R 634])
  | 1174 -> One ([R 635])
  | 1173 -> One ([R 636])
  | 1207 -> One ([R 639])
  | 88 -> One ([R 642])
  | 85 -> One ([R 644])
  | 626 -> One ([R 668])
  | 689 -> One ([R 669])
  | 688 | 703 -> One ([R 670])
  | 629 | 684 -> One ([R 671])
  | 1456 | 1506 -> One ([R 676])
  | 687 -> One ([R 681])
  | 1661 -> One ([R 691])
  | 1724 -> One ([R 692])
  | 1573 -> One ([R 693])
  | 1559 -> One ([R 694])
  | 396 -> One ([R 698])
  | 400 -> One ([R 701])
  | 401 -> One ([R 705])
  | 432 -> One ([R 707])
  | 405 -> One ([R 708])
  | 461 -> One ([R 710])
  | 423 -> One ([R 715])
  | 30 -> One ([R 716])
  | 8 -> One ([R 717])
  | 54 -> One ([R 719])
  | 53 -> One ([R 720])
  | 52 -> One ([R 721])
  | 51 -> One ([R 722])
  | 50 -> One ([R 723])
  | 49 -> One ([R 724])
  | 48 -> One ([R 725])
  | 47 -> One ([R 726])
  | 46 -> One ([R 727])
  | 45 -> One ([R 728])
  | 44 -> One ([R 729])
  | 43 -> One ([R 730])
  | 42 -> One ([R 731])
  | 41 -> One ([R 732])
  | 40 -> One ([R 733])
  | 39 -> One ([R 734])
  | 38 -> One ([R 735])
  | 23 -> One ([R 736])
  | 37 -> One ([R 737])
  | 36 -> One ([R 738])
  | 35 -> One ([R 739])
  | 34 -> One ([R 740])
  | 33 -> One ([R 741])
  | 32 -> One ([R 742])
  | 31 -> One ([R 743])
  | 29 -> One ([R 744])
  | 28 -> One ([R 745])
  | 27 -> One ([R 746])
  | 26 -> One ([R 747])
  | 25 -> One ([R 748])
  | 24 -> One ([R 749])
  | 22 -> One ([R 750])
  | 21 -> One ([R 751])
  | 20 -> One ([R 752])
  | 19 -> One ([R 753])
  | 18 -> One ([R 754])
  | 17 -> One ([R 755])
  | 16 -> One ([R 756])
  | 15 -> One ([R 757])
  | 14 -> One ([R 758])
  | 13 -> One ([R 759])
  | 12 -> One ([R 760])
  | 11 -> One ([R 761])
  | 10 -> One ([R 762])
  | 9 -> One ([R 763])
  | 7 -> One ([R 764])
  | 6 -> One ([R 765])
  | 5 -> One ([R 766])
  | 4 -> One ([R 767])
  | 3 -> One ([R 768])
  | 1401 -> One ([R 769])
  | 321 -> One ([R 774])
  | 315 -> One ([R 775])
  | 1778 -> One ([R 776])
  | 1772 -> One ([R 777])
  | 323 -> One ([R 778])
  | 307 -> One ([R 779])
  | 1428 -> One ([R 792])
  | 1405 | 1427 -> One ([R 794])
  | 1408 | 1429 -> One ([R 795])
  | 1419 -> One ([R 797])
  | 1402 -> One ([R 798])
  | 1392 -> One ([R 799])
  | 1400 -> One ([R 803])
  | 1404 -> One ([R 806])
  | 1403 -> One ([R 807])
  | 1420 -> One ([R 809])
  | 240 -> One ([R 811])
  | 239 -> One ([R 812])
  | 1948 -> One ([R 816])
  | 1949 -> One ([R 817])
  | 1951 -> One ([R 818])
  | 1952 -> One ([R 819])
  | 1950 -> One ([R 820])
  | 1947 -> One ([R 821])
  | 1953 -> One ([R 825])
  | 295 -> One ([R 827])
  | 520 -> One (R 835 :: r467)
  | 534 -> One ([R 836])
  | 179 -> One ([R 841])
  | 182 -> One ([R 842])
  | 186 -> One ([R 843])
  | 180 -> One ([R 844])
  | 187 -> One ([R 845])
  | 183 -> One ([R 846])
  | 188 -> One ([R 847])
  | 185 -> One ([R 848])
  | 178 -> One ([R 849])
  | 397 -> One ([R 854])
  | 686 -> One ([R 855])
  | 1047 -> One ([R 863])
  | 1240 -> One ([R 864])
  | 1243 -> One ([R 865])
  | 1241 -> One ([R 866])
  | 1274 -> One ([R 867])
  | 1277 -> One ([R 868])
  | 1275 -> One ([R 869])
  | 523 -> One ([R 876])
  | 524 -> One ([R 877])
  | 1578 -> One (S (T T_WITH) :: r1150)
  | 168 -> One (S (T T_TYPE) :: r119)
  | 488 -> One (S (T T_TYPE) :: r415)
  | 1693 -> One (S (T T_TYPE) :: r1186)
  | 895 -> One (S (T T_STAR) :: r743)
  | 1955 -> One (S (T T_SEMISEMI) :: r1252)
  | 1962 -> One (S (T T_SEMISEMI) :: r1256)
  | 1885 -> One (S (T T_RPAREN) :: r54)
  | 408 -> One (S (T T_RPAREN) :: r371)
  | 452 -> One (S (T T_RPAREN) :: r394)
  | 506 -> One (S (T T_RPAREN) :: r441)
  | 571 -> One (S (T T_RPAREN) :: r482)
  | 1530 -> One (S (T T_RPAREN) :: r1119)
  | 1736 -> One (S (T T_RPAREN) :: r1203)
  | 1792 -> One (S (T T_RPAREN) :: r1217)
  | 1799 -> One (S (T T_RPAREN) :: r1220)
  | 1886 -> One (S (T T_RPAREN) :: r1235)
  | 268 -> One (S (T T_RBRACKET) :: r269)
  | 872 | 931 -> One (S (T T_RBRACKET) :: r341)
  | 1560 -> One (S (T T_RBRACKET) :: r1138)
  | 1562 -> One (S (T T_RBRACKET) :: r1139)
  | 339 -> One (S (T T_QUOTE) :: r317)
  | 1085 -> One (S (T T_OPEN) :: r878)
  | 1303 -> One (S (T T_OPEN) :: r1031)
  | 162 -> One (S (T T_MODULE) :: r115)
  | 911 -> One (S (T T_MINUSGREATER) :: r749)
  | 915 -> One (S (T T_MINUSGREATER) :: r751)
  | 1146 -> One (S (T T_MINUSGREATER) :: r912)
  | 126 -> One (S (T T_LPAREN) :: r98)
  | 493 -> One (S (T T_LOCAL) :: r419)
  | 610 | 1218 | 1612 -> One (S (T T_LOCAL) :: r533)
  | 158 -> One (S (T T_LIDENT) :: r110)
  | 260 -> One (S (T T_LIDENT) :: r254)
  | 369 -> One (S (T T_LIDENT) :: r325)
  | 660 -> One (S (T T_LIDENT) :: r580)
  | 661 -> One (S (T T_LIDENT) :: r586)
  | 672 -> One (S (T T_LIDENT) :: r589)
  | 676 -> One (S (T T_LIDENT) :: r591)
  | 879 -> One (S (T T_LIDENT) :: r739)
  | 1244 -> One (S (T T_LIDENT) :: r981)
  | 1278 -> One (S (T T_LIDENT) :: r1005)
  | 1355 -> One (S (T T_LIDENT) :: r1058)
  | 83 -> One (S (T T_INT) :: r52)
  | 86 -> One (S (T T_INT) :: r53)
  | 690 -> One (S (T T_IN) :: r598)
  | 694 -> One (S (T T_IN) :: r600)
  | 1323 -> One (S (T T_IN) :: r1051)
  | 585 -> One (S (T T_GREATERRBRACE) :: r489)
  | 1656 -> One (S (T T_GREATERRBRACE) :: r1167)
  | 207 -> One (S (T T_GREATER) :: r185)
  | 1789 -> One (S (T T_GREATER) :: r1215)
  | 552 -> One (S (T T_EQUAL) :: r478)
  | 769 -> One (S (T T_EQUAL) :: r640)
  | 775 -> One (S (T T_EQUAL) :: r643)
  | 785 -> One (S (T T_EQUAL) :: r648)
  | 1234 -> One (S (T T_EQUAL) :: r978)
  | 1252 -> One (S (T T_EQUAL) :: r983)
  | 1520 -> One (S (T T_EQUAL) :: r1117)
  | 1699 -> One (S (T T_EQUAL) :: r1188)
  | 1712 -> One (S (T T_EQUAL) :: r1195)
  | 1877 -> One (S (T T_EOF) :: r1233)
  | 1881 -> One (S (T T_EOF) :: r1234)
  | 1900 -> One (S (T T_EOF) :: r1240)
  | 1904 -> One (S (T T_EOF) :: r1241)
  | 1908 -> One (S (T T_EOF) :: r1242)
  | 1911 -> One (S (T T_EOF) :: r1243)
  | 1916 -> One (S (T T_EOF) :: r1244)
  | 1920 -> One (S (T T_EOF) :: r1245)
  | 1924 -> One (S (T T_EOF) :: r1246)
  | 1928 -> One (S (T T_EOF) :: r1247)
  | 1932 -> One (S (T T_EOF) :: r1248)
  | 1935 -> One (S (T T_EOF) :: r1249)
  | 1939 -> One (S (T T_EOF) :: r1250)
  | 1979 -> One (S (T T_EOF) :: r1265)
  | 1643 -> One (S (T T_END) :: r1166)
  | 128 -> One (S (T T_DOTDOT) :: r99)
  | 203 -> One (S (T T_DOTDOT) :: r179)
  | 949 -> One (S (T T_DOTDOT) :: r778)
  | 950 -> One (S (T T_DOTDOT) :: r779)
  | 230 | 1442 | 1489 -> One (S (T T_DOT) :: r223)
  | 1942 -> One (S (T T_DOT) :: r479)
  | 762 -> One (S (T T_DOT) :: r637)
  | 855 -> One (S (T T_DOT) :: r705)
  | 882 -> One (S (T T_DOT) :: r741)
  | 909 -> One (S (T T_DOT) :: r747)
  | 1707 -> One (S (T T_DOT) :: r1193)
  | 1890 -> One (S (T T_DOT) :: r1239)
  | 204 | 869 -> One (S (T T_COLONCOLON) :: r181)
  | 208 -> One (S (T T_COLON) :: r190)
  | 508 -> One (S (T T_COLON) :: r444)
  | 1140 -> One (S (T T_COLON) :: r910)
  | 249 -> One (S (T T_BARRBRACKET) :: r239)
  | 375 -> One (S (T T_BARRBRACKET) :: r340)
  | 470 -> One (S (T T_BARRBRACKET) :: r396)
  | 1532 -> One (S (T T_BARRBRACKET) :: r1120)
  | 1534 -> One (S (T T_BARRBRACKET) :: r1121)
  | 1721 -> One (S (T T_BARRBRACKET) :: r1196)
  | 350 -> One (S (T T_BAR) :: r320)
  | 81 -> One (S (N N_pattern) :: r50)
  | 425 -> One (S (N N_pattern) :: r56)
  | 386 -> One (S (N N_pattern) :: r355)
  | 416 -> One (S (N N_pattern) :: r375)
  | 418 -> One (S (N N_pattern) :: r376)
  | 439 -> One (S (N N_pattern) :: r387)
  | 444 -> One (S (N N_pattern) :: r390)
  | 788 -> One (S (N N_pattern) :: r649)
  | 790 -> One (S (N N_pattern) :: r650)
  | 792 -> One (S (N N_pattern) :: r651)
  | 799 -> One (S (N N_pattern) :: r653)
  | 805 -> One (S (N N_pattern) :: r657)
  | 105 -> One (S (N N_module_type) :: r69)
  | 510 -> One (S (N N_module_type) :: r446)
  | 548 -> One (S (N N_module_type) :: r475)
  | 550 -> One (S (N N_module_type) :: r476)
  | 575 -> One (S (N N_module_type) :: r484)
  | 814 -> One (S (N N_module_type) :: r673)
  | 826 -> One (S (N N_module_type) :: r681)
  | 1731 -> One (S (N N_module_type) :: r1202)
  | 1746 -> One (S (N N_module_type) :: r1205)
  | 1749 -> One (S (N N_module_type) :: r1207)
  | 1752 -> One (S (N N_module_type) :: r1209)
  | 223 -> One (S (N N_module_expr) :: r211)
  | 475 -> One (S (N N_let_pattern) :: r402)
  | 476 -> One (S (N N_let_pattern) :: r405)
  | 252 -> One (S (N N_expr) :: r241)
  | 587 -> One (S (N N_expr) :: r492)
  | 591 -> One (S (N N_expr) :: r503)
  | 658 -> One (S (N N_expr) :: r579)
  | 683 -> One (S (N N_expr) :: r596)
  | 698 -> One (S (N N_expr) :: r601)
  | 700 -> One (S (N N_expr) :: r602)
  | 705 -> One (S (N N_expr) :: r603)
  | 712 -> One (S (N N_expr) :: r606)
  | 714 -> One (S (N N_expr) :: r607)
  | 716 -> One (S (N N_expr) :: r608)
  | 718 -> One (S (N N_expr) :: r609)
  | 720 -> One (S (N N_expr) :: r610)
  | 722 -> One (S (N N_expr) :: r611)
  | 724 -> One (S (N N_expr) :: r612)
  | 726 -> One (S (N N_expr) :: r613)
  | 728 -> One (S (N N_expr) :: r614)
  | 730 -> One (S (N N_expr) :: r615)
  | 732 -> One (S (N N_expr) :: r616)
  | 734 -> One (S (N N_expr) :: r617)
  | 736 -> One (S (N N_expr) :: r618)
  | 738 -> One (S (N N_expr) :: r619)
  | 740 -> One (S (N N_expr) :: r620)
  | 742 -> One (S (N N_expr) :: r621)
  | 744 -> One (S (N N_expr) :: r622)
  | 746 -> One (S (N N_expr) :: r623)
  | 748 -> One (S (N N_expr) :: r624)
  | 750 -> One (S (N N_expr) :: r625)
  | 1461 -> One (S (N N_expr) :: r1100)
  | 1466 -> One (S (N N_expr) :: r1104)
  | 1471 -> One (S (N N_expr) :: r1108)
  | 1477 -> One (S (N N_expr) :: r1109)
  | 1482 -> One (S (N N_expr) :: r1110)
  | 1487 -> One (S (N N_expr) :: r1111)
  | 1494 -> One (S (N N_expr) :: r1112)
  | 1499 -> One (S (N N_expr) :: r1113)
  | 1504 -> One (S (N N_expr) :: r1114)
  | 1507 -> One (S (N N_expr) :: r1115)
  | 1539 -> One (S (N N_expr) :: r1133)
  | 1550 -> One (S (N N_expr) :: r1137)
  | 1567 -> One (S (N N_expr) :: r1143)
  | 1640 -> One (S (N N_expr) :: r1165)
  | 250 -> One (Sub (r1) :: r240)
  | 373 -> One (Sub (r1) :: r332)
  | 606 -> One (Sub (r1) :: r521)
  | 807 -> One (Sub (r1) :: r658)
  | 1604 -> One (Sub (r1) :: r1156)
  | 1862 -> One (Sub (r1) :: r1231)
  | 1864 -> One (Sub (r1) :: r1232)
  | 2 -> One (Sub (r11) :: r12)
  | 57 -> One (Sub (r11) :: r13)
  | 61 -> One (Sub (r11) :: r18)
  | 213 -> One (Sub (r11) :: r200)
  | 708 -> One (Sub (r11) :: r605)
  | 803 -> One (Sub (r11) :: r656)
  | 844 -> One (Sub (r11) :: r690)
  | 846 -> One (Sub (r11) :: r693)
  | 1304 -> One (Sub (r11) :: r1036)
  | 604 -> One (Sub (r33) :: r518)
  | 1634 -> One (Sub (r33) :: r1164)
  | 1860 -> One (Sub (r35) :: r1230)
  | 77 -> One (Sub (r42) :: r43)
  | 590 -> One (Sub (r42) :: r501)
  | 625 -> One (Sub (r42) :: r557)
  | 654 -> One (Sub (r42) :: r574)
  | 674 -> One (Sub (r42) :: r590)
  | 1327 -> One (Sub (r42) :: r1052)
  | 822 -> One (Sub (r63) :: r678)
  | 1016 -> One (Sub (r63) :: r816)
  | 923 -> One (Sub (r72) :: r752)
  | 258 -> One (Sub (r77) :: r253)
  | 446 -> One (Sub (r77) :: r391)
  | 794 -> One (Sub (r77) :: r652)
  | 296 -> One (Sub (r79) :: r298)
  | 304 -> One (Sub (r79) :: r300)
  | 908 -> One (Sub (r79) :: r745)
  | 1616 -> One (Sub (r79) :: r1162)
  | 205 -> One (Sub (r81) :: r184)
  | 212 -> One (Sub (r81) :: r197)
  | 289 -> One (Sub (r81) :: r295)
  | 290 -> One (Sub (r81) :: r296)
  | 293 -> One (Sub (r81) :: r297)
  | 308 -> One (Sub (r81) :: r303)
  | 309 -> One (Sub (r81) :: r306)
  | 312 -> One (Sub (r81) :: r307)
  | 318 -> One (Sub (r81) :: r308)
  | 1148 -> One (Sub (r81) :: r915)
  | 1769 -> One (Sub (r81) :: r1212)
  | 1775 -> One (Sub (r81) :: r1213)
  | 336 -> One (Sub (r85) :: r315)
  | 527 -> One (Sub (r85) :: r469)
  | 264 -> One (Sub (r87) :: r261)
  | 383 -> One (Sub (r87) :: r354)
  | 441 -> One (Sub (r87) :: r389)
  | 478 -> One (Sub (r87) :: r406)
  | 530 -> One (Sub (r87) :: r472)
  | 647 -> One (Sub (r87) :: r570)
  | 663 -> One (Sub (r87) :: r587)
  | 667 -> One (Sub (r87) :: r588)
  | 781 -> One (Sub (r87) :: r646)
  | 1057 -> One (Sub (r87) :: r858)
  | 1095 -> One (Sub (r87) :: r889)
  | 1797 -> One (Sub (r87) :: r1219)
  | 1801 -> One (Sub (r87) :: r1221)
  | 1850 -> One (Sub (r87) :: r1229)
  | 1260 -> One (Sub (r89) :: r997)
  | 1284 -> One (Sub (r89) :: r1008)
  | 191 -> One (Sub (r105) :: r174)
  | 763 -> One (Sub (r105) :: r638)
  | 1945 -> One (Sub (r105) :: r1251)
  | 391 -> One (Sub (r126) :: r363)
  | 197 -> One (Sub (r169) :: r175)
  | 184 -> One (Sub (r171) :: r173)
  | 1049 -> One (Sub (r171) :: r852)
  | 201 -> One (Sub (r177) :: r178)
  | 930 -> One (Sub (r177) :: r771)
  | 979 -> One (Sub (r177) :: r786)
  | 261 -> One (Sub (r256) :: r258)
  | 262 -> One (Sub (r256) :: r260)
  | 370 -> One (Sub (r256) :: r328)
  | 371 -> One (Sub (r256) :: r331)
  | 329 -> One (Sub (r263) :: r309)
  | 270 -> One (Sub (r265) :: r271)
  | 284 -> One (Sub (r265) :: r294)
  | 271 -> One (Sub (r277) :: r279)
  | 272 -> One (Sub (r281) :: r282)
  | 300 -> One (Sub (r281) :: r299)
  | 1794 -> One (Sub (r281) :: r1218)
  | 274 -> One (Sub (r290) :: r292)
  | 556 -> One (Sub (r290) :: r480)
  | 1013 -> One (Sub (r290) :: r811)
  | 358 -> One (Sub (r322) :: r324)
  | 1526 -> One (Sub (r334) :: r1118)
  | 374 -> One (Sub (r336) :: r339)
  | 378 -> One (Sub (r351) :: r353)
  | 492 -> One (Sub (r358) :: r416)
  | 402 -> One (Sub (r366) :: r367)
  | 426 -> One (Sub (r380) :: r383)
  | 611 -> One (Sub (r380) :: r536)
  | 756 -> One (Sub (r380) :: r633)
  | 1261 -> One (Sub (r380) :: r1002)
  | 1285 -> One (Sub (r380) :: r1013)
  | 1613 -> One (Sub (r380) :: r1159)
  | 473 -> One (Sub (r398) :: r399)
  | 774 -> One (Sub (r411) :: r641)
  | 560 -> One (Sub (r460) :: r481)
  | 519 -> One (Sub (r462) :: r463)
  | 588 -> One (Sub (r498) :: r500)
  | 1577 -> One (Sub (r498) :: r1148)
  | 1620 -> One (Sub (r526) :: r1163)
  | 838 -> One (Sub (r661) :: r687)
  | 1815 -> One (Sub (r706) :: r1225)
  | 1827 -> One (Sub (r706) :: r1227)
  | 874 -> One (Sub (r722) :: r723)
  | 875 -> One (Sub (r731) :: r733)
  | 932 -> One (Sub (r731) :: r773)
  | 951 -> One (Sub (r731) :: r781)
  | 959 -> One (Sub (r731) :: r783)
  | 1803 -> One (Sub (r731) :: r1223)
  | 1037 -> One (Sub (r798) :: r827)
  | 1030 -> One (Sub (r824) :: r826)
  | 1351 -> One (Sub (r832) :: r1057)
  | 1375 -> One (Sub (r832) :: r1066)
  | 1315 -> One (Sub (r884) :: r1043)
  | 1302 -> One (Sub (r948) :: r1026)
  | 1379 -> One (Sub (r951) :: r1067)
  | 1226 -> One (Sub (r969) :: r971)
  | 1255 -> One (Sub (r988) :: r990)
  | 1544 -> One (Sub (r1127) :: r1134)
  | 1537 -> One (Sub (r1129) :: r1131)
  | 1565 -> One (Sub (r1129) :: r1141)
  | 1574 -> One (Sub (r1144) :: r1146)
  | 1704 -> One (Sub (r1182) :: r1189)
  | 697 -> One (r0)
  | 1876 -> One (r2)
  | 1875 -> One (r3)
  | 1874 -> One (r4)
  | 1873 -> One (r5)
  | 1872 -> One (r6)
  | 60 -> One (r7)
  | 55 -> One (r8)
  | 56 -> One (r10)
  | 59 -> One (r12)
  | 58 -> One (r13)
  | 1421 -> One (r14)
  | 1871 -> One (r16)
  | 1870 -> One (r17)
  | 62 -> One (r18)
  | 1869 -> One (r19)
  | 1868 -> One (r20)
  | 1867 -> One (r21)
  | 1866 -> One (r22)
  | 65 -> One (r23)
  | 64 -> One (r24)
  | 66 -> One (r25)
  | 67 -> One (r26)
  | 1859 -> One (r27)
  | 70 -> One (r28)
  | 69 -> One (r29)
  | 1631 -> One (r30)
  | 1629 -> One (r31)
  | 605 -> One (r32)
  | 1636 -> One (r34)
  | 1858 -> One (r36)
  | 1857 -> One (r37)
  | 1856 -> One (r38)
  | 73 -> One (r39)
  | 72 -> One (r40)
  | 76 -> One (r41)
  | 1725 -> One (r43)
  | 1855 -> One (r44)
  | 1854 -> One (r45)
  | 1853 -> One (r46)
  | 80 -> One (r47)
  | 79 -> One (r48)
  | 1849 -> One (r49)
  | 1848 -> One (r50)
  | 82 -> One (r51)
  | 84 -> One (r52)
  | 87 -> One (r53)
  | 91 -> One (r54)
  | 438 -> One (r55)
  | 437 -> One (r56)
  | 146 -> One (r57)
  | 148 -> One (r59)
  | 147 -> One (r60)
  | 112 -> One (r61)
  | 101 -> One (r62)
  | 104 -> One (r64)
  | 103 -> One (r65)
  | 100 -> One (r66)
  | 99 -> One (r67)
  | 1847 -> One (r68)
  | 1846 -> One (r69)
  | 106 | 153 -> One (r70)
  | 1210 -> One (r71)
  | 1845 -> One (r73)
  | 1844 -> One (r74)
  | 108 -> One (r75)
  | 149 | 251 | 589 | 1592 -> One (r76)
  | 152 -> One (r78)
  | 303 -> One (r80)
  | 288 -> One (r82)
  | 337 -> One (r84)
  | 347 -> One (r86)
  | 864 -> One (r88)
  | 1843 -> One (r90)
  | 1842 -> One (r91)
  | 151 -> One (r92)
  | 150 -> One (r93)
  | 111 -> One (r94)
  | 110 -> One (r95)
  | 131 -> One (r96)
  | 130 -> One (r97)
  | 127 -> One (r98)
  | 129 -> One (r99)
  | 135 -> One (r100)
  | 134 -> One (r101)
  | 139 -> One (r102)
  | 138 -> One (r103)
  | 156 -> One (r104)
  | 164 -> One (r106)
  | 163 -> One (r107)
  | 160 -> One (r109)
  | 159 -> One (r110)
  | 1841 -> One (r111)
  | 1840 -> One (r112)
  | 167 -> One (r113)
  | 166 -> One (r114)
  | 165 -> One (r115)
  | 1839 -> One (r116)
  | 171 -> One (r117)
  | 170 -> One (r118)
  | 169 -> One (r119)
  | 1838 -> One (r120)
  | 1837 -> One (r121)
  | 174 -> One (r122)
  | 206 -> One (r123)
  | 297 -> One (r125)
  | 394 -> One (r127)
  | 922 -> One (r129)
  | 958 -> One (r131)
  | 957 -> One (r132)
  | 956 | 1826 -> One (r133)
  | 1822 -> One (r135)
  | 1836 -> One (r137)
  | 1835 -> One (r138)
  | 1834 -> One (r139)
  | 1833 -> One (r140)
  | 1832 -> One (r141)
  | 985 -> One (r145)
  | 984 -> One (r146)
  | 983 -> One (r147)
  | 1819 -> One (r153)
  | 1818 -> One (r154)
  | 1812 -> One (r155)
  | 1811 -> One (r156)
  | 1810 -> One (r157)
  | 967 -> One (r159)
  | 966 -> One (r160)
  | 965 -> One (r161)
  | 190 -> One (r165)
  | 193 -> One (r167)
  | 189 -> One (r168)
  | 194 -> One (r170)
  | 196 -> One (r172)
  | 195 -> One (r173)
  | 192 -> One (r174)
  | 198 -> One (r175)
  | 935 -> One (r176)
  | 1809 -> One (r178)
  | 1806 -> One (r179)
  | 871 -> One (r180)
  | 870 -> One (r181)
  | 306 -> One (r182)
  | 292 -> One (r183)
  | 1791 -> One (r184)
  | 1788 -> One (r185)
  | 889 -> One (r186)
  | 1780 -> One (r188)
  | 1779 -> One (r189)
  | 209 -> One (r190)
  | 1777 -> One (r191)
  | 1774 -> One (r192)
  | 1773 -> One (r193)
  | 211 -> One (r194)
  | 1771 -> One (r195)
  | 1768 -> One (r196)
  | 1767 -> One (r197)
  | 1766 -> One (r198)
  | 1765 -> One (r199)
  | 214 -> One (r200)
  | 1764 -> One (r201)
  | 1760 -> One (r202)
  | 1759 -> One (r203)
  | 1758 -> One (r204)
  | 1757 -> One (r205)
  | 1756 -> One (r206)
  | 1755 -> One (r207)
  | 222 -> One (r208)
  | 221 -> One (r209)
  | 574 -> One (r210)
  | 573 -> One (r211)
  | 1745 -> One (r212)
  | 1744 -> One (r213)
  | 225 -> One (r214)
  | 229 -> One (r215)
  | 235 -> One (r217)
  | 236 -> One (r219)
  | 228 -> One (r220)
  | 227 -> One (r221)
  | 233 -> One (r222)
  | 231 -> One (r223)
  | 232 -> One (r224)
  | 234 -> One (r225)
  | 238 -> One (r226)
  | 1743 -> One (r227)
  | 1742 -> One (r228)
  | 1741 -> One (r229)
  | 243 -> One (r230)
  | 242 -> One (r231)
  | 1740 -> One (r232)
  | 1739 -> One (r233)
  | 1738 -> One (r234)
  | 246 -> One (r235)
  | 245 -> One (r236)
  | 1735 -> One (r237)
  | 1734 -> One (r238)
  | 1720 -> One (r239)
  | 1719 -> One (r240)
  | 1718 -> One (r241)
  | 754 -> One (r242)
  | 1717 -> One (r244)
  | 1716 -> One (r245)
  | 257 -> One (r246)
  | 255 -> One (r247)
  | 254 -> One (r248)
  | 1698 -> One (r249)
  | 1697 -> One (r250)
  | 1715 -> One (r252)
  | 259 -> One (r253)
  | 368 -> One (r254)
  | 263 -> One (r255)
  | 367 -> One (r257)
  | 366 -> One (r258)
  | 365 -> One (r259)
  | 364 -> One (r260)
  | 363 -> One (r261)
  | 345 -> One (r262)
  | 326 -> One (r264)
  | 357 -> One (r266)
  | 356 -> One (r267)
  | 267 -> One (r268)
  | 269 -> One (r269)
  | 355 -> One (r270)
  | 354 -> One (r271)
  | 286 -> One (r272)
  | 285 -> One (r273)
  | 344 -> One (r275)
  | 331 -> One (r276)
  | 349 -> One (r278)
  | 348 -> One (r279)
  | 282 | 1151 -> One (r280)
  | 283 -> One (r282)
  | 278 -> One (r283)
  | 277 -> One (r284)
  | 281 -> One (r286)
  | 279 -> One (r289)
  | 276 -> One (r291)
  | 275 -> One (r292)
  | 328 -> One (r293)
  | 327 -> One (r294)
  | 322 -> One (r295)
  | 291 -> One (r296)
  | 294 -> One (r297)
  | 299 -> One (r298)
  | 301 -> One (r299)
  | 305 -> One (r300)
  | 320 -> One (r301)
  | 317 -> One (r302)
  | 316 -> One (r303)
  | 314 -> One (r304)
  | 311 -> One (r305)
  | 310 -> One (r306)
  | 313 -> One (r307)
  | 319 -> One (r308)
  | 330 -> One (r309)
  | 343 -> One (r310)
  | 342 -> One (r312)
  | 335 -> One (r313)
  | 334 -> One (r314)
  | 338 -> One (r315)
  | 341 -> One (r316)
  | 340 -> One (r317)
  | 353 -> One (r318)
  | 352 -> One (r319)
  | 351 -> One (r320)
  | 362 -> One (r321)
  | 360 -> One (r323)
  | 359 -> One (r324)
  | 1692 -> One (r325)
  | 1691 -> One (r326)
  | 1690 -> One (r327)
  | 1689 -> One (r328)
  | 1688 -> One (r329)
  | 1687 -> One (r330)
  | 372 -> One (r331)
  | 1686 -> One (r332)
  | 472 -> One (r333)
  | 1528 -> One (r335)
  | 1525 -> One (r337)
  | 1524 -> One (r338)
  | 1523 -> One (r339)
  | 469 -> One (r340)
  | 377 -> One (r341)
  | 458 -> One (r342)
  | 457 -> One (r344)
  | 456 -> One (r345)
  | 379 -> One (r346)
  | 463 -> One (r348)
  | 385 -> One (r349)
  | 382 -> One (r350)
  | 381 -> One (r352)
  | 380 -> One (r353)
  | 384 -> One (r354)
  | 462 -> One (r355)
  | 398 | 780 -> One (r357)
  | 399 -> One (r359)
  | 389 -> One (r360)
  | 388 -> One (r361)
  | 390 -> One (r362)
  | 392 -> One (r363)
  | 404 -> One (r365)
  | 403 -> One (r367)
  | 455 -> One (r368)
  | 454 -> One (r369)
  | 407 -> One (r370)
  | 409 -> One (r371)
  | 449 -> One (r372)
  | 412 -> One (r373)
  | 411 -> One (r374)
  | 417 -> One (r375)
  | 419 -> One (r376)
  | 422 -> One (r377)
  | 448 -> One (r378)
  | 427 -> One (r379)
  | 431 -> One (r381)
  | 430 -> One (r382)
  | 429 -> One (r383)
  | 433 -> One (r384)
  | 436 -> One (r385)
  | 435 -> One (r386)
  | 440 -> One (r387)
  | 443 -> One (r388)
  | 442 -> One (r389)
  | 445 -> One (r390)
  | 447 -> One (r391)
  | 451 -> One (r392)
  | 450 -> One (r393)
  | 453 -> One (r394)
  | 467 -> One (r395)
  | 471 -> One (r396)
  | 474 -> One (r397)
  | 487 -> One (r399)
  | 485 -> One (r400)
  | 484 -> One (r401)
  | 483 -> One (r402)
  | 482 -> One (r403)
  | 481 -> One (r404)
  | 480 -> One (r405)
  | 479 -> One (r406)
  | 1683 -> One (r407)
  | 498 -> One (r408)
  | 778 -> One (r410)
  | 1684 -> One (r412)
  | 491 -> One (r413)
  | 490 -> One (r414)
  | 489 -> One (r415)
  | 497 -> One (r416)
  | 496 -> One (r417)
  | 495 -> One (r418)
  | 494 -> One (r419)
  | 1667 -> One (r420)
  | 1666 -> One (r421)
  | 1665 -> One (r422)
  | 1664 -> One (r423)
  | 1663 -> One (r424)
  | 500 -> One (r425)
  | 1399 -> One (r426)
  | 1398 -> One (r427)
  | 1397 -> One (r428)
  | 1396 -> One (r429)
  | 1395 -> One (r430)
  | 1394 -> One (r431)
  | 1662 -> One (r432)
  | 583 -> One (r433)
  | 582 -> One (r434)
  | 503 -> One (r435)
  | 502 -> One (r436)
  | 570 -> One (r437)
  | 568 -> One (r438)
  | 567 -> One (r439)
  | 505 -> One (r440)
  | 507 -> One (r441)
  | 566 -> One (r442)
  | 565 -> One (r443)
  | 509 -> One (r444)
  | 564 -> One (r445)
  | 563 -> One (r446)
  | 518 -> One (r447)
  | 516 -> One (r448)
  | 515 -> One (r449)
  | 512 -> One (r450)
  | 546 -> One (r451)
  | 545 -> One (r453)
  | 539 -> One (r455)
  | 538 -> One (r456)
  | 537 -> One (r457)
  | 536 -> One (r458)
  | 535 -> One (r459)
  | 558 -> One (r461)
  | 559 -> One (r463)
  | 526 -> One (r464)
  | 525 -> One (r465)
  | 522 -> One (r466)
  | 521 -> One (r467)
  | 529 -> One (r468)
  | 528 -> One (r469)
  | 533 -> One (r470)
  | 532 -> One (r471)
  | 531 -> One (r472)
  | 544 -> One (r473)
  | 549 -> One (r475)
  | 551 -> One (r476)
  | 554 -> One (r477)
  | 553 -> One (r478)
  | 555 | 1943 -> One (r479)
  | 557 -> One (r480)
  | 561 -> One (r481)
  | 572 -> One (r482)
  | 577 -> One (r483)
  | 576 -> One (r484)
  | 1447 | 1536 | 1564 | 1660 | 1723 -> One (r485)
  | 1659 -> One (r487)
  | 1658 -> One (r488)
  | 1655 -> One (r489)
  | 1652 -> One (r490)
  | 586 -> One (r491)
  | 1651 -> One (r492)
  | 1584 -> One (r493)
  | 1583 -> One (r494)
  | 1581 -> One (r495)
  | 1587 -> One (r497)
  | 1650 -> One (r499)
  | 1649 -> One (r500)
  | 1648 -> One (r501)
  | 1647 -> One (r502)
  | 1646 -> One (r503)
  | 1645 -> One (r504)
  | 594 -> One (r505)
  | 593 -> One (r506)
  | 1642 -> One (r507)
  | 597 -> One (r508)
  | 596 -> One (r509)
  | 1639 -> One (r510)
  | 1638 -> One (r511)
  | 1637 -> One (r512)
  | 600 -> One (r513)
  | 599 -> One (r514)
  | 1633 -> One (r515)
  | 603 -> One (r516)
  | 602 -> One (r517)
  | 1632 -> One (r518)
  | 1628 -> One (r519)
  | 1627 -> One (r520)
  | 1626 -> One (r521)
  | 773 -> One (r522)
  | 1611 -> One (r524)
  | 614 -> One (r525)
  | 1625 -> One (r527)
  | 1624 -> One (r528)
  | 609 -> One (r529)
  | 608 -> One (r530)
  | 1221 -> One (r531)
  | 1220 -> One (r532)
  | 1219 -> One (r533)
  | 1623 -> One (r534)
  | 613 -> One (r535)
  | 612 -> One (r536)
  | 1603 -> One (r537)
  | 1602 -> One (r538)
  | 1601 -> One (r539)
  | 1600 -> One (r540)
  | 619 -> One (r541)
  | 618 -> One (r542)
  | 617 -> One (r543)
  | 616 -> One (r544)
  | 1553 -> One (r545)
  | 1599 -> One (r547)
  | 1598 -> One (r548)
  | 1597 -> One (r549)
  | 1596 -> One (r550)
  | 1595 -> One (r551)
  | 1594 -> One (r552)
  | 624 -> One (r553)
  | 623 -> One (r554)
  | 622 -> One (r555)
  | 621 -> One (r556)
  | 628 -> One (r557)
  | 633 -> One (r558)
  | 632 -> One (r559)
  | 631 | 1591 -> One (r560)
  | 1590 -> One (r561)
  | 642 -> One (r562)
  | 641 -> One (r563)
  | 640 -> One (r564)
  | 639 -> One (r565)
  | 638 -> One (r566)
  | 637 -> One (r567)
  | 1519 -> One (r568)
  | 649 -> One (r569)
  | 648 -> One (r570)
  | 653 -> One (r571)
  | 652 -> One (r572)
  | 651 -> One (r573)
  | 655 -> One (r574)
  | 1460 | 1512 -> One (r575)
  | 1459 | 1511 -> One (r576)
  | 657 | 1458 -> One (r577)
  | 656 | 1457 -> One (r578)
  | 1510 -> One (r579)
  | 671 -> One (r580)
  | 666 -> One (r581)
  | 665 | 755 | 1706 -> One (r582)
  | 670 -> One (r584)
  | 669 -> One (r585)
  | 662 -> One (r586)
  | 664 -> One (r587)
  | 668 -> One (r588)
  | 673 -> One (r589)
  | 675 -> One (r590)
  | 677 -> One (r591)
  | 681 | 1476 -> One (r592)
  | 680 | 1475 -> One (r593)
  | 679 | 1474 -> One (r594)
  | 678 | 1473 -> One (r595)
  | 1435 -> One (r596)
  | 692 -> One (r597)
  | 691 -> One (r598)
  | 696 -> One (r599)
  | 695 -> One (r600)
  | 699 -> One (r601)
  | 701 -> One (r602)
  | 706 -> One (r603)
  | 710 -> One (r604)
  | 709 -> One (r605)
  | 713 -> One (r606)
  | 715 -> One (r607)
  | 717 -> One (r608)
  | 719 -> One (r609)
  | 721 -> One (r610)
  | 723 -> One (r611)
  | 725 -> One (r612)
  | 727 -> One (r613)
  | 729 -> One (r614)
  | 731 -> One (r615)
  | 733 -> One (r616)
  | 735 -> One (r617)
  | 737 -> One (r618)
  | 739 -> One (r619)
  | 741 -> One (r620)
  | 743 -> One (r621)
  | 745 -> One (r622)
  | 747 -> One (r623)
  | 749 -> One (r624)
  | 751 -> One (r625)
  | 1434 -> One (r626)
  | 802 -> One (r627)
  | 753 -> One (r628)
  | 761 -> One (r629)
  | 760 -> One (r630)
  | 759 -> One (r631)
  | 758 -> One (r632)
  | 757 -> One (r633)
  | 768 -> One (r634)
  | 767 -> One (r635)
  | 766 -> One (r636)
  | 765 -> One (r637)
  | 764 -> One (r638)
  | 771 -> One (r639)
  | 770 -> One (r640)
  | 779 -> One (r641)
  | 777 -> One (r642)
  | 776 -> One (r643)
  | 784 -> One (r644)
  | 783 -> One (r645)
  | 782 -> One (r646)
  | 787 -> One (r647)
  | 786 -> One (r648)
  | 789 -> One (r649)
  | 791 -> One (r650)
  | 793 -> One (r651)
  | 795 -> One (r652)
  | 800 -> One (r653)
  | 1433 -> One (r654)
  | 1432 -> One (r655)
  | 804 -> One (r656)
  | 806 -> One (r657)
  | 808 -> One (r658)
  | 825 -> One (r659)
  | 824 -> One (r660)
  | 843 -> One (r662)
  | 842 -> One (r663)
  | 841 -> One (r664)
  | 821 -> One (r665)
  | 820 -> One (r666)
  | 819 -> One (r667)
  | 816 -> One (r668)
  | 813 -> One (r669)
  | 812 -> One (r670)
  | 811 -> One (r671)
  | 810 -> One (r672)
  | 815 -> One (r673)
  | 818 -> One (r674)
  | 840 -> One (r675)
  | 831 -> One (r676)
  | 830 -> One (r677)
  | 823 -> One (r678)
  | 829 -> One (r679)
  | 828 -> One (r680)
  | 827 -> One (r681)
  | 837 -> One (r682)
  | 836 -> One (r683)
  | 835 -> One (r684)
  | 834 -> One (r685)
  | 833 -> One (r686)
  | 839 -> One (r687)
  | 1431 -> One (r688)
  | 1430 -> One (r689)
  | 845 -> One (r690)
  | 1426 -> One (r691)
  | 1425 -> One (r692)
  | 847 -> One (r693)
  | 860 -> One (r694)
  | 863 -> One (r696)
  | 862 -> One (r697)
  | 859 -> One (r698)
  | 858 -> One (r699)
  | 854 -> One (r700)
  | 853 -> One (r701)
  | 852 -> One (r702)
  | 851 -> One (r703)
  | 857 -> One (r704)
  | 856 -> One (r705)
  | 907 -> One (r707)
  | 906 -> One (r708)
  | 905 -> One (r709)
  | 900 -> One (r710)
  | 921 -> One (r714)
  | 920 -> One (r715)
  | 919 -> One (r716)
  | 1042 -> One (r717)
  | 1041 -> One (r718)
  | 1040 -> One (r719)
  | 1039 -> One (r720)
  | 899 -> One (r721)
  | 898 -> One (r723)
  | 894 -> One (r730)
  | 891 -> One (r732)
  | 890 -> One (r733)
  | 888 -> One (r734)
  | 887 -> One (r735)
  | 886 -> One (r736)
  | 885 -> One (r737)
  | 881 -> One (r738)
  | 880 -> One (r739)
  | 884 -> One (r740)
  | 883 -> One (r741)
  | 897 -> One (r742)
  | 896 -> One (r743)
  | 904 -> One (r744)
  | 918 -> One (r745)
  | 914 -> One (r746)
  | 910 -> One (r747)
  | 913 -> One (r748)
  | 912 -> One (r749)
  | 917 -> One (r750)
  | 916 -> One (r751)
  | 1209 -> One (r752)
  | 975 -> One (r753)
  | 990 -> One (r755)
  | 989 -> One (r756)
  | 988 -> One (r757)
  | 987 -> One (r758)
  | 986 -> One (r759)
  | 973 -> One (r763)
  | 972 -> One (r764)
  | 971 -> One (r765)
  | 969 -> One (r766)
  | 968 -> One (r767)
  | 945 -> One (r769)
  | 944 -> One (r770)
  | 943 -> One (r771)
  | 934 -> One (r772)
  | 933 -> One (r773)
  | 939 -> One (r774)
  | 938 -> One (r775)
  | 937 | 1814 -> One (r776)
  | 941 | 1813 -> One (r777)
  | 962 -> One (r778)
  | 954 -> One (r779)
  | 953 -> One (r780)
  | 952 -> One (r781)
  | 961 -> One (r782)
  | 960 -> One (r783)
  | 982 -> One (r784)
  | 981 -> One (r785)
  | 980 -> One (r786)
  | 1208 -> One (r787)
  | 1001 -> One (r788)
  | 1000 -> One (r789)
  | 999 -> One (r790)
  | 998 -> One (r791)
  | 997 -> One (r792)
  | 996 -> One (r793)
  | 995 -> One (r794)
  | 994 -> One (r795)
  | 1034 -> One (r796)
  | 1033 -> One (r797)
  | 1036 -> One (r799)
  | 1035 -> One (r800)
  | 1029 -> One (r801)
  | 1011 -> One (r802)
  | 1010 -> One (r803)
  | 1009 -> One (r804)
  | 1008 -> One (r805)
  | 1007 -> One (r806)
  | 1015 -> One (r810)
  | 1014 -> One (r811)
  | 1028 -> One (r812)
  | 1020 -> One (r813)
  | 1019 -> One (r814)
  | 1018 -> One (r815)
  | 1017 -> One (r816)
  | 1027 -> One (r817)
  | 1026 -> One (r818)
  | 1025 -> One (r819)
  | 1024 -> One (r820)
  | 1023 -> One (r821)
  | 1022 -> One (r822)
  | 1032 -> One (r825)
  | 1031 -> One (r826)
  | 1038 -> One (r827)
  | 1098 | 1152 -> One (r829)
  | 1154 -> One (r831)
  | 1168 -> One (r833)
  | 1158 -> One (r834)
  | 1157 -> One (r835)
  | 1139 -> One (r836)
  | 1138 -> One (r837)
  | 1137 -> One (r838)
  | 1136 -> One (r839)
  | 1135 -> One (r840)
  | 1134 -> One (r841)
  | 1133 -> One (r842)
  | 1123 -> One (r843)
  | 1122 -> One (r844)
  | 1054 -> One (r845)
  | 1053 -> One (r846)
  | 1052 -> One (r847)
  | 1048 -> One (r848)
  | 1046 -> One (r849)
  | 1045 -> One (r850)
  | 1051 -> One (r851)
  | 1050 -> One (r852)
  | 1116 -> One (r853)
  | 1115 -> One (r854)
  | 1060 -> One (r855)
  | 1056 -> One (r856)
  | 1059 -> One (r857)
  | 1058 -> One (r858)
  | 1071 -> One (r859)
  | 1070 -> One (r860)
  | 1069 -> One (r861)
  | 1068 -> One (r862)
  | 1067 -> One (r863)
  | 1062 -> One (r864)
  | 1082 -> One (r865)
  | 1081 -> One (r866)
  | 1080 -> One (r867)
  | 1079 -> One (r868)
  | 1078 -> One (r869)
  | 1073 -> One (r870)
  | 1107 -> One (r871)
  | 1106 -> One (r872)
  | 1084 -> One (r873)
  | 1105 -> One (r874)
  | 1104 -> One (r875)
  | 1103 -> One (r876)
  | 1102 -> One (r877)
  | 1086 -> One (r878)
  | 1100 -> One (r879)
  | 1090 -> One (r880)
  | 1089 -> One (r881)
  | 1088 -> One (r882)
  | 1097 | 1145 -> One (r883)
  | 1094 -> One (r885)
  | 1093 -> One (r886)
  | 1092 -> One (r887)
  | 1091 | 1144 -> One (r888)
  | 1096 -> One (r889)
  | 1112 -> One (r890)
  | 1111 -> One (r891)
  | 1110 -> One (r892)
  | 1114 -> One (r894)
  | 1113 -> One (r895)
  | 1109 -> One (r896)
  | 1118 -> One (r897)
  | 1121 -> One (r898)
  | 1132 -> One (r899)
  | 1131 -> One (r900)
  | 1130 -> One (r901)
  | 1129 -> One (r902)
  | 1128 -> One (r903)
  | 1127 -> One (r904)
  | 1126 -> One (r905)
  | 1125 -> One (r906)
  | 1156 -> One (r907)
  | 1143 -> One (r908)
  | 1142 -> One (r909)
  | 1141 -> One (r910)
  | 1155 -> One (r911)
  | 1147 -> One (r912)
  | 1153 -> One (r913)
  | 1150 -> One (r914)
  | 1149 -> One (r915)
  | 1167 -> One (r916)
  | 1166 -> One (r917)
  | 1165 -> One (r918)
  | 1164 -> One (r919)
  | 1163 -> One (r920)
  | 1162 -> One (r921)
  | 1161 -> One (r922)
  | 1160 -> One (r923)
  | 1177 -> One (r924)
  | 1179 -> One (r925)
  | 1184 -> One (r926)
  | 1183 -> One (r927)
  | 1182 -> One (r928)
  | 1181 -> One (r929)
  | 1194 -> One (r930)
  | 1193 -> One (r931)
  | 1192 -> One (r932)
  | 1191 -> One (r933)
  | 1190 -> One (r934)
  | 1189 -> One (r935)
  | 1188 -> One (r936)
  | 1187 -> One (r937)
  | 1205 -> One (r938)
  | 1204 -> One (r939)
  | 1203 -> One (r940)
  | 1202 -> One (r941)
  | 1201 -> One (r942)
  | 1200 -> One (r943)
  | 1199 -> One (r944)
  | 1198 -> One (r945)
  | 1197 -> One (r946)
  | 1325 -> One (r947)
  | 1374 -> One (r949)
  | 1222 -> One (r950)
  | 1391 -> One (r952)
  | 1382 -> One (r953)
  | 1381 -> One (r954)
  | 1217 -> One (r955)
  | 1216 -> One (r956)
  | 1215 -> One (r957)
  | 1214 -> One (r958)
  | 1213 -> One (r959)
  | 1368 -> One (r960)
  | 1367 -> One (r961)
  | 1225 -> One (r962)
  | 1224 -> One (r963)
  | 1251 -> One (r964)
  | 1250 -> One (r965)
  | 1249 -> One (r966)
  | 1248 -> One (r967)
  | 1239 -> One (r968)
  | 1238 -> One (r970)
  | 1237 -> One (r971)
  | 1233 -> One (r972)
  | 1232 -> One (r973)
  | 1231 -> One (r974)
  | 1230 -> One (r975)
  | 1228 -> One (r976)
  | 1236 -> One (r977)
  | 1235 -> One (r978)
  | 1247 -> One (r979)
  | 1246 -> One (r980)
  | 1245 -> One (r981)
  | 1254 -> One (r982)
  | 1253 -> One (r983)
  | 1294 -> One (r984)
  | 1283 -> One (r985)
  | 1282 -> One (r986)
  | 1273 -> One (r987)
  | 1272 -> One (r989)
  | 1271 -> One (r990)
  | 1270 -> One (r991)
  | 1259 -> One (r992)
  | 1258 -> One (r993)
  | 1257 -> One (r994)
  | 1269 -> One (r995)
  | 1268 -> One (r996)
  | 1267 -> One (r997)
  | 1266 -> One (r998)
  | 1265 -> One (r999)
  | 1264 -> One (r1000)
  | 1263 -> One (r1001)
  | 1262 -> One (r1002)
  | 1281 -> One (r1003)
  | 1280 -> One (r1004)
  | 1279 -> One (r1005)
  | 1293 -> One (r1006)
  | 1292 -> One (r1007)
  | 1291 -> One (r1008)
  | 1290 -> One (r1009)
  | 1289 -> One (r1010)
  | 1288 -> One (r1011)
  | 1287 -> One (r1012)
  | 1286 -> One (r1013)
  | 1298 -> One (r1014)
  | 1297 -> One (r1015)
  | 1296 -> One (r1016)
  | 1362 -> One (r1017)
  | 1361 -> One (r1018)
  | 1360 -> One (r1019)
  | 1359 -> One (r1020)
  | 1358 -> One (r1021)
  | 1357 -> One (r1022)
  | 1354 -> One (r1023)
  | 1301 -> One (r1024)
  | 1350 -> One (r1025)
  | 1349 -> One (r1026)
  | 1344 -> One (r1027)
  | 1343 -> One (r1028)
  | 1342 -> One (r1029)
  | 1341 -> One (r1030)
  | 1310 -> One (r1031)
  | 1309 -> One (r1032)
  | 1308 -> One (r1033)
  | 1307 -> One (r1034)
  | 1306 -> One (r1035)
  | 1305 -> One (r1036)
  | 1340 -> One (r1037)
  | 1314 -> One (r1038)
  | 1313 -> One (r1039)
  | 1312 -> One (r1040)
  | 1318 -> One (r1041)
  | 1317 -> One (r1042)
  | 1316 -> One (r1043)
  | 1337 -> One (r1044)
  | 1322 -> One (r1045)
  | 1321 -> One (r1046)
  | 1339 -> One (r1048)
  | 1320 -> One (r1049)
  | 1334 -> One (r1050)
  | 1324 -> One (r1051)
  | 1328 -> One (r1052)
  | 1348 -> One (r1053)
  | 1347 -> One (r1054)
  | 1346 -> One (r1055)
  | 1353 -> One (r1056)
  | 1352 -> One (r1057)
  | 1356 -> One (r1058)
  | 1366 -> One (r1059)
  | 1365 -> One (r1060)
  | 1364 -> One (r1061)
  | 1370 -> One (r1062)
  | 1373 -> One (r1063)
  | 1378 -> One (r1064)
  | 1377 -> One (r1065)
  | 1376 -> One (r1066)
  | 1380 -> One (r1067)
  | 1390 -> One (r1068)
  | 1389 -> One (r1069)
  | 1388 -> One (r1070)
  | 1387 -> One (r1071)
  | 1386 -> One (r1072)
  | 1385 -> One (r1073)
  | 1384 -> One (r1074)
  | 1407 -> One (r1075)
  | 1411 -> One (r1076)
  | 1416 -> One (r1077)
  | 1415 -> One (r1078)
  | 1414 -> One (r1079)
  | 1413 -> One (r1080)
  | 1418 -> One (r1081)
  | 1424 -> One (r1082)
  | 1423 -> One (r1083)
  | 1438 | 1481 -> One (r1084)
  | 1437 | 1480 -> One (r1085)
  | 1436 | 1479 -> One (r1086)
  | 1441 | 1486 -> One (r1087)
  | 1440 | 1485 -> One (r1088)
  | 1439 | 1484 -> One (r1089)
  | 1446 | 1493 -> One (r1090)
  | 1445 | 1492 -> One (r1091)
  | 1444 | 1491 -> One (r1092)
  | 1443 | 1490 -> One (r1093)
  | 1452 | 1498 -> One (r1094)
  | 1451 | 1497 -> One (r1095)
  | 1450 | 1496 -> One (r1096)
  | 1455 | 1503 -> One (r1097)
  | 1454 | 1502 -> One (r1098)
  | 1453 | 1501 -> One (r1099)
  | 1462 -> One (r1100)
  | 1465 | 1515 -> One (r1101)
  | 1464 | 1514 -> One (r1102)
  | 1463 | 1513 -> One (r1103)
  | 1467 -> One (r1104)
  | 1470 | 1518 -> One (r1105)
  | 1469 | 1517 -> One (r1106)
  | 1468 | 1516 -> One (r1107)
  | 1472 -> One (r1108)
  | 1478 -> One (r1109)
  | 1483 -> One (r1110)
  | 1488 -> One (r1111)
  | 1495 -> One (r1112)
  | 1500 -> One (r1113)
  | 1505 -> One (r1114)
  | 1508 -> One (r1115)
  | 1522 -> One (r1116)
  | 1521 -> One (r1117)
  | 1527 -> One (r1118)
  | 1531 -> One (r1119)
  | 1533 -> One (r1120)
  | 1535 -> One (r1121)
  | 1549 -> One (r1122)
  | 1548 -> One (r1123)
  | 1547 -> One (r1124)
  | 1546 -> One (r1125)
  | 1545 -> One (r1126)
  | 1558 -> One (r1128)
  | 1543 -> One (r1130)
  | 1538 -> One (r1131)
  | 1541 -> One (r1132)
  | 1540 -> One (r1133)
  | 1556 -> One (r1134)
  | 1555 -> One (r1135)
  | 1554 -> One (r1136)
  | 1551 -> One (r1137)
  | 1561 -> One (r1138)
  | 1563 -> One (r1139)
  | 1571 -> One (r1140)
  | 1566 -> One (r1141)
  | 1569 -> One (r1142)
  | 1568 -> One (r1143)
  | 1576 -> One (r1145)
  | 1575 -> One (r1146)
  | 1589 -> One (r1147)
  | 1588 -> One (r1148)
  | 1580 -> One (r1149)
  | 1579 -> One (r1150)
  | 1610 -> One (r1151)
  | 1609 -> One (r1152)
  | 1608 -> One (r1153)
  | 1607 -> One (r1154)
  | 1606 -> One (r1155)
  | 1605 -> One (r1156)
  | 1622 -> One (r1157)
  | 1615 -> One (r1158)
  | 1614 -> One (r1159)
  | 1619 -> One (r1160)
  | 1618 -> One (r1161)
  | 1617 -> One (r1162)
  | 1621 -> One (r1163)
  | 1635 -> One (r1164)
  | 1641 -> One (r1165)
  | 1644 -> One (r1166)
  | 1657 -> One (r1167)
  | 1674 -> One (r1168)
  | 1673 -> One (r1169)
  | 1672 -> One (r1170)
  | 1671 -> One (r1171)
  | 1670 -> One (r1172)
  | 1669 -> One (r1173)
  | 1682 -> One (r1174)
  | 1681 -> One (r1175)
  | 1680 -> One (r1176)
  | 1679 -> One (r1177)
  | 1678 -> One (r1178)
  | 1677 -> One (r1179)
  | 1676 -> One (r1180)
  | 1702 -> One (r1181)
  | 1703 -> One (r1183)
  | 1696 -> One (r1184)
  | 1695 -> One (r1185)
  | 1694 -> One (r1186)
  | 1701 -> One (r1187)
  | 1700 -> One (r1188)
  | 1705 -> One (r1189)
  | 1711 -> One (r1190)
  | 1710 -> One (r1191)
  | 1709 -> One (r1192)
  | 1708 -> One (r1193)
  | 1714 -> One (r1194)
  | 1713 -> One (r1195)
  | 1722 -> One (r1196)
  | 1730 -> One (r1197)
  | 1729 -> One (r1198)
  | 1728 -> One (r1199)
  | 1727 -> One (r1200)
  | 1733 -> One (r1201)
  | 1732 -> One (r1202)
  | 1737 -> One (r1203)
  | 1748 -> One (r1204)
  | 1747 -> One (r1205)
  | 1751 -> One (r1206)
  | 1750 -> One (r1207)
  | 1754 -> One (r1208)
  | 1753 -> One (r1209)
  | 1763 -> One (r1210)
  | 1762 -> One (r1211)
  | 1770 -> One (r1212)
  | 1776 -> One (r1213)
  | 1782 -> One (r1214)
  | 1790 -> One (r1215)
  | 1796 -> One (r1216)
  | 1793 -> One (r1217)
  | 1795 -> One (r1218)
  | 1798 -> One (r1219)
  | 1800 -> One (r1220)
  | 1802 -> One (r1221)
  | 1805 -> One (r1222)
  | 1804 -> One (r1223)
  | 1817 -> One (r1224)
  | 1816 -> One (r1225)
  | 1829 -> One (r1226)
  | 1828 -> One (r1227)
  | 1852 -> One (r1228)
  | 1851 -> One (r1229)
  | 1861 -> One (r1230)
  | 1863 -> One (r1231)
  | 1865 -> One (r1232)
  | 1878 -> One (r1233)
  | 1882 -> One (r1234)
  | 1887 -> One (r1235)
  | 1894 -> One (r1236)
  | 1893 -> One (r1237)
  | 1892 -> One (r1238)
  | 1891 -> One (r1239)
  | 1901 -> One (r1240)
  | 1905 -> One (r1241)
  | 1909 -> One (r1242)
  | 1912 -> One (r1243)
  | 1917 -> One (r1244)
  | 1921 -> One (r1245)
  | 1925 -> One (r1246)
  | 1929 -> One (r1247)
  | 1933 -> One (r1248)
  | 1936 -> One (r1249)
  | 1940 -> One (r1250)
  | 1946 -> One (r1251)
  | 1956 -> One (r1252)
  | 1958 -> One (r1253)
  | 1961 -> One (r1254)
  | 1960 -> One (r1255)
  | 1963 -> One (r1256)
  | 1973 -> One (r1257)
  | 1969 -> One (r1258)
  | 1968 -> One (r1259)
  | 1972 -> One (r1260)
  | 1971 -> One (r1261)
  | 1978 -> One (r1262)
  | 1977 -> One (r1263)
  | 1976 -> One (r1264)
  | 1980 -> One (r1265)
  | 406 -> Select (function
    | -1 -> [R 117]
    | _ -> S (T T_DOT) :: r370)
  | 630 -> Select (function
    | -1 -> [R 117]
    | _ -> r561)
  | 175 -> Select (function
    | -1 -> r152
    | _ -> R 198 :: r144)
  | 865 -> Select (function
    | -1 -> r720
    | _ -> R 198 :: r713)
  | 924 -> Select (function
    | -1 -> r152
    | _ -> R 198 :: r762)
  | 1003 -> Select (function
    | -1 -> r672
    | _ -> R 198 :: r809)
  | 543 -> Select (function
    | -1 -> r283
    | _ -> [R 230])
  | 424 -> Select (function
    | -1 -> [R 707]
    | _ -> S (N N_pattern) :: r378)
  | 421 -> Select (function
    | -1 -> [R 708]
    | _ -> S (N N_pattern) :: r377)
  | 181 -> Select (function
    | -1 -> r164
    | _ -> R 835 :: r158)
  | 927 -> Select (function
    | -1 -> r164
    | _ -> R 835 :: r768)
  | 901 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (T T_COLONCOLON) :: r386)
  | 89 -> Select (function
    | 257 | 374 | 645 | 753 | 1307 | 1346 | 1397 | 1526 -> r61
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (N N_pattern) :: r56)
  | 247 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> Sub (r1) :: r238)
  | 376 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r341
    | _ -> Sub (r343) :: r345)
  | 584 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r341
    | _ -> Sub (r486) :: r488)
  | 499 -> Select (function
    | 62 | 174 | 214 | 804 | 845 | 847 -> r431
    | _ -> S (T T_OPEN) :: r425)
  | 903 -> Select (function
    | -1 -> r479
    | _ -> S (T T_LPAREN) :: r744)
  | 273 -> Select (function
    | -1 -> r285
    | _ -> S (T T_DOT) :: r287)
  | 541 -> Select (function
    | -1 -> r285
    | _ -> S (T T_DOT) :: r474)
  | 210 -> Select (function
    | -1 | 289 | 292 | 311 | 317 | 1768 | 1774 -> r123
    | _ -> S (T T_COLON) :: r194)
  | 154 -> Select (function
    | 755 | 908 | 1706 -> r107
    | _ -> Sub (r105) :: r108)
  | 157 -> Select (function
    | 755 | 908 | 1706 -> r106
    | _ -> r108)
  | 1831 -> Select (function
    | -1 -> r148
    | _ -> r123)
  | 200 -> Select (function
    | -1 -> r162
    | _ -> r123)
  | 978 -> Select (function
    | -1 -> r148
    | _ -> r123)
  | 929 -> Select (function
    | -1 -> r162
    | _ -> r123)
  | 1830 -> Select (function
    | -1 -> r149
    | _ -> r142)
  | 177 -> Select (function
    | -1 -> r150
    | _ -> r143)
  | 176 -> Select (function
    | -1 -> r151
    | _ -> r144)
  | 977 -> Select (function
    | -1 -> r149
    | _ -> r760)
  | 926 -> Select (function
    | -1 -> r150
    | _ -> r761)
  | 925 -> Select (function
    | -1 -> r151
    | _ -> r762)
  | 199 -> Select (function
    | -1 -> r163
    | _ -> r158)
  | 928 -> Select (function
    | -1 -> r163
    | _ -> r768)
  | 280 -> Select (function
    | -1 -> r284
    | _ -> r287)
  | 542 -> Select (function
    | -1 -> r284
    | _ -> r474)
  | 1006 -> Select (function
    | -1 -> r669
    | _ -> r807)
  | 1005 -> Select (function
    | -1 -> r670
    | _ -> r808)
  | 1004 -> Select (function
    | -1 -> r671
    | _ -> r809)
  | 873 -> Select (function
    | -1 -> r717
    | _ -> r711)
  | 867 -> Select (function
    | -1 -> r718
    | _ -> r712)
  | 866 -> Select (function
    | -1 -> r719
    | _ -> r713)
  | _ -> raise Not_found
