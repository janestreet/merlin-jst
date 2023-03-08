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
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETCOLON -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_COLONRBRACKET -> ()
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_gbl_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_comprehension_clause_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_comprehension_clause_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_46_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_comprehension_iterator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_comprehension_clause_binding -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_attr_payload -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;1;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;2;1;1;1;2;1;2;3;1;1;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;2;1;1;1;2;3;1;2;3;1;1;1;1;1;2;1;2;3;1;4;1;2;1;2;3;1;2;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;3;4;3;4;5;1;2;1;1;3;2;3;2;1;2;3;4;4;1;2;1;2;3;4;5;4;2;1;3;2;1;2;3;4;3;2;3;4;5;6;7;8;9;8;8;2;3;2;3;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;5;5;2;3;4;5;4;4;3;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;3;4;5;3;4;5;3;4;2;1;2;3;4;1;1;1;1;1;2;1;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;2;2;3;1;1;1;2;3;1;2;3;4;5;3;4;5;6;3;4;5;1;2;1;2;3;4;1;2;3;4;5;5;1;2;6;7;8;9;3;4;5;6;7;8;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;1;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;2;1;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;1;2;3;4;3;4;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;1;1;2;3;1;1;2;3;4;1;1;2;8;9;10;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;7;8;9;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;2;3;4;5;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;2;3;4;2;3;4;5;6;7;1;1;2;3;1;1;2;1;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;3;4;5;1;2;1;1;1;1;2;3;1;1;1;1;3;4;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;4;1;2;1;2;2;1;2;3;4;5;2;1;2;1;2;1;1;3;4;3;3;1;5;6;2;1;3;4;4;5;4;5;6;3;4;4;5;4;5;6;3;4;5;3;1;2;3;1;1;2;3;4;5;1;4;5;1;2;3;3;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;3;4;2;2;3;2;3;4;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;5;2;2;5;6;3;4;5;2;1;2;3;4;1;2;1;2;3;1;5;1;2;3;4;5;6;7;8;3;4;5;3;5;6;3;2;2;2;3;2;3;4;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;1;2;3;4;5;6;1;7;1;2;3;2;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;7;8;9;10;11;10;10;4;5;6;7;6;6;3;4;5;6;5;5;3;4;5;6;7;8;9;8;8;2;2;3;4;5;6;7;8;7;7;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  | T_LBRACKETCOLON -> true
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
  | T_COLONRBRACKET -> true
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
  let r0 = [R 627] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 138] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 307 :: r6 in
  let r8 = [R 738] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 42] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 200] in
  let r13 = [R 43] in
  let r14 = [R 540] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 44] in
  let r17 = Sub (r15) :: r16 in
  let r18 = [R 45] in
  let r19 = S (T T_RBRACKET) :: r18 in
  let r20 = Sub (r17) :: r19 in
  let r21 = [R 153] in
  let r22 = S (T T_DONE) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = S (T T_DO) :: r23 in
  let r25 = Sub (r1) :: r24 in
  let r26 = R 307 :: r25 in
  let r27 = [R 696] in
  let r28 = [R 376] in
  let r29 = [R 134] in
  let r30 = Sub (r1) :: r29 in
  let r31 = R 307 :: r30 in
  let r32 = [R 345] in
  let r33 = Sub (r1) :: r32 in
  let r34 = S (T T_MINUSGREATER) :: r33 in
  let r35 = S (N N_pattern) :: r34 in
  let r36 = [R 586] in
  let r37 = Sub (r35) :: r36 in
  let r38 = [R 150] in
  let r39 = Sub (r37) :: r38 in
  let r40 = S (T T_WITH) :: r39 in
  let r41 = Sub (r1) :: r40 in
  let r42 = R 307 :: r41 in
  let r43 = [R 202] in
  let r44 = S (T T_UNDERSCORE) :: r27 in
  let r45 = [R 686] in
  let r46 = [R 681] in
  let r47 = S (T T_END) :: r46 in
  let r48 = R 324 :: r47 in
  let r49 = R 70 :: r48 in
  let r50 = R 307 :: r49 in
  let r51 = [R 68] in
  let r52 = S (T T_RPAREN) :: r51 in
  let r53 = [R 724] in
  let r54 = [R 655] in
  let r55 = [R 653] in
  let r56 = [R 108] in
  let r57 = [R 720] in
  let r58 = S (T T_RPAREN) :: r57 in
  let r59 = [R 475] in
  let r60 = S (T T_AMPERAMPER) :: r59 in
  let r61 = [R 890] in
  let r62 = S (T T_RPAREN) :: r61 in
  let r63 = Sub (r60) :: r62 in
  let r64 = [R 398] in
  let r65 = S (T T_UNDERSCORE) :: r64 in
  let r66 = [R 722] in
  let r67 = S (T T_RPAREN) :: r66 in
  let r68 = Sub (r65) :: r67 in
  let r69 = R 307 :: r68 in
  let r70 = [R 723] in
  let r71 = S (T T_RPAREN) :: r70 in
  let r72 = [R 364] in
  let r73 = [R 632] in
  let r74 = R 315 :: r73 in
  let r75 = [R 400] in
  let r76 = S (T T_END) :: r75 in
  let r77 = Sub (r74) :: r76 in
  let r78 = [R 891] in
  let r79 = S (T T_LIDENT) :: r78 in
  let r80 = [R 25] in
  let r81 = S (T T_UNDERSCORE) :: r80 in
  let r82 = [R 864] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 214] in
  let r85 = Sub (r83) :: r84 in
  let r86 = [R 17] in
  let r87 = Sub (r85) :: r86 in
  let r88 = [R 128] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 545] in
  let r91 = Sub (r89) :: r90 in
  let r92 = [R 899] in
  let r93 = R 313 :: r92 in
  let r94 = Sub (r91) :: r93 in
  let r95 = S (T T_COLON) :: r94 in
  let r96 = Sub (r79) :: r95 in
  let r97 = R 307 :: r96 in
  let r98 = [R 449] in
  let r99 = S (T T_RPAREN) :: r98 in
  let r100 = R 236 :: r99 in
  let r101 = [R 237] in
  let r102 = [R 451] in
  let r103 = S (T T_RBRACKET) :: r102 in
  let r104 = [R 453] in
  let r105 = S (T T_RBRACE) :: r104 in
  let r106 = [R 232] in
  let r107 = S (T T_LIDENT) :: r106 in
  let r108 = [R 24] in
  let r109 = Sub (r107) :: r108 in
  let r110 = [R 584] in
  let r111 = Sub (r107) :: r110 in
  let r112 = [R 498] in
  let r113 = S (T T_COLON) :: r112 in
  let r114 = [R 23] in
  let r115 = S (T T_RPAREN) :: r114 in
  let r116 = S (N N_module_type) :: r115 in
  let r117 = R 307 :: r116 in
  let r118 = R 199 :: r117 in
  let r119 = S (T T_QUOTE) :: r111 in
  let r120 = [R 822] in
  let r121 = Sub (r83) :: r120 in
  let r122 = S (T T_MINUSGREATER) :: r121 in
  let r123 = S (T T_RPAREN) :: r122 in
  let r124 = Sub (r89) :: r123 in
  let r125 = S (T T_DOT) :: r124 in
  let r126 = [R 402] in
  let r127 = S (N N_module_expr) :: r126 in
  let r128 = R 307 :: r127 in
  let r129 = S (T T_OF) :: r128 in
  let r130 = [R 388] in
  let r131 = S (T T_END) :: r130 in
  let r132 = S (N N_structure) :: r131 in
  let r133 = [R 362] in
  let r134 = S (T T_LIDENT) :: r133 in
  let r135 = [R 871] in
  let r136 = Sub (r134) :: r135 in
  let r137 = [R 109] in
  let r138 = S (T T_FALSE) :: r137 in
  let r139 = [R 113] in
  let r140 = Sub (r138) :: r139 in
  let r141 = [R 226] in
  let r142 = R 307 :: r141 in
  let r143 = R 219 :: r142 in
  let r144 = Sub (r140) :: r143 in
  let r145 = [R 565] in
  let r146 = Sub (r144) :: r145 in
  let r147 = [R 839] in
  let r148 = R 313 :: r147 in
  let r149 = Sub (r146) :: r148 in
  let r150 = R 551 :: r149 in
  let r151 = S (T T_PLUSEQ) :: r150 in
  let r152 = Sub (r136) :: r151 in
  let r153 = R 873 :: r152 in
  let r154 = R 307 :: r153 in
  let r155 = [R 229] in
  let r156 = R 313 :: r155 in
  let r157 = R 574 :: r156 in
  let r158 = R 869 :: r157 in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = R 873 :: r159 in
  let r161 = R 307 :: r160 in
  let r162 = R 199 :: r161 in
  let r163 = [R 840] in
  let r164 = R 313 :: r163 in
  let r165 = Sub (r146) :: r164 in
  let r166 = R 551 :: r165 in
  let r167 = S (T T_PLUSEQ) :: r166 in
  let r168 = Sub (r136) :: r167 in
  let r169 = [R 230] in
  let r170 = R 313 :: r169 in
  let r171 = R 574 :: r170 in
  let r172 = R 869 :: r171 in
  let r173 = S (T T_LIDENT) :: r172 in
  let r174 = R 873 :: r173 in
  let r175 = [R 877] in
  let r176 = S (T T_UNDERSCORE) :: r175 in
  let r177 = [R 872] in
  let r178 = Sub (r176) :: r177 in
  let r179 = R 878 :: r178 in
  let r180 = [R 599] in
  let r181 = Sub (r179) :: r180 in
  let r182 = [R 875] in
  let r183 = S (T T_RPAREN) :: r182 in
  let r184 = [R 876] in
  let r185 = [R 600] in
  let r186 = [R 434] in
  let r187 = S (T T_DOTDOT) :: r186 in
  let r188 = [R 870] in
  let r189 = [R 435] in
  let r190 = [R 112] in
  let r191 = S (T T_RPAREN) :: r190 in
  let r192 = [R 828] in
  let r193 = Sub (r83) :: r192 in
  let r194 = S (T T_MINUSGREATER) :: r193 in
  let r195 = [R 816] in
  let r196 = Sub (r83) :: r195 in
  let r197 = S (T T_MINUSGREATER) :: r196 in
  let r198 = Sub (r83) :: r197 in
  let r199 = [R 30] in
  let r200 = [R 201] in
  let r201 = S (T T_RBRACKET) :: r200 in
  let r202 = Sub (r15) :: r201 in
  let r203 = [R 319] in
  let r204 = [R 442] in
  let r205 = R 313 :: r204 in
  let r206 = S (N N_module_expr) :: r205 in
  let r207 = R 307 :: r206 in
  let r208 = [R 443] in
  let r209 = R 313 :: r208 in
  let r210 = S (N N_module_expr) :: r209 in
  let r211 = R 307 :: r210 in
  let r212 = [R 500] in
  let r213 = S (T T_RPAREN) :: r212 in
  let r214 = [R 501] in
  let r215 = S (T T_RPAREN) :: r214 in
  let r216 = S (N N_expr) :: r215 in
  let r217 = [R 374] in
  let r218 = S (T T_LIDENT) :: r217 in
  let r219 = [R 67] in
  let r220 = Sub (r218) :: r219 in
  let r221 = [R 678] in
  let r222 = Sub (r220) :: r221 in
  let r223 = R 307 :: r222 in
  let r224 = [R 375] in
  let r225 = S (T T_LIDENT) :: r224 in
  let r226 = [R 377] in
  let r227 = [R 382] in
  let r228 = [R 308] in
  let r229 = [R 133] in
  let r230 = Sub (r37) :: r229 in
  let r231 = S (T T_WITH) :: r230 in
  let r232 = Sub (r1) :: r231 in
  let r233 = R 307 :: r232 in
  let r234 = [R 149] in
  let r235 = Sub (r37) :: r234 in
  let r236 = S (T T_WITH) :: r235 in
  let r237 = Sub (r1) :: r236 in
  let r238 = R 307 :: r237 in
  let r239 = [R 665] in
  let r240 = S (T T_RPAREN) :: r239 in
  let r241 = [R 705] in
  let r242 = [R 198] in
  let r243 = [R 186] in
  let r244 = [R 273] in
  let r245 = Sub (r79) :: r244 in
  let r246 = [R 342] in
  let r247 = R 313 :: r246 in
  let r248 = Sub (r245) :: r247 in
  let r249 = R 558 :: r248 in
  let r250 = R 307 :: r249 in
  let r251 = [R 339] in
  let r252 = Sub (r1) :: r251 in
  let r253 = S (T T_EQUAL) :: r252 in
  let r254 = [R 282] in
  let r255 = Sub (r253) :: r254 in
  let r256 = [R 264] in
  let r257 = [R 246] in
  let r258 = S (T T_LIDENT) :: r257 in
  let r259 = [R 262] in
  let r260 = S (T T_RPAREN) :: r259 in
  let r261 = [R 263] in
  let r262 = S (T T_RPAREN) :: r261 in
  let r263 = [R 247] in
  let r264 = [R 614] in
  let r265 = Sub (r89) :: r264 in
  let r266 = [R 595] in
  let r267 = Sub (r265) :: r266 in
  let r268 = [R 39] in
  let r269 = S (T T_RBRACKET) :: r268 in
  let r270 = Sub (r267) :: r269 in
  let r271 = [R 38] in
  let r272 = [R 37] in
  let r273 = S (T T_RBRACKET) :: r272 in
  let r274 = [R 423] in
  let r275 = Sub (r107) :: r274 in
  let r276 = S (T T_BACKQUOTE) :: r275 in
  let r277 = [R 852] in
  let r278 = R 307 :: r277 in
  let r279 = Sub (r276) :: r278 in
  let r280 = [R 34] in
  let r281 = S (T T_RBRACKET) :: r280 in
  let r282 = [R 96] in
  let r283 = Sub (r134) :: r282 in
  let r284 = [R 31] in
  let r285 = [R 365] in
  let r286 = S (T T_UIDENT) :: r285 in
  let r287 = S (T T_DOT) :: r286 in
  let r288 = [R 363] in
  let r289 = S (T T_LIDENT) :: r288 in
  let r290 = S (T T_UIDENT) :: r72 in
  let r291 = [R 380] in
  let r292 = Sub (r290) :: r291 in
  let r293 = [R 381] in
  let r294 = S (T T_RPAREN) :: r293 in
  let r295 = [R 35] in
  let r296 = S (T T_RBRACKET) :: r295 in
  let r297 = [R 824] in
  let r298 = [R 825] in
  let r299 = [R 829] in
  let r300 = [R 611] in
  let r301 = [R 32] in
  let r302 = [R 612] in
  let r303 = [R 808] in
  let r304 = Sub (r83) :: r303 in
  let r305 = S (T T_MINUSGREATER) :: r304 in
  let r306 = [R 28] in
  let r307 = Sub (r136) :: r306 in
  let r308 = [R 33] in
  let r309 = [R 607] in
  let r310 = [R 18] in
  let r311 = Sub (r107) :: r310 in
  let r312 = [R 806] in
  let r313 = Sub (r83) :: r312 in
  let r314 = S (T T_MINUSGREATER) :: r313 in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = Sub (r89) :: r315 in
  let r317 = [R 585] in
  let r318 = [R 807] in
  let r319 = [R 22] in
  let r320 = [R 608] in
  let r321 = [R 812] in
  let r322 = Sub (r83) :: r321 in
  let r323 = S (T T_MINUSGREATER) :: r322 in
  let r324 = [R 810] in
  let r325 = Sub (r83) :: r324 in
  let r326 = S (T T_MINUSGREATER) :: r325 in
  let r327 = S (T T_RPAREN) :: r326 in
  let r328 = Sub (r89) :: r327 in
  let r329 = [R 811] in
  let r330 = [R 813] in
  let r331 = [R 809] in
  let r332 = [R 596] in
  let r333 = [R 589] in
  let r334 = Sub (r87) :: r333 in
  let r335 = [R 851] in
  let r336 = R 307 :: r335 in
  let r337 = Sub (r334) :: r336 in
  let r338 = [R 590] in
  let r339 = [R 36] in
  let r340 = S (T T_RBRACKET) :: r339 in
  let r341 = Sub (r267) :: r340 in
  let r342 = [R 582] in
  let r343 = Sub (r276) :: r342 in
  let r344 = [R 40] in
  let r345 = S (T T_RBRACKET) :: r344 in
  let r346 = [R 248] in
  let r347 = Sub (r89) :: r346 in
  let r348 = [R 258] in
  let r349 = [R 256] in
  let r350 = S (T T_RPAREN) :: r349 in
  let r351 = R 493 :: r350 in
  let r352 = [R 257] in
  let r353 = S (T T_RPAREN) :: r352 in
  let r354 = R 493 :: r353 in
  let r355 = [R 494] in
  let r356 = [R 292] in
  let r357 = Sub (r79) :: r356 in
  let r358 = [R 295] in
  let r359 = Sub (r357) :: r358 in
  let r360 = [R 184] in
  let r361 = Sub (r1) :: r360 in
  let r362 = S (T T_IN) :: r361 in
  let r363 = [R 662] in
  let r364 = [R 660] in
  let r365 = [R 107] in
  let r366 = [R 621] in
  let r367 = S (N N_pattern) :: r366 in
  let r368 = [R 658] in
  let r369 = S (T T_RBRACKET) :: r368 in
  let r370 = [R 249] in
  let r371 = Sub (r218) :: r370 in
  let r372 = [R 333] in
  let r373 = R 491 :: r372 in
  let r374 = R 485 :: r373 in
  let r375 = Sub (r371) :: r374 in
  let r376 = [R 657] in
  let r377 = S (T T_RBRACE) :: r376 in
  let r378 = [R 486] in
  let r379 = [R 492] in
  let r380 = S (T T_UNDERSCORE) :: r53 in
  let r381 = [R 719] in
  let r382 = Sub (r380) :: r381 in
  let r383 = [R 531] in
  let r384 = Sub (r382) :: r383 in
  let r385 = R 307 :: r384 in
  let r386 = [R 103] in
  let r387 = [R 729] in
  let r388 = S (T T_INT) :: r386 in
  let r389 = [R 652] in
  let r390 = Sub (r388) :: r389 in
  let r391 = [R 726] in
  let r392 = [R 731] in
  let r393 = S (T T_RBRACKET) :: r392 in
  let r394 = S (T T_LBRACKET) :: r393 in
  let r395 = [R 732] in
  let r396 = [R 522] in
  let r397 = S (N N_pattern) :: r396 in
  let r398 = R 307 :: r397 in
  let r399 = [R 523] in
  let r400 = [R 516] in
  let r401 = [R 530] in
  let r402 = [R 528] in
  let r403 = [R 424] in
  let r404 = S (T T_LIDENT) :: r403 in
  let r405 = [R 529] in
  let r406 = Sub (r382) :: r405 in
  let r407 = S (T T_RPAREN) :: r406 in
  let r408 = [R 117] in
  let r409 = [R 116] in
  let r410 = S (T T_RPAREN) :: r409 in
  let r411 = [R 524] in
  let r412 = [R 734] in
  let r413 = S (T T_RPAREN) :: r412 in
  let r414 = Sub (r89) :: r413 in
  let r415 = [R 521] in
  let r416 = [R 519] in
  let r417 = [R 115] in
  let r418 = S (T T_RPAREN) :: r417 in
  let r419 = [R 733] in
  let r420 = [R 335] in
  let r421 = [R 659] in
  let r422 = [R 661] in
  let r423 = [R 539] in
  let r424 = S (T T_UNDERSCORE) :: r423 in
  let r425 = [R 261] in
  let r426 = [R 259] in
  let r427 = S (T T_RPAREN) :: r426 in
  let r428 = R 493 :: r427 in
  let r429 = [R 260] in
  let r430 = S (T T_RPAREN) :: r429 in
  let r431 = R 493 :: r430 in
  let r432 = [R 289] in
  let r433 = [R 290] in
  let r434 = Sub (r89) :: r433 in
  let r435 = [R 791] in
  let r436 = Sub (r1) :: r435 in
  let r437 = S (T T_EQUAL) :: r436 in
  let r438 = [R 208] in
  let r439 = Sub (r437) :: r438 in
  let r440 = [R 793] in
  let r441 = Sub (r439) :: r440 in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = Sub (r404) :: r442 in
  let r444 = [R 265] in
  let r445 = [R 266] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = S (N N_pattern) :: r446 in
  let r448 = [R 270] in
  let r449 = S (T T_RPAREN) :: r448 in
  let r450 = Sub (r89) :: r449 in
  let r451 = S (T T_DOT) :: r450 in
  let r452 = [R 269] in
  let r453 = S (T T_RPAREN) :: r452 in
  let r454 = Sub (r89) :: r453 in
  let r455 = [R 144] in
  let r456 = Sub (r1) :: r455 in
  let r457 = S (T T_IN) :: r456 in
  let r458 = S (N N_module_expr) :: r457 in
  let r459 = R 307 :: r458 in
  let r460 = R 199 :: r459 in
  let r461 = [R 283] in
  let r462 = R 313 :: r461 in
  let r463 = Sub (r245) :: r462 in
  let r464 = R 558 :: r463 in
  let r465 = R 307 :: r464 in
  let r466 = R 199 :: r465 in
  let r467 = [R 145] in
  let r468 = Sub (r1) :: r467 in
  let r469 = S (T T_IN) :: r468 in
  let r470 = S (N N_module_expr) :: r469 in
  let r471 = R 307 :: r470 in
  let r472 = [R 389] in
  let r473 = S (N N_module_expr) :: r472 in
  let r474 = S (T T_MINUSGREATER) :: r473 in
  let r475 = S (N N_functor_args) :: r474 in
  let r476 = [R 216] in
  let r477 = [R 217] in
  let r478 = S (T T_RPAREN) :: r477 in
  let r479 = S (N N_module_type) :: r478 in
  let r480 = [R 403] in
  let r481 = S (T T_RPAREN) :: r480 in
  let r482 = [R 401] in
  let r483 = S (N N_module_type) :: r482 in
  let r484 = S (T T_MINUSGREATER) :: r483 in
  let r485 = S (N N_functor_args) :: r484 in
  let r486 = [R 372] in
  let r487 = Sub (r107) :: r486 in
  let r488 = [R 411] in
  let r489 = Sub (r487) :: r488 in
  let r490 = [R 912] in
  let r491 = S (N N_module_type) :: r490 in
  let r492 = S (T T_EQUAL) :: r491 in
  let r493 = Sub (r489) :: r492 in
  let r494 = S (T T_TYPE) :: r493 in
  let r495 = S (T T_MODULE) :: r494 in
  let r496 = [R 593] in
  let r497 = Sub (r495) :: r496 in
  let r498 = [R 407] in
  let r499 = [R 909] in
  let r500 = Sub (r87) :: r499 in
  let r501 = S (T T_COLONEQUAL) :: r500 in
  let r502 = Sub (r371) :: r501 in
  let r503 = [R 908] in
  let r504 = R 574 :: r503 in
  let r505 = [R 575] in
  let r506 = Sub (r89) :: r505 in
  let r507 = S (T T_EQUAL) :: r506 in
  let r508 = [R 373] in
  let r509 = Sub (r107) :: r508 in
  let r510 = [R 913] in
  let r511 = [R 406] in
  let r512 = [R 910] in
  let r513 = Sub (r292) :: r512 in
  let r514 = S (T T_UIDENT) :: r226 in
  let r515 = [R 911] in
  let r516 = [R 594] in
  let r517 = [R 394] in
  let r518 = [R 499] in
  let r519 = S (T T_RPAREN) :: r518 in
  let r520 = [R 701] in
  let r521 = [R 615] in
  let r522 = S (N N_expr) :: r521 in
  let r523 = [R 708] in
  let r524 = S (T T_RBRACKET) :: r523 in
  let r525 = [R 689] in
  let r526 = [R 618] in
  let r527 = R 487 :: r526 in
  let r528 = [R 488] in
  let r529 = [R 624] in
  let r530 = R 487 :: r529 in
  let r531 = R 495 :: r530 in
  let r532 = Sub (r371) :: r531 in
  let r533 = [R 560] in
  let r534 = Sub (r532) :: r533 in
  let r535 = [R 698] in
  let r536 = S (T T_RBRACE) :: r535 in
  let r537 = [R 664] in
  let r538 = [R 663] in
  let r539 = S (T T_GREATERDOT) :: r538 in
  let r540 = [R 156] in
  let r541 = Sub (r44) :: r540 in
  let r542 = R 307 :: r541 in
  let r543 = [R 677] in
  let r544 = S (T T_END) :: r543 in
  let r545 = R 307 :: r544 in
  let r546 = [R 152] in
  let r547 = S (N N_expr) :: r546 in
  let r548 = S (T T_THEN) :: r547 in
  let r549 = Sub (r1) :: r548 in
  let r550 = R 307 :: r549 in
  let r551 = [R 146] in
  let r552 = Sub (r37) :: r551 in
  let r553 = R 307 :: r552 in
  let r554 = [R 587] in
  let r555 = [R 346] in
  let r556 = Sub (r1) :: r555 in
  let r557 = S (T T_MINUSGREATER) :: r556 in
  let r558 = [R 267] in
  let r559 = Sub (r382) :: r558 in
  let r560 = [R 210] in
  let r561 = Sub (r1) :: r560 in
  let r562 = S (T T_MINUSGREATER) :: r561 in
  let r563 = [R 147] in
  let r564 = Sub (r562) :: r563 in
  let r565 = Sub (r559) :: r564 in
  let r566 = R 307 :: r565 in
  let r567 = [R 268] in
  let r568 = S (T T_RPAREN) :: r567 in
  let r569 = S (N N_let_pattern) :: r568 in
  let r570 = [R 148] in
  let r571 = Sub (r562) :: r570 in
  let r572 = S (T T_RPAREN) :: r571 in
  let r573 = [R 140] in
  let r574 = S (T T_DONE) :: r573 in
  let r575 = Sub (r1) :: r574 in
  let r576 = S (T T_DO) :: r575 in
  let r577 = Sub (r1) :: r576 in
  let r578 = S (T T_IN) :: r577 in
  let r579 = S (N N_pattern) :: r578 in
  let r580 = R 307 :: r579 in
  let r581 = [R 131] in
  let r582 = S (T T_DOWNTO) :: r581 in
  let r583 = [R 154] in
  let r584 = S (T T_DONE) :: r583 in
  let r585 = Sub (r1) :: r584 in
  let r586 = S (T T_DO) :: r585 in
  let r587 = Sub (r1) :: r586 in
  let r588 = Sub (r582) :: r587 in
  let r589 = Sub (r1) :: r588 in
  let r590 = S (T T_EQUAL) :: r589 in
  let r591 = S (N N_pattern) :: r590 in
  let r592 = R 307 :: r591 in
  let r593 = [R 687] in
  let r594 = [R 697] in
  let r595 = S (T T_RPAREN) :: r594 in
  let r596 = S (T T_LPAREN) :: r595 in
  let r597 = S (T T_DOT) :: r596 in
  let r598 = [R 717] in
  let r599 = S (T T_RPAREN) :: r598 in
  let r600 = S (N N_module_type) :: r599 in
  let r601 = S (T T_COLON) :: r600 in
  let r602 = S (N N_module_expr) :: r601 in
  let r603 = R 307 :: r602 in
  let r604 = [R 293] in
  let r605 = Sub (r1) :: r604 in
  let r606 = S (T T_EQUAL) :: r605 in
  let r607 = [R 155] in
  let r608 = Sub (r44) :: r607 in
  let r609 = R 307 :: r608 in
  let r610 = [R 694] in
  let r611 = [R 670] in
  let r612 = S (T T_RPAREN) :: r611 in
  let r613 = Sub (r522) :: r612 in
  let r614 = S (T T_LPAREN) :: r613 in
  let r615 = [R 181] in
  let r616 = [R 252] in
  let r617 = [R 866] in
  let r618 = Sub (r89) :: r617 in
  let r619 = S (T T_COLON) :: r618 in
  let r620 = [R 253] in
  let r621 = S (T T_RPAREN) :: r620 in
  let r622 = Sub (r619) :: r621 in
  let r623 = [R 868] in
  let r624 = [R 867] in
  let r625 = [R 254] in
  let r626 = [R 255] in
  let r627 = [R 693] in
  let r628 = [R 667] in
  let r629 = S (T T_RPAREN) :: r628 in
  let r630 = Sub (r1) :: r629 in
  let r631 = S (T T_LPAREN) :: r630 in
  let r632 = [R 609] in
  let r633 = [R 132] in
  let r634 = Sub (r1) :: r633 in
  let r635 = [R 183] in
  let r636 = Sub (r1) :: r635 in
  let r637 = [R 171] in
  let r638 = [R 165] in
  let r639 = [R 182] in
  let r640 = [R 630] in
  let r641 = Sub (r1) :: r640 in
  let r642 = [R 168] in
  let r643 = [R 172] in
  let r644 = [R 164] in
  let r645 = [R 167] in
  let r646 = [R 166] in
  let r647 = [R 176] in
  let r648 = [R 170] in
  let r649 = [R 169] in
  let r650 = [R 174] in
  let r651 = [R 163] in
  let r652 = [R 162] in
  let r653 = [R 185] in
  let r654 = [R 161] in
  let r655 = [R 175] in
  let r656 = [R 173] in
  let r657 = [R 177] in
  let r658 = [R 178] in
  let r659 = [R 179] in
  let r660 = [R 610] in
  let r661 = [R 180] in
  let r662 = [R 19] in
  let r663 = R 313 :: r662 in
  let r664 = Sub (r245) :: r663 in
  let r665 = [R 279] in
  let r666 = Sub (r1) :: r665 in
  let r667 = S (T T_EQUAL) :: r666 in
  let r668 = Sub (r89) :: r667 in
  let r669 = S (T T_DOT) :: r668 in
  let r670 = [R 277] in
  let r671 = Sub (r1) :: r670 in
  let r672 = S (T T_EQUAL) :: r671 in
  let r673 = Sub (r89) :: r672 in
  let r674 = [R 275] in
  let r675 = Sub (r1) :: r674 in
  let r676 = [R 792] in
  let r677 = [R 209] in
  let r678 = Sub (r1) :: r677 in
  let r679 = [R 281] in
  let r680 = Sub (r1) :: r679 in
  let r681 = S (T T_EQUAL) :: r680 in
  let r682 = [R 280] in
  let r683 = Sub (r1) :: r682 in
  let r684 = [R 526] in
  let r685 = [R 532] in
  let r686 = [R 537] in
  let r687 = [R 535] in
  let r688 = [R 525] in
  let r689 = [R 549] in
  let r690 = S (T T_RBRACKET) :: r689 in
  let r691 = Sub (r17) :: r690 in
  let r692 = [R 543] in
  let r693 = [R 544] in
  let r694 = [R 383] in
  let r695 = S (N N_module_expr) :: r694 in
  let r696 = S (T T_EQUAL) :: r695 in
  let r697 = [R 842] in
  let r698 = R 313 :: r697 in
  let r699 = Sub (r696) :: r698 in
  let r700 = Sub (r65) :: r699 in
  let r701 = R 307 :: r700 in
  let r702 = [R 409] in
  let r703 = R 313 :: r702 in
  let r704 = R 489 :: r703 in
  let r705 = Sub (r107) :: r704 in
  let r706 = R 307 :: r705 in
  let r707 = R 199 :: r706 in
  let r708 = [R 490] in
  let r709 = [R 314] in
  let r710 = [R 843] in
  let r711 = R 303 :: r710 in
  let r712 = R 313 :: r711 in
  let r713 = Sub (r696) :: r712 in
  let r714 = [R 384] in
  let r715 = S (N N_module_expr) :: r714 in
  let r716 = S (T T_EQUAL) :: r715 in
  let r717 = [R 304] in
  let r718 = R 303 :: r717 in
  let r719 = R 313 :: r718 in
  let r720 = Sub (r696) :: r719 in
  let r721 = Sub (r65) :: r720 in
  let r722 = [R 385] in
  let r723 = [R 239] in
  let r724 = S (T T_RBRACKET) :: r723 in
  let r725 = Sub (r15) :: r724 in
  let r726 = [R 205] in
  let r727 = S (T T_RBRACKET) :: r726 in
  let r728 = Sub (r17) :: r727 in
  let r729 = [R 426] in
  let r730 = S (T T_STRING) :: r729 in
  let r731 = [R 550] in
  let r732 = R 313 :: r731 in
  let r733 = Sub (r730) :: r732 in
  let r734 = S (T T_EQUAL) :: r733 in
  let r735 = Sub (r91) :: r734 in
  let r736 = S (T T_COLON) :: r735 in
  let r737 = Sub (r79) :: r736 in
  let r738 = R 307 :: r737 in
  let r739 = [R 546] in
  let r740 = Sub (r89) :: r739 in
  let r741 = Sub (r138) :: r408 in
  let r742 = [R 790] in
  let r743 = R 313 :: r742 in
  let r744 = R 307 :: r743 in
  let r745 = Sub (r741) :: r744 in
  let r746 = S (T T_EQUAL) :: r745 in
  let r747 = Sub (r140) :: r746 in
  let r748 = R 307 :: r747 in
  let r749 = [R 631] in
  let r750 = R 313 :: r749 in
  let r751 = R 307 :: r750 in
  let r752 = R 219 :: r751 in
  let r753 = Sub (r140) :: r752 in
  let r754 = R 307 :: r753 in
  let r755 = R 199 :: r754 in
  let r756 = [R 119] in
  let r757 = Sub (r81) :: r756 in
  let r758 = [R 220] in
  let r759 = [R 121] in
  let r760 = [R 547] in
  let r761 = Sub (r87) :: r760 in
  let r762 = [R 241] in
  let r763 = R 307 :: r762 in
  let r764 = Sub (r761) :: r763 in
  let r765 = S (T T_COLON) :: r764 in
  let r766 = S (T T_LIDENT) :: r765 in
  let r767 = R 414 :: r766 in
  let r768 = [R 243] in
  let r769 = Sub (r767) :: r768 in
  let r770 = [R 125] in
  let r771 = S (T T_RBRACE) :: r770 in
  let r772 = [R 242] in
  let r773 = R 307 :: r772 in
  let r774 = S (T T_SEMI) :: r773 in
  let r775 = R 307 :: r774 in
  let r776 = Sub (r761) :: r775 in
  let r777 = S (T T_COLON) :: r776 in
  let r778 = [R 548] in
  let r779 = Sub (r87) :: r778 in
  let r780 = [R 120] in
  let r781 = [R 122] in
  let r782 = Sub (r81) :: r781 in
  let r783 = [R 124] in
  let r784 = [R 123] in
  let r785 = S (T T_COLONCOLON) :: r418 in
  let r786 = [R 223] in
  let r787 = [R 224] in
  let r788 = Sub (r81) :: r787 in
  let r789 = [R 222] in
  let r790 = Sub (r81) :: r789 in
  let r791 = [R 221] in
  let r792 = Sub (r81) :: r791 in
  let r793 = [R 541] in
  let r794 = [R 571] in
  let r795 = Sub (r144) :: r794 in
  let r796 = [R 639] in
  let r797 = R 313 :: r796 in
  let r798 = Sub (r795) :: r797 in
  let r799 = R 551 :: r798 in
  let r800 = S (T T_PLUSEQ) :: r799 in
  let r801 = Sub (r136) :: r800 in
  let r802 = R 873 :: r801 in
  let r803 = R 307 :: r802 in
  let r804 = [R 640] in
  let r805 = R 313 :: r804 in
  let r806 = Sub (r795) :: r805 in
  let r807 = R 551 :: r806 in
  let r808 = S (T T_PLUSEQ) :: r807 in
  let r809 = Sub (r136) :: r808 in
  let r810 = [R 228] in
  let r811 = R 313 :: r810 in
  let r812 = R 574 :: r811 in
  let r813 = [R 438] in
  let r814 = S (T T_RBRACE) :: r813 in
  let r815 = [R 225] in
  let r816 = R 307 :: r815 in
  let r817 = R 219 :: r816 in
  let r818 = Sub (r140) :: r817 in
  let r819 = [R 436] in
  let r820 = [R 437] in
  let r821 = [R 441] in
  let r822 = S (T T_RBRACE) :: r821 in
  let r823 = [R 440] in
  let r824 = S (T T_RBRACE) :: r823 in
  let r825 = [R 227] in
  let r826 = R 313 :: r825 in
  let r827 = R 574 :: r826 in
  let r828 = [R 316] in
  let r829 = [R 444] in
  let r830 = R 313 :: r829 in
  let r831 = Sub (r292) :: r830 in
  let r832 = R 307 :: r831 in
  let r833 = [R 445] in
  let r834 = R 313 :: r833 in
  let r835 = Sub (r292) :: r834 in
  let r836 = R 307 :: r835 in
  let r837 = [R 386] in
  let r838 = S (N N_module_type) :: r837 in
  let r839 = S (T T_COLON) :: r838 in
  let r840 = [R 642] in
  let r841 = R 313 :: r840 in
  let r842 = Sub (r839) :: r841 in
  let r843 = Sub (r65) :: r842 in
  let r844 = R 307 :: r843 in
  let r845 = [R 410] in
  let r846 = R 313 :: r845 in
  let r847 = S (N N_module_type) :: r846 in
  let r848 = S (T T_COLONEQUAL) :: r847 in
  let r849 = Sub (r107) :: r848 in
  let r850 = R 307 :: r849 in
  let r851 = [R 399] in
  let r852 = R 313 :: r851 in
  let r853 = [R 645] in
  let r854 = R 305 :: r853 in
  let r855 = R 313 :: r854 in
  let r856 = S (N N_module_type) :: r855 in
  let r857 = S (T T_COLON) :: r856 in
  let r858 = [R 306] in
  let r859 = R 305 :: r858 in
  let r860 = R 313 :: r859 in
  let r861 = S (N N_module_type) :: r860 in
  let r862 = S (T T_COLON) :: r861 in
  let r863 = Sub (r65) :: r862 in
  let r864 = S (T T_UIDENT) :: r28 in
  let r865 = Sub (r864) :: r227 in
  let r866 = [R 643] in
  let r867 = R 313 :: r866 in
  let r868 = [R 387] in
  let r869 = S (T T_QUOTED_STRING_EXPR) :: r43 in
  let r870 = [R 81] in
  let r871 = Sub (r869) :: r870 in
  let r872 = [R 91] in
  let r873 = Sub (r871) :: r872 in
  let r874 = [R 650] in
  let r875 = R 299 :: r874 in
  let r876 = R 313 :: r875 in
  let r877 = Sub (r873) :: r876 in
  let r878 = S (T T_COLON) :: r877 in
  let r879 = S (T T_LIDENT) :: r878 in
  let r880 = R 206 :: r879 in
  let r881 = R 900 :: r880 in
  let r882 = R 307 :: r881 in
  let r883 = [R 95] in
  let r884 = R 301 :: r883 in
  let r885 = R 313 :: r884 in
  let r886 = Sub (r871) :: r885 in
  let r887 = S (T T_EQUAL) :: r886 in
  let r888 = S (T T_LIDENT) :: r887 in
  let r889 = R 206 :: r888 in
  let r890 = R 900 :: r889 in
  let r891 = R 307 :: r890 in
  let r892 = [R 207] in
  let r893 = S (T T_RBRACKET) :: r892 in
  let r894 = [R 82] in
  let r895 = S (T T_END) :: r894 in
  let r896 = R 322 :: r895 in
  let r897 = R 72 :: r896 in
  let r898 = [R 71] in
  let r899 = S (T T_RPAREN) :: r898 in
  let r900 = [R 74] in
  let r901 = R 313 :: r900 in
  let r902 = Sub (r89) :: r901 in
  let r903 = S (T T_COLON) :: r902 in
  let r904 = S (T T_LIDENT) :: r903 in
  let r905 = R 418 :: r904 in
  let r906 = [R 75] in
  let r907 = R 313 :: r906 in
  let r908 = Sub (r91) :: r907 in
  let r909 = S (T T_COLON) :: r908 in
  let r910 = S (T T_LIDENT) :: r909 in
  let r911 = R 553 :: r910 in
  let r912 = [R 73] in
  let r913 = R 313 :: r912 in
  let r914 = Sub (r871) :: r913 in
  let r915 = [R 84] in
  let r916 = Sub (r871) :: r915 in
  let r917 = S (T T_IN) :: r916 in
  let r918 = Sub (r865) :: r917 in
  let r919 = R 307 :: r918 in
  let r920 = [R 85] in
  let r921 = Sub (r871) :: r920 in
  let r922 = S (T T_IN) :: r921 in
  let r923 = Sub (r865) :: r922 in
  let r924 = [R 597] in
  let r925 = Sub (r89) :: r924 in
  let r926 = [R 80] in
  let r927 = Sub (r283) :: r926 in
  let r928 = S (T T_RBRACKET) :: r927 in
  let r929 = Sub (r925) :: r928 in
  let r930 = [R 598] in
  let r931 = [R 118] in
  let r932 = Sub (r89) :: r931 in
  let r933 = S (T T_EQUAL) :: r932 in
  let r934 = Sub (r89) :: r933 in
  let r935 = [R 76] in
  let r936 = R 313 :: r935 in
  let r937 = Sub (r934) :: r936 in
  let r938 = [R 77] in
  let r939 = [R 323] in
  let r940 = [R 302] in
  let r941 = R 301 :: r940 in
  let r942 = R 313 :: r941 in
  let r943 = Sub (r871) :: r942 in
  let r944 = S (T T_EQUAL) :: r943 in
  let r945 = S (T T_LIDENT) :: r944 in
  let r946 = R 206 :: r945 in
  let r947 = R 900 :: r946 in
  let r948 = [R 93] in
  let r949 = Sub (r873) :: r948 in
  let r950 = S (T T_MINUSGREATER) :: r949 in
  let r951 = Sub (r83) :: r950 in
  let r952 = [R 94] in
  let r953 = Sub (r873) :: r952 in
  let r954 = [R 92] in
  let r955 = Sub (r873) :: r954 in
  let r956 = S (T T_MINUSGREATER) :: r955 in
  let r957 = [R 300] in
  let r958 = R 299 :: r957 in
  let r959 = R 313 :: r958 in
  let r960 = Sub (r873) :: r959 in
  let r961 = S (T T_COLON) :: r960 in
  let r962 = S (T T_LIDENT) :: r961 in
  let r963 = R 206 :: r962 in
  let r964 = R 900 :: r963 in
  let r965 = [R 317] in
  let r966 = [R 633] in
  let r967 = [R 649] in
  let r968 = R 313 :: r967 in
  let r969 = S (N N_module_type) :: r968 in
  let r970 = R 307 :: r969 in
  let r971 = [R 637] in
  let r972 = [R 310] in
  let r973 = R 309 :: r972 in
  let r974 = R 313 :: r973 in
  let r975 = R 574 :: r974 in
  let r976 = R 869 :: r975 in
  let r977 = S (T T_LIDENT) :: r976 in
  let r978 = R 873 :: r977 in
  let r979 = [R 638] in
  let r980 = [R 312] in
  let r981 = R 311 :: r980 in
  let r982 = R 313 :: r981 in
  let r983 = R 574 :: r982 in
  let r984 = Sub (r187) :: r983 in
  let r985 = S (T T_COLONEQUAL) :: r984 in
  let r986 = S (T T_LIDENT) :: r985 in
  let r987 = R 873 :: r986 in
  let r988 = [R 53] in
  let r989 = Sub (r869) :: r988 in
  let r990 = [R 62] in
  let r991 = Sub (r989) :: r990 in
  let r992 = S (T T_EQUAL) :: r991 in
  let r993 = [R 846] in
  let r994 = R 297 :: r993 in
  let r995 = R 313 :: r994 in
  let r996 = Sub (r992) :: r995 in
  let r997 = S (T T_LIDENT) :: r996 in
  let r998 = R 206 :: r997 in
  let r999 = R 900 :: r998 in
  let r1000 = R 307 :: r999 in
  let r1001 = [R 271] in
  let r1002 = S (T T_RPAREN) :: r1001 in
  let r1003 = Sub (r89) :: r1002 in
  let r1004 = [R 90] in
  let r1005 = S (T T_END) :: r1004 in
  let r1006 = R 324 :: r1005 in
  let r1007 = R 70 :: r1006 in
  let r1008 = [R 895] in
  let r1009 = Sub (r1) :: r1008 in
  let r1010 = S (T T_EQUAL) :: r1009 in
  let r1011 = S (T T_LIDENT) :: r1010 in
  let r1012 = R 412 :: r1011 in
  let r1013 = R 307 :: r1012 in
  let r1014 = [R 56] in
  let r1015 = R 313 :: r1014 in
  let r1016 = [R 896] in
  let r1017 = Sub (r1) :: r1016 in
  let r1018 = S (T T_EQUAL) :: r1017 in
  let r1019 = S (T T_LIDENT) :: r1018 in
  let r1020 = R 412 :: r1019 in
  let r1021 = [R 898] in
  let r1022 = Sub (r1) :: r1021 in
  let r1023 = [R 894] in
  let r1024 = Sub (r89) :: r1023 in
  let r1025 = S (T T_COLON) :: r1024 in
  let r1026 = [R 897] in
  let r1027 = Sub (r1) :: r1026 in
  let r1028 = [R 356] in
  let r1029 = Sub (r437) :: r1028 in
  let r1030 = S (T T_LIDENT) :: r1029 in
  let r1031 = R 551 :: r1030 in
  let r1032 = R 307 :: r1031 in
  let r1033 = [R 57] in
  let r1034 = R 313 :: r1033 in
  let r1035 = [R 357] in
  let r1036 = Sub (r437) :: r1035 in
  let r1037 = S (T T_LIDENT) :: r1036 in
  let r1038 = R 551 :: r1037 in
  let r1039 = [R 359] in
  let r1040 = Sub (r1) :: r1039 in
  let r1041 = S (T T_EQUAL) :: r1040 in
  let r1042 = [R 361] in
  let r1043 = Sub (r1) :: r1042 in
  let r1044 = S (T T_EQUAL) :: r1043 in
  let r1045 = Sub (r89) :: r1044 in
  let r1046 = S (T T_DOT) :: r1045 in
  let r1047 = [R 355] in
  let r1048 = Sub (r91) :: r1047 in
  let r1049 = S (T T_COLON) :: r1048 in
  let r1050 = [R 358] in
  let r1051 = Sub (r1) :: r1050 in
  let r1052 = S (T T_EQUAL) :: r1051 in
  let r1053 = [R 360] in
  let r1054 = Sub (r1) :: r1053 in
  let r1055 = S (T T_EQUAL) :: r1054 in
  let r1056 = Sub (r89) :: r1055 in
  let r1057 = S (T T_DOT) :: r1056 in
  let r1058 = [R 59] in
  let r1059 = R 313 :: r1058 in
  let r1060 = Sub (r1) :: r1059 in
  let r1061 = [R 54] in
  let r1062 = R 313 :: r1061 in
  let r1063 = R 483 :: r1062 in
  let r1064 = Sub (r989) :: r1063 in
  let r1065 = [R 55] in
  let r1066 = R 313 :: r1065 in
  let r1067 = R 483 :: r1066 in
  let r1068 = Sub (r989) :: r1067 in
  let r1069 = [R 86] in
  let r1070 = S (T T_RPAREN) :: r1069 in
  let r1071 = [R 49] in
  let r1072 = Sub (r989) :: r1071 in
  let r1073 = S (T T_IN) :: r1072 in
  let r1074 = Sub (r865) :: r1073 in
  let r1075 = R 307 :: r1074 in
  let r1076 = [R 286] in
  let r1077 = R 313 :: r1076 in
  let r1078 = Sub (r245) :: r1077 in
  let r1079 = R 558 :: r1078 in
  let r1080 = R 307 :: r1079 in
  let r1081 = [R 50] in
  let r1082 = Sub (r989) :: r1081 in
  let r1083 = S (T T_IN) :: r1082 in
  let r1084 = Sub (r865) :: r1083 in
  let r1085 = [R 88] in
  let r1086 = Sub (r220) :: r1085 in
  let r1087 = S (T T_RBRACKET) :: r1086 in
  let r1088 = [R 65] in
  let r1089 = Sub (r989) :: r1088 in
  let r1090 = S (T T_MINUSGREATER) :: r1089 in
  let r1091 = Sub (r559) :: r1090 in
  let r1092 = [R 47] in
  let r1093 = Sub (r1091) :: r1092 in
  let r1094 = [R 48] in
  let r1095 = Sub (r989) :: r1094 in
  let r1096 = [R 251] in
  let r1097 = [R 285] in
  let r1098 = R 313 :: r1097 in
  let r1099 = Sub (r245) :: r1098 in
  let r1100 = [R 89] in
  let r1101 = S (T T_RPAREN) :: r1100 in
  let r1102 = [R 484] in
  let r1103 = [R 58] in
  let r1104 = R 313 :: r1103 in
  let r1105 = Sub (r934) :: r1104 in
  let r1106 = [R 60] in
  let r1107 = [R 325] in
  let r1108 = [R 63] in
  let r1109 = Sub (r989) :: r1108 in
  let r1110 = S (T T_EQUAL) :: r1109 in
  let r1111 = [R 64] in
  let r1112 = [R 298] in
  let r1113 = R 297 :: r1112 in
  let r1114 = R 313 :: r1113 in
  let r1115 = Sub (r992) :: r1114 in
  let r1116 = S (T T_LIDENT) :: r1115 in
  let r1117 = R 206 :: r1116 in
  let r1118 = R 900 :: r1117 in
  let r1119 = [R 321] in
  let r1120 = [R 834] in
  let r1121 = [R 848] in
  let r1122 = R 313 :: r1121 in
  let r1123 = S (N N_module_expr) :: r1122 in
  let r1124 = R 307 :: r1123 in
  let r1125 = [R 838] in
  let r1126 = [R 831] in
  let r1127 = R 318 :: r1126 in
  let r1128 = [R 669] in
  let r1129 = S (T T_RBRACKET) :: r1128 in
  let r1130 = Sub (r1) :: r1129 in
  let r1131 = [R 668] in
  let r1132 = S (T T_RBRACE) :: r1131 in
  let r1133 = Sub (r1) :: r1132 in
  let r1134 = [R 671] in
  let r1135 = S (T T_RPAREN) :: r1134 in
  let r1136 = Sub (r522) :: r1135 in
  let r1137 = S (T T_LPAREN) :: r1136 in
  let r1138 = [R 675] in
  let r1139 = S (T T_RBRACKET) :: r1138 in
  let r1140 = Sub (r522) :: r1139 in
  let r1141 = [R 673] in
  let r1142 = S (T T_RBRACE) :: r1141 in
  let r1143 = Sub (r522) :: r1142 in
  let r1144 = [R 191] in
  let r1145 = [R 674] in
  let r1146 = S (T T_RBRACKET) :: r1145 in
  let r1147 = Sub (r522) :: r1146 in
  let r1148 = [R 195] in
  let r1149 = [R 672] in
  let r1150 = S (T T_RBRACE) :: r1149 in
  let r1151 = Sub (r522) :: r1150 in
  let r1152 = [R 193] in
  let r1153 = [R 188] in
  let r1154 = [R 190] in
  let r1155 = [R 189] in
  let r1156 = [R 192] in
  let r1157 = [R 196] in
  let r1158 = [R 194] in
  let r1159 = [R 187] in
  let r1160 = [R 294] in
  let r1161 = Sub (r1) :: r1160 in
  let r1162 = [R 296] in
  let r1163 = [R 691] in
  let r1164 = [R 707] in
  let r1165 = [R 706] in
  let r1166 = [R 98] in
  let r1167 = [R 102] in
  let r1168 = S (N N_expr) :: r1167 in
  let r1169 = S (T T_IN) :: r1168 in
  let r1170 = [R 99] in
  let r1171 = Sub (r1169) :: r1170 in
  let r1172 = S (N N_pattern) :: r1171 in
  let r1173 = R 307 :: r1172 in
  let r1174 = [R 591] in
  let r1175 = Sub (r1173) :: r1174 in
  let r1176 = [R 97] in
  let r1177 = [R 592] in
  let r1178 = [R 100] in
  let r1179 = S (N N_expr) :: r1178 in
  let r1180 = S (T T_IN) :: r1179 in
  let r1181 = [R 101] in
  let r1182 = S (N N_expr) :: r1181 in
  let r1183 = Sub (r582) :: r1182 in
  let r1184 = [R 714] in
  let r1185 = [R 703] in
  let r1186 = [R 702] in
  let r1187 = [R 713] in
  let r1188 = [R 716] in
  let r1189 = [R 715] in
  let r1190 = [R 712] in
  let r1191 = S (T T_LIDENT) :: r527 in
  let r1192 = [R 692] in
  let r1193 = S (T T_GREATERRBRACE) :: r1192 in
  let r1194 = [R 699] in
  let r1195 = S (T T_RBRACE) :: r1194 in
  let r1196 = [R 561] in
  let r1197 = Sub (r532) :: r1196 in
  let r1198 = [R 139] in
  let r1199 = S (T T_DONE) :: r1198 in
  let r1200 = Sub (r1) :: r1199 in
  let r1201 = S (T T_DO) :: r1200 in
  let r1202 = Sub (r1) :: r1201 in
  let r1203 = Sub (r582) :: r1202 in
  let r1204 = [R 213] in
  let r1205 = Sub (r562) :: r1204 in
  let r1206 = S (T T_RPAREN) :: r1205 in
  let r1207 = [R 211] in
  let r1208 = Sub (r1) :: r1207 in
  let r1209 = S (T T_MINUSGREATER) :: r1208 in
  let r1210 = [R 212] in
  let r1211 = [R 588] in
  let r1212 = [R 151] in
  let r1213 = [R 676] in
  let r1214 = [R 688] in
  let r1215 = [R 709] in
  let r1216 = [R 700] in
  let r1217 = [R 710] in
  let r1218 = [R 142] in
  let r1219 = Sub (r1) :: r1218 in
  let r1220 = S (T T_IN) :: r1219 in
  let r1221 = Sub (r696) :: r1220 in
  let r1222 = Sub (r65) :: r1221 in
  let r1223 = R 307 :: r1222 in
  let r1224 = [R 143] in
  let r1225 = Sub (r1) :: r1224 in
  let r1226 = S (T T_IN) :: r1225 in
  let r1227 = R 307 :: r1226 in
  let r1228 = R 219 :: r1227 in
  let r1229 = Sub (r140) :: r1228 in
  let r1230 = R 307 :: r1229 in
  let r1231 = [R 337] in
  let r1232 = Sub (r253) :: r1231 in
  let r1233 = [R 341] in
  let r1234 = Sub (r1232) :: r1233 in
  let r1235 = S (T T_RPAREN) :: r1234 in
  let r1236 = Sub (r404) :: r1235 in
  let r1237 = [R 338] in
  let r1238 = Sub (r1) :: r1237 in
  let r1239 = [R 340] in
  let r1240 = [R 278] in
  let r1241 = Sub (r1) :: r1240 in
  let r1242 = S (T T_EQUAL) :: r1241 in
  let r1243 = Sub (r89) :: r1242 in
  let r1244 = [R 276] in
  let r1245 = Sub (r1) :: r1244 in
  let r1246 = [R 704] in
  let r1247 = [R 711] in
  let r1248 = [R 679] in
  let r1249 = S (T T_RPAREN) :: r1248 in
  let r1250 = S (N N_module_expr) :: r1249 in
  let r1251 = R 307 :: r1250 in
  let r1252 = [R 680] in
  let r1253 = S (T T_RPAREN) :: r1252 in
  let r1254 = [R 666] in
  let r1255 = [R 504] in
  let r1256 = S (T T_RPAREN) :: r1255 in
  let r1257 = [R 502] in
  let r1258 = S (T T_RPAREN) :: r1257 in
  let r1259 = [R 503] in
  let r1260 = S (T T_RPAREN) :: r1259 in
  let r1261 = [R 320] in
  let r1262 = R 318 :: r1261 in
  let r1263 = [R 352] in
  let r1264 = R 307 :: r1263 in
  let r1265 = Sub (r761) :: r1264 in
  let r1266 = [R 350] in
  let r1267 = [R 29] in
  let r1268 = [R 814] in
  let r1269 = Sub (r83) :: r1268 in
  let r1270 = S (T T_MINUSGREATER) :: r1269 in
  let r1271 = S (T T_RPAREN) :: r1270 in
  let r1272 = Sub (r89) :: r1271 in
  let r1273 = [R 815] in
  let r1274 = [R 820] in
  let r1275 = Sub (r83) :: r1274 in
  let r1276 = S (T T_MINUSGREATER) :: r1275 in
  let r1277 = [R 818] in
  let r1278 = Sub (r83) :: r1277 in
  let r1279 = S (T T_MINUSGREATER) :: r1278 in
  let r1280 = S (T T_RPAREN) :: r1279 in
  let r1281 = Sub (r89) :: r1280 in
  let r1282 = [R 819] in
  let r1283 = [R 821] in
  let r1284 = [R 817] in
  let r1285 = [R 826] in
  let r1286 = Sub (r83) :: r1285 in
  let r1287 = S (T T_MINUSGREATER) :: r1286 in
  let r1288 = S (T T_RPAREN) :: r1287 in
  let r1289 = Sub (r89) :: r1288 in
  let r1290 = [R 827] in
  let r1291 = [R 823] in
  let r1292 = [R 439] in
  let r1293 = S (T T_RBRACE) :: r1292 in
  let r1294 = [R 203] in
  let r1295 = R 307 :: r1294 in
  let r1296 = [R 204] in
  let r1297 = R 307 :: r1296 in
  let r1298 = [R 69] in
  let r1299 = S (T T_RPAREN) :: r1298 in
  let r1300 = [R 135] in
  let r1301 = [R 137] in
  let r1302 = [R 136] in
  let r1303 = [R 233] in
  let r1304 = [R 238] in
  let r1305 = [R 367] in
  let r1306 = [R 370] in
  let r1307 = S (T T_RPAREN) :: r1306 in
  let r1308 = S (T T_COLONCOLON) :: r1307 in
  let r1309 = S (T T_LPAREN) :: r1308 in
  let r1310 = [R 505] in
  let r1311 = [R 506] in
  let r1312 = [R 507] in
  let r1313 = [R 508] in
  let r1314 = [R 509] in
  let r1315 = [R 510] in
  let r1316 = [R 511] in
  let r1317 = [R 512] in
  let r1318 = [R 513] in
  let r1319 = [R 514] in
  let r1320 = [R 515] in
  let r1321 = [R 853] in
  let r1322 = [R 862] in
  let r1323 = [R 327] in
  let r1324 = [R 860] in
  let r1325 = S (T T_SEMISEMI) :: r1324 in
  let r1326 = [R 861] in
  let r1327 = [R 329] in
  let r1328 = [R 332] in
  let r1329 = [R 331] in
  let r1330 = [R 330] in
  let r1331 = R 328 :: r1330 in
  let r1332 = [R 889] in
  let r1333 = S (T T_EOF) :: r1332 in
  let r1334 = R 328 :: r1333 in
  let r1335 = [R 888] in
  function
  | 0 | 1990 | 1994 | 2012 | 2016 | 2020 | 2024 | 2028 | 2032 | 2036 | 2040 | 2044 | 2048 | 2054 | 2074 -> Nothing
  | 1989 -> One ([R 0])
  | 1993 -> One ([R 1])
  | 1999 -> One ([R 2])
  | 2013 -> One ([R 3])
  | 2017 -> One ([R 4])
  | 2023 -> One ([R 5])
  | 2025 -> One ([R 6])
  | 2029 -> One ([R 7])
  | 2033 -> One ([R 8])
  | 2037 -> One ([R 9])
  | 2041 -> One ([R 10])
  | 2047 -> One ([R 11])
  | 2051 -> One ([R 12])
  | 2064 -> One ([R 13])
  | 2084 -> One ([R 14])
  | 217 -> One ([R 15])
  | 216 -> One ([R 16])
  | 2007 -> One ([R 20])
  | 2009 -> One ([R 21])
  | 287 -> One ([R 26])
  | 302 -> One ([R 27])
  | 298 -> One ([R 41])
  | 1395 -> One ([R 46])
  | 1404 -> One ([R 51])
  | 1399 -> One ([R 52])
  | 1440 -> One ([R 61])
  | 1407 -> One ([R 66])
  | 1182 -> One ([R 78])
  | 1162 -> One ([R 79])
  | 1164 -> One ([R 83])
  | 1402 -> One ([R 87])
  | 432 -> One ([R 104])
  | 75 -> One ([R 105])
  | 430 -> One ([R 106])
  | 74 -> One ([R 110])
  | 200 | 923 -> One ([R 111])
  | 965 -> One ([R 114])
  | 999 -> One ([R 126])
  | 1003 -> One ([R 127])
  | 319 -> One ([R 129])
  | 1622 -> One ([R 130])
  | 716 -> One ([R 141])
  | 1579 -> One ([R 157])
  | 739 -> One ([R 158])
  | 761 -> One ([R 159])
  | 742 -> One ([R 160])
  | 759 -> One ([R 197])
  | 1 -> One (R 199 :: r7)
  | 63 -> One (R 199 :: r26)
  | 68 -> One (R 199 :: r31)
  | 71 -> One (R 199 :: r42)
  | 78 -> One (R 199 :: r50)
  | 98 -> One (R 199 :: r69)
  | 109 -> One (R 199 :: r97)
  | 218 -> One (R 199 :: r207)
  | 219 -> One (R 199 :: r211)
  | 225 -> One (R 199 :: r223)
  | 240 -> One (R 199 :: r233)
  | 243 -> One (R 199 :: r238)
  | 252 -> One (R 199 :: r250)
  | 424 -> One (R 199 :: r385)
  | 447 -> One (R 199 :: r398)
  | 557 -> One (R 199 :: r471)
  | 649 -> One (R 199 :: r542)
  | 652 -> One (R 199 :: r545)
  | 655 -> One (R 199 :: r550)
  | 658 -> One (R 199 :: r553)
  | 664 -> One (R 199 :: r566)
  | 672 -> One (R 199 :: r580)
  | 677 -> One (R 199 :: r592)
  | 693 -> One (R 199 :: r603)
  | 707 -> One (R 199 :: r609)
  | 864 -> One (R 199 :: r701)
  | 905 -> One (R 199 :: r738)
  | 1055 -> One (R 199 :: r832)
  | 1056 -> One (R 199 :: r836)
  | 1065 -> One (R 199 :: r844)
  | 1106 -> One (R 199 :: r882)
  | 1107 -> One (R 199 :: r891)
  | 1243 -> One (R 199 :: r970)
  | 1275 -> One (R 199 :: r1000)
  | 1481 -> One (R 199 :: r1124)
  | 1748 -> One (R 199 :: r1223)
  | 1755 -> One (R 199 :: r1230)
  | 1807 -> One (R 199 :: r1251)
  | 310 -> One ([R 215])
  | 569 -> One ([R 218])
  | 155 -> One ([R 231])
  | 903 -> One ([R 234])
  | 904 -> One ([R 235])
  | 133 -> One (R 236 :: r103)
  | 137 -> One (R 236 :: r105)
  | 215 -> One ([R 240])
  | 949 -> One ([R 244])
  | 950 -> One ([R 245])
  | 1398 -> One ([R 250])
  | 856 -> One ([R 272])
  | 827 -> One ([R 274])
  | 1478 -> One ([R 284])
  | 1405 -> One ([R 287])
  | 517 -> One ([R 288])
  | 1765 -> One ([R 291])
  | 107 -> One (R 307 :: r77)
  | 171 -> One (R 307 :: r132)
  | 223 -> One (R 307 :: r216)
  | 236 -> One (R 307 :: r228)
  | 560 -> One (R 307 :: r475)
  | 567 -> One (R 307 :: r485)
  | 809 -> One (R 307 :: r664)
  | 887 -> One (R 307 :: r721)
  | 1084 -> One (R 307 :: r863)
  | 1118 -> One (R 307 :: r897)
  | 1124 -> One (R 307 :: r905)
  | 1135 -> One (R 307 :: r911)
  | 1146 -> One (R 307 :: r914)
  | 1150 -> One (R 307 :: r923)
  | 1171 -> One (R 307 :: r937)
  | 1187 -> One (R 307 :: r947)
  | 1222 -> One (R 307 :: r964)
  | 1249 -> One (R 307 :: r978)
  | 1259 -> One (R 307 :: r987)
  | 1292 -> One (R 307 :: r1007)
  | 1296 -> One (R 307 :: r1020)
  | 1325 -> One (R 307 :: r1038)
  | 1364 -> One (R 307 :: r1060)
  | 1368 -> One (R 307 :: r1064)
  | 1369 -> One (R 307 :: r1068)
  | 1380 -> One (R 307 :: r1084)
  | 1388 -> One (R 307 :: r1093)
  | 1432 -> One (R 307 :: r1105)
  | 1452 -> One (R 307 :: r1118)
  | 1852 -> One (R 307 :: r1266)
  | 1248 -> One (R 309 :: r971)
  | 1486 -> One (R 309 :: r1125)
  | 1258 -> One (R 311 :: r979)
  | 872 -> One (R 313 :: r709)
  | 1180 -> One (R 313 :: r938)
  | 1241 -> One (R 313 :: r966)
  | 1438 -> One (R 313 :: r1106)
  | 1479 -> One (R 313 :: r1120)
  | 1491 -> One (R 313 :: r1127)
  | 1842 -> One (R 313 :: r1262)
  | 2069 -> One (R 313 :: r1325)
  | 2080 -> One (R 313 :: r1331)
  | 2085 -> One (R 313 :: r1334)
  | 1054 -> One (R 315 :: r828)
  | 1233 -> One (R 315 :: r965)
  | 214 -> One (R 318 :: r203)
  | 1462 -> One (R 318 :: r1119)
  | 1183 -> One (R 322 :: r939)
  | 1441 -> One (R 324 :: r1107)
  | 2067 -> One (R 326 :: r1323)
  | 2075 -> One (R 328 :: r1327)
  | 2076 -> One (R 328 :: r1328)
  | 2077 -> One (R 328 :: r1329)
  | 501 -> One ([R 334])
  | 505 -> One ([R 336])
  | 750 -> One ([R 343])
  | 1475 -> One ([R 344])
  | 1703 -> One ([R 347])
  | 1855 -> One ([R 348])
  | 1858 -> One ([R 349])
  | 1857 -> One ([R 351])
  | 1856 -> One ([R 353])
  | 1854 -> One ([R 354])
  | 2008 -> One ([R 366])
  | 1998 -> One ([R 368])
  | 2006 -> One ([R 369])
  | 2005 -> One ([R 371])
  | 684 -> One ([R 378])
  | 1666 -> One ([R 379])
  | 625 -> One ([R 390])
  | 635 -> One ([R 391])
  | 636 -> One ([R 392])
  | 634 -> One ([R 393])
  | 637 -> One ([R 395])
  | 170 -> One ([R 396])
  | 102 | 1075 -> One ([R 397])
  | 596 -> One ([R 404])
  | 573 -> One ([R 405])
  | 603 -> One ([R 408])
  | 1298 | 1311 -> One ([R 413])
  | 934 -> One ([R 415])
  | 935 -> One ([R 416])
  | 933 -> One ([R 417])
  | 1128 -> One ([R 419])
  | 1126 -> One ([R 420])
  | 1129 -> One ([R 421])
  | 1127 -> One ([R 422])
  | 465 -> One ([R 425])
  | 916 -> One ([R 427])
  | 1011 -> One ([R 428])
  | 1917 -> One ([R 429])
  | 1027 -> One ([R 430])
  | 1918 -> One ([R 431])
  | 1026 -> One ([R 432])
  | 1018 -> One ([R 433])
  | 92 | 247 -> One ([R 446])
  | 116 | 702 -> One ([R 447])
  | 144 -> One ([R 448])
  | 132 -> One ([R 450])
  | 136 -> One ([R 452])
  | 140 -> One ([R 454])
  | 123 -> One ([R 455])
  | 143 | 1599 -> One ([R 456])
  | 122 -> One ([R 457])
  | 121 -> One ([R 458])
  | 120 -> One ([R 459])
  | 119 -> One ([R 460])
  | 118 -> One ([R 461])
  | 95 | 113 | 692 -> One ([R 462])
  | 94 | 691 -> One ([R 463])
  | 93 -> One ([R 464])
  | 115 | 471 | 701 -> One ([R 465])
  | 114 | 700 -> One ([R 466])
  | 90 -> One ([R 467])
  | 96 -> One ([R 468])
  | 125 -> One ([R 469])
  | 117 -> One ([R 470])
  | 124 -> One ([R 471])
  | 97 -> One ([R 472])
  | 142 -> One ([R 473])
  | 145 -> One ([R 474])
  | 141 -> One ([R 476])
  | 372 -> One ([R 477])
  | 371 -> One (R 478 :: r337)
  | 264 -> One (R 479 :: r270)
  | 265 -> One ([R 480])
  | 502 -> One (R 481 :: r420)
  | 503 -> One ([R 482])
  | 1655 -> One ([R 496])
  | 161 -> One ([R 497])
  | 457 -> One ([R 517])
  | 451 -> One ([R 518])
  | 452 -> One ([R 520])
  | 450 | 703 -> One ([R 527])
  | 851 -> One ([R 533])
  | 852 -> One ([R 534])
  | 853 -> One ([R 536])
  | 529 -> One ([R 538])
  | 1274 -> One ([R 542])
  | 1033 | 1345 -> One ([R 552])
  | 1139 -> One ([R 554])
  | 1137 -> One ([R 555])
  | 1140 -> One ([R 556])
  | 1138 -> One ([R 557])
  | 1414 -> One (R 558 :: r1099)
  | 255 -> One ([R 559])
  | 1009 -> One ([R 562])
  | 1010 -> One ([R 563])
  | 1005 -> One ([R 564])
  | 1934 -> One ([R 566])
  | 1933 -> One ([R 567])
  | 1935 -> One ([R 568])
  | 1930 -> One ([R 569])
  | 1931 -> One ([R 570])
  | 1039 -> One ([R 572])
  | 1037 -> One ([R 573])
  | 1632 -> One ([R 576])
  | 1631 -> One ([R 577])
  | 618 -> One ([R 578])
  | 570 -> One ([R 579])
  | 1401 -> One ([R 580])
  | 1400 -> One ([R 581])
  | 394 -> One ([R 583])
  | 364 -> One ([R 613])
  | 1518 -> One ([R 616])
  | 1519 -> One ([R 617])
  | 1726 -> One ([R 619])
  | 1727 -> One ([R 620])
  | 496 -> One ([R 622])
  | 497 -> One ([R 623])
  | 1658 -> One ([R 625])
  | 1659 -> One ([R 626])
  | 764 -> One ([R 628])
  | 768 -> One ([R 629])
  | 1269 -> One ([R 634])
  | 1232 -> One ([R 635])
  | 1235 -> One ([R 636])
  | 1234 -> One ([R 641])
  | 1239 -> One ([R 644])
  | 1238 -> One ([R 646])
  | 1237 -> One ([R 647])
  | 1236 -> One ([R 648])
  | 1270 -> One ([R 651])
  | 88 -> One ([R 654])
  | 85 -> One ([R 656])
  | 683 -> One ([R 682])
  | 746 -> One ([R 683])
  | 745 | 760 -> One ([R 684])
  | 686 | 741 -> One ([R 685])
  | 1526 | 1576 -> One ([R 690])
  | 744 -> One ([R 695])
  | 433 -> One ([R 718])
  | 437 -> One ([R 721])
  | 438 -> One ([R 725])
  | 469 -> One ([R 727])
  | 442 -> One ([R 728])
  | 498 -> One ([R 730])
  | 460 -> One ([R 735])
  | 30 -> One ([R 736])
  | 8 -> One ([R 737])
  | 54 -> One ([R 739])
  | 53 -> One ([R 740])
  | 52 -> One ([R 741])
  | 51 -> One ([R 742])
  | 50 -> One ([R 743])
  | 49 -> One ([R 744])
  | 48 -> One ([R 745])
  | 47 -> One ([R 746])
  | 46 -> One ([R 747])
  | 45 -> One ([R 748])
  | 44 -> One ([R 749])
  | 43 -> One ([R 750])
  | 42 -> One ([R 751])
  | 41 -> One ([R 752])
  | 40 -> One ([R 753])
  | 39 -> One ([R 754])
  | 38 -> One ([R 755])
  | 23 -> One ([R 756])
  | 37 -> One ([R 757])
  | 36 -> One ([R 758])
  | 35 -> One ([R 759])
  | 34 -> One ([R 760])
  | 33 -> One ([R 761])
  | 32 -> One ([R 762])
  | 31 -> One ([R 763])
  | 29 -> One ([R 764])
  | 28 -> One ([R 765])
  | 27 -> One ([R 766])
  | 26 -> One ([R 767])
  | 25 -> One ([R 768])
  | 24 -> One ([R 769])
  | 22 -> One ([R 770])
  | 21 -> One ([R 771])
  | 20 -> One ([R 772])
  | 19 -> One ([R 773])
  | 18 -> One ([R 774])
  | 17 -> One ([R 775])
  | 16 -> One ([R 776])
  | 15 -> One ([R 777])
  | 14 -> One ([R 778])
  | 13 -> One ([R 779])
  | 12 -> One ([R 780])
  | 11 -> One ([R 781])
  | 10 -> One ([R 782])
  | 9 -> One ([R 783])
  | 7 -> One ([R 784])
  | 6 -> One ([R 785])
  | 5 -> One ([R 786])
  | 4 -> One ([R 787])
  | 3 -> One ([R 788])
  | 1470 -> One ([R 789])
  | 334 -> One ([R 794])
  | 361 -> One ([R 795])
  | 349 -> One ([R 796])
  | 355 -> One ([R 797])
  | 1870 -> One ([R 798])
  | 1893 -> One ([R 799])
  | 1881 -> One ([R 800])
  | 1887 -> One ([R 801])
  | 1912 -> One ([R 802])
  | 363 -> One ([R 803])
  | 1902 -> One ([R 804])
  | 307 -> One ([R 805])
  | 1496 -> One ([R 830])
  | 1474 | 1495 -> One ([R 832])
  | 1477 | 1497 -> One ([R 833])
  | 1488 -> One ([R 835])
  | 1471 -> One ([R 836])
  | 1461 -> One ([R 837])
  | 1469 -> One ([R 841])
  | 1473 -> One ([R 844])
  | 1472 -> One ([R 845])
  | 1489 -> One ([R 847])
  | 239 -> One ([R 849])
  | 238 -> One ([R 850])
  | 2058 -> One ([R 854])
  | 2059 -> One ([R 855])
  | 2061 -> One ([R 856])
  | 2062 -> One ([R 857])
  | 2060 -> One ([R 858])
  | 2057 -> One ([R 859])
  | 2063 -> One ([R 863])
  | 295 -> One ([R 865])
  | 576 -> One (R 873 :: r502)
  | 590 -> One ([R 874])
  | 177 -> One ([R 879])
  | 180 -> One ([R 880])
  | 184 -> One ([R 881])
  | 178 -> One ([R 882])
  | 185 -> One ([R 883])
  | 181 -> One ([R 884])
  | 186 -> One ([R 885])
  | 183 -> One ([R 886])
  | 176 -> One ([R 887])
  | 434 -> One ([R 892])
  | 743 -> One ([R 893])
  | 1110 -> One ([R 901])
  | 1309 -> One ([R 902])
  | 1312 -> One ([R 903])
  | 1310 -> One ([R 904])
  | 1343 -> One ([R 905])
  | 1346 -> One ([R 906])
  | 1344 -> One ([R 907])
  | 579 -> One ([R 914])
  | 580 -> One ([R 915])
  | 1651 -> One (S (T T_WITH) :: r1197)
  | 166 -> One (S (T T_TYPE) :: r129)
  | 531 -> One (S (T T_TYPE) :: r443)
  | 1773 -> One (S (T T_TYPE) :: r1236)
  | 954 -> One (S (T T_STAR) :: r782)
  | 2065 -> One (S (T T_SEMISEMI) :: r1322)
  | 2072 -> One (S (T T_SEMISEMI) :: r1326)
  | 1995 -> One (S (T T_RPAREN) :: r56)
  | 311 -> One (S (T T_RPAREN) :: r307)
  | 335 -> One (S (T T_RPAREN) :: r319)
  | 445 -> One (S (T T_RPAREN) :: r395)
  | 489 -> One (S (T T_RPAREN) :: r419)
  | 562 -> One (S (T T_RPAREN) :: r476)
  | 627 -> One (S (T T_RPAREN) :: r517)
  | 1600 -> One (S (T T_RPAREN) :: r1163)
  | 1817 -> One (S (T T_RPAREN) :: r1254)
  | 1996 -> One (S (T T_RPAREN) :: r1305)
  | 267 -> One (S (T T_RBRACKET) :: r271)
  | 927 | 994 -> One (S (T T_RBRACKET) :: r365)
  | 1640 -> One (S (T T_RBRACKET) :: r1188)
  | 1642 -> One (S (T T_RBRACKET) :: r1189)
  | 1645 -> One (S (T T_RBRACKET) :: r1190)
  | 1734 -> One (S (T T_RBRACKET) :: r1215)
  | 321 -> One (S (T T_QUOTE) :: r311)
  | 1148 -> One (S (T T_OPEN) :: r919)
  | 1372 -> One (S (T T_OPEN) :: r1075)
  | 206 | 209 | 211 | 309 | 340 | 1872 -> One (S (T T_MODULE) :: r118)
  | 974 -> One (S (T T_MINUSGREATER) :: r790)
  | 978 -> One (S (T T_MINUSGREATER) :: r792)
  | 1209 -> One (S (T T_MINUSGREATER) :: r953)
  | 126 -> One (S (T T_LPAREN) :: r100)
  | 536 -> One (S (T T_LOCAL) :: r447)
  | 667 | 1281 | 1685 -> One (S (T T_LOCAL) :: r569)
  | 158 -> One (S (T T_LIDENT) :: r113)
  | 259 -> One (S (T T_LIDENT) :: r256)
  | 405 -> One (S (T T_LIDENT) :: r348)
  | 717 -> One (S (T T_LIDENT) :: r616)
  | 718 -> One (S (T T_LIDENT) :: r622)
  | 729 -> One (S (T T_LIDENT) :: r625)
  | 733 -> One (S (T T_LIDENT) :: r627)
  | 936 -> One (S (T T_LIDENT) :: r777)
  | 1313 -> One (S (T T_LIDENT) :: r1025)
  | 1347 -> One (S (T T_LIDENT) :: r1049)
  | 1424 -> One (S (T T_LIDENT) :: r1102)
  | 83 -> One (S (T T_INT) :: r54)
  | 86 -> One (S (T T_INT) :: r55)
  | 747 -> One (S (T T_IN) :: r634)
  | 751 -> One (S (T T_IN) :: r636)
  | 1392 -> One (S (T T_IN) :: r1095)
  | 642 -> One (S (T T_GREATERRBRACE) :: r525)
  | 1729 -> One (S (T T_GREATERRBRACE) :: r1214)
  | 210 -> One (S (T T_GREATER) :: r199)
  | 1860 -> One (S (T T_GREATER) :: r1267)
  | 608 -> One (S (T T_EQUAL) :: r513)
  | 824 -> One (S (T T_EQUAL) :: r675)
  | 830 -> One (S (T T_EQUAL) :: r678)
  | 840 -> One (S (T T_EQUAL) :: r683)
  | 1303 -> One (S (T T_EQUAL) :: r1022)
  | 1321 -> One (S (T T_EQUAL) :: r1027)
  | 1590 -> One (S (T T_EQUAL) :: r1161)
  | 1779 -> One (S (T T_EQUAL) :: r1238)
  | 1792 -> One (S (T T_EQUAL) :: r1245)
  | 1987 -> One (S (T T_EOF) :: r1303)
  | 1991 -> One (S (T T_EOF) :: r1304)
  | 2010 -> One (S (T T_EOF) :: r1310)
  | 2014 -> One (S (T T_EOF) :: r1311)
  | 2018 -> One (S (T T_EOF) :: r1312)
  | 2021 -> One (S (T T_EOF) :: r1313)
  | 2026 -> One (S (T T_EOF) :: r1314)
  | 2030 -> One (S (T T_EOF) :: r1315)
  | 2034 -> One (S (T T_EOF) :: r1316)
  | 2038 -> One (S (T T_EOF) :: r1317)
  | 2042 -> One (S (T T_EOF) :: r1318)
  | 2045 -> One (S (T T_EOF) :: r1319)
  | 2049 -> One (S (T T_EOF) :: r1320)
  | 2089 -> One (S (T T_EOF) :: r1335)
  | 1716 -> One (S (T T_END) :: r1213)
  | 128 -> One (S (T T_DOTDOT) :: r101)
  | 203 -> One (S (T T_DOTDOT) :: r189)
  | 1012 -> One (S (T T_DOTDOT) :: r819)
  | 1013 -> One (S (T T_DOTDOT) :: r820)
  | 229 | 1512 | 1559 -> One (S (T T_DOT) :: r225)
  | 324 -> One (S (T T_DOT) :: r316)
  | 341 -> One (S (T T_DOT) :: r328)
  | 396 -> One (S (T T_DOT) :: r347)
  | 519 -> One (S (T T_DOT) :: r434)
  | 549 -> One (S (T T_DOT) :: r454)
  | 2052 -> One (S (T T_DOT) :: r514)
  | 819 -> One (S (T T_DOT) :: r673)
  | 910 -> One (S (T T_DOT) :: r740)
  | 939 -> One (S (T T_DOT) :: r779)
  | 972 -> One (S (T T_DOT) :: r788)
  | 1287 -> One (S (T T_DOT) :: r1003)
  | 1787 -> One (S (T T_DOT) :: r1243)
  | 1862 -> One (S (T T_DOT) :: r1272)
  | 1873 -> One (S (T T_DOT) :: r1281)
  | 1894 -> One (S (T T_DOT) :: r1289)
  | 2000 -> One (S (T T_DOT) :: r1309)
  | 248 -> One (S (T T_COLONRBRACKET) :: r241)
  | 411 -> One (S (T T_COLONRBRACKET) :: r363)
  | 510 -> One (S (T T_COLONRBRACKET) :: r422)
  | 1602 -> One (S (T T_COLONRBRACKET) :: r1164)
  | 1604 -> One (S (T T_COLONRBRACKET) :: r1165)
  | 1629 -> One (S (T T_COLONRBRACKET) :: r1184)
  | 1801 -> One (S (T T_COLONRBRACKET) :: r1246)
  | 1804 -> One (S (T T_COLONRBRACKET) :: r1247)
  | 204 | 924 -> One (S (T T_COLONCOLON) :: r191)
  | 564 -> One (S (T T_COLON) :: r479)
  | 1203 -> One (S (T T_COLON) :: r951)
  | 1848 -> One (S (T T_COLON) :: r1265)
  | 412 -> One (S (T T_BARRBRACKET) :: r364)
  | 507 -> One (S (T T_BARRBRACKET) :: r421)
  | 640 -> One (S (T T_BARRBRACKET) :: r520)
  | 1633 -> One (S (T T_BARRBRACKET) :: r1185)
  | 1635 -> One (S (T T_BARRBRACKET) :: r1186)
  | 1638 -> One (S (T T_BARRBRACKET) :: r1187)
  | 1737 -> One (S (T T_BARRBRACKET) :: r1216)
  | 1740 -> One (S (T T_BARRBRACKET) :: r1217)
  | 383 -> One (S (T T_BAR) :: r341)
  | 81 -> One (S (N N_pattern) :: r52)
  | 462 -> One (S (N N_pattern) :: r58)
  | 423 -> One (S (N N_pattern) :: r379)
  | 453 -> One (S (N N_pattern) :: r399)
  | 455 -> One (S (N N_pattern) :: r400)
  | 476 -> One (S (N N_pattern) :: r411)
  | 481 -> One (S (N N_pattern) :: r415)
  | 843 -> One (S (N N_pattern) :: r684)
  | 845 -> One (S (N N_pattern) :: r685)
  | 847 -> One (S (N N_pattern) :: r686)
  | 854 -> One (S (N N_pattern) :: r688)
  | 860 -> One (S (N N_pattern) :: r692)
  | 1613 -> One (S (N N_pattern) :: r1180)
  | 105 -> One (S (N N_module_type) :: r71)
  | 566 -> One (S (N N_module_type) :: r481)
  | 604 -> One (S (N N_module_type) :: r510)
  | 606 -> One (S (N N_module_type) :: r511)
  | 631 -> One (S (N N_module_type) :: r519)
  | 869 -> One (S (N N_module_type) :: r708)
  | 881 -> One (S (N N_module_type) :: r716)
  | 1812 -> One (S (N N_module_type) :: r1253)
  | 1827 -> One (S (N N_module_type) :: r1256)
  | 1830 -> One (S (N N_module_type) :: r1258)
  | 1833 -> One (S (N N_module_type) :: r1260)
  | 222 -> One (S (N N_module_expr) :: r213)
  | 515 -> One (S (N N_let_pattern) :: r428)
  | 516 -> One (S (N N_let_pattern) :: r431)
  | 251 -> One (S (N N_expr) :: r243)
  | 644 -> One (S (N N_expr) :: r528)
  | 648 -> One (S (N N_expr) :: r539)
  | 715 -> One (S (N N_expr) :: r615)
  | 740 -> One (S (N N_expr) :: r632)
  | 755 -> One (S (N N_expr) :: r637)
  | 757 -> One (S (N N_expr) :: r638)
  | 762 -> One (S (N N_expr) :: r639)
  | 769 -> One (S (N N_expr) :: r642)
  | 771 -> One (S (N N_expr) :: r643)
  | 773 -> One (S (N N_expr) :: r644)
  | 775 -> One (S (N N_expr) :: r645)
  | 777 -> One (S (N N_expr) :: r646)
  | 779 -> One (S (N N_expr) :: r647)
  | 781 -> One (S (N N_expr) :: r648)
  | 783 -> One (S (N N_expr) :: r649)
  | 785 -> One (S (N N_expr) :: r650)
  | 787 -> One (S (N N_expr) :: r651)
  | 789 -> One (S (N N_expr) :: r652)
  | 791 -> One (S (N N_expr) :: r653)
  | 793 -> One (S (N N_expr) :: r654)
  | 795 -> One (S (N N_expr) :: r655)
  | 797 -> One (S (N N_expr) :: r656)
  | 799 -> One (S (N N_expr) :: r657)
  | 801 -> One (S (N N_expr) :: r658)
  | 803 -> One (S (N N_expr) :: r659)
  | 805 -> One (S (N N_expr) :: r660)
  | 807 -> One (S (N N_expr) :: r661)
  | 1531 -> One (S (N N_expr) :: r1144)
  | 1536 -> One (S (N N_expr) :: r1148)
  | 1541 -> One (S (N N_expr) :: r1152)
  | 1547 -> One (S (N N_expr) :: r1153)
  | 1552 -> One (S (N N_expr) :: r1154)
  | 1557 -> One (S (N N_expr) :: r1155)
  | 1564 -> One (S (N N_expr) :: r1156)
  | 1569 -> One (S (N N_expr) :: r1157)
  | 1574 -> One (S (N N_expr) :: r1158)
  | 1577 -> One (S (N N_expr) :: r1159)
  | 1607 -> One (S (N N_expr) :: r1166)
  | 1620 -> One (S (N N_expr) :: r1183)
  | 1713 -> One (S (N N_expr) :: r1212)
  | 249 -> One (Sub (r1) :: r242)
  | 409 -> One (Sub (r1) :: r355)
  | 663 -> One (Sub (r1) :: r557)
  | 862 -> One (Sub (r1) :: r693)
  | 1677 -> One (Sub (r1) :: r1203)
  | 1972 -> One (Sub (r1) :: r1301)
  | 1974 -> One (Sub (r1) :: r1302)
  | 2 -> One (Sub (r11) :: r12)
  | 57 -> One (Sub (r11) :: r13)
  | 61 -> One (Sub (r11) :: r20)
  | 212 -> One (Sub (r11) :: r202)
  | 765 -> One (Sub (r11) :: r641)
  | 858 -> One (Sub (r11) :: r691)
  | 899 -> One (Sub (r11) :: r725)
  | 901 -> One (Sub (r11) :: r728)
  | 1373 -> One (Sub (r11) :: r1080)
  | 661 -> One (Sub (r35) :: r554)
  | 1707 -> One (Sub (r35) :: r1211)
  | 1970 -> One (Sub (r37) :: r1300)
  | 77 -> One (Sub (r44) :: r45)
  | 647 -> One (Sub (r44) :: r537)
  | 682 -> One (Sub (r44) :: r593)
  | 711 -> One (Sub (r44) :: r610)
  | 731 -> One (Sub (r44) :: r626)
  | 1396 -> One (Sub (r44) :: r1096)
  | 877 -> One (Sub (r65) :: r713)
  | 1079 -> One (Sub (r65) :: r857)
  | 986 -> One (Sub (r74) :: r793)
  | 257 -> One (Sub (r79) :: r255)
  | 483 -> One (Sub (r79) :: r416)
  | 849 -> One (Sub (r79) :: r687)
  | 296 -> One (Sub (r81) :: r300)
  | 304 -> One (Sub (r81) :: r302)
  | 930 -> One (Sub (r81) :: r759)
  | 952 -> One (Sub (r81) :: r780)
  | 956 -> One (Sub (r81) :: r783)
  | 958 -> One (Sub (r81) :: r784)
  | 971 -> One (Sub (r81) :: r786)
  | 1689 -> One (Sub (r81) :: r1209)
  | 205 -> One (Sub (r83) :: r194)
  | 289 -> One (Sub (r83) :: r297)
  | 290 -> One (Sub (r83) :: r298)
  | 293 -> One (Sub (r83) :: r299)
  | 308 -> One (Sub (r83) :: r305)
  | 331 -> One (Sub (r83) :: r318)
  | 339 -> One (Sub (r83) :: r323)
  | 346 -> One (Sub (r83) :: r329)
  | 352 -> One (Sub (r83) :: r330)
  | 358 -> One (Sub (r83) :: r331)
  | 1211 -> One (Sub (r83) :: r956)
  | 1867 -> One (Sub (r83) :: r1273)
  | 1871 -> One (Sub (r83) :: r1276)
  | 1878 -> One (Sub (r83) :: r1282)
  | 1884 -> One (Sub (r83) :: r1283)
  | 1890 -> One (Sub (r83) :: r1284)
  | 1899 -> One (Sub (r83) :: r1290)
  | 1909 -> One (Sub (r83) :: r1291)
  | 375 -> One (Sub (r87) :: r338)
  | 583 -> One (Sub (r87) :: r504)
  | 263 -> One (Sub (r89) :: r263)
  | 316 -> One (Sub (r89) :: r309)
  | 337 -> One (Sub (r89) :: r320)
  | 420 -> One (Sub (r89) :: r378)
  | 518 -> One (Sub (r89) :: r432)
  | 586 -> One (Sub (r89) :: r507)
  | 704 -> One (Sub (r89) :: r606)
  | 720 -> One (Sub (r89) :: r623)
  | 724 -> One (Sub (r89) :: r624)
  | 836 -> One (Sub (r89) :: r681)
  | 1120 -> One (Sub (r89) :: r899)
  | 1158 -> One (Sub (r89) :: r930)
  | 1960 -> One (Sub (r89) :: r1299)
  | 1329 -> One (Sub (r91) :: r1041)
  | 1353 -> One (Sub (r91) :: r1052)
  | 189 -> One (Sub (r107) :: r184)
  | 325 -> One (Sub (r107) :: r317)
  | 2055 -> One (Sub (r107) :: r1321)
  | 540 -> One (Sub (r119) :: r451)
  | 428 -> One (Sub (r136) :: r387)
  | 195 -> One (Sub (r179) :: r185)
  | 182 -> One (Sub (r181) :: r183)
  | 1112 -> One (Sub (r181) :: r893)
  | 199 -> One (Sub (r187) :: r188)
  | 993 -> One (Sub (r187) :: r812)
  | 1042 -> One (Sub (r187) :: r827)
  | 260 -> One (Sub (r258) :: r260)
  | 261 -> One (Sub (r258) :: r262)
  | 406 -> One (Sub (r258) :: r351)
  | 407 -> One (Sub (r258) :: r354)
  | 368 -> One (Sub (r265) :: r332)
  | 269 -> One (Sub (r267) :: r273)
  | 284 -> One (Sub (r267) :: r296)
  | 270 -> One (Sub (r279) :: r281)
  | 271 -> One (Sub (r283) :: r284)
  | 300 -> One (Sub (r283) :: r301)
  | 313 -> One (Sub (r283) :: r308)
  | 274 -> One (Sub (r292) :: r294)
  | 612 -> One (Sub (r292) :: r515)
  | 1076 -> One (Sub (r292) :: r852)
  | 391 -> One (Sub (r343) :: r345)
  | 1596 -> One (Sub (r357) :: r1162)
  | 410 -> One (Sub (r359) :: r362)
  | 415 -> One (Sub (r375) :: r377)
  | 535 -> One (Sub (r382) :: r444)
  | 439 -> One (Sub (r390) :: r391)
  | 463 -> One (Sub (r404) :: r407)
  | 668 -> One (Sub (r404) :: r572)
  | 813 -> One (Sub (r404) :: r669)
  | 1330 -> One (Sub (r404) :: r1046)
  | 1354 -> One (Sub (r404) :: r1057)
  | 1686 -> One (Sub (r404) :: r1206)
  | 513 -> One (Sub (r424) :: r425)
  | 829 -> One (Sub (r439) :: r676)
  | 616 -> One (Sub (r495) :: r516)
  | 575 -> One (Sub (r497) :: r498)
  | 645 -> One (Sub (r534) :: r536)
  | 1650 -> One (Sub (r534) :: r1195)
  | 1693 -> One (Sub (r562) :: r1210)
  | 893 -> One (Sub (r696) :: r722)
  | 1925 -> One (Sub (r741) :: r1295)
  | 1937 -> One (Sub (r741) :: r1297)
  | 929 -> One (Sub (r757) :: r758)
  | 932 -> One (Sub (r769) :: r771)
  | 995 -> One (Sub (r769) :: r814)
  | 1014 -> One (Sub (r769) :: r822)
  | 1022 -> One (Sub (r769) :: r824)
  | 1913 -> One (Sub (r769) :: r1293)
  | 1100 -> One (Sub (r839) :: r868)
  | 1093 -> One (Sub (r865) :: r867)
  | 1420 -> One (Sub (r873) :: r1101)
  | 1444 -> One (Sub (r873) :: r1110)
  | 1384 -> One (Sub (r925) :: r1087)
  | 1371 -> One (Sub (r989) :: r1070)
  | 1448 -> One (Sub (r992) :: r1111)
  | 1295 -> One (Sub (r1013) :: r1015)
  | 1324 -> One (Sub (r1032) :: r1034)
  | 1611 -> One (Sub (r1173) :: r1177)
  | 1609 -> One (Sub (r1175) :: r1176)
  | 1647 -> One (Sub (r1191) :: r1193)
  | 1784 -> One (Sub (r1232) :: r1239)
  | 754 -> One (r0)
  | 1986 -> One (r2)
  | 1985 -> One (r3)
  | 1984 -> One (r4)
  | 1983 -> One (r5)
  | 1982 -> One (r6)
  | 60 -> One (r7)
  | 55 -> One (r8)
  | 56 -> One (r10)
  | 59 -> One (r12)
  | 58 -> One (r13)
  | 1490 -> One (r14)
  | 1494 -> One (r16)
  | 1981 -> One (r18)
  | 1980 -> One (r19)
  | 62 -> One (r20)
  | 1979 -> One (r21)
  | 1978 -> One (r22)
  | 1977 -> One (r23)
  | 1976 -> One (r24)
  | 65 -> One (r25)
  | 64 -> One (r26)
  | 66 -> One (r27)
  | 67 -> One (r28)
  | 1969 -> One (r29)
  | 70 -> One (r30)
  | 69 -> One (r31)
  | 1704 -> One (r32)
  | 1702 -> One (r33)
  | 662 -> One (r34)
  | 1709 -> One (r36)
  | 1968 -> One (r38)
  | 1967 -> One (r39)
  | 1966 -> One (r40)
  | 73 -> One (r41)
  | 72 -> One (r42)
  | 76 -> One (r43)
  | 1806 -> One (r45)
  | 1965 -> One (r46)
  | 1964 -> One (r47)
  | 1963 -> One (r48)
  | 80 -> One (r49)
  | 79 -> One (r50)
  | 1959 -> One (r51)
  | 1958 -> One (r52)
  | 82 -> One (r53)
  | 84 -> One (r54)
  | 87 -> One (r55)
  | 91 -> One (r56)
  | 475 -> One (r57)
  | 474 | 547 | 1285 -> One (r58)
  | 146 -> One (r59)
  | 148 -> One (r61)
  | 147 -> One (r62)
  | 112 -> One (r63)
  | 101 -> One (r64)
  | 104 -> One (r66)
  | 103 -> One (r67)
  | 100 -> One (r68)
  | 99 -> One (r69)
  | 1957 -> One (r70)
  | 1956 -> One (r71)
  | 106 | 153 -> One (r72)
  | 1273 -> One (r73)
  | 1955 -> One (r75)
  | 1954 -> One (r76)
  | 108 -> One (r77)
  | 149 | 250 | 646 | 1665 -> One (r78)
  | 152 -> One (r80)
  | 303 -> One (r82)
  | 288 -> One (r84)
  | 317 -> One (r86)
  | 320 -> One (r88)
  | 919 -> One (r90)
  | 1953 -> One (r92)
  | 1952 -> One (r93)
  | 151 -> One (r94)
  | 150 -> One (r95)
  | 111 -> One (r96)
  | 110 -> One (r97)
  | 131 -> One (r98)
  | 130 -> One (r99)
  | 127 -> One (r100)
  | 129 -> One (r101)
  | 135 -> One (r102)
  | 134 -> One (r103)
  | 139 -> One (r104)
  | 138 -> One (r105)
  | 156 -> One (r106)
  | 202 -> One (r108)
  | 201 -> One (r109)
  | 542 -> One (r110)
  | 541 -> One (r111)
  | 160 -> One (r112)
  | 159 -> One (r113)
  | 1951 -> One (r114)
  | 1950 -> One (r115)
  | 165 -> One (r116)
  | 164 -> One (r117)
  | 163 -> One (r118)
  | 1911 -> One (r120)
  | 1908 -> One (r121)
  | 1907 -> One (r122)
  | 1906 -> One (r123)
  | 1905 -> One (r124)
  | 1904 -> One (r125)
  | 1949 -> One (r126)
  | 169 -> One (r127)
  | 168 -> One (r128)
  | 167 -> One (r129)
  | 1948 -> One (r130)
  | 1947 -> One (r131)
  | 172 -> One (r132)
  | 272 -> One (r133)
  | 297 -> One (r135)
  | 431 -> One (r137)
  | 985 -> One (r139)
  | 1021 -> One (r141)
  | 1020 -> One (r142)
  | 1019 | 1936 -> One (r143)
  | 1932 -> One (r145)
  | 1946 -> One (r147)
  | 1945 -> One (r148)
  | 1944 -> One (r149)
  | 1943 -> One (r150)
  | 1942 -> One (r151)
  | 1048 -> One (r155)
  | 1047 -> One (r156)
  | 1046 -> One (r157)
  | 1929 -> One (r163)
  | 1928 -> One (r164)
  | 1922 -> One (r165)
  | 1921 -> One (r166)
  | 1920 -> One (r167)
  | 1030 -> One (r169)
  | 1029 -> One (r170)
  | 1028 -> One (r171)
  | 188 -> One (r175)
  | 191 -> One (r177)
  | 187 -> One (r178)
  | 192 -> One (r180)
  | 194 -> One (r182)
  | 193 -> One (r183)
  | 190 -> One (r184)
  | 196 -> One (r185)
  | 998 -> One (r186)
  | 1919 -> One (r188)
  | 1916 -> One (r189)
  | 926 -> One (r190)
  | 925 -> One (r191)
  | 306 -> One (r192)
  | 292 -> One (r193)
  | 1903 -> One (r194)
  | 1892 -> One (r195)
  | 1889 -> One (r196)
  | 1888 -> One (r197)
  | 208 -> One (r198)
  | 1859 -> One (r199)
  | 1847 -> One (r200)
  | 1846 -> One (r201)
  | 213 -> One (r202)
  | 1845 -> One (r203)
  | 1841 -> One (r204)
  | 1840 -> One (r205)
  | 1839 -> One (r206)
  | 1838 -> One (r207)
  | 1837 -> One (r208)
  | 1836 -> One (r209)
  | 221 -> One (r210)
  | 220 -> One (r211)
  | 630 -> One (r212)
  | 629 -> One (r213)
  | 1826 -> One (r214)
  | 1825 -> One (r215)
  | 224 -> One (r216)
  | 228 -> One (r217)
  | 234 -> One (r219)
  | 235 -> One (r221)
  | 227 -> One (r222)
  | 226 -> One (r223)
  | 232 -> One (r224)
  | 230 -> One (r225)
  | 231 -> One (r226)
  | 233 -> One (r227)
  | 237 -> One (r228)
  | 1824 -> One (r229)
  | 1823 -> One (r230)
  | 1822 -> One (r231)
  | 242 -> One (r232)
  | 241 -> One (r233)
  | 1821 -> One (r234)
  | 1820 -> One (r235)
  | 1819 -> One (r236)
  | 245 -> One (r237)
  | 244 -> One (r238)
  | 1816 -> One (r239)
  | 1815 -> One (r240)
  | 1800 -> One (r241)
  | 1799 -> One (r242)
  | 1798 -> One (r243)
  | 811 -> One (r244)
  | 1797 -> One (r246)
  | 1796 -> One (r247)
  | 256 -> One (r248)
  | 254 -> One (r249)
  | 253 -> One (r250)
  | 1778 -> One (r251)
  | 1777 -> One (r252)
  | 1795 -> One (r254)
  | 258 -> One (r255)
  | 404 -> One (r256)
  | 262 -> One (r257)
  | 403 -> One (r259)
  | 402 -> One (r260)
  | 401 -> One (r261)
  | 400 -> One (r262)
  | 399 -> One (r263)
  | 380 -> One (r264)
  | 365 -> One (r266)
  | 390 -> One (r268)
  | 389 -> One (r269)
  | 266 -> One (r270)
  | 268 -> One (r271)
  | 388 -> One (r272)
  | 387 -> One (r273)
  | 286 -> One (r274)
  | 285 -> One (r275)
  | 379 -> One (r277)
  | 370 -> One (r278)
  | 382 -> One (r280)
  | 381 -> One (r281)
  | 282 | 1214 -> One (r282)
  | 283 -> One (r284)
  | 278 -> One (r285)
  | 277 -> One (r286)
  | 281 -> One (r288)
  | 279 -> One (r291)
  | 276 -> One (r293)
  | 275 -> One (r294)
  | 367 -> One (r295)
  | 366 -> One (r296)
  | 362 -> One (r297)
  | 291 -> One (r298)
  | 294 -> One (r299)
  | 299 -> One (r300)
  | 301 -> One (r301)
  | 305 -> One (r302)
  | 360 -> One (r303)
  | 357 -> One (r304)
  | 356 -> One (r305)
  | 315 -> One (r306)
  | 312 -> One (r307)
  | 314 -> One (r308)
  | 318 -> One (r309)
  | 323 -> One (r310)
  | 322 -> One (r311)
  | 333 -> One (r312)
  | 330 -> One (r313)
  | 329 -> One (r314)
  | 328 -> One (r315)
  | 327 -> One (r316)
  | 326 -> One (r317)
  | 332 -> One (r318)
  | 336 -> One (r319)
  | 338 -> One (r320)
  | 354 -> One (r321)
  | 351 -> One (r322)
  | 350 -> One (r323)
  | 348 -> One (r324)
  | 345 -> One (r325)
  | 344 -> One (r326)
  | 343 -> One (r327)
  | 342 -> One (r328)
  | 347 -> One (r329)
  | 353 -> One (r330)
  | 359 -> One (r331)
  | 369 -> One (r332)
  | 378 -> One (r333)
  | 377 -> One (r335)
  | 374 -> One (r336)
  | 373 -> One (r337)
  | 376 -> One (r338)
  | 386 -> One (r339)
  | 385 -> One (r340)
  | 384 -> One (r341)
  | 395 -> One (r342)
  | 393 -> One (r344)
  | 392 -> One (r345)
  | 398 -> One (r346)
  | 397 -> One (r347)
  | 1772 -> One (r348)
  | 1771 -> One (r349)
  | 1770 -> One (r350)
  | 1769 -> One (r351)
  | 1768 -> One (r352)
  | 1767 -> One (r353)
  | 408 -> One (r354)
  | 1766 -> One (r355)
  | 512 -> One (r356)
  | 1598 -> One (r358)
  | 1595 -> One (r360)
  | 1594 -> One (r361)
  | 1593 -> One (r362)
  | 509 -> One (r363)
  | 506 -> One (r364)
  | 414 -> One (r365)
  | 495 -> One (r366)
  | 494 -> One (r368)
  | 493 -> One (r369)
  | 416 -> One (r370)
  | 500 -> One (r372)
  | 422 -> One (r373)
  | 419 -> One (r374)
  | 418 -> One (r376)
  | 417 -> One (r377)
  | 421 -> One (r378)
  | 499 -> One (r379)
  | 435 | 835 -> One (r381)
  | 436 -> One (r383)
  | 426 -> One (r384)
  | 425 -> One (r385)
  | 427 -> One (r386)
  | 429 -> One (r387)
  | 441 -> One (r389)
  | 440 -> One (r391)
  | 492 -> One (r392)
  | 491 -> One (r393)
  | 444 -> One (r394)
  | 446 -> One (r395)
  | 486 -> One (r396)
  | 449 -> One (r397)
  | 448 -> One (r398)
  | 454 -> One (r399)
  | 456 -> One (r400)
  | 459 -> One (r401)
  | 485 -> One (r402)
  | 464 -> One (r403)
  | 468 -> One (r405)
  | 467 -> One (r406)
  | 466 -> One (r407)
  | 470 -> One (r408)
  | 473 -> One (r409)
  | 472 -> One (r410)
  | 477 -> One (r411)
  | 480 -> One (r412)
  | 479 -> One (r413)
  | 478 | 548 | 1286 -> One (r414)
  | 482 -> One (r415)
  | 484 -> One (r416)
  | 488 -> One (r417)
  | 487 -> One (r418)
  | 490 -> One (r419)
  | 504 -> One (r420)
  | 508 -> One (r421)
  | 511 -> One (r422)
  | 514 -> One (r423)
  | 530 -> One (r425)
  | 528 -> One (r426)
  | 527 -> One (r427)
  | 526 -> One (r428)
  | 525 -> One (r429)
  | 524 -> One (r430)
  | 523 -> One (r431)
  | 522 -> One (r432)
  | 521 -> One (r433)
  | 520 -> One (r434)
  | 1763 -> One (r435)
  | 554 -> One (r436)
  | 833 -> One (r438)
  | 1764 -> One (r440)
  | 534 -> One (r441)
  | 533 -> One (r442)
  | 532 -> One (r443)
  | 553 -> One (r444)
  | 539 -> One (r445)
  | 538 -> One (r446)
  | 537 -> One (r447)
  | 546 -> One (r448)
  | 545 -> One (r449)
  | 544 -> One (r450)
  | 543 -> One (r451)
  | 552 -> One (r452)
  | 551 -> One (r453)
  | 550 -> One (r454)
  | 1747 -> One (r455)
  | 1746 -> One (r456)
  | 1745 -> One (r457)
  | 1744 -> One (r458)
  | 1743 -> One (r459)
  | 556 -> One (r460)
  | 1468 -> One (r461)
  | 1467 -> One (r462)
  | 1466 -> One (r463)
  | 1465 -> One (r464)
  | 1464 -> One (r465)
  | 1463 -> One (r466)
  | 1742 -> One (r467)
  | 639 -> One (r468)
  | 638 -> One (r469)
  | 559 -> One (r470)
  | 558 -> One (r471)
  | 626 -> One (r472)
  | 624 -> One (r473)
  | 623 -> One (r474)
  | 561 -> One (r475)
  | 563 -> One (r476)
  | 622 -> One (r477)
  | 621 -> One (r478)
  | 565 -> One (r479)
  | 620 -> One (r480)
  | 619 -> One (r481)
  | 574 -> One (r482)
  | 572 -> One (r483)
  | 571 -> One (r484)
  | 568 -> One (r485)
  | 602 -> One (r486)
  | 601 -> One (r488)
  | 595 -> One (r490)
  | 594 -> One (r491)
  | 593 -> One (r492)
  | 592 -> One (r493)
  | 591 -> One (r494)
  | 614 -> One (r496)
  | 615 -> One (r498)
  | 582 -> One (r499)
  | 581 -> One (r500)
  | 578 -> One (r501)
  | 577 -> One (r502)
  | 585 -> One (r503)
  | 584 -> One (r504)
  | 589 -> One (r505)
  | 588 -> One (r506)
  | 587 -> One (r507)
  | 600 -> One (r508)
  | 605 -> One (r510)
  | 607 -> One (r511)
  | 610 -> One (r512)
  | 609 -> One (r513)
  | 611 | 2053 -> One (r514)
  | 613 -> One (r515)
  | 617 -> One (r516)
  | 628 -> One (r517)
  | 633 -> One (r518)
  | 632 -> One (r519)
  | 1736 -> One (r520)
  | 1517 | 1606 | 1637 | 1644 | 1733 | 1739 | 1803 -> One (r521)
  | 1732 -> One (r523)
  | 1731 -> One (r524)
  | 1728 -> One (r525)
  | 1725 -> One (r526)
  | 643 -> One (r527)
  | 1724 -> One (r528)
  | 1657 -> One (r529)
  | 1656 -> One (r530)
  | 1654 -> One (r531)
  | 1660 -> One (r533)
  | 1723 -> One (r535)
  | 1722 -> One (r536)
  | 1721 -> One (r537)
  | 1720 -> One (r538)
  | 1719 -> One (r539)
  | 1718 -> One (r540)
  | 651 -> One (r541)
  | 650 -> One (r542)
  | 1715 -> One (r543)
  | 654 -> One (r544)
  | 653 -> One (r545)
  | 1712 -> One (r546)
  | 1711 -> One (r547)
  | 1710 -> One (r548)
  | 657 -> One (r549)
  | 656 -> One (r550)
  | 1706 -> One (r551)
  | 660 -> One (r552)
  | 659 -> One (r553)
  | 1705 -> One (r554)
  | 1701 -> One (r555)
  | 1700 -> One (r556)
  | 1699 -> One (r557)
  | 828 -> One (r558)
  | 1684 -> One (r560)
  | 671 -> One (r561)
  | 1698 -> One (r563)
  | 1697 -> One (r564)
  | 666 -> One (r565)
  | 665 -> One (r566)
  | 1284 -> One (r567)
  | 1283 -> One (r568)
  | 1282 -> One (r569)
  | 1696 -> One (r570)
  | 670 -> One (r571)
  | 669 -> One (r572)
  | 1676 -> One (r573)
  | 1675 -> One (r574)
  | 1674 -> One (r575)
  | 1673 -> One (r576)
  | 676 -> One (r577)
  | 675 -> One (r578)
  | 674 -> One (r579)
  | 673 -> One (r580)
  | 1623 -> One (r581)
  | 1672 -> One (r583)
  | 1671 -> One (r584)
  | 1670 -> One (r585)
  | 1669 -> One (r586)
  | 1668 -> One (r587)
  | 1667 -> One (r588)
  | 681 -> One (r589)
  | 680 -> One (r590)
  | 679 -> One (r591)
  | 678 -> One (r592)
  | 685 -> One (r593)
  | 690 -> One (r594)
  | 689 -> One (r595)
  | 688 | 1664 -> One (r596)
  | 1663 -> One (r597)
  | 699 -> One (r598)
  | 698 -> One (r599)
  | 697 -> One (r600)
  | 696 -> One (r601)
  | 695 -> One (r602)
  | 694 -> One (r603)
  | 1589 -> One (r604)
  | 706 -> One (r605)
  | 705 -> One (r606)
  | 710 -> One (r607)
  | 709 -> One (r608)
  | 708 -> One (r609)
  | 712 -> One (r610)
  | 1530 | 1582 -> One (r611)
  | 1529 | 1581 -> One (r612)
  | 714 | 1528 -> One (r613)
  | 713 | 1527 -> One (r614)
  | 1580 -> One (r615)
  | 728 -> One (r616)
  | 723 -> One (r617)
  | 722 | 812 | 1786 -> One (r618)
  | 727 -> One (r620)
  | 726 -> One (r621)
  | 719 -> One (r622)
  | 721 -> One (r623)
  | 725 -> One (r624)
  | 730 -> One (r625)
  | 732 -> One (r626)
  | 734 -> One (r627)
  | 738 | 1546 -> One (r628)
  | 737 | 1545 -> One (r629)
  | 736 | 1544 -> One (r630)
  | 735 | 1543 -> One (r631)
  | 1505 -> One (r632)
  | 749 -> One (r633)
  | 748 -> One (r634)
  | 753 -> One (r635)
  | 752 -> One (r636)
  | 756 -> One (r637)
  | 758 -> One (r638)
  | 763 -> One (r639)
  | 767 -> One (r640)
  | 766 -> One (r641)
  | 770 -> One (r642)
  | 772 -> One (r643)
  | 774 -> One (r644)
  | 776 -> One (r645)
  | 778 -> One (r646)
  | 780 -> One (r647)
  | 782 -> One (r648)
  | 784 -> One (r649)
  | 786 -> One (r650)
  | 788 -> One (r651)
  | 790 -> One (r652)
  | 792 -> One (r653)
  | 794 -> One (r654)
  | 796 -> One (r655)
  | 798 -> One (r656)
  | 800 -> One (r657)
  | 802 -> One (r658)
  | 804 -> One (r659)
  | 806 -> One (r660)
  | 808 -> One (r661)
  | 1504 -> One (r662)
  | 857 -> One (r663)
  | 810 -> One (r664)
  | 818 -> One (r665)
  | 817 -> One (r666)
  | 816 -> One (r667)
  | 815 -> One (r668)
  | 814 -> One (r669)
  | 823 -> One (r670)
  | 822 -> One (r671)
  | 821 -> One (r672)
  | 820 -> One (r673)
  | 826 -> One (r674)
  | 825 -> One (r675)
  | 834 -> One (r676)
  | 832 -> One (r677)
  | 831 -> One (r678)
  | 839 -> One (r679)
  | 838 -> One (r680)
  | 837 -> One (r681)
  | 842 -> One (r682)
  | 841 -> One (r683)
  | 844 -> One (r684)
  | 846 -> One (r685)
  | 848 -> One (r686)
  | 850 -> One (r687)
  | 855 -> One (r688)
  | 1503 -> One (r689)
  | 1502 -> One (r690)
  | 859 -> One (r691)
  | 861 -> One (r692)
  | 863 -> One (r693)
  | 880 -> One (r694)
  | 879 -> One (r695)
  | 898 -> One (r697)
  | 897 -> One (r698)
  | 896 -> One (r699)
  | 876 -> One (r700)
  | 875 -> One (r701)
  | 874 -> One (r702)
  | 871 -> One (r703)
  | 868 -> One (r704)
  | 867 -> One (r705)
  | 866 -> One (r706)
  | 865 -> One (r707)
  | 870 -> One (r708)
  | 873 -> One (r709)
  | 895 -> One (r710)
  | 886 -> One (r711)
  | 885 -> One (r712)
  | 878 -> One (r713)
  | 884 -> One (r714)
  | 883 -> One (r715)
  | 882 -> One (r716)
  | 892 -> One (r717)
  | 891 -> One (r718)
  | 890 -> One (r719)
  | 889 -> One (r720)
  | 888 -> One (r721)
  | 894 -> One (r722)
  | 1501 -> One (r723)
  | 1500 -> One (r724)
  | 900 -> One (r725)
  | 1499 -> One (r726)
  | 1498 -> One (r727)
  | 902 -> One (r728)
  | 915 -> One (r729)
  | 918 -> One (r731)
  | 917 -> One (r732)
  | 914 -> One (r733)
  | 913 -> One (r734)
  | 909 -> One (r735)
  | 908 -> One (r736)
  | 907 -> One (r737)
  | 906 -> One (r738)
  | 912 -> One (r739)
  | 911 -> One (r740)
  | 970 -> One (r742)
  | 969 -> One (r743)
  | 968 -> One (r744)
  | 963 -> One (r745)
  | 984 -> One (r749)
  | 983 -> One (r750)
  | 982 -> One (r751)
  | 1105 -> One (r752)
  | 1104 -> One (r753)
  | 1103 -> One (r754)
  | 1102 -> One (r755)
  | 962 -> One (r756)
  | 961 -> One (r758)
  | 931 -> One (r759)
  | 946 -> One (r760)
  | 951 -> One (r768)
  | 948 -> One (r770)
  | 947 -> One (r771)
  | 945 -> One (r772)
  | 944 -> One (r773)
  | 943 -> One (r774)
  | 942 -> One (r775)
  | 938 -> One (r776)
  | 937 -> One (r777)
  | 941 -> One (r778)
  | 940 -> One (r779)
  | 953 -> One (r780)
  | 960 -> One (r781)
  | 955 -> One (r782)
  | 957 -> One (r783)
  | 959 -> One (r784)
  | 967 -> One (r785)
  | 981 -> One (r786)
  | 977 -> One (r787)
  | 973 -> One (r788)
  | 976 -> One (r789)
  | 975 -> One (r790)
  | 980 -> One (r791)
  | 979 -> One (r792)
  | 1272 -> One (r793)
  | 1038 -> One (r794)
  | 1053 -> One (r796)
  | 1052 -> One (r797)
  | 1051 -> One (r798)
  | 1050 -> One (r799)
  | 1049 -> One (r800)
  | 1036 -> One (r804)
  | 1035 -> One (r805)
  | 1034 -> One (r806)
  | 1032 -> One (r807)
  | 1031 -> One (r808)
  | 1008 -> One (r810)
  | 1007 -> One (r811)
  | 1006 -> One (r812)
  | 997 -> One (r813)
  | 996 -> One (r814)
  | 1002 -> One (r815)
  | 1001 -> One (r816)
  | 1000 | 1924 -> One (r817)
  | 1004 | 1923 -> One (r818)
  | 1025 -> One (r819)
  | 1017 -> One (r820)
  | 1016 -> One (r821)
  | 1015 -> One (r822)
  | 1024 -> One (r823)
  | 1023 -> One (r824)
  | 1045 -> One (r825)
  | 1044 -> One (r826)
  | 1043 -> One (r827)
  | 1271 -> One (r828)
  | 1064 -> One (r829)
  | 1063 -> One (r830)
  | 1062 -> One (r831)
  | 1061 -> One (r832)
  | 1060 -> One (r833)
  | 1059 -> One (r834)
  | 1058 -> One (r835)
  | 1057 -> One (r836)
  | 1097 -> One (r837)
  | 1096 -> One (r838)
  | 1099 -> One (r840)
  | 1098 -> One (r841)
  | 1092 -> One (r842)
  | 1074 -> One (r843)
  | 1073 -> One (r844)
  | 1072 -> One (r845)
  | 1071 -> One (r846)
  | 1070 -> One (r847)
  | 1078 -> One (r851)
  | 1077 -> One (r852)
  | 1091 -> One (r853)
  | 1083 -> One (r854)
  | 1082 -> One (r855)
  | 1081 -> One (r856)
  | 1080 -> One (r857)
  | 1090 -> One (r858)
  | 1089 -> One (r859)
  | 1088 -> One (r860)
  | 1087 -> One (r861)
  | 1086 -> One (r862)
  | 1085 -> One (r863)
  | 1095 -> One (r866)
  | 1094 -> One (r867)
  | 1101 -> One (r868)
  | 1161 | 1215 -> One (r870)
  | 1217 -> One (r872)
  | 1231 -> One (r874)
  | 1221 -> One (r875)
  | 1220 -> One (r876)
  | 1202 -> One (r877)
  | 1201 -> One (r878)
  | 1200 -> One (r879)
  | 1199 -> One (r880)
  | 1198 -> One (r881)
  | 1197 -> One (r882)
  | 1196 -> One (r883)
  | 1186 -> One (r884)
  | 1185 -> One (r885)
  | 1117 -> One (r886)
  | 1116 -> One (r887)
  | 1115 -> One (r888)
  | 1111 -> One (r889)
  | 1109 -> One (r890)
  | 1108 -> One (r891)
  | 1114 -> One (r892)
  | 1113 -> One (r893)
  | 1179 -> One (r894)
  | 1178 -> One (r895)
  | 1123 -> One (r896)
  | 1119 -> One (r897)
  | 1122 -> One (r898)
  | 1121 -> One (r899)
  | 1134 -> One (r900)
  | 1133 -> One (r901)
  | 1132 -> One (r902)
  | 1131 -> One (r903)
  | 1130 -> One (r904)
  | 1125 -> One (r905)
  | 1145 -> One (r906)
  | 1144 -> One (r907)
  | 1143 -> One (r908)
  | 1142 -> One (r909)
  | 1141 -> One (r910)
  | 1136 -> One (r911)
  | 1170 -> One (r912)
  | 1169 -> One (r913)
  | 1147 -> One (r914)
  | 1168 -> One (r915)
  | 1167 -> One (r916)
  | 1166 -> One (r917)
  | 1165 -> One (r918)
  | 1149 -> One (r919)
  | 1163 -> One (r920)
  | 1153 -> One (r921)
  | 1152 -> One (r922)
  | 1151 -> One (r923)
  | 1160 | 1208 -> One (r924)
  | 1157 -> One (r926)
  | 1156 -> One (r927)
  | 1155 -> One (r928)
  | 1154 | 1207 -> One (r929)
  | 1159 -> One (r930)
  | 1175 -> One (r931)
  | 1174 -> One (r932)
  | 1173 -> One (r933)
  | 1177 -> One (r935)
  | 1176 -> One (r936)
  | 1172 -> One (r937)
  | 1181 -> One (r938)
  | 1184 -> One (r939)
  | 1195 -> One (r940)
  | 1194 -> One (r941)
  | 1193 -> One (r942)
  | 1192 -> One (r943)
  | 1191 -> One (r944)
  | 1190 -> One (r945)
  | 1189 -> One (r946)
  | 1188 -> One (r947)
  | 1219 -> One (r948)
  | 1206 -> One (r949)
  | 1205 -> One (r950)
  | 1204 -> One (r951)
  | 1218 -> One (r952)
  | 1210 -> One (r953)
  | 1216 -> One (r954)
  | 1213 -> One (r955)
  | 1212 -> One (r956)
  | 1230 -> One (r957)
  | 1229 -> One (r958)
  | 1228 -> One (r959)
  | 1227 -> One (r960)
  | 1226 -> One (r961)
  | 1225 -> One (r962)
  | 1224 -> One (r963)
  | 1223 -> One (r964)
  | 1240 -> One (r965)
  | 1242 -> One (r966)
  | 1247 -> One (r967)
  | 1246 -> One (r968)
  | 1245 -> One (r969)
  | 1244 -> One (r970)
  | 1257 -> One (r971)
  | 1256 -> One (r972)
  | 1255 -> One (r973)
  | 1254 -> One (r974)
  | 1253 -> One (r975)
  | 1252 -> One (r976)
  | 1251 -> One (r977)
  | 1250 -> One (r978)
  | 1268 -> One (r979)
  | 1267 -> One (r980)
  | 1266 -> One (r981)
  | 1265 -> One (r982)
  | 1264 -> One (r983)
  | 1263 -> One (r984)
  | 1262 -> One (r985)
  | 1261 -> One (r986)
  | 1260 -> One (r987)
  | 1394 -> One (r988)
  | 1443 -> One (r990)
  | 1291 -> One (r991)
  | 1460 -> One (r993)
  | 1451 -> One (r994)
  | 1450 -> One (r995)
  | 1280 -> One (r996)
  | 1279 -> One (r997)
  | 1278 -> One (r998)
  | 1277 -> One (r999)
  | 1276 -> One (r1000)
  | 1290 -> One (r1001)
  | 1289 -> One (r1002)
  | 1288 -> One (r1003)
  | 1437 -> One (r1004)
  | 1436 -> One (r1005)
  | 1294 -> One (r1006)
  | 1293 -> One (r1007)
  | 1320 -> One (r1008)
  | 1319 -> One (r1009)
  | 1318 -> One (r1010)
  | 1317 -> One (r1011)
  | 1308 -> One (r1012)
  | 1307 -> One (r1014)
  | 1306 -> One (r1015)
  | 1302 -> One (r1016)
  | 1301 -> One (r1017)
  | 1300 -> One (r1018)
  | 1299 -> One (r1019)
  | 1297 -> One (r1020)
  | 1305 -> One (r1021)
  | 1304 -> One (r1022)
  | 1316 -> One (r1023)
  | 1315 -> One (r1024)
  | 1314 -> One (r1025)
  | 1323 -> One (r1026)
  | 1322 -> One (r1027)
  | 1363 -> One (r1028)
  | 1352 -> One (r1029)
  | 1351 -> One (r1030)
  | 1342 -> One (r1031)
  | 1341 -> One (r1033)
  | 1340 -> One (r1034)
  | 1339 -> One (r1035)
  | 1328 -> One (r1036)
  | 1327 -> One (r1037)
  | 1326 -> One (r1038)
  | 1338 -> One (r1039)
  | 1337 -> One (r1040)
  | 1336 -> One (r1041)
  | 1335 -> One (r1042)
  | 1334 -> One (r1043)
  | 1333 -> One (r1044)
  | 1332 -> One (r1045)
  | 1331 -> One (r1046)
  | 1350 -> One (r1047)
  | 1349 -> One (r1048)
  | 1348 -> One (r1049)
  | 1362 -> One (r1050)
  | 1361 -> One (r1051)
  | 1360 -> One (r1052)
  | 1359 -> One (r1053)
  | 1358 -> One (r1054)
  | 1357 -> One (r1055)
  | 1356 -> One (r1056)
  | 1355 -> One (r1057)
  | 1367 -> One (r1058)
  | 1366 -> One (r1059)
  | 1365 -> One (r1060)
  | 1431 -> One (r1061)
  | 1430 -> One (r1062)
  | 1429 -> One (r1063)
  | 1428 -> One (r1064)
  | 1427 -> One (r1065)
  | 1426 -> One (r1066)
  | 1423 -> One (r1067)
  | 1370 -> One (r1068)
  | 1419 -> One (r1069)
  | 1418 -> One (r1070)
  | 1413 -> One (r1071)
  | 1412 -> One (r1072)
  | 1411 -> One (r1073)
  | 1410 -> One (r1074)
  | 1379 -> One (r1075)
  | 1378 -> One (r1076)
  | 1377 -> One (r1077)
  | 1376 -> One (r1078)
  | 1375 -> One (r1079)
  | 1374 -> One (r1080)
  | 1409 -> One (r1081)
  | 1383 -> One (r1082)
  | 1382 -> One (r1083)
  | 1381 -> One (r1084)
  | 1387 -> One (r1085)
  | 1386 -> One (r1086)
  | 1385 -> One (r1087)
  | 1406 -> One (r1088)
  | 1391 -> One (r1089)
  | 1390 -> One (r1090)
  | 1408 -> One (r1092)
  | 1389 -> One (r1093)
  | 1403 -> One (r1094)
  | 1393 -> One (r1095)
  | 1397 -> One (r1096)
  | 1417 -> One (r1097)
  | 1416 -> One (r1098)
  | 1415 -> One (r1099)
  | 1422 -> One (r1100)
  | 1421 -> One (r1101)
  | 1425 -> One (r1102)
  | 1435 -> One (r1103)
  | 1434 -> One (r1104)
  | 1433 -> One (r1105)
  | 1439 -> One (r1106)
  | 1442 -> One (r1107)
  | 1447 -> One (r1108)
  | 1446 -> One (r1109)
  | 1445 -> One (r1110)
  | 1449 -> One (r1111)
  | 1459 -> One (r1112)
  | 1458 -> One (r1113)
  | 1457 -> One (r1114)
  | 1456 -> One (r1115)
  | 1455 -> One (r1116)
  | 1454 -> One (r1117)
  | 1453 -> One (r1118)
  | 1476 -> One (r1119)
  | 1480 -> One (r1120)
  | 1485 -> One (r1121)
  | 1484 -> One (r1122)
  | 1483 -> One (r1123)
  | 1482 -> One (r1124)
  | 1487 -> One (r1125)
  | 1493 -> One (r1126)
  | 1492 -> One (r1127)
  | 1508 | 1551 -> One (r1128)
  | 1507 | 1550 -> One (r1129)
  | 1506 | 1549 -> One (r1130)
  | 1511 | 1556 -> One (r1131)
  | 1510 | 1555 -> One (r1132)
  | 1509 | 1554 -> One (r1133)
  | 1516 | 1563 -> One (r1134)
  | 1515 | 1562 -> One (r1135)
  | 1514 | 1561 -> One (r1136)
  | 1513 | 1560 -> One (r1137)
  | 1522 | 1568 -> One (r1138)
  | 1521 | 1567 -> One (r1139)
  | 1520 | 1566 -> One (r1140)
  | 1525 | 1573 -> One (r1141)
  | 1524 | 1572 -> One (r1142)
  | 1523 | 1571 -> One (r1143)
  | 1532 -> One (r1144)
  | 1535 | 1585 -> One (r1145)
  | 1534 | 1584 -> One (r1146)
  | 1533 | 1583 -> One (r1147)
  | 1537 -> One (r1148)
  | 1540 | 1588 -> One (r1149)
  | 1539 | 1587 -> One (r1150)
  | 1538 | 1586 -> One (r1151)
  | 1542 -> One (r1152)
  | 1548 -> One (r1153)
  | 1553 -> One (r1154)
  | 1558 -> One (r1155)
  | 1565 -> One (r1156)
  | 1570 -> One (r1157)
  | 1575 -> One (r1158)
  | 1578 -> One (r1159)
  | 1592 -> One (r1160)
  | 1591 -> One (r1161)
  | 1597 -> One (r1162)
  | 1601 -> One (r1163)
  | 1603 -> One (r1164)
  | 1605 -> One (r1165)
  | 1608 -> One (r1166)
  | 1619 -> One (r1167)
  | 1618 -> One (r1168)
  | 1626 -> One (r1170)
  | 1617 -> One (r1171)
  | 1612 -> One (r1172)
  | 1628 -> One (r1174)
  | 1610 -> One (r1176)
  | 1627 -> One (r1177)
  | 1616 -> One (r1178)
  | 1615 -> One (r1179)
  | 1614 -> One (r1180)
  | 1625 -> One (r1181)
  | 1624 -> One (r1182)
  | 1621 -> One (r1183)
  | 1630 -> One (r1184)
  | 1634 -> One (r1185)
  | 1636 -> One (r1186)
  | 1639 -> One (r1187)
  | 1641 -> One (r1188)
  | 1643 -> One (r1189)
  | 1646 -> One (r1190)
  | 1649 -> One (r1192)
  | 1648 -> One (r1193)
  | 1662 -> One (r1194)
  | 1661 -> One (r1195)
  | 1653 -> One (r1196)
  | 1652 -> One (r1197)
  | 1683 -> One (r1198)
  | 1682 -> One (r1199)
  | 1681 -> One (r1200)
  | 1680 -> One (r1201)
  | 1679 -> One (r1202)
  | 1678 -> One (r1203)
  | 1695 -> One (r1204)
  | 1688 -> One (r1205)
  | 1687 -> One (r1206)
  | 1692 -> One (r1207)
  | 1691 -> One (r1208)
  | 1690 -> One (r1209)
  | 1694 -> One (r1210)
  | 1708 -> One (r1211)
  | 1714 -> One (r1212)
  | 1717 -> One (r1213)
  | 1730 -> One (r1214)
  | 1735 -> One (r1215)
  | 1738 -> One (r1216)
  | 1741 -> One (r1217)
  | 1754 -> One (r1218)
  | 1753 -> One (r1219)
  | 1752 -> One (r1220)
  | 1751 -> One (r1221)
  | 1750 -> One (r1222)
  | 1749 -> One (r1223)
  | 1762 -> One (r1224)
  | 1761 -> One (r1225)
  | 1760 -> One (r1226)
  | 1759 -> One (r1227)
  | 1758 -> One (r1228)
  | 1757 -> One (r1229)
  | 1756 -> One (r1230)
  | 1782 -> One (r1231)
  | 1783 -> One (r1233)
  | 1776 -> One (r1234)
  | 1775 -> One (r1235)
  | 1774 -> One (r1236)
  | 1781 -> One (r1237)
  | 1780 -> One (r1238)
  | 1785 -> One (r1239)
  | 1791 -> One (r1240)
  | 1790 -> One (r1241)
  | 1789 -> One (r1242)
  | 1788 -> One (r1243)
  | 1794 -> One (r1244)
  | 1793 -> One (r1245)
  | 1802 -> One (r1246)
  | 1805 -> One (r1247)
  | 1811 -> One (r1248)
  | 1810 -> One (r1249)
  | 1809 -> One (r1250)
  | 1808 -> One (r1251)
  | 1814 -> One (r1252)
  | 1813 -> One (r1253)
  | 1818 -> One (r1254)
  | 1829 -> One (r1255)
  | 1828 -> One (r1256)
  | 1832 -> One (r1257)
  | 1831 -> One (r1258)
  | 1835 -> One (r1259)
  | 1834 -> One (r1260)
  | 1844 -> One (r1261)
  | 1843 -> One (r1262)
  | 1851 -> One (r1263)
  | 1850 -> One (r1264)
  | 1849 -> One (r1265)
  | 1853 -> One (r1266)
  | 1861 -> One (r1267)
  | 1869 -> One (r1268)
  | 1866 -> One (r1269)
  | 1865 -> One (r1270)
  | 1864 -> One (r1271)
  | 1863 -> One (r1272)
  | 1868 -> One (r1273)
  | 1886 -> One (r1274)
  | 1883 -> One (r1275)
  | 1882 -> One (r1276)
  | 1880 -> One (r1277)
  | 1877 -> One (r1278)
  | 1876 -> One (r1279)
  | 1875 -> One (r1280)
  | 1874 -> One (r1281)
  | 1879 -> One (r1282)
  | 1885 -> One (r1283)
  | 1891 -> One (r1284)
  | 1901 -> One (r1285)
  | 1898 -> One (r1286)
  | 1897 -> One (r1287)
  | 1896 -> One (r1288)
  | 1895 -> One (r1289)
  | 1900 -> One (r1290)
  | 1910 -> One (r1291)
  | 1915 -> One (r1292)
  | 1914 -> One (r1293)
  | 1927 -> One (r1294)
  | 1926 -> One (r1295)
  | 1939 -> One (r1296)
  | 1938 -> One (r1297)
  | 1962 -> One (r1298)
  | 1961 -> One (r1299)
  | 1971 -> One (r1300)
  | 1973 -> One (r1301)
  | 1975 -> One (r1302)
  | 1988 -> One (r1303)
  | 1992 -> One (r1304)
  | 1997 -> One (r1305)
  | 2004 -> One (r1306)
  | 2003 -> One (r1307)
  | 2002 -> One (r1308)
  | 2001 -> One (r1309)
  | 2011 -> One (r1310)
  | 2015 -> One (r1311)
  | 2019 -> One (r1312)
  | 2022 -> One (r1313)
  | 2027 -> One (r1314)
  | 2031 -> One (r1315)
  | 2035 -> One (r1316)
  | 2039 -> One (r1317)
  | 2043 -> One (r1318)
  | 2046 -> One (r1319)
  | 2050 -> One (r1320)
  | 2056 -> One (r1321)
  | 2066 -> One (r1322)
  | 2068 -> One (r1323)
  | 2071 -> One (r1324)
  | 2070 -> One (r1325)
  | 2073 -> One (r1326)
  | 2083 -> One (r1327)
  | 2079 -> One (r1328)
  | 2078 -> One (r1329)
  | 2082 -> One (r1330)
  | 2081 -> One (r1331)
  | 2088 -> One (r1332)
  | 2087 -> One (r1333)
  | 2086 -> One (r1334)
  | 2090 -> One (r1335)
  | 443 -> Select (function
    | -1 -> [R 114]
    | _ -> S (T T_DOT) :: r394)
  | 687 -> Select (function
    | -1 -> [R 114]
    | _ -> r597)
  | 173 -> Select (function
    | -1 -> r162
    | _ -> R 199 :: r154)
  | 920 -> Select (function
    | -1 -> r755
    | _ -> R 199 :: r748)
  | 987 -> Select (function
    | -1 -> r162
    | _ -> R 199 :: r803)
  | 1066 -> Select (function
    | -1 -> r707
    | _ -> R 199 :: r850)
  | 599 -> Select (function
    | -1 -> r285
    | _ -> [R 231])
  | 461 -> Select (function
    | -1 -> [R 727]
    | _ -> S (N N_pattern) :: r402)
  | 458 -> Select (function
    | -1 -> [R 728]
    | _ -> S (N N_pattern) :: r401)
  | 179 -> Select (function
    | -1 -> r174
    | _ -> R 873 :: r168)
  | 990 -> Select (function
    | -1 -> r174
    | _ -> R 873 :: r809)
  | 964 -> Select (function
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> S (T T_COLONCOLON) :: r410)
  | 89 -> Select (function
    | 256 | 410 | 702 | 810 | 1376 | 1415 | 1466 | 1596 -> r63
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> S (N N_pattern) :: r58)
  | 246 -> Select (function
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> Sub (r1) :: r240)
  | 413 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r365
    | _ -> Sub (r367) :: r369)
  | 641 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r365
    | _ -> Sub (r522) :: r524)
  | 555 -> Select (function
    | 62 | 172 | 213 | 859 | 900 | 902 -> r466
    | _ -> S (T T_OPEN) :: r460)
  | 162 -> Select (function
    | -1 | 289 | 292 | 330 | 345 | 351 | 357 | 1866 | 1877 | 1883 | 1889 | 1898 | 1908 -> S (T T_MODULE) :: r118
    | _ -> Sub (r119) :: r125)
  | 966 -> Select (function
    | -1 -> r514
    | _ -> S (T T_LPAREN) :: r785)
  | 273 -> Select (function
    | -1 -> r287
    | _ -> S (T T_DOT) :: r289)
  | 597 -> Select (function
    | -1 -> r287
    | _ -> S (T T_DOT) :: r509)
  | 207 -> Select (function
    | -1 | 289 | 292 | 330 | 345 | 351 | 357 | 1866 | 1877 | 1883 | 1889 | 1898 | 1908 -> r133
    | _ -> S (T T_COLON) :: r198)
  | 157 -> Select (function
    | 151 | 909 | 938 | 1143 | 1329 | 1349 | 1353 | 1849 -> r110
    | _ -> r108)
  | 154 -> Select (function
    | 151 | 909 | 938 | 1143 | 1329 | 1349 | 1353 | 1849 -> r111
    | _ -> r109)
  | 1941 -> Select (function
    | -1 -> r158
    | _ -> r133)
  | 198 -> Select (function
    | -1 -> r172
    | _ -> r133)
  | 1041 -> Select (function
    | -1 -> r158
    | _ -> r133)
  | 992 -> Select (function
    | -1 -> r172
    | _ -> r133)
  | 1940 -> Select (function
    | -1 -> r159
    | _ -> r152)
  | 175 -> Select (function
    | -1 -> r160
    | _ -> r153)
  | 174 -> Select (function
    | -1 -> r161
    | _ -> r154)
  | 1040 -> Select (function
    | -1 -> r159
    | _ -> r801)
  | 989 -> Select (function
    | -1 -> r160
    | _ -> r802)
  | 988 -> Select (function
    | -1 -> r161
    | _ -> r803)
  | 197 -> Select (function
    | -1 -> r173
    | _ -> r168)
  | 991 -> Select (function
    | -1 -> r173
    | _ -> r809)
  | 280 -> Select (function
    | -1 -> r286
    | _ -> r289)
  | 598 -> Select (function
    | -1 -> r286
    | _ -> r509)
  | 1069 -> Select (function
    | -1 -> r704
    | _ -> r848)
  | 1068 -> Select (function
    | -1 -> r705
    | _ -> r849)
  | 1067 -> Select (function
    | -1 -> r706
    | _ -> r850)
  | 928 -> Select (function
    | -1 -> r752
    | _ -> r746)
  | 922 -> Select (function
    | -1 -> r753
    | _ -> r747)
  | 921 -> Select (function
    | -1 -> r754
    | _ -> r748)
  | _ -> raise Not_found
