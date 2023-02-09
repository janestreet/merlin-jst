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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_gbl_ -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;1;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;2;1;1;1;2;1;2;3;1;1;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;2;1;1;1;2;3;1;2;3;1;1;1;1;1;2;1;2;3;1;4;1;2;1;2;3;1;2;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;3;4;3;4;5;1;2;1;1;3;2;3;2;1;2;3;4;4;1;2;1;2;3;4;5;4;2;1;3;2;1;2;3;4;3;2;3;4;5;6;7;8;9;8;8;2;3;2;3;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;5;5;2;3;4;5;4;4;3;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;3;4;5;3;4;5;3;4;2;1;2;3;4;1;1;1;1;2;1;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;1;1;1;2;3;1;2;3;4;5;3;4;5;6;3;4;5;1;2;1;2;3;4;1;2;3;4;5;5;1;2;6;7;8;9;3;4;5;6;7;8;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;1;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;2;1;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;1;2;3;4;3;4;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;1;1;2;3;1;1;2;3;4;1;1;2;8;9;10;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;7;8;9;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;2;3;4;5;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;2;3;4;2;3;4;5;6;7;1;1;2;3;1;1;2;1;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;3;4;5;1;2;1;1;1;1;2;3;3;4;1;1;1;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;4;1;2;3;4;5;5;3;2;1;2;3;4;5;4;5;1;1;6;7;3;3;1;5;3;4;4;5;4;1;2;3;4;5;5;3;3;5;3;4;5;3;1;2;3;1;1;2;3;4;5;1;4;5;1;2;3;3;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;3;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;5;2;2;5;6;3;4;5;2;1;2;3;4;1;2;1;2;3;1;5;1;2;3;4;5;6;7;8;3;4;5;3;5;6;3;2;2;2;3;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;1;2;3;4;5;6;1;7;1;2;3;2;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;7;8;9;10;11;10;10;4;5;6;7;6;6;3;4;5;6;5;5;3;4;5;6;7;8;9;8;8;2;2;3;4;5;6;7;8;7;7;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  let r0 = [R 628] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 141] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 310 :: r6 in
  let r8 = [R 731] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 42] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 203] in
  let r13 = [R 43] in
  let r14 = [R 543] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 44] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 156] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = R 310 :: r23 in
  let r25 = [R 695] in
  let r26 = [R 379] in
  let r27 = [R 137] in
  let r28 = Sub (r1) :: r27 in
  let r29 = R 310 :: r28 in
  let r30 = [R 348] in
  let r31 = Sub (r1) :: r30 in
  let r32 = S (T T_MINUSGREATER) :: r31 in
  let r33 = S (N N_pattern) :: r32 in
  let r34 = [R 587] in
  let r35 = Sub (r33) :: r34 in
  let r36 = [R 153] in
  let r37 = Sub (r35) :: r36 in
  let r38 = S (T T_WITH) :: r37 in
  let r39 = Sub (r1) :: r38 in
  let r40 = R 310 :: r39 in
  let r41 = [R 205] in
  let r42 = S (T T_UNDERSCORE) :: r25 in
  let r43 = [R 685] in
  let r44 = [R 680] in
  let r45 = S (T T_END) :: r44 in
  let r46 = R 327 :: r45 in
  let r47 = R 69 :: r46 in
  let r48 = R 310 :: r47 in
  let r49 = [R 67] in
  let r50 = S (T T_RPAREN) :: r49 in
  let r51 = [R 717] in
  let r52 = [R 656] in
  let r53 = [R 654] in
  let r54 = [R 111] in
  let r55 = [R 713] in
  let r56 = S (T T_RPAREN) :: r55 in
  let r57 = [R 478] in
  let r58 = S (T T_AMPERAMPER) :: r57 in
  let r59 = [R 883] in
  let r60 = S (T T_RPAREN) :: r59 in
  let r61 = Sub (r58) :: r60 in
  let r62 = [R 401] in
  let r63 = S (T T_UNDERSCORE) :: r62 in
  let r64 = [R 715] in
  let r65 = S (T T_RPAREN) :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = R 310 :: r66 in
  let r68 = [R 716] in
  let r69 = S (T T_RPAREN) :: r68 in
  let r70 = [R 367] in
  let r71 = [R 633] in
  let r72 = R 318 :: r71 in
  let r73 = [R 403] in
  let r74 = S (T T_END) :: r73 in
  let r75 = Sub (r72) :: r74 in
  let r76 = [R 884] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 25] in
  let r79 = S (T T_UNDERSCORE) :: r78 in
  let r80 = [R 857] in
  let r81 = Sub (r79) :: r80 in
  let r82 = [R 217] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 17] in
  let r85 = Sub (r83) :: r84 in
  let r86 = [R 131] in
  let r87 = Sub (r85) :: r86 in
  let r88 = [R 548] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 892] in
  let r91 = R 316 :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = S (T T_COLON) :: r92 in
  let r94 = Sub (r77) :: r93 in
  let r95 = R 310 :: r94 in
  let r96 = [R 452] in
  let r97 = S (T T_RPAREN) :: r96 in
  let r98 = R 239 :: r97 in
  let r99 = [R 240] in
  let r100 = [R 454] in
  let r101 = S (T T_RBRACKET) :: r100 in
  let r102 = [R 456] in
  let r103 = S (T T_RBRACE) :: r102 in
  let r104 = [R 235] in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = [R 24] in
  let r107 = Sub (r105) :: r106 in
  let r108 = [R 585] in
  let r109 = Sub (r105) :: r108 in
  let r110 = [R 501] in
  let r111 = S (T T_COLON) :: r110 in
  let r112 = [R 23] in
  let r113 = S (T T_RPAREN) :: r112 in
  let r114 = S (N N_module_type) :: r113 in
  let r115 = R 310 :: r114 in
  let r116 = R 202 :: r115 in
  let r117 = S (T T_QUOTE) :: r109 in
  let r118 = [R 815] in
  let r119 = Sub (r81) :: r118 in
  let r120 = S (T T_MINUSGREATER) :: r119 in
  let r121 = S (T T_RPAREN) :: r120 in
  let r122 = Sub (r87) :: r121 in
  let r123 = S (T T_DOT) :: r122 in
  let r124 = [R 405] in
  let r125 = S (N N_module_expr) :: r124 in
  let r126 = R 310 :: r125 in
  let r127 = S (T T_OF) :: r126 in
  let r128 = [R 391] in
  let r129 = S (T T_END) :: r128 in
  let r130 = S (N N_structure) :: r129 in
  let r131 = [R 365] in
  let r132 = S (T T_LIDENT) :: r131 in
  let r133 = [R 864] in
  let r134 = Sub (r132) :: r133 in
  let r135 = [R 112] in
  let r136 = S (T T_FALSE) :: r135 in
  let r137 = [R 116] in
  let r138 = Sub (r136) :: r137 in
  let r139 = [R 229] in
  let r140 = R 310 :: r139 in
  let r141 = R 222 :: r140 in
  let r142 = Sub (r138) :: r141 in
  let r143 = [R 568] in
  let r144 = Sub (r142) :: r143 in
  let r145 = [R 832] in
  let r146 = R 316 :: r145 in
  let r147 = Sub (r144) :: r146 in
  let r148 = R 554 :: r147 in
  let r149 = S (T T_PLUSEQ) :: r148 in
  let r150 = Sub (r134) :: r149 in
  let r151 = R 866 :: r150 in
  let r152 = R 310 :: r151 in
  let r153 = [R 232] in
  let r154 = R 316 :: r153 in
  let r155 = R 577 :: r154 in
  let r156 = R 862 :: r155 in
  let r157 = S (T T_LIDENT) :: r156 in
  let r158 = R 866 :: r157 in
  let r159 = R 310 :: r158 in
  let r160 = R 202 :: r159 in
  let r161 = [R 833] in
  let r162 = R 316 :: r161 in
  let r163 = Sub (r144) :: r162 in
  let r164 = R 554 :: r163 in
  let r165 = S (T T_PLUSEQ) :: r164 in
  let r166 = Sub (r134) :: r165 in
  let r167 = [R 233] in
  let r168 = R 316 :: r167 in
  let r169 = R 577 :: r168 in
  let r170 = R 862 :: r169 in
  let r171 = S (T T_LIDENT) :: r170 in
  let r172 = R 866 :: r171 in
  let r173 = [R 870] in
  let r174 = S (T T_UNDERSCORE) :: r173 in
  let r175 = [R 865] in
  let r176 = Sub (r174) :: r175 in
  let r177 = R 871 :: r176 in
  let r178 = [R 600] in
  let r179 = Sub (r177) :: r178 in
  let r180 = [R 868] in
  let r181 = S (T T_RPAREN) :: r180 in
  let r182 = [R 869] in
  let r183 = [R 601] in
  let r184 = [R 437] in
  let r185 = S (T T_DOTDOT) :: r184 in
  let r186 = [R 863] in
  let r187 = [R 438] in
  let r188 = [R 115] in
  let r189 = S (T T_RPAREN) :: r188 in
  let r190 = [R 821] in
  let r191 = Sub (r81) :: r190 in
  let r192 = S (T T_MINUSGREATER) :: r191 in
  let r193 = [R 809] in
  let r194 = Sub (r81) :: r193 in
  let r195 = S (T T_MINUSGREATER) :: r194 in
  let r196 = Sub (r81) :: r195 in
  let r197 = [R 30] in
  let r198 = [R 204] in
  let r199 = S (T T_RBRACKET) :: r198 in
  let r200 = Sub (r15) :: r199 in
  let r201 = [R 322] in
  let r202 = [R 445] in
  let r203 = R 316 :: r202 in
  let r204 = S (N N_module_expr) :: r203 in
  let r205 = R 310 :: r204 in
  let r206 = [R 446] in
  let r207 = R 316 :: r206 in
  let r208 = S (N N_module_expr) :: r207 in
  let r209 = R 310 :: r208 in
  let r210 = [R 503] in
  let r211 = S (T T_RPAREN) :: r210 in
  let r212 = [R 504] in
  let r213 = S (T T_RPAREN) :: r212 in
  let r214 = S (N N_expr) :: r213 in
  let r215 = [R 377] in
  let r216 = S (T T_LIDENT) :: r215 in
  let r217 = [R 66] in
  let r218 = Sub (r216) :: r217 in
  let r219 = [R 677] in
  let r220 = Sub (r218) :: r219 in
  let r221 = R 310 :: r220 in
  let r222 = [R 378] in
  let r223 = S (T T_LIDENT) :: r222 in
  let r224 = [R 380] in
  let r225 = [R 385] in
  let r226 = [R 311] in
  let r227 = [R 136] in
  let r228 = Sub (r35) :: r227 in
  let r229 = S (T T_WITH) :: r228 in
  let r230 = Sub (r1) :: r229 in
  let r231 = R 310 :: r230 in
  let r232 = [R 152] in
  let r233 = Sub (r35) :: r232 in
  let r234 = S (T T_WITH) :: r233 in
  let r235 = Sub (r1) :: r234 in
  let r236 = R 310 :: r235 in
  let r237 = [R 664] in
  let r238 = S (T T_RPAREN) :: r237 in
  let r239 = [R 700] in
  let r240 = [R 201] in
  let r241 = [R 189] in
  let r242 = [R 276] in
  let r243 = Sub (r77) :: r242 in
  let r244 = [R 345] in
  let r245 = R 316 :: r244 in
  let r246 = Sub (r243) :: r245 in
  let r247 = R 561 :: r246 in
  let r248 = R 310 :: r247 in
  let r249 = [R 342] in
  let r250 = Sub (r1) :: r249 in
  let r251 = S (T T_EQUAL) :: r250 in
  let r252 = [R 285] in
  let r253 = Sub (r251) :: r252 in
  let r254 = [R 267] in
  let r255 = [R 249] in
  let r256 = S (T T_LIDENT) :: r255 in
  let r257 = [R 265] in
  let r258 = S (T T_RPAREN) :: r257 in
  let r259 = [R 266] in
  let r260 = S (T T_RPAREN) :: r259 in
  let r261 = [R 250] in
  let r262 = [R 615] in
  let r263 = Sub (r87) :: r262 in
  let r264 = [R 596] in
  let r265 = Sub (r263) :: r264 in
  let r266 = [R 39] in
  let r267 = S (T T_RBRACKET) :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = [R 38] in
  let r270 = [R 37] in
  let r271 = S (T T_RBRACKET) :: r270 in
  let r272 = [R 426] in
  let r273 = Sub (r105) :: r272 in
  let r274 = S (T T_BACKQUOTE) :: r273 in
  let r275 = [R 845] in
  let r276 = R 310 :: r275 in
  let r277 = Sub (r274) :: r276 in
  let r278 = [R 34] in
  let r279 = S (T T_RBRACKET) :: r278 in
  let r280 = [R 95] in
  let r281 = Sub (r132) :: r280 in
  let r282 = [R 31] in
  let r283 = [R 368] in
  let r284 = S (T T_UIDENT) :: r283 in
  let r285 = S (T T_DOT) :: r284 in
  let r286 = [R 366] in
  let r287 = S (T T_LIDENT) :: r286 in
  let r288 = S (T T_UIDENT) :: r70 in
  let r289 = [R 383] in
  let r290 = Sub (r288) :: r289 in
  let r291 = [R 384] in
  let r292 = S (T T_RPAREN) :: r291 in
  let r293 = [R 35] in
  let r294 = S (T T_RBRACKET) :: r293 in
  let r295 = [R 817] in
  let r296 = [R 818] in
  let r297 = [R 822] in
  let r298 = [R 612] in
  let r299 = [R 32] in
  let r300 = [R 613] in
  let r301 = [R 801] in
  let r302 = Sub (r81) :: r301 in
  let r303 = S (T T_MINUSGREATER) :: r302 in
  let r304 = [R 28] in
  let r305 = Sub (r134) :: r304 in
  let r306 = [R 33] in
  let r307 = [R 608] in
  let r308 = [R 18] in
  let r309 = Sub (r105) :: r308 in
  let r310 = [R 799] in
  let r311 = Sub (r81) :: r310 in
  let r312 = S (T T_MINUSGREATER) :: r311 in
  let r313 = S (T T_RPAREN) :: r312 in
  let r314 = Sub (r87) :: r313 in
  let r315 = [R 586] in
  let r316 = [R 800] in
  let r317 = [R 22] in
  let r318 = [R 609] in
  let r319 = [R 805] in
  let r320 = Sub (r81) :: r319 in
  let r321 = S (T T_MINUSGREATER) :: r320 in
  let r322 = [R 803] in
  let r323 = Sub (r81) :: r322 in
  let r324 = S (T T_MINUSGREATER) :: r323 in
  let r325 = S (T T_RPAREN) :: r324 in
  let r326 = Sub (r87) :: r325 in
  let r327 = [R 804] in
  let r328 = [R 806] in
  let r329 = [R 802] in
  let r330 = [R 597] in
  let r331 = [R 590] in
  let r332 = Sub (r85) :: r331 in
  let r333 = [R 844] in
  let r334 = R 310 :: r333 in
  let r335 = Sub (r332) :: r334 in
  let r336 = [R 591] in
  let r337 = [R 36] in
  let r338 = S (T T_RBRACKET) :: r337 in
  let r339 = Sub (r265) :: r338 in
  let r340 = [R 583] in
  let r341 = Sub (r274) :: r340 in
  let r342 = [R 40] in
  let r343 = S (T T_RBRACKET) :: r342 in
  let r344 = [R 251] in
  let r345 = Sub (r87) :: r344 in
  let r346 = [R 261] in
  let r347 = [R 259] in
  let r348 = S (T T_RPAREN) :: r347 in
  let r349 = R 496 :: r348 in
  let r350 = [R 260] in
  let r351 = S (T T_RPAREN) :: r350 in
  let r352 = R 496 :: r351 in
  let r353 = [R 497] in
  let r354 = [R 295] in
  let r355 = Sub (r77) :: r354 in
  let r356 = [R 298] in
  let r357 = Sub (r355) :: r356 in
  let r358 = [R 187] in
  let r359 = Sub (r1) :: r358 in
  let r360 = S (T T_IN) :: r359 in
  let r361 = [R 661] in
  let r362 = [R 110] in
  let r363 = [R 622] in
  let r364 = S (N N_pattern) :: r363 in
  let r365 = [R 659] in
  let r366 = S (T T_RBRACKET) :: r365 in
  let r367 = [R 252] in
  let r368 = Sub (r216) :: r367 in
  let r369 = [R 336] in
  let r370 = R 494 :: r369 in
  let r371 = R 488 :: r370 in
  let r372 = Sub (r368) :: r371 in
  let r373 = [R 658] in
  let r374 = S (T T_RBRACE) :: r373 in
  let r375 = [R 489] in
  let r376 = [R 495] in
  let r377 = S (T T_UNDERSCORE) :: r51 in
  let r378 = [R 712] in
  let r379 = Sub (r377) :: r378 in
  let r380 = [R 534] in
  let r381 = Sub (r379) :: r380 in
  let r382 = R 310 :: r381 in
  let r383 = [R 106] in
  let r384 = [R 722] in
  let r385 = S (T T_INT) :: r383 in
  let r386 = [R 653] in
  let r387 = Sub (r385) :: r386 in
  let r388 = [R 719] in
  let r389 = [R 724] in
  let r390 = S (T T_RBRACKET) :: r389 in
  let r391 = S (T T_LBRACKET) :: r390 in
  let r392 = [R 725] in
  let r393 = [R 525] in
  let r394 = S (N N_pattern) :: r393 in
  let r395 = R 310 :: r394 in
  let r396 = [R 526] in
  let r397 = [R 519] in
  let r398 = [R 533] in
  let r399 = [R 531] in
  let r400 = [R 427] in
  let r401 = S (T T_LIDENT) :: r400 in
  let r402 = [R 532] in
  let r403 = Sub (r379) :: r402 in
  let r404 = S (T T_RPAREN) :: r403 in
  let r405 = [R 120] in
  let r406 = [R 119] in
  let r407 = S (T T_RPAREN) :: r406 in
  let r408 = [R 527] in
  let r409 = [R 727] in
  let r410 = S (T T_RPAREN) :: r409 in
  let r411 = Sub (r87) :: r410 in
  let r412 = [R 524] in
  let r413 = [R 522] in
  let r414 = [R 118] in
  let r415 = S (T T_RPAREN) :: r414 in
  let r416 = [R 726] in
  let r417 = [R 338] in
  let r418 = [R 660] in
  let r419 = [R 542] in
  let r420 = S (T T_UNDERSCORE) :: r419 in
  let r421 = [R 264] in
  let r422 = [R 262] in
  let r423 = S (T T_RPAREN) :: r422 in
  let r424 = R 496 :: r423 in
  let r425 = [R 263] in
  let r426 = S (T T_RPAREN) :: r425 in
  let r427 = R 496 :: r426 in
  let r428 = [R 292] in
  let r429 = [R 293] in
  let r430 = Sub (r87) :: r429 in
  let r431 = [R 784] in
  let r432 = Sub (r1) :: r431 in
  let r433 = S (T T_EQUAL) :: r432 in
  let r434 = [R 211] in
  let r435 = Sub (r433) :: r434 in
  let r436 = [R 786] in
  let r437 = Sub (r435) :: r436 in
  let r438 = S (T T_RPAREN) :: r437 in
  let r439 = Sub (r401) :: r438 in
  let r440 = [R 268] in
  let r441 = [R 269] in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = S (N N_pattern) :: r442 in
  let r444 = [R 273] in
  let r445 = S (T T_RPAREN) :: r444 in
  let r446 = Sub (r87) :: r445 in
  let r447 = S (T T_DOT) :: r446 in
  let r448 = [R 272] in
  let r449 = S (T T_RPAREN) :: r448 in
  let r450 = Sub (r87) :: r449 in
  let r451 = [R 147] in
  let r452 = Sub (r1) :: r451 in
  let r453 = S (T T_IN) :: r452 in
  let r454 = S (N N_module_expr) :: r453 in
  let r455 = R 310 :: r454 in
  let r456 = R 202 :: r455 in
  let r457 = [R 286] in
  let r458 = R 316 :: r457 in
  let r459 = Sub (r243) :: r458 in
  let r460 = R 561 :: r459 in
  let r461 = R 310 :: r460 in
  let r462 = R 202 :: r461 in
  let r463 = [R 148] in
  let r464 = Sub (r1) :: r463 in
  let r465 = S (T T_IN) :: r464 in
  let r466 = S (N N_module_expr) :: r465 in
  let r467 = R 310 :: r466 in
  let r468 = [R 392] in
  let r469 = S (N N_module_expr) :: r468 in
  let r470 = S (T T_MINUSGREATER) :: r469 in
  let r471 = S (N N_functor_args) :: r470 in
  let r472 = [R 219] in
  let r473 = [R 220] in
  let r474 = S (T T_RPAREN) :: r473 in
  let r475 = S (N N_module_type) :: r474 in
  let r476 = [R 406] in
  let r477 = S (T T_RPAREN) :: r476 in
  let r478 = [R 404] in
  let r479 = S (N N_module_type) :: r478 in
  let r480 = S (T T_MINUSGREATER) :: r479 in
  let r481 = S (N N_functor_args) :: r480 in
  let r482 = [R 375] in
  let r483 = Sub (r105) :: r482 in
  let r484 = [R 414] in
  let r485 = Sub (r483) :: r484 in
  let r486 = [R 905] in
  let r487 = S (N N_module_type) :: r486 in
  let r488 = S (T T_EQUAL) :: r487 in
  let r489 = Sub (r485) :: r488 in
  let r490 = S (T T_TYPE) :: r489 in
  let r491 = S (T T_MODULE) :: r490 in
  let r492 = [R 594] in
  let r493 = Sub (r491) :: r492 in
  let r494 = [R 410] in
  let r495 = [R 902] in
  let r496 = Sub (r85) :: r495 in
  let r497 = S (T T_COLONEQUAL) :: r496 in
  let r498 = Sub (r368) :: r497 in
  let r499 = [R 901] in
  let r500 = R 577 :: r499 in
  let r501 = [R 578] in
  let r502 = Sub (r87) :: r501 in
  let r503 = S (T T_EQUAL) :: r502 in
  let r504 = [R 376] in
  let r505 = Sub (r105) :: r504 in
  let r506 = [R 906] in
  let r507 = [R 409] in
  let r508 = [R 903] in
  let r509 = Sub (r290) :: r508 in
  let r510 = S (T T_UIDENT) :: r224 in
  let r511 = [R 904] in
  let r512 = [R 595] in
  let r513 = [R 397] in
  let r514 = [R 502] in
  let r515 = S (T T_RPAREN) :: r514 in
  let r516 = [R 616] in
  let r517 = S (N N_expr) :: r516 in
  let r518 = [R 703] in
  let r519 = S (T T_RBRACKET) :: r518 in
  let r520 = [R 688] in
  let r521 = [R 619] in
  let r522 = R 490 :: r521 in
  let r523 = [R 491] in
  let r524 = [R 625] in
  let r525 = R 490 :: r524 in
  let r526 = R 498 :: r525 in
  let r527 = Sub (r368) :: r526 in
  let r528 = [R 563] in
  let r529 = Sub (r527) :: r528 in
  let r530 = [R 697] in
  let r531 = S (T T_RBRACE) :: r530 in
  let r532 = [R 663] in
  let r533 = [R 662] in
  let r534 = S (T T_GREATERDOT) :: r533 in
  let r535 = [R 159] in
  let r536 = Sub (r42) :: r535 in
  let r537 = R 310 :: r536 in
  let r538 = [R 676] in
  let r539 = S (T T_END) :: r538 in
  let r540 = R 310 :: r539 in
  let r541 = [R 155] in
  let r542 = S (N N_expr) :: r541 in
  let r543 = S (T T_THEN) :: r542 in
  let r544 = Sub (r1) :: r543 in
  let r545 = R 310 :: r544 in
  let r546 = [R 149] in
  let r547 = Sub (r35) :: r546 in
  let r548 = R 310 :: r547 in
  let r549 = [R 588] in
  let r550 = [R 349] in
  let r551 = Sub (r1) :: r550 in
  let r552 = S (T T_MINUSGREATER) :: r551 in
  let r553 = [R 270] in
  let r554 = Sub (r379) :: r553 in
  let r555 = [R 213] in
  let r556 = Sub (r1) :: r555 in
  let r557 = S (T T_MINUSGREATER) :: r556 in
  let r558 = [R 150] in
  let r559 = Sub (r557) :: r558 in
  let r560 = Sub (r554) :: r559 in
  let r561 = R 310 :: r560 in
  let r562 = [R 271] in
  let r563 = S (T T_RPAREN) :: r562 in
  let r564 = S (N N_let_pattern) :: r563 in
  let r565 = [R 151] in
  let r566 = Sub (r557) :: r565 in
  let r567 = S (T T_RPAREN) :: r566 in
  let r568 = [R 143] in
  let r569 = S (T T_DONE) :: r568 in
  let r570 = Sub (r1) :: r569 in
  let r571 = S (T T_DO) :: r570 in
  let r572 = Sub (r1) :: r571 in
  let r573 = S (T T_IN) :: r572 in
  let r574 = S (N N_pattern) :: r573 in
  let r575 = R 310 :: r574 in
  let r576 = [R 134] in
  let r577 = S (T T_DOWNTO) :: r576 in
  let r578 = [R 157] in
  let r579 = S (T T_DONE) :: r578 in
  let r580 = Sub (r1) :: r579 in
  let r581 = S (T T_DO) :: r580 in
  let r582 = Sub (r1) :: r581 in
  let r583 = Sub (r577) :: r582 in
  let r584 = Sub (r1) :: r583 in
  let r585 = S (T T_EQUAL) :: r584 in
  let r586 = S (N N_pattern) :: r585 in
  let r587 = R 310 :: r586 in
  let r588 = [R 686] in
  let r589 = [R 696] in
  let r590 = S (T T_RPAREN) :: r589 in
  let r591 = S (T T_LPAREN) :: r590 in
  let r592 = S (T T_DOT) :: r591 in
  let r593 = [R 710] in
  let r594 = S (T T_RPAREN) :: r593 in
  let r595 = S (N N_module_type) :: r594 in
  let r596 = S (T T_COLON) :: r595 in
  let r597 = S (N N_module_expr) :: r596 in
  let r598 = R 310 :: r597 in
  let r599 = [R 296] in
  let r600 = Sub (r1) :: r599 in
  let r601 = S (T T_EQUAL) :: r600 in
  let r602 = [R 158] in
  let r603 = Sub (r42) :: r602 in
  let r604 = R 310 :: r603 in
  let r605 = [R 693] in
  let r606 = [R 669] in
  let r607 = S (T T_RPAREN) :: r606 in
  let r608 = Sub (r517) :: r607 in
  let r609 = S (T T_LPAREN) :: r608 in
  let r610 = [R 184] in
  let r611 = [R 255] in
  let r612 = [R 859] in
  let r613 = Sub (r87) :: r612 in
  let r614 = S (T T_COLON) :: r613 in
  let r615 = [R 256] in
  let r616 = S (T T_RPAREN) :: r615 in
  let r617 = Sub (r614) :: r616 in
  let r618 = [R 861] in
  let r619 = [R 860] in
  let r620 = [R 257] in
  let r621 = [R 258] in
  let r622 = [R 692] in
  let r623 = [R 666] in
  let r624 = S (T T_RPAREN) :: r623 in
  let r625 = Sub (r1) :: r624 in
  let r626 = S (T T_LPAREN) :: r625 in
  let r627 = [R 610] in
  let r628 = [R 135] in
  let r629 = Sub (r1) :: r628 in
  let r630 = [R 186] in
  let r631 = Sub (r1) :: r630 in
  let r632 = [R 174] in
  let r633 = [R 168] in
  let r634 = [R 185] in
  let r635 = [R 631] in
  let r636 = Sub (r1) :: r635 in
  let r637 = [R 171] in
  let r638 = [R 175] in
  let r639 = [R 167] in
  let r640 = [R 170] in
  let r641 = [R 169] in
  let r642 = [R 179] in
  let r643 = [R 173] in
  let r644 = [R 172] in
  let r645 = [R 177] in
  let r646 = [R 166] in
  let r647 = [R 165] in
  let r648 = [R 188] in
  let r649 = [R 164] in
  let r650 = [R 178] in
  let r651 = [R 176] in
  let r652 = [R 180] in
  let r653 = [R 181] in
  let r654 = [R 182] in
  let r655 = [R 611] in
  let r656 = [R 183] in
  let r657 = [R 19] in
  let r658 = R 316 :: r657 in
  let r659 = Sub (r243) :: r658 in
  let r660 = [R 282] in
  let r661 = Sub (r1) :: r660 in
  let r662 = S (T T_EQUAL) :: r661 in
  let r663 = Sub (r87) :: r662 in
  let r664 = S (T T_DOT) :: r663 in
  let r665 = [R 280] in
  let r666 = Sub (r1) :: r665 in
  let r667 = S (T T_EQUAL) :: r666 in
  let r668 = Sub (r87) :: r667 in
  let r669 = [R 278] in
  let r670 = Sub (r1) :: r669 in
  let r671 = [R 785] in
  let r672 = [R 212] in
  let r673 = Sub (r1) :: r672 in
  let r674 = [R 284] in
  let r675 = Sub (r1) :: r674 in
  let r676 = S (T T_EQUAL) :: r675 in
  let r677 = [R 283] in
  let r678 = Sub (r1) :: r677 in
  let r679 = [R 529] in
  let r680 = [R 535] in
  let r681 = [R 540] in
  let r682 = [R 538] in
  let r683 = [R 528] in
  let r684 = [R 552] in
  let r685 = S (T T_RBRACKET) :: r684 in
  let r686 = Sub (r15) :: r685 in
  let r687 = [R 546] in
  let r688 = [R 547] in
  let r689 = [R 386] in
  let r690 = S (N N_module_expr) :: r689 in
  let r691 = S (T T_EQUAL) :: r690 in
  let r692 = [R 835] in
  let r693 = R 316 :: r692 in
  let r694 = Sub (r691) :: r693 in
  let r695 = Sub (r63) :: r694 in
  let r696 = R 310 :: r695 in
  let r697 = [R 412] in
  let r698 = R 316 :: r697 in
  let r699 = R 492 :: r698 in
  let r700 = Sub (r105) :: r699 in
  let r701 = R 310 :: r700 in
  let r702 = R 202 :: r701 in
  let r703 = [R 493] in
  let r704 = [R 317] in
  let r705 = [R 836] in
  let r706 = R 306 :: r705 in
  let r707 = R 316 :: r706 in
  let r708 = Sub (r691) :: r707 in
  let r709 = [R 387] in
  let r710 = S (N N_module_expr) :: r709 in
  let r711 = S (T T_EQUAL) :: r710 in
  let r712 = [R 307] in
  let r713 = R 306 :: r712 in
  let r714 = R 316 :: r713 in
  let r715 = Sub (r691) :: r714 in
  let r716 = Sub (r63) :: r715 in
  let r717 = [R 388] in
  let r718 = [R 242] in
  let r719 = S (T T_RBRACKET) :: r718 in
  let r720 = Sub (r15) :: r719 in
  let r721 = [R 208] in
  let r722 = S (T T_RBRACKET) :: r721 in
  let r723 = Sub (r15) :: r722 in
  let r724 = [R 429] in
  let r725 = S (T T_STRING) :: r724 in
  let r726 = [R 553] in
  let r727 = R 316 :: r726 in
  let r728 = Sub (r725) :: r727 in
  let r729 = S (T T_EQUAL) :: r728 in
  let r730 = Sub (r89) :: r729 in
  let r731 = S (T T_COLON) :: r730 in
  let r732 = Sub (r77) :: r731 in
  let r733 = R 310 :: r732 in
  let r734 = [R 549] in
  let r735 = Sub (r87) :: r734 in
  let r736 = Sub (r136) :: r405 in
  let r737 = [R 783] in
  let r738 = R 316 :: r737 in
  let r739 = R 310 :: r738 in
  let r740 = Sub (r736) :: r739 in
  let r741 = S (T T_EQUAL) :: r740 in
  let r742 = Sub (r138) :: r741 in
  let r743 = R 310 :: r742 in
  let r744 = [R 632] in
  let r745 = R 316 :: r744 in
  let r746 = R 310 :: r745 in
  let r747 = R 222 :: r746 in
  let r748 = Sub (r138) :: r747 in
  let r749 = R 310 :: r748 in
  let r750 = R 202 :: r749 in
  let r751 = [R 122] in
  let r752 = Sub (r79) :: r751 in
  let r753 = [R 223] in
  let r754 = [R 124] in
  let r755 = [R 550] in
  let r756 = Sub (r85) :: r755 in
  let r757 = [R 244] in
  let r758 = R 310 :: r757 in
  let r759 = Sub (r756) :: r758 in
  let r760 = S (T T_COLON) :: r759 in
  let r761 = S (T T_LIDENT) :: r760 in
  let r762 = R 417 :: r761 in
  let r763 = [R 246] in
  let r764 = Sub (r762) :: r763 in
  let r765 = [R 128] in
  let r766 = S (T T_RBRACE) :: r765 in
  let r767 = [R 245] in
  let r768 = R 310 :: r767 in
  let r769 = S (T T_SEMI) :: r768 in
  let r770 = R 310 :: r769 in
  let r771 = Sub (r756) :: r770 in
  let r772 = S (T T_COLON) :: r771 in
  let r773 = [R 551] in
  let r774 = Sub (r85) :: r773 in
  let r775 = [R 123] in
  let r776 = [R 125] in
  let r777 = Sub (r79) :: r776 in
  let r778 = [R 127] in
  let r779 = [R 126] in
  let r780 = S (T T_COLONCOLON) :: r415 in
  let r781 = [R 226] in
  let r782 = [R 227] in
  let r783 = Sub (r79) :: r782 in
  let r784 = [R 225] in
  let r785 = Sub (r79) :: r784 in
  let r786 = [R 224] in
  let r787 = Sub (r79) :: r786 in
  let r788 = [R 544] in
  let r789 = [R 574] in
  let r790 = Sub (r142) :: r789 in
  let r791 = [R 640] in
  let r792 = R 316 :: r791 in
  let r793 = Sub (r790) :: r792 in
  let r794 = R 554 :: r793 in
  let r795 = S (T T_PLUSEQ) :: r794 in
  let r796 = Sub (r134) :: r795 in
  let r797 = R 866 :: r796 in
  let r798 = R 310 :: r797 in
  let r799 = [R 641] in
  let r800 = R 316 :: r799 in
  let r801 = Sub (r790) :: r800 in
  let r802 = R 554 :: r801 in
  let r803 = S (T T_PLUSEQ) :: r802 in
  let r804 = Sub (r134) :: r803 in
  let r805 = [R 231] in
  let r806 = R 316 :: r805 in
  let r807 = R 577 :: r806 in
  let r808 = [R 441] in
  let r809 = S (T T_RBRACE) :: r808 in
  let r810 = [R 228] in
  let r811 = R 310 :: r810 in
  let r812 = R 222 :: r811 in
  let r813 = Sub (r138) :: r812 in
  let r814 = [R 439] in
  let r815 = [R 440] in
  let r816 = [R 444] in
  let r817 = S (T T_RBRACE) :: r816 in
  let r818 = [R 443] in
  let r819 = S (T T_RBRACE) :: r818 in
  let r820 = [R 230] in
  let r821 = R 316 :: r820 in
  let r822 = R 577 :: r821 in
  let r823 = [R 319] in
  let r824 = [R 447] in
  let r825 = R 316 :: r824 in
  let r826 = Sub (r290) :: r825 in
  let r827 = R 310 :: r826 in
  let r828 = [R 448] in
  let r829 = R 316 :: r828 in
  let r830 = Sub (r290) :: r829 in
  let r831 = R 310 :: r830 in
  let r832 = [R 389] in
  let r833 = S (N N_module_type) :: r832 in
  let r834 = S (T T_COLON) :: r833 in
  let r835 = [R 643] in
  let r836 = R 316 :: r835 in
  let r837 = Sub (r834) :: r836 in
  let r838 = Sub (r63) :: r837 in
  let r839 = R 310 :: r838 in
  let r840 = [R 413] in
  let r841 = R 316 :: r840 in
  let r842 = S (N N_module_type) :: r841 in
  let r843 = S (T T_COLONEQUAL) :: r842 in
  let r844 = Sub (r105) :: r843 in
  let r845 = R 310 :: r844 in
  let r846 = [R 402] in
  let r847 = R 316 :: r846 in
  let r848 = [R 646] in
  let r849 = R 308 :: r848 in
  let r850 = R 316 :: r849 in
  let r851 = S (N N_module_type) :: r850 in
  let r852 = S (T T_COLON) :: r851 in
  let r853 = [R 309] in
  let r854 = R 308 :: r853 in
  let r855 = R 316 :: r854 in
  let r856 = S (N N_module_type) :: r855 in
  let r857 = S (T T_COLON) :: r856 in
  let r858 = Sub (r63) :: r857 in
  let r859 = S (T T_UIDENT) :: r26 in
  let r860 = Sub (r859) :: r225 in
  let r861 = [R 644] in
  let r862 = R 316 :: r861 in
  let r863 = [R 390] in
  let r864 = S (T T_QUOTED_STRING_EXPR) :: r41 in
  let r865 = [R 80] in
  let r866 = Sub (r864) :: r865 in
  let r867 = [R 90] in
  let r868 = Sub (r866) :: r867 in
  let r869 = [R 651] in
  let r870 = R 302 :: r869 in
  let r871 = R 316 :: r870 in
  let r872 = Sub (r868) :: r871 in
  let r873 = S (T T_COLON) :: r872 in
  let r874 = S (T T_LIDENT) :: r873 in
  let r875 = R 209 :: r874 in
  let r876 = R 893 :: r875 in
  let r877 = R 310 :: r876 in
  let r878 = [R 94] in
  let r879 = R 304 :: r878 in
  let r880 = R 316 :: r879 in
  let r881 = Sub (r866) :: r880 in
  let r882 = S (T T_EQUAL) :: r881 in
  let r883 = S (T T_LIDENT) :: r882 in
  let r884 = R 209 :: r883 in
  let r885 = R 893 :: r884 in
  let r886 = R 310 :: r885 in
  let r887 = [R 210] in
  let r888 = S (T T_RBRACKET) :: r887 in
  let r889 = [R 81] in
  let r890 = S (T T_END) :: r889 in
  let r891 = R 325 :: r890 in
  let r892 = R 71 :: r891 in
  let r893 = [R 70] in
  let r894 = S (T T_RPAREN) :: r893 in
  let r895 = [R 73] in
  let r896 = R 316 :: r895 in
  let r897 = Sub (r87) :: r896 in
  let r898 = S (T T_COLON) :: r897 in
  let r899 = S (T T_LIDENT) :: r898 in
  let r900 = R 421 :: r899 in
  let r901 = [R 74] in
  let r902 = R 316 :: r901 in
  let r903 = Sub (r89) :: r902 in
  let r904 = S (T T_COLON) :: r903 in
  let r905 = S (T T_LIDENT) :: r904 in
  let r906 = R 556 :: r905 in
  let r907 = [R 72] in
  let r908 = R 316 :: r907 in
  let r909 = Sub (r866) :: r908 in
  let r910 = [R 83] in
  let r911 = Sub (r866) :: r910 in
  let r912 = S (T T_IN) :: r911 in
  let r913 = Sub (r860) :: r912 in
  let r914 = R 310 :: r913 in
  let r915 = [R 84] in
  let r916 = Sub (r866) :: r915 in
  let r917 = S (T T_IN) :: r916 in
  let r918 = Sub (r860) :: r917 in
  let r919 = [R 598] in
  let r920 = Sub (r87) :: r919 in
  let r921 = [R 79] in
  let r922 = Sub (r281) :: r921 in
  let r923 = S (T T_RBRACKET) :: r922 in
  let r924 = Sub (r920) :: r923 in
  let r925 = [R 599] in
  let r926 = [R 121] in
  let r927 = Sub (r87) :: r926 in
  let r928 = S (T T_EQUAL) :: r927 in
  let r929 = Sub (r87) :: r928 in
  let r930 = [R 75] in
  let r931 = R 316 :: r930 in
  let r932 = Sub (r929) :: r931 in
  let r933 = [R 76] in
  let r934 = [R 326] in
  let r935 = [R 305] in
  let r936 = R 304 :: r935 in
  let r937 = R 316 :: r936 in
  let r938 = Sub (r866) :: r937 in
  let r939 = S (T T_EQUAL) :: r938 in
  let r940 = S (T T_LIDENT) :: r939 in
  let r941 = R 209 :: r940 in
  let r942 = R 893 :: r941 in
  let r943 = [R 92] in
  let r944 = Sub (r868) :: r943 in
  let r945 = S (T T_MINUSGREATER) :: r944 in
  let r946 = Sub (r81) :: r945 in
  let r947 = [R 93] in
  let r948 = Sub (r868) :: r947 in
  let r949 = [R 91] in
  let r950 = Sub (r868) :: r949 in
  let r951 = S (T T_MINUSGREATER) :: r950 in
  let r952 = [R 303] in
  let r953 = R 302 :: r952 in
  let r954 = R 316 :: r953 in
  let r955 = Sub (r868) :: r954 in
  let r956 = S (T T_COLON) :: r955 in
  let r957 = S (T T_LIDENT) :: r956 in
  let r958 = R 209 :: r957 in
  let r959 = R 893 :: r958 in
  let r960 = [R 320] in
  let r961 = [R 634] in
  let r962 = [R 650] in
  let r963 = R 316 :: r962 in
  let r964 = S (N N_module_type) :: r963 in
  let r965 = R 310 :: r964 in
  let r966 = [R 638] in
  let r967 = [R 313] in
  let r968 = R 312 :: r967 in
  let r969 = R 316 :: r968 in
  let r970 = R 577 :: r969 in
  let r971 = R 862 :: r970 in
  let r972 = S (T T_LIDENT) :: r971 in
  let r973 = R 866 :: r972 in
  let r974 = [R 639] in
  let r975 = [R 315] in
  let r976 = R 314 :: r975 in
  let r977 = R 316 :: r976 in
  let r978 = R 577 :: r977 in
  let r979 = Sub (r185) :: r978 in
  let r980 = S (T T_COLONEQUAL) :: r979 in
  let r981 = S (T T_LIDENT) :: r980 in
  let r982 = R 866 :: r981 in
  let r983 = [R 52] in
  let r984 = Sub (r864) :: r983 in
  let r985 = [R 61] in
  let r986 = Sub (r984) :: r985 in
  let r987 = S (T T_EQUAL) :: r986 in
  let r988 = [R 839] in
  let r989 = R 300 :: r988 in
  let r990 = R 316 :: r989 in
  let r991 = Sub (r987) :: r990 in
  let r992 = S (T T_LIDENT) :: r991 in
  let r993 = R 209 :: r992 in
  let r994 = R 893 :: r993 in
  let r995 = R 310 :: r994 in
  let r996 = [R 274] in
  let r997 = S (T T_RPAREN) :: r996 in
  let r998 = Sub (r87) :: r997 in
  let r999 = [R 89] in
  let r1000 = S (T T_END) :: r999 in
  let r1001 = R 327 :: r1000 in
  let r1002 = R 69 :: r1001 in
  let r1003 = [R 888] in
  let r1004 = Sub (r1) :: r1003 in
  let r1005 = S (T T_EQUAL) :: r1004 in
  let r1006 = S (T T_LIDENT) :: r1005 in
  let r1007 = R 415 :: r1006 in
  let r1008 = R 310 :: r1007 in
  let r1009 = [R 55] in
  let r1010 = R 316 :: r1009 in
  let r1011 = [R 889] in
  let r1012 = Sub (r1) :: r1011 in
  let r1013 = S (T T_EQUAL) :: r1012 in
  let r1014 = S (T T_LIDENT) :: r1013 in
  let r1015 = R 415 :: r1014 in
  let r1016 = [R 891] in
  let r1017 = Sub (r1) :: r1016 in
  let r1018 = [R 887] in
  let r1019 = Sub (r87) :: r1018 in
  let r1020 = S (T T_COLON) :: r1019 in
  let r1021 = [R 890] in
  let r1022 = Sub (r1) :: r1021 in
  let r1023 = [R 359] in
  let r1024 = Sub (r433) :: r1023 in
  let r1025 = S (T T_LIDENT) :: r1024 in
  let r1026 = R 554 :: r1025 in
  let r1027 = R 310 :: r1026 in
  let r1028 = [R 56] in
  let r1029 = R 316 :: r1028 in
  let r1030 = [R 360] in
  let r1031 = Sub (r433) :: r1030 in
  let r1032 = S (T T_LIDENT) :: r1031 in
  let r1033 = R 554 :: r1032 in
  let r1034 = [R 362] in
  let r1035 = Sub (r1) :: r1034 in
  let r1036 = S (T T_EQUAL) :: r1035 in
  let r1037 = [R 364] in
  let r1038 = Sub (r1) :: r1037 in
  let r1039 = S (T T_EQUAL) :: r1038 in
  let r1040 = Sub (r87) :: r1039 in
  let r1041 = S (T T_DOT) :: r1040 in
  let r1042 = [R 358] in
  let r1043 = Sub (r89) :: r1042 in
  let r1044 = S (T T_COLON) :: r1043 in
  let r1045 = [R 361] in
  let r1046 = Sub (r1) :: r1045 in
  let r1047 = S (T T_EQUAL) :: r1046 in
  let r1048 = [R 363] in
  let r1049 = Sub (r1) :: r1048 in
  let r1050 = S (T T_EQUAL) :: r1049 in
  let r1051 = Sub (r87) :: r1050 in
  let r1052 = S (T T_DOT) :: r1051 in
  let r1053 = [R 58] in
  let r1054 = R 316 :: r1053 in
  let r1055 = Sub (r1) :: r1054 in
  let r1056 = [R 53] in
  let r1057 = R 316 :: r1056 in
  let r1058 = R 486 :: r1057 in
  let r1059 = Sub (r984) :: r1058 in
  let r1060 = [R 54] in
  let r1061 = R 316 :: r1060 in
  let r1062 = R 486 :: r1061 in
  let r1063 = Sub (r984) :: r1062 in
  let r1064 = [R 85] in
  let r1065 = S (T T_RPAREN) :: r1064 in
  let r1066 = [R 48] in
  let r1067 = Sub (r984) :: r1066 in
  let r1068 = S (T T_IN) :: r1067 in
  let r1069 = Sub (r860) :: r1068 in
  let r1070 = R 310 :: r1069 in
  let r1071 = [R 289] in
  let r1072 = R 316 :: r1071 in
  let r1073 = Sub (r243) :: r1072 in
  let r1074 = R 561 :: r1073 in
  let r1075 = R 310 :: r1074 in
  let r1076 = [R 49] in
  let r1077 = Sub (r984) :: r1076 in
  let r1078 = S (T T_IN) :: r1077 in
  let r1079 = Sub (r860) :: r1078 in
  let r1080 = [R 87] in
  let r1081 = Sub (r218) :: r1080 in
  let r1082 = S (T T_RBRACKET) :: r1081 in
  let r1083 = [R 64] in
  let r1084 = Sub (r984) :: r1083 in
  let r1085 = S (T T_MINUSGREATER) :: r1084 in
  let r1086 = Sub (r554) :: r1085 in
  let r1087 = [R 46] in
  let r1088 = Sub (r1086) :: r1087 in
  let r1089 = [R 47] in
  let r1090 = Sub (r984) :: r1089 in
  let r1091 = [R 254] in
  let r1092 = [R 288] in
  let r1093 = R 316 :: r1092 in
  let r1094 = Sub (r243) :: r1093 in
  let r1095 = [R 88] in
  let r1096 = S (T T_RPAREN) :: r1095 in
  let r1097 = [R 487] in
  let r1098 = [R 57] in
  let r1099 = R 316 :: r1098 in
  let r1100 = Sub (r929) :: r1099 in
  let r1101 = [R 59] in
  let r1102 = [R 328] in
  let r1103 = [R 62] in
  let r1104 = Sub (r984) :: r1103 in
  let r1105 = S (T T_EQUAL) :: r1104 in
  let r1106 = [R 63] in
  let r1107 = [R 301] in
  let r1108 = R 300 :: r1107 in
  let r1109 = R 316 :: r1108 in
  let r1110 = Sub (r987) :: r1109 in
  let r1111 = S (T T_LIDENT) :: r1110 in
  let r1112 = R 209 :: r1111 in
  let r1113 = R 893 :: r1112 in
  let r1114 = [R 324] in
  let r1115 = [R 827] in
  let r1116 = [R 841] in
  let r1117 = R 316 :: r1116 in
  let r1118 = S (N N_module_expr) :: r1117 in
  let r1119 = R 310 :: r1118 in
  let r1120 = [R 831] in
  let r1121 = [R 824] in
  let r1122 = R 321 :: r1121 in
  let r1123 = [R 668] in
  let r1124 = S (T T_RBRACKET) :: r1123 in
  let r1125 = Sub (r1) :: r1124 in
  let r1126 = [R 667] in
  let r1127 = S (T T_RBRACE) :: r1126 in
  let r1128 = Sub (r1) :: r1127 in
  let r1129 = [R 670] in
  let r1130 = S (T T_RPAREN) :: r1129 in
  let r1131 = Sub (r517) :: r1130 in
  let r1132 = S (T T_LPAREN) :: r1131 in
  let r1133 = [R 674] in
  let r1134 = S (T T_RBRACKET) :: r1133 in
  let r1135 = Sub (r517) :: r1134 in
  let r1136 = [R 672] in
  let r1137 = S (T T_RBRACE) :: r1136 in
  let r1138 = Sub (r517) :: r1137 in
  let r1139 = [R 194] in
  let r1140 = [R 673] in
  let r1141 = S (T T_RBRACKET) :: r1140 in
  let r1142 = Sub (r517) :: r1141 in
  let r1143 = [R 198] in
  let r1144 = [R 671] in
  let r1145 = S (T T_RBRACE) :: r1144 in
  let r1146 = Sub (r517) :: r1145 in
  let r1147 = [R 196] in
  let r1148 = [R 191] in
  let r1149 = [R 193] in
  let r1150 = [R 192] in
  let r1151 = [R 195] in
  let r1152 = [R 199] in
  let r1153 = [R 197] in
  let r1154 = [R 190] in
  let r1155 = [R 297] in
  let r1156 = Sub (r1) :: r1155 in
  let r1157 = [R 299] in
  let r1158 = [R 690] in
  let r1159 = [R 702] in
  let r1160 = [R 701] in
  let r1161 = [R 97] in
  let r1162 = S (N N_expr) :: r1161 in
  let r1163 = S (T T_IN) :: r1162 in
  let r1164 = S (N N_pattern) :: r1163 in
  let r1165 = R 310 :: r1164 in
  let r1166 = R 202 :: r1165 in
  let r1167 = [R 592] in
  let r1168 = Sub (r1166) :: r1167 in
  let r1169 = [R 98] in
  let r1170 = S (T T_BARRBRACKET) :: r1169 in
  let r1171 = [R 99] in
  let r1172 = S (T T_BARRBRACKET) :: r1171 in
  let r1173 = [R 593] in
  let r1174 = [R 96] in
  let r1175 = S (N N_expr) :: r1174 in
  let r1176 = Sub (r577) :: r1175 in
  let r1177 = [R 709] in
  let r1178 = [R 708] in
  let r1179 = [R 102] in
  let r1180 = S (T T_RBRACKET) :: r1179 in
  let r1181 = [R 103] in
  let r1182 = S (T T_RBRACKET) :: r1181 in
  let r1183 = S (T T_LIDENT) :: r522 in
  let r1184 = [R 691] in
  let r1185 = S (T T_GREATERRBRACE) :: r1184 in
  let r1186 = [R 698] in
  let r1187 = S (T T_RBRACE) :: r1186 in
  let r1188 = [R 564] in
  let r1189 = Sub (r527) :: r1188 in
  let r1190 = [R 142] in
  let r1191 = S (T T_DONE) :: r1190 in
  let r1192 = Sub (r1) :: r1191 in
  let r1193 = S (T T_DO) :: r1192 in
  let r1194 = Sub (r1) :: r1193 in
  let r1195 = Sub (r577) :: r1194 in
  let r1196 = [R 216] in
  let r1197 = Sub (r557) :: r1196 in
  let r1198 = S (T T_RPAREN) :: r1197 in
  let r1199 = [R 214] in
  let r1200 = Sub (r1) :: r1199 in
  let r1201 = S (T T_MINUSGREATER) :: r1200 in
  let r1202 = [R 215] in
  let r1203 = [R 589] in
  let r1204 = [R 154] in
  let r1205 = [R 675] in
  let r1206 = [R 687] in
  let r1207 = [R 145] in
  let r1208 = Sub (r1) :: r1207 in
  let r1209 = S (T T_IN) :: r1208 in
  let r1210 = Sub (r691) :: r1209 in
  let r1211 = Sub (r63) :: r1210 in
  let r1212 = R 310 :: r1211 in
  let r1213 = [R 146] in
  let r1214 = Sub (r1) :: r1213 in
  let r1215 = S (T T_IN) :: r1214 in
  let r1216 = R 310 :: r1215 in
  let r1217 = R 222 :: r1216 in
  let r1218 = Sub (r138) :: r1217 in
  let r1219 = R 310 :: r1218 in
  let r1220 = [R 340] in
  let r1221 = Sub (r251) :: r1220 in
  let r1222 = [R 344] in
  let r1223 = Sub (r1221) :: r1222 in
  let r1224 = S (T T_RPAREN) :: r1223 in
  let r1225 = Sub (r401) :: r1224 in
  let r1226 = [R 341] in
  let r1227 = Sub (r1) :: r1226 in
  let r1228 = [R 343] in
  let r1229 = [R 281] in
  let r1230 = Sub (r1) :: r1229 in
  let r1231 = S (T T_EQUAL) :: r1230 in
  let r1232 = Sub (r87) :: r1231 in
  let r1233 = [R 279] in
  let r1234 = Sub (r1) :: r1233 in
  let r1235 = [R 699] in
  let r1236 = [R 678] in
  let r1237 = S (T T_RPAREN) :: r1236 in
  let r1238 = S (N N_module_expr) :: r1237 in
  let r1239 = R 310 :: r1238 in
  let r1240 = [R 679] in
  let r1241 = S (T T_RPAREN) :: r1240 in
  let r1242 = [R 665] in
  let r1243 = [R 507] in
  let r1244 = S (T T_RPAREN) :: r1243 in
  let r1245 = [R 505] in
  let r1246 = S (T T_RPAREN) :: r1245 in
  let r1247 = [R 506] in
  let r1248 = S (T T_RPAREN) :: r1247 in
  let r1249 = [R 323] in
  let r1250 = R 321 :: r1249 in
  let r1251 = [R 355] in
  let r1252 = R 310 :: r1251 in
  let r1253 = Sub (r756) :: r1252 in
  let r1254 = [R 353] in
  let r1255 = [R 29] in
  let r1256 = [R 807] in
  let r1257 = Sub (r81) :: r1256 in
  let r1258 = S (T T_MINUSGREATER) :: r1257 in
  let r1259 = S (T T_RPAREN) :: r1258 in
  let r1260 = Sub (r87) :: r1259 in
  let r1261 = [R 808] in
  let r1262 = [R 813] in
  let r1263 = Sub (r81) :: r1262 in
  let r1264 = S (T T_MINUSGREATER) :: r1263 in
  let r1265 = [R 811] in
  let r1266 = Sub (r81) :: r1265 in
  let r1267 = S (T T_MINUSGREATER) :: r1266 in
  let r1268 = S (T T_RPAREN) :: r1267 in
  let r1269 = Sub (r87) :: r1268 in
  let r1270 = [R 812] in
  let r1271 = [R 814] in
  let r1272 = [R 810] in
  let r1273 = [R 819] in
  let r1274 = Sub (r81) :: r1273 in
  let r1275 = S (T T_MINUSGREATER) :: r1274 in
  let r1276 = S (T T_RPAREN) :: r1275 in
  let r1277 = Sub (r87) :: r1276 in
  let r1278 = [R 820] in
  let r1279 = [R 816] in
  let r1280 = [R 442] in
  let r1281 = S (T T_RBRACE) :: r1280 in
  let r1282 = [R 206] in
  let r1283 = R 310 :: r1282 in
  let r1284 = [R 207] in
  let r1285 = R 310 :: r1284 in
  let r1286 = [R 68] in
  let r1287 = S (T T_RPAREN) :: r1286 in
  let r1288 = [R 138] in
  let r1289 = [R 140] in
  let r1290 = [R 139] in
  let r1291 = [R 236] in
  let r1292 = [R 241] in
  let r1293 = [R 370] in
  let r1294 = [R 373] in
  let r1295 = S (T T_RPAREN) :: r1294 in
  let r1296 = S (T T_COLONCOLON) :: r1295 in
  let r1297 = S (T T_LPAREN) :: r1296 in
  let r1298 = [R 508] in
  let r1299 = [R 509] in
  let r1300 = [R 510] in
  let r1301 = [R 511] in
  let r1302 = [R 512] in
  let r1303 = [R 513] in
  let r1304 = [R 514] in
  let r1305 = [R 515] in
  let r1306 = [R 516] in
  let r1307 = [R 517] in
  let r1308 = [R 518] in
  let r1309 = [R 846] in
  let r1310 = [R 855] in
  let r1311 = [R 330] in
  let r1312 = [R 853] in
  let r1313 = S (T T_SEMISEMI) :: r1312 in
  let r1314 = [R 854] in
  let r1315 = [R 332] in
  let r1316 = [R 335] in
  let r1317 = [R 334] in
  let r1318 = [R 333] in
  let r1319 = R 331 :: r1318 in
  let r1320 = [R 882] in
  let r1321 = S (T T_EOF) :: r1320 in
  let r1322 = R 331 :: r1321 in
  let r1323 = [R 881] in
  function
  | 0 | 1973 | 1977 | 1995 | 1999 | 2003 | 2007 | 2011 | 2015 | 2019 | 2023 | 2027 | 2031 | 2037 | 2057 -> Nothing
  | 1972 -> One ([R 0])
  | 1976 -> One ([R 1])
  | 1982 -> One ([R 2])
  | 1996 -> One ([R 3])
  | 2000 -> One ([R 4])
  | 2006 -> One ([R 5])
  | 2008 -> One ([R 6])
  | 2012 -> One ([R 7])
  | 2016 -> One ([R 8])
  | 2020 -> One ([R 9])
  | 2024 -> One ([R 10])
  | 2030 -> One ([R 11])
  | 2034 -> One ([R 12])
  | 2047 -> One ([R 13])
  | 2067 -> One ([R 14])
  | 217 -> One ([R 15])
  | 216 -> One ([R 16])
  | 1990 -> One ([R 20])
  | 1992 -> One ([R 21])
  | 287 -> One ([R 26])
  | 302 -> One ([R 27])
  | 298 -> One ([R 41])
  | 1390 -> One ([R 45])
  | 1399 -> One ([R 50])
  | 1394 -> One ([R 51])
  | 1435 -> One ([R 60])
  | 1402 -> One ([R 65])
  | 1177 -> One ([R 77])
  | 1157 -> One ([R 78])
  | 1159 -> One ([R 82])
  | 1397 -> One ([R 86])
  | 1621 -> One ([R 100])
  | 1606 -> One ([R 101])
  | 1636 -> One ([R 104])
  | 1634 -> One ([R 105])
  | 431 -> One ([R 107])
  | 75 -> One ([R 108])
  | 429 -> One ([R 109])
  | 74 -> One ([R 113])
  | 200 | 918 -> One ([R 114])
  | 960 -> One ([R 117])
  | 994 -> One ([R 129])
  | 998 -> One ([R 130])
  | 319 -> One ([R 132])
  | 1616 -> One ([R 133])
  | 711 -> One ([R 144])
  | 1573 -> One ([R 160])
  | 734 -> One ([R 161])
  | 756 -> One ([R 162])
  | 737 -> One ([R 163])
  | 754 -> One ([R 200])
  | 1 -> One (R 202 :: r7)
  | 63 -> One (R 202 :: r24)
  | 68 -> One (R 202 :: r29)
  | 71 -> One (R 202 :: r40)
  | 78 -> One (R 202 :: r48)
  | 98 -> One (R 202 :: r67)
  | 109 -> One (R 202 :: r95)
  | 218 -> One (R 202 :: r205)
  | 219 -> One (R 202 :: r209)
  | 225 -> One (R 202 :: r221)
  | 240 -> One (R 202 :: r231)
  | 243 -> One (R 202 :: r236)
  | 252 -> One (R 202 :: r248)
  | 423 -> One (R 202 :: r382)
  | 446 -> One (R 202 :: r395)
  | 553 -> One (R 202 :: r467)
  | 644 -> One (R 202 :: r537)
  | 647 -> One (R 202 :: r540)
  | 650 -> One (R 202 :: r545)
  | 653 -> One (R 202 :: r548)
  | 659 -> One (R 202 :: r561)
  | 667 -> One (R 202 :: r575)
  | 672 -> One (R 202 :: r587)
  | 688 -> One (R 202 :: r598)
  | 702 -> One (R 202 :: r604)
  | 859 -> One (R 202 :: r696)
  | 900 -> One (R 202 :: r733)
  | 1050 -> One (R 202 :: r827)
  | 1051 -> One (R 202 :: r831)
  | 1060 -> One (R 202 :: r839)
  | 1101 -> One (R 202 :: r877)
  | 1102 -> One (R 202 :: r886)
  | 1238 -> One (R 202 :: r965)
  | 1270 -> One (R 202 :: r995)
  | 1476 -> One (R 202 :: r1119)
  | 1732 -> One (R 202 :: r1212)
  | 1739 -> One (R 202 :: r1219)
  | 1790 -> One (R 202 :: r1239)
  | 310 -> One ([R 218])
  | 565 -> One ([R 221])
  | 155 -> One ([R 234])
  | 898 -> One ([R 237])
  | 899 -> One ([R 238])
  | 133 -> One (R 239 :: r101)
  | 137 -> One (R 239 :: r103)
  | 215 -> One ([R 243])
  | 944 -> One ([R 247])
  | 945 -> One ([R 248])
  | 1393 -> One ([R 253])
  | 851 -> One ([R 275])
  | 822 -> One ([R 277])
  | 1473 -> One ([R 287])
  | 1400 -> One ([R 290])
  | 513 -> One ([R 291])
  | 1749 -> One ([R 294])
  | 107 -> One (R 310 :: r75)
  | 171 -> One (R 310 :: r130)
  | 223 -> One (R 310 :: r214)
  | 236 -> One (R 310 :: r226)
  | 556 -> One (R 310 :: r471)
  | 563 -> One (R 310 :: r481)
  | 804 -> One (R 310 :: r659)
  | 882 -> One (R 310 :: r716)
  | 1079 -> One (R 310 :: r858)
  | 1113 -> One (R 310 :: r892)
  | 1119 -> One (R 310 :: r900)
  | 1130 -> One (R 310 :: r906)
  | 1141 -> One (R 310 :: r909)
  | 1145 -> One (R 310 :: r918)
  | 1166 -> One (R 310 :: r932)
  | 1182 -> One (R 310 :: r942)
  | 1217 -> One (R 310 :: r959)
  | 1244 -> One (R 310 :: r973)
  | 1254 -> One (R 310 :: r982)
  | 1287 -> One (R 310 :: r1002)
  | 1291 -> One (R 310 :: r1015)
  | 1320 -> One (R 310 :: r1033)
  | 1359 -> One (R 310 :: r1055)
  | 1363 -> One (R 310 :: r1059)
  | 1364 -> One (R 310 :: r1063)
  | 1375 -> One (R 310 :: r1079)
  | 1383 -> One (R 310 :: r1088)
  | 1427 -> One (R 310 :: r1100)
  | 1447 -> One (R 310 :: r1113)
  | 1835 -> One (R 310 :: r1254)
  | 1243 -> One (R 312 :: r966)
  | 1481 -> One (R 312 :: r1120)
  | 1253 -> One (R 314 :: r974)
  | 867 -> One (R 316 :: r704)
  | 1175 -> One (R 316 :: r933)
  | 1236 -> One (R 316 :: r961)
  | 1433 -> One (R 316 :: r1101)
  | 1474 -> One (R 316 :: r1115)
  | 1486 -> One (R 316 :: r1122)
  | 1825 -> One (R 316 :: r1250)
  | 2052 -> One (R 316 :: r1313)
  | 2063 -> One (R 316 :: r1319)
  | 2068 -> One (R 316 :: r1322)
  | 1049 -> One (R 318 :: r823)
  | 1228 -> One (R 318 :: r960)
  | 214 -> One (R 321 :: r201)
  | 1457 -> One (R 321 :: r1114)
  | 1178 -> One (R 325 :: r934)
  | 1436 -> One (R 327 :: r1102)
  | 2050 -> One (R 329 :: r1311)
  | 2058 -> One (R 331 :: r1315)
  | 2059 -> One (R 331 :: r1316)
  | 2060 -> One (R 331 :: r1317)
  | 500 -> One ([R 337])
  | 504 -> One ([R 339])
  | 745 -> One ([R 346])
  | 1470 -> One ([R 347])
  | 1694 -> One ([R 350])
  | 1838 -> One ([R 351])
  | 1841 -> One ([R 352])
  | 1840 -> One ([R 354])
  | 1839 -> One ([R 356])
  | 1837 -> One ([R 357])
  | 1991 -> One ([R 369])
  | 1981 -> One ([R 371])
  | 1989 -> One ([R 372])
  | 1988 -> One ([R 374])
  | 679 -> One ([R 381])
  | 1657 -> One ([R 382])
  | 621 -> One ([R 393])
  | 631 -> One ([R 394])
  | 632 -> One ([R 395])
  | 630 -> One ([R 396])
  | 633 -> One ([R 398])
  | 170 -> One ([R 399])
  | 102 | 1070 -> One ([R 400])
  | 592 -> One ([R 407])
  | 569 -> One ([R 408])
  | 599 -> One ([R 411])
  | 1293 | 1306 -> One ([R 416])
  | 929 -> One ([R 418])
  | 930 -> One ([R 419])
  | 928 -> One ([R 420])
  | 1123 -> One ([R 422])
  | 1121 -> One ([R 423])
  | 1124 -> One ([R 424])
  | 1122 -> One ([R 425])
  | 464 -> One ([R 428])
  | 911 -> One ([R 430])
  | 1006 -> One ([R 431])
  | 1900 -> One ([R 432])
  | 1022 -> One ([R 433])
  | 1901 -> One ([R 434])
  | 1021 -> One ([R 435])
  | 1013 -> One ([R 436])
  | 92 | 247 -> One ([R 449])
  | 116 | 697 -> One ([R 450])
  | 144 -> One ([R 451])
  | 132 -> One ([R 453])
  | 136 -> One ([R 455])
  | 140 -> One ([R 457])
  | 123 -> One ([R 458])
  | 143 | 1593 -> One ([R 459])
  | 122 -> One ([R 460])
  | 121 -> One ([R 461])
  | 120 -> One ([R 462])
  | 119 -> One ([R 463])
  | 118 -> One ([R 464])
  | 95 | 113 | 687 -> One ([R 465])
  | 94 | 686 -> One ([R 466])
  | 93 -> One ([R 467])
  | 115 | 470 | 696 -> One ([R 468])
  | 114 | 695 -> One ([R 469])
  | 90 -> One ([R 470])
  | 96 -> One ([R 471])
  | 125 -> One ([R 472])
  | 117 -> One ([R 473])
  | 124 -> One ([R 474])
  | 97 -> One ([R 475])
  | 142 -> One ([R 476])
  | 145 -> One ([R 477])
  | 141 -> One ([R 479])
  | 372 -> One ([R 480])
  | 371 -> One (R 481 :: r335)
  | 264 -> One (R 482 :: r268)
  | 265 -> One ([R 483])
  | 501 -> One (R 484 :: r417)
  | 502 -> One ([R 485])
  | 1646 -> One ([R 499])
  | 161 -> One ([R 500])
  | 456 -> One ([R 520])
  | 450 -> One ([R 521])
  | 451 -> One ([R 523])
  | 449 | 698 -> One ([R 530])
  | 846 -> One ([R 536])
  | 847 -> One ([R 537])
  | 848 -> One ([R 539])
  | 525 -> One ([R 541])
  | 1269 -> One ([R 545])
  | 1028 | 1340 -> One ([R 555])
  | 1134 -> One ([R 557])
  | 1132 -> One ([R 558])
  | 1135 -> One ([R 559])
  | 1133 -> One ([R 560])
  | 1409 -> One (R 561 :: r1094)
  | 255 -> One ([R 562])
  | 1004 -> One ([R 565])
  | 1005 -> One ([R 566])
  | 1000 -> One ([R 567])
  | 1917 -> One ([R 569])
  | 1916 -> One ([R 570])
  | 1918 -> One ([R 571])
  | 1913 -> One ([R 572])
  | 1914 -> One ([R 573])
  | 1034 -> One ([R 575])
  | 1032 -> One ([R 576])
  | 614 -> One ([R 579])
  | 566 -> One ([R 580])
  | 1396 -> One ([R 581])
  | 1395 -> One ([R 582])
  | 394 -> One ([R 584])
  | 364 -> One ([R 614])
  | 1512 -> One ([R 617])
  | 1513 -> One ([R 618])
  | 1717 -> One ([R 620])
  | 1718 -> One ([R 621])
  | 495 -> One ([R 623])
  | 496 -> One ([R 624])
  | 1649 -> One ([R 626])
  | 1650 -> One ([R 627])
  | 759 -> One ([R 629])
  | 763 -> One ([R 630])
  | 1264 -> One ([R 635])
  | 1227 -> One ([R 636])
  | 1230 -> One ([R 637])
  | 1229 -> One ([R 642])
  | 1234 -> One ([R 645])
  | 1233 -> One ([R 647])
  | 1232 -> One ([R 648])
  | 1231 -> One ([R 649])
  | 1265 -> One ([R 652])
  | 88 -> One ([R 655])
  | 85 -> One ([R 657])
  | 678 -> One ([R 681])
  | 741 -> One ([R 682])
  | 740 | 755 -> One ([R 683])
  | 681 | 736 -> One ([R 684])
  | 1520 | 1570 -> One ([R 689])
  | 739 -> One ([R 694])
  | 1725 -> One ([R 704])
  | 1788 -> One ([R 705])
  | 1637 -> One ([R 706])
  | 1623 -> One ([R 707])
  | 432 -> One ([R 711])
  | 436 -> One ([R 714])
  | 437 -> One ([R 718])
  | 468 -> One ([R 720])
  | 441 -> One ([R 721])
  | 497 -> One ([R 723])
  | 459 -> One ([R 728])
  | 30 -> One ([R 729])
  | 8 -> One ([R 730])
  | 54 -> One ([R 732])
  | 53 -> One ([R 733])
  | 52 -> One ([R 734])
  | 51 -> One ([R 735])
  | 50 -> One ([R 736])
  | 49 -> One ([R 737])
  | 48 -> One ([R 738])
  | 47 -> One ([R 739])
  | 46 -> One ([R 740])
  | 45 -> One ([R 741])
  | 44 -> One ([R 742])
  | 43 -> One ([R 743])
  | 42 -> One ([R 744])
  | 41 -> One ([R 745])
  | 40 -> One ([R 746])
  | 39 -> One ([R 747])
  | 38 -> One ([R 748])
  | 23 -> One ([R 749])
  | 37 -> One ([R 750])
  | 36 -> One ([R 751])
  | 35 -> One ([R 752])
  | 34 -> One ([R 753])
  | 33 -> One ([R 754])
  | 32 -> One ([R 755])
  | 31 -> One ([R 756])
  | 29 -> One ([R 757])
  | 28 -> One ([R 758])
  | 27 -> One ([R 759])
  | 26 -> One ([R 760])
  | 25 -> One ([R 761])
  | 24 -> One ([R 762])
  | 22 -> One ([R 763])
  | 21 -> One ([R 764])
  | 20 -> One ([R 765])
  | 19 -> One ([R 766])
  | 18 -> One ([R 767])
  | 17 -> One ([R 768])
  | 16 -> One ([R 769])
  | 15 -> One ([R 770])
  | 14 -> One ([R 771])
  | 13 -> One ([R 772])
  | 12 -> One ([R 773])
  | 11 -> One ([R 774])
  | 10 -> One ([R 775])
  | 9 -> One ([R 776])
  | 7 -> One ([R 777])
  | 6 -> One ([R 778])
  | 5 -> One ([R 779])
  | 4 -> One ([R 780])
  | 3 -> One ([R 781])
  | 1465 -> One ([R 782])
  | 334 -> One ([R 787])
  | 361 -> One ([R 788])
  | 349 -> One ([R 789])
  | 355 -> One ([R 790])
  | 1853 -> One ([R 791])
  | 1876 -> One ([R 792])
  | 1864 -> One ([R 793])
  | 1870 -> One ([R 794])
  | 1895 -> One ([R 795])
  | 363 -> One ([R 796])
  | 1885 -> One ([R 797])
  | 307 -> One ([R 798])
  | 1492 -> One ([R 823])
  | 1469 | 1491 -> One ([R 825])
  | 1472 | 1493 -> One ([R 826])
  | 1483 -> One ([R 828])
  | 1466 -> One ([R 829])
  | 1456 -> One ([R 830])
  | 1464 -> One ([R 834])
  | 1468 -> One ([R 837])
  | 1467 -> One ([R 838])
  | 1484 -> One ([R 840])
  | 239 -> One ([R 842])
  | 238 -> One ([R 843])
  | 2041 -> One ([R 847])
  | 2042 -> One ([R 848])
  | 2044 -> One ([R 849])
  | 2045 -> One ([R 850])
  | 2043 -> One ([R 851])
  | 2040 -> One ([R 852])
  | 2046 -> One ([R 856])
  | 295 -> One ([R 858])
  | 572 -> One (R 866 :: r498)
  | 586 -> One ([R 867])
  | 177 -> One ([R 872])
  | 180 -> One ([R 873])
  | 184 -> One ([R 874])
  | 178 -> One ([R 875])
  | 185 -> One ([R 876])
  | 181 -> One ([R 877])
  | 186 -> One ([R 878])
  | 183 -> One ([R 879])
  | 176 -> One ([R 880])
  | 433 -> One ([R 885])
  | 738 -> One ([R 886])
  | 1105 -> One ([R 894])
  | 1304 -> One ([R 895])
  | 1307 -> One ([R 896])
  | 1305 -> One ([R 897])
  | 1338 -> One ([R 898])
  | 1341 -> One ([R 899])
  | 1339 -> One ([R 900])
  | 575 -> One ([R 907])
  | 576 -> One ([R 908])
  | 1642 -> One (S (T T_WITH) :: r1189)
  | 166 -> One (S (T T_TYPE) :: r127)
  | 527 -> One (S (T T_TYPE) :: r439)
  | 1757 -> One (S (T T_TYPE) :: r1225)
  | 949 -> One (S (T T_STAR) :: r777)
  | 2048 -> One (S (T T_SEMISEMI) :: r1310)
  | 2055 -> One (S (T T_SEMISEMI) :: r1314)
  | 1978 -> One (S (T T_RPAREN) :: r54)
  | 311 -> One (S (T T_RPAREN) :: r305)
  | 335 -> One (S (T T_RPAREN) :: r317)
  | 444 -> One (S (T T_RPAREN) :: r392)
  | 488 -> One (S (T T_RPAREN) :: r416)
  | 558 -> One (S (T T_RPAREN) :: r472)
  | 623 -> One (S (T T_RPAREN) :: r513)
  | 1594 -> One (S (T T_RPAREN) :: r1158)
  | 1800 -> One (S (T T_RPAREN) :: r1242)
  | 1979 -> One (S (T T_RPAREN) :: r1293)
  | 267 -> One (S (T T_RBRACKET) :: r269)
  | 922 | 989 -> One (S (T T_RBRACKET) :: r362)
  | 1624 -> One (S (T T_RBRACKET) :: r1177)
  | 1626 -> One (S (T T_RBRACKET) :: r1178)
  | 321 -> One (S (T T_QUOTE) :: r309)
  | 1143 -> One (S (T T_OPEN) :: r914)
  | 1367 -> One (S (T T_OPEN) :: r1070)
  | 206 | 209 | 211 | 309 | 340 | 1855 -> One (S (T T_MODULE) :: r116)
  | 969 -> One (S (T T_MINUSGREATER) :: r785)
  | 973 -> One (S (T T_MINUSGREATER) :: r787)
  | 1204 -> One (S (T T_MINUSGREATER) :: r948)
  | 126 -> One (S (T T_LPAREN) :: r98)
  | 532 -> One (S (T T_LOCAL) :: r443)
  | 662 | 1276 | 1676 -> One (S (T T_LOCAL) :: r564)
  | 158 -> One (S (T T_LIDENT) :: r111)
  | 259 -> One (S (T T_LIDENT) :: r254)
  | 405 -> One (S (T T_LIDENT) :: r346)
  | 712 -> One (S (T T_LIDENT) :: r611)
  | 713 -> One (S (T T_LIDENT) :: r617)
  | 724 -> One (S (T T_LIDENT) :: r620)
  | 728 -> One (S (T T_LIDENT) :: r622)
  | 931 -> One (S (T T_LIDENT) :: r772)
  | 1308 -> One (S (T T_LIDENT) :: r1020)
  | 1342 -> One (S (T T_LIDENT) :: r1044)
  | 1419 -> One (S (T T_LIDENT) :: r1097)
  | 83 -> One (S (T T_INT) :: r52)
  | 86 -> One (S (T T_INT) :: r53)
  | 742 -> One (S (T T_IN) :: r629)
  | 746 -> One (S (T T_IN) :: r631)
  | 1387 -> One (S (T T_IN) :: r1090)
  | 637 -> One (S (T T_GREATERRBRACE) :: r520)
  | 1720 -> One (S (T T_GREATERRBRACE) :: r1206)
  | 210 -> One (S (T T_GREATER) :: r197)
  | 1843 -> One (S (T T_GREATER) :: r1255)
  | 604 -> One (S (T T_EQUAL) :: r509)
  | 819 -> One (S (T T_EQUAL) :: r670)
  | 825 -> One (S (T T_EQUAL) :: r673)
  | 835 -> One (S (T T_EQUAL) :: r678)
  | 1298 -> One (S (T T_EQUAL) :: r1017)
  | 1316 -> One (S (T T_EQUAL) :: r1022)
  | 1584 -> One (S (T T_EQUAL) :: r1156)
  | 1763 -> One (S (T T_EQUAL) :: r1227)
  | 1776 -> One (S (T T_EQUAL) :: r1234)
  | 1970 -> One (S (T T_EOF) :: r1291)
  | 1974 -> One (S (T T_EOF) :: r1292)
  | 1993 -> One (S (T T_EOF) :: r1298)
  | 1997 -> One (S (T T_EOF) :: r1299)
  | 2001 -> One (S (T T_EOF) :: r1300)
  | 2004 -> One (S (T T_EOF) :: r1301)
  | 2009 -> One (S (T T_EOF) :: r1302)
  | 2013 -> One (S (T T_EOF) :: r1303)
  | 2017 -> One (S (T T_EOF) :: r1304)
  | 2021 -> One (S (T T_EOF) :: r1305)
  | 2025 -> One (S (T T_EOF) :: r1306)
  | 2028 -> One (S (T T_EOF) :: r1307)
  | 2032 -> One (S (T T_EOF) :: r1308)
  | 2072 -> One (S (T T_EOF) :: r1323)
  | 1707 -> One (S (T T_END) :: r1205)
  | 128 -> One (S (T T_DOTDOT) :: r99)
  | 203 -> One (S (T T_DOTDOT) :: r187)
  | 1007 -> One (S (T T_DOTDOT) :: r814)
  | 1008 -> One (S (T T_DOTDOT) :: r815)
  | 229 | 1506 | 1553 -> One (S (T T_DOT) :: r223)
  | 324 -> One (S (T T_DOT) :: r314)
  | 341 -> One (S (T T_DOT) :: r326)
  | 396 -> One (S (T T_DOT) :: r345)
  | 515 -> One (S (T T_DOT) :: r430)
  | 545 -> One (S (T T_DOT) :: r450)
  | 2035 -> One (S (T T_DOT) :: r510)
  | 814 -> One (S (T T_DOT) :: r668)
  | 905 -> One (S (T T_DOT) :: r735)
  | 934 -> One (S (T T_DOT) :: r774)
  | 967 -> One (S (T T_DOT) :: r783)
  | 1282 -> One (S (T T_DOT) :: r998)
  | 1771 -> One (S (T T_DOT) :: r1232)
  | 1845 -> One (S (T T_DOT) :: r1260)
  | 1856 -> One (S (T T_DOT) :: r1269)
  | 1877 -> One (S (T T_DOT) :: r1277)
  | 1983 -> One (S (T T_DOT) :: r1297)
  | 204 | 919 -> One (S (T T_COLONCOLON) :: r189)
  | 560 -> One (S (T T_COLON) :: r475)
  | 1198 -> One (S (T T_COLON) :: r946)
  | 1831 -> One (S (T T_COLON) :: r1253)
  | 248 -> One (S (T T_BARRBRACKET) :: r239)
  | 411 -> One (S (T T_BARRBRACKET) :: r361)
  | 506 -> One (S (T T_BARRBRACKET) :: r418)
  | 1596 -> One (S (T T_BARRBRACKET) :: r1159)
  | 1598 -> One (S (T T_BARRBRACKET) :: r1160)
  | 1785 -> One (S (T T_BARRBRACKET) :: r1235)
  | 383 -> One (S (T T_BAR) :: r339)
  | 81 -> One (S (N N_pattern) :: r50)
  | 461 -> One (S (N N_pattern) :: r56)
  | 422 -> One (S (N N_pattern) :: r376)
  | 452 -> One (S (N N_pattern) :: r396)
  | 454 -> One (S (N N_pattern) :: r397)
  | 475 -> One (S (N N_pattern) :: r408)
  | 480 -> One (S (N N_pattern) :: r412)
  | 838 -> One (S (N N_pattern) :: r679)
  | 840 -> One (S (N N_pattern) :: r680)
  | 842 -> One (S (N N_pattern) :: r681)
  | 849 -> One (S (N N_pattern) :: r683)
  | 855 -> One (S (N N_pattern) :: r687)
  | 105 -> One (S (N N_module_type) :: r69)
  | 562 -> One (S (N N_module_type) :: r477)
  | 600 -> One (S (N N_module_type) :: r506)
  | 602 -> One (S (N N_module_type) :: r507)
  | 627 -> One (S (N N_module_type) :: r515)
  | 864 -> One (S (N N_module_type) :: r703)
  | 876 -> One (S (N N_module_type) :: r711)
  | 1795 -> One (S (N N_module_type) :: r1241)
  | 1810 -> One (S (N N_module_type) :: r1244)
  | 1813 -> One (S (N N_module_type) :: r1246)
  | 1816 -> One (S (N N_module_type) :: r1248)
  | 222 -> One (S (N N_module_expr) :: r211)
  | 511 -> One (S (N N_let_pattern) :: r424)
  | 512 -> One (S (N N_let_pattern) :: r427)
  | 251 -> One (S (N N_expr) :: r241)
  | 639 -> One (S (N N_expr) :: r523)
  | 643 -> One (S (N N_expr) :: r534)
  | 710 -> One (S (N N_expr) :: r610)
  | 735 -> One (S (N N_expr) :: r627)
  | 750 -> One (S (N N_expr) :: r632)
  | 752 -> One (S (N N_expr) :: r633)
  | 757 -> One (S (N N_expr) :: r634)
  | 764 -> One (S (N N_expr) :: r637)
  | 766 -> One (S (N N_expr) :: r638)
  | 768 -> One (S (N N_expr) :: r639)
  | 770 -> One (S (N N_expr) :: r640)
  | 772 -> One (S (N N_expr) :: r641)
  | 774 -> One (S (N N_expr) :: r642)
  | 776 -> One (S (N N_expr) :: r643)
  | 778 -> One (S (N N_expr) :: r644)
  | 780 -> One (S (N N_expr) :: r645)
  | 782 -> One (S (N N_expr) :: r646)
  | 784 -> One (S (N N_expr) :: r647)
  | 786 -> One (S (N N_expr) :: r648)
  | 788 -> One (S (N N_expr) :: r649)
  | 790 -> One (S (N N_expr) :: r650)
  | 792 -> One (S (N N_expr) :: r651)
  | 794 -> One (S (N N_expr) :: r652)
  | 796 -> One (S (N N_expr) :: r653)
  | 798 -> One (S (N N_expr) :: r654)
  | 800 -> One (S (N N_expr) :: r655)
  | 802 -> One (S (N N_expr) :: r656)
  | 1525 -> One (S (N N_expr) :: r1139)
  | 1530 -> One (S (N N_expr) :: r1143)
  | 1535 -> One (S (N N_expr) :: r1147)
  | 1541 -> One (S (N N_expr) :: r1148)
  | 1546 -> One (S (N N_expr) :: r1149)
  | 1551 -> One (S (N N_expr) :: r1150)
  | 1558 -> One (S (N N_expr) :: r1151)
  | 1563 -> One (S (N N_expr) :: r1152)
  | 1568 -> One (S (N N_expr) :: r1153)
  | 1571 -> One (S (N N_expr) :: r1154)
  | 1603 -> One (S (N N_expr) :: r1172)
  | 1614 -> One (S (N N_expr) :: r1176)
  | 1631 -> One (S (N N_expr) :: r1182)
  | 1704 -> One (S (N N_expr) :: r1204)
  | 249 -> One (Sub (r1) :: r240)
  | 409 -> One (Sub (r1) :: r353)
  | 658 -> One (Sub (r1) :: r552)
  | 857 -> One (Sub (r1) :: r688)
  | 1668 -> One (Sub (r1) :: r1195)
  | 1955 -> One (Sub (r1) :: r1289)
  | 1957 -> One (Sub (r1) :: r1290)
  | 2 -> One (Sub (r11) :: r12)
  | 57 -> One (Sub (r11) :: r13)
  | 61 -> One (Sub (r11) :: r18)
  | 212 -> One (Sub (r11) :: r200)
  | 760 -> One (Sub (r11) :: r636)
  | 853 -> One (Sub (r11) :: r686)
  | 894 -> One (Sub (r11) :: r720)
  | 896 -> One (Sub (r11) :: r723)
  | 1368 -> One (Sub (r11) :: r1075)
  | 656 -> One (Sub (r33) :: r549)
  | 1698 -> One (Sub (r33) :: r1203)
  | 1953 -> One (Sub (r35) :: r1288)
  | 77 -> One (Sub (r42) :: r43)
  | 642 -> One (Sub (r42) :: r532)
  | 677 -> One (Sub (r42) :: r588)
  | 706 -> One (Sub (r42) :: r605)
  | 726 -> One (Sub (r42) :: r621)
  | 1391 -> One (Sub (r42) :: r1091)
  | 872 -> One (Sub (r63) :: r708)
  | 1074 -> One (Sub (r63) :: r852)
  | 981 -> One (Sub (r72) :: r788)
  | 257 -> One (Sub (r77) :: r253)
  | 482 -> One (Sub (r77) :: r413)
  | 844 -> One (Sub (r77) :: r682)
  | 296 -> One (Sub (r79) :: r298)
  | 304 -> One (Sub (r79) :: r300)
  | 925 -> One (Sub (r79) :: r754)
  | 947 -> One (Sub (r79) :: r775)
  | 951 -> One (Sub (r79) :: r778)
  | 953 -> One (Sub (r79) :: r779)
  | 966 -> One (Sub (r79) :: r781)
  | 1680 -> One (Sub (r79) :: r1201)
  | 205 -> One (Sub (r81) :: r192)
  | 289 -> One (Sub (r81) :: r295)
  | 290 -> One (Sub (r81) :: r296)
  | 293 -> One (Sub (r81) :: r297)
  | 308 -> One (Sub (r81) :: r303)
  | 331 -> One (Sub (r81) :: r316)
  | 339 -> One (Sub (r81) :: r321)
  | 346 -> One (Sub (r81) :: r327)
  | 352 -> One (Sub (r81) :: r328)
  | 358 -> One (Sub (r81) :: r329)
  | 1206 -> One (Sub (r81) :: r951)
  | 1850 -> One (Sub (r81) :: r1261)
  | 1854 -> One (Sub (r81) :: r1264)
  | 1861 -> One (Sub (r81) :: r1270)
  | 1867 -> One (Sub (r81) :: r1271)
  | 1873 -> One (Sub (r81) :: r1272)
  | 1882 -> One (Sub (r81) :: r1278)
  | 1892 -> One (Sub (r81) :: r1279)
  | 375 -> One (Sub (r85) :: r336)
  | 579 -> One (Sub (r85) :: r500)
  | 263 -> One (Sub (r87) :: r261)
  | 316 -> One (Sub (r87) :: r307)
  | 337 -> One (Sub (r87) :: r318)
  | 419 -> One (Sub (r87) :: r375)
  | 514 -> One (Sub (r87) :: r428)
  | 582 -> One (Sub (r87) :: r503)
  | 699 -> One (Sub (r87) :: r601)
  | 715 -> One (Sub (r87) :: r618)
  | 719 -> One (Sub (r87) :: r619)
  | 831 -> One (Sub (r87) :: r676)
  | 1115 -> One (Sub (r87) :: r894)
  | 1153 -> One (Sub (r87) :: r925)
  | 1943 -> One (Sub (r87) :: r1287)
  | 1324 -> One (Sub (r89) :: r1036)
  | 1348 -> One (Sub (r89) :: r1047)
  | 189 -> One (Sub (r105) :: r182)
  | 325 -> One (Sub (r105) :: r315)
  | 2038 -> One (Sub (r105) :: r1309)
  | 536 -> One (Sub (r117) :: r447)
  | 427 -> One (Sub (r134) :: r384)
  | 195 -> One (Sub (r177) :: r183)
  | 182 -> One (Sub (r179) :: r181)
  | 1107 -> One (Sub (r179) :: r888)
  | 199 -> One (Sub (r185) :: r186)
  | 988 -> One (Sub (r185) :: r807)
  | 1037 -> One (Sub (r185) :: r822)
  | 260 -> One (Sub (r256) :: r258)
  | 261 -> One (Sub (r256) :: r260)
  | 406 -> One (Sub (r256) :: r349)
  | 407 -> One (Sub (r256) :: r352)
  | 368 -> One (Sub (r263) :: r330)
  | 269 -> One (Sub (r265) :: r271)
  | 284 -> One (Sub (r265) :: r294)
  | 270 -> One (Sub (r277) :: r279)
  | 271 -> One (Sub (r281) :: r282)
  | 300 -> One (Sub (r281) :: r299)
  | 313 -> One (Sub (r281) :: r306)
  | 274 -> One (Sub (r290) :: r292)
  | 608 -> One (Sub (r290) :: r511)
  | 1071 -> One (Sub (r290) :: r847)
  | 391 -> One (Sub (r341) :: r343)
  | 1590 -> One (Sub (r355) :: r1157)
  | 410 -> One (Sub (r357) :: r360)
  | 414 -> One (Sub (r372) :: r374)
  | 531 -> One (Sub (r379) :: r440)
  | 438 -> One (Sub (r387) :: r388)
  | 462 -> One (Sub (r401) :: r404)
  | 663 -> One (Sub (r401) :: r567)
  | 808 -> One (Sub (r401) :: r664)
  | 1325 -> One (Sub (r401) :: r1041)
  | 1349 -> One (Sub (r401) :: r1052)
  | 1677 -> One (Sub (r401) :: r1198)
  | 509 -> One (Sub (r420) :: r421)
  | 824 -> One (Sub (r435) :: r671)
  | 612 -> One (Sub (r491) :: r512)
  | 571 -> One (Sub (r493) :: r494)
  | 640 -> One (Sub (r529) :: r531)
  | 1641 -> One (Sub (r529) :: r1187)
  | 1684 -> One (Sub (r557) :: r1202)
  | 888 -> One (Sub (r691) :: r717)
  | 1908 -> One (Sub (r736) :: r1283)
  | 1920 -> One (Sub (r736) :: r1285)
  | 924 -> One (Sub (r752) :: r753)
  | 927 -> One (Sub (r764) :: r766)
  | 990 -> One (Sub (r764) :: r809)
  | 1009 -> One (Sub (r764) :: r817)
  | 1017 -> One (Sub (r764) :: r819)
  | 1896 -> One (Sub (r764) :: r1281)
  | 1095 -> One (Sub (r834) :: r863)
  | 1088 -> One (Sub (r860) :: r862)
  | 1415 -> One (Sub (r868) :: r1096)
  | 1439 -> One (Sub (r868) :: r1105)
  | 1379 -> One (Sub (r920) :: r1082)
  | 1366 -> One (Sub (r984) :: r1065)
  | 1443 -> One (Sub (r987) :: r1106)
  | 1290 -> One (Sub (r1008) :: r1010)
  | 1319 -> One (Sub (r1027) :: r1029)
  | 1608 -> One (Sub (r1166) :: r1173)
  | 1601 -> One (Sub (r1168) :: r1170)
  | 1629 -> One (Sub (r1168) :: r1180)
  | 1638 -> One (Sub (r1183) :: r1185)
  | 1768 -> One (Sub (r1221) :: r1228)
  | 749 -> One (r0)
  | 1969 -> One (r2)
  | 1968 -> One (r3)
  | 1967 -> One (r4)
  | 1966 -> One (r5)
  | 1965 -> One (r6)
  | 60 -> One (r7)
  | 55 -> One (r8)
  | 56 -> One (r10)
  | 59 -> One (r12)
  | 58 -> One (r13)
  | 1485 -> One (r14)
  | 1964 -> One (r16)
  | 1963 -> One (r17)
  | 62 -> One (r18)
  | 1962 -> One (r19)
  | 1961 -> One (r20)
  | 1960 -> One (r21)
  | 1959 -> One (r22)
  | 65 -> One (r23)
  | 64 -> One (r24)
  | 66 -> One (r25)
  | 67 -> One (r26)
  | 1952 -> One (r27)
  | 70 -> One (r28)
  | 69 -> One (r29)
  | 1695 -> One (r30)
  | 1693 -> One (r31)
  | 657 -> One (r32)
  | 1700 -> One (r34)
  | 1951 -> One (r36)
  | 1950 -> One (r37)
  | 1949 -> One (r38)
  | 73 -> One (r39)
  | 72 -> One (r40)
  | 76 -> One (r41)
  | 1789 -> One (r43)
  | 1948 -> One (r44)
  | 1947 -> One (r45)
  | 1946 -> One (r46)
  | 80 -> One (r47)
  | 79 -> One (r48)
  | 1942 -> One (r49)
  | 1941 -> One (r50)
  | 82 -> One (r51)
  | 84 -> One (r52)
  | 87 -> One (r53)
  | 91 -> One (r54)
  | 474 -> One (r55)
  | 473 | 543 | 1280 -> One (r56)
  | 146 -> One (r57)
  | 148 -> One (r59)
  | 147 -> One (r60)
  | 112 -> One (r61)
  | 101 -> One (r62)
  | 104 -> One (r64)
  | 103 -> One (r65)
  | 100 -> One (r66)
  | 99 -> One (r67)
  | 1940 -> One (r68)
  | 1939 -> One (r69)
  | 106 | 153 -> One (r70)
  | 1268 -> One (r71)
  | 1938 -> One (r73)
  | 1937 -> One (r74)
  | 108 -> One (r75)
  | 149 | 250 | 641 | 1656 -> One (r76)
  | 152 -> One (r78)
  | 303 -> One (r80)
  | 288 -> One (r82)
  | 317 -> One (r84)
  | 320 -> One (r86)
  | 914 -> One (r88)
  | 1936 -> One (r90)
  | 1935 -> One (r91)
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
  | 202 -> One (r106)
  | 201 -> One (r107)
  | 538 -> One (r108)
  | 537 -> One (r109)
  | 160 -> One (r110)
  | 159 -> One (r111)
  | 1934 -> One (r112)
  | 1933 -> One (r113)
  | 165 -> One (r114)
  | 164 -> One (r115)
  | 163 -> One (r116)
  | 1894 -> One (r118)
  | 1891 -> One (r119)
  | 1890 -> One (r120)
  | 1889 -> One (r121)
  | 1888 -> One (r122)
  | 1887 -> One (r123)
  | 1932 -> One (r124)
  | 169 -> One (r125)
  | 168 -> One (r126)
  | 167 -> One (r127)
  | 1931 -> One (r128)
  | 1930 -> One (r129)
  | 172 -> One (r130)
  | 272 -> One (r131)
  | 297 -> One (r133)
  | 430 -> One (r135)
  | 980 -> One (r137)
  | 1016 -> One (r139)
  | 1015 -> One (r140)
  | 1014 | 1919 -> One (r141)
  | 1915 -> One (r143)
  | 1929 -> One (r145)
  | 1928 -> One (r146)
  | 1927 -> One (r147)
  | 1926 -> One (r148)
  | 1925 -> One (r149)
  | 1043 -> One (r153)
  | 1042 -> One (r154)
  | 1041 -> One (r155)
  | 1912 -> One (r161)
  | 1911 -> One (r162)
  | 1905 -> One (r163)
  | 1904 -> One (r164)
  | 1903 -> One (r165)
  | 1025 -> One (r167)
  | 1024 -> One (r168)
  | 1023 -> One (r169)
  | 188 -> One (r173)
  | 191 -> One (r175)
  | 187 -> One (r176)
  | 192 -> One (r178)
  | 194 -> One (r180)
  | 193 -> One (r181)
  | 190 -> One (r182)
  | 196 -> One (r183)
  | 993 -> One (r184)
  | 1902 -> One (r186)
  | 1899 -> One (r187)
  | 921 -> One (r188)
  | 920 -> One (r189)
  | 306 -> One (r190)
  | 292 -> One (r191)
  | 1886 -> One (r192)
  | 1875 -> One (r193)
  | 1872 -> One (r194)
  | 1871 -> One (r195)
  | 208 -> One (r196)
  | 1842 -> One (r197)
  | 1830 -> One (r198)
  | 1829 -> One (r199)
  | 213 -> One (r200)
  | 1828 -> One (r201)
  | 1824 -> One (r202)
  | 1823 -> One (r203)
  | 1822 -> One (r204)
  | 1821 -> One (r205)
  | 1820 -> One (r206)
  | 1819 -> One (r207)
  | 221 -> One (r208)
  | 220 -> One (r209)
  | 626 -> One (r210)
  | 625 -> One (r211)
  | 1809 -> One (r212)
  | 1808 -> One (r213)
  | 224 -> One (r214)
  | 228 -> One (r215)
  | 234 -> One (r217)
  | 235 -> One (r219)
  | 227 -> One (r220)
  | 226 -> One (r221)
  | 232 -> One (r222)
  | 230 -> One (r223)
  | 231 -> One (r224)
  | 233 -> One (r225)
  | 237 -> One (r226)
  | 1807 -> One (r227)
  | 1806 -> One (r228)
  | 1805 -> One (r229)
  | 242 -> One (r230)
  | 241 -> One (r231)
  | 1804 -> One (r232)
  | 1803 -> One (r233)
  | 1802 -> One (r234)
  | 245 -> One (r235)
  | 244 -> One (r236)
  | 1799 -> One (r237)
  | 1798 -> One (r238)
  | 1784 -> One (r239)
  | 1783 -> One (r240)
  | 1782 -> One (r241)
  | 806 -> One (r242)
  | 1781 -> One (r244)
  | 1780 -> One (r245)
  | 256 -> One (r246)
  | 254 -> One (r247)
  | 253 -> One (r248)
  | 1762 -> One (r249)
  | 1761 -> One (r250)
  | 1779 -> One (r252)
  | 258 -> One (r253)
  | 404 -> One (r254)
  | 262 -> One (r255)
  | 403 -> One (r257)
  | 402 -> One (r258)
  | 401 -> One (r259)
  | 400 -> One (r260)
  | 399 -> One (r261)
  | 380 -> One (r262)
  | 365 -> One (r264)
  | 390 -> One (r266)
  | 389 -> One (r267)
  | 266 -> One (r268)
  | 268 -> One (r269)
  | 388 -> One (r270)
  | 387 -> One (r271)
  | 286 -> One (r272)
  | 285 -> One (r273)
  | 379 -> One (r275)
  | 370 -> One (r276)
  | 382 -> One (r278)
  | 381 -> One (r279)
  | 282 | 1209 -> One (r280)
  | 283 -> One (r282)
  | 278 -> One (r283)
  | 277 -> One (r284)
  | 281 -> One (r286)
  | 279 -> One (r289)
  | 276 -> One (r291)
  | 275 -> One (r292)
  | 367 -> One (r293)
  | 366 -> One (r294)
  | 362 -> One (r295)
  | 291 -> One (r296)
  | 294 -> One (r297)
  | 299 -> One (r298)
  | 301 -> One (r299)
  | 305 -> One (r300)
  | 360 -> One (r301)
  | 357 -> One (r302)
  | 356 -> One (r303)
  | 315 -> One (r304)
  | 312 -> One (r305)
  | 314 -> One (r306)
  | 318 -> One (r307)
  | 323 -> One (r308)
  | 322 -> One (r309)
  | 333 -> One (r310)
  | 330 -> One (r311)
  | 329 -> One (r312)
  | 328 -> One (r313)
  | 327 -> One (r314)
  | 326 -> One (r315)
  | 332 -> One (r316)
  | 336 -> One (r317)
  | 338 -> One (r318)
  | 354 -> One (r319)
  | 351 -> One (r320)
  | 350 -> One (r321)
  | 348 -> One (r322)
  | 345 -> One (r323)
  | 344 -> One (r324)
  | 343 -> One (r325)
  | 342 -> One (r326)
  | 347 -> One (r327)
  | 353 -> One (r328)
  | 359 -> One (r329)
  | 369 -> One (r330)
  | 378 -> One (r331)
  | 377 -> One (r333)
  | 374 -> One (r334)
  | 373 -> One (r335)
  | 376 -> One (r336)
  | 386 -> One (r337)
  | 385 -> One (r338)
  | 384 -> One (r339)
  | 395 -> One (r340)
  | 393 -> One (r342)
  | 392 -> One (r343)
  | 398 -> One (r344)
  | 397 -> One (r345)
  | 1756 -> One (r346)
  | 1755 -> One (r347)
  | 1754 -> One (r348)
  | 1753 -> One (r349)
  | 1752 -> One (r350)
  | 1751 -> One (r351)
  | 408 -> One (r352)
  | 1750 -> One (r353)
  | 508 -> One (r354)
  | 1592 -> One (r356)
  | 1589 -> One (r358)
  | 1588 -> One (r359)
  | 1587 -> One (r360)
  | 505 -> One (r361)
  | 413 -> One (r362)
  | 494 -> One (r363)
  | 493 -> One (r365)
  | 492 -> One (r366)
  | 415 -> One (r367)
  | 499 -> One (r369)
  | 421 -> One (r370)
  | 418 -> One (r371)
  | 417 -> One (r373)
  | 416 -> One (r374)
  | 420 -> One (r375)
  | 498 -> One (r376)
  | 434 | 830 -> One (r378)
  | 435 -> One (r380)
  | 425 -> One (r381)
  | 424 -> One (r382)
  | 426 -> One (r383)
  | 428 -> One (r384)
  | 440 -> One (r386)
  | 439 -> One (r388)
  | 491 -> One (r389)
  | 490 -> One (r390)
  | 443 -> One (r391)
  | 445 -> One (r392)
  | 485 -> One (r393)
  | 448 -> One (r394)
  | 447 -> One (r395)
  | 453 -> One (r396)
  | 455 -> One (r397)
  | 458 -> One (r398)
  | 484 -> One (r399)
  | 463 -> One (r400)
  | 467 -> One (r402)
  | 466 -> One (r403)
  | 465 -> One (r404)
  | 469 -> One (r405)
  | 472 -> One (r406)
  | 471 -> One (r407)
  | 476 -> One (r408)
  | 479 -> One (r409)
  | 478 -> One (r410)
  | 477 | 544 | 1281 -> One (r411)
  | 481 -> One (r412)
  | 483 -> One (r413)
  | 487 -> One (r414)
  | 486 -> One (r415)
  | 489 -> One (r416)
  | 503 -> One (r417)
  | 507 -> One (r418)
  | 510 -> One (r419)
  | 526 -> One (r421)
  | 524 -> One (r422)
  | 523 -> One (r423)
  | 522 -> One (r424)
  | 521 -> One (r425)
  | 520 -> One (r426)
  | 519 -> One (r427)
  | 518 -> One (r428)
  | 517 -> One (r429)
  | 516 -> One (r430)
  | 1747 -> One (r431)
  | 550 -> One (r432)
  | 828 -> One (r434)
  | 1748 -> One (r436)
  | 530 -> One (r437)
  | 529 -> One (r438)
  | 528 -> One (r439)
  | 549 -> One (r440)
  | 535 -> One (r441)
  | 534 -> One (r442)
  | 533 -> One (r443)
  | 542 -> One (r444)
  | 541 -> One (r445)
  | 540 -> One (r446)
  | 539 -> One (r447)
  | 548 -> One (r448)
  | 547 -> One (r449)
  | 546 -> One (r450)
  | 1731 -> One (r451)
  | 1730 -> One (r452)
  | 1729 -> One (r453)
  | 1728 -> One (r454)
  | 1727 -> One (r455)
  | 552 -> One (r456)
  | 1463 -> One (r457)
  | 1462 -> One (r458)
  | 1461 -> One (r459)
  | 1460 -> One (r460)
  | 1459 -> One (r461)
  | 1458 -> One (r462)
  | 1726 -> One (r463)
  | 635 -> One (r464)
  | 634 -> One (r465)
  | 555 -> One (r466)
  | 554 -> One (r467)
  | 622 -> One (r468)
  | 620 -> One (r469)
  | 619 -> One (r470)
  | 557 -> One (r471)
  | 559 -> One (r472)
  | 618 -> One (r473)
  | 617 -> One (r474)
  | 561 -> One (r475)
  | 616 -> One (r476)
  | 615 -> One (r477)
  | 570 -> One (r478)
  | 568 -> One (r479)
  | 567 -> One (r480)
  | 564 -> One (r481)
  | 598 -> One (r482)
  | 597 -> One (r484)
  | 591 -> One (r486)
  | 590 -> One (r487)
  | 589 -> One (r488)
  | 588 -> One (r489)
  | 587 -> One (r490)
  | 610 -> One (r492)
  | 611 -> One (r494)
  | 578 -> One (r495)
  | 577 -> One (r496)
  | 574 -> One (r497)
  | 573 -> One (r498)
  | 581 -> One (r499)
  | 580 -> One (r500)
  | 585 -> One (r501)
  | 584 -> One (r502)
  | 583 -> One (r503)
  | 596 -> One (r504)
  | 601 -> One (r506)
  | 603 -> One (r507)
  | 606 -> One (r508)
  | 605 -> One (r509)
  | 607 | 2036 -> One (r510)
  | 609 -> One (r511)
  | 613 -> One (r512)
  | 624 -> One (r513)
  | 629 -> One (r514)
  | 628 -> One (r515)
  | 1511 | 1600 | 1628 | 1724 | 1787 -> One (r516)
  | 1723 -> One (r518)
  | 1722 -> One (r519)
  | 1719 -> One (r520)
  | 1716 -> One (r521)
  | 638 -> One (r522)
  | 1715 -> One (r523)
  | 1648 -> One (r524)
  | 1647 -> One (r525)
  | 1645 -> One (r526)
  | 1651 -> One (r528)
  | 1714 -> One (r530)
  | 1713 -> One (r531)
  | 1712 -> One (r532)
  | 1711 -> One (r533)
  | 1710 -> One (r534)
  | 1709 -> One (r535)
  | 646 -> One (r536)
  | 645 -> One (r537)
  | 1706 -> One (r538)
  | 649 -> One (r539)
  | 648 -> One (r540)
  | 1703 -> One (r541)
  | 1702 -> One (r542)
  | 1701 -> One (r543)
  | 652 -> One (r544)
  | 651 -> One (r545)
  | 1697 -> One (r546)
  | 655 -> One (r547)
  | 654 -> One (r548)
  | 1696 -> One (r549)
  | 1692 -> One (r550)
  | 1691 -> One (r551)
  | 1690 -> One (r552)
  | 823 -> One (r553)
  | 1675 -> One (r555)
  | 666 -> One (r556)
  | 1689 -> One (r558)
  | 1688 -> One (r559)
  | 661 -> One (r560)
  | 660 -> One (r561)
  | 1279 -> One (r562)
  | 1278 -> One (r563)
  | 1277 -> One (r564)
  | 1687 -> One (r565)
  | 665 -> One (r566)
  | 664 -> One (r567)
  | 1667 -> One (r568)
  | 1666 -> One (r569)
  | 1665 -> One (r570)
  | 1664 -> One (r571)
  | 671 -> One (r572)
  | 670 -> One (r573)
  | 669 -> One (r574)
  | 668 -> One (r575)
  | 1617 -> One (r576)
  | 1663 -> One (r578)
  | 1662 -> One (r579)
  | 1661 -> One (r580)
  | 1660 -> One (r581)
  | 1659 -> One (r582)
  | 1658 -> One (r583)
  | 676 -> One (r584)
  | 675 -> One (r585)
  | 674 -> One (r586)
  | 673 -> One (r587)
  | 680 -> One (r588)
  | 685 -> One (r589)
  | 684 -> One (r590)
  | 683 | 1655 -> One (r591)
  | 1654 -> One (r592)
  | 694 -> One (r593)
  | 693 -> One (r594)
  | 692 -> One (r595)
  | 691 -> One (r596)
  | 690 -> One (r597)
  | 689 -> One (r598)
  | 1583 -> One (r599)
  | 701 -> One (r600)
  | 700 -> One (r601)
  | 705 -> One (r602)
  | 704 -> One (r603)
  | 703 -> One (r604)
  | 707 -> One (r605)
  | 1524 | 1576 -> One (r606)
  | 1523 | 1575 -> One (r607)
  | 709 | 1522 -> One (r608)
  | 708 | 1521 -> One (r609)
  | 1574 -> One (r610)
  | 723 -> One (r611)
  | 718 -> One (r612)
  | 717 | 807 | 1770 -> One (r613)
  | 722 -> One (r615)
  | 721 -> One (r616)
  | 714 -> One (r617)
  | 716 -> One (r618)
  | 720 -> One (r619)
  | 725 -> One (r620)
  | 727 -> One (r621)
  | 729 -> One (r622)
  | 733 | 1540 -> One (r623)
  | 732 | 1539 -> One (r624)
  | 731 | 1538 -> One (r625)
  | 730 | 1537 -> One (r626)
  | 1499 -> One (r627)
  | 744 -> One (r628)
  | 743 -> One (r629)
  | 748 -> One (r630)
  | 747 -> One (r631)
  | 751 -> One (r632)
  | 753 -> One (r633)
  | 758 -> One (r634)
  | 762 -> One (r635)
  | 761 -> One (r636)
  | 765 -> One (r637)
  | 767 -> One (r638)
  | 769 -> One (r639)
  | 771 -> One (r640)
  | 773 -> One (r641)
  | 775 -> One (r642)
  | 777 -> One (r643)
  | 779 -> One (r644)
  | 781 -> One (r645)
  | 783 -> One (r646)
  | 785 -> One (r647)
  | 787 -> One (r648)
  | 789 -> One (r649)
  | 791 -> One (r650)
  | 793 -> One (r651)
  | 795 -> One (r652)
  | 797 -> One (r653)
  | 799 -> One (r654)
  | 801 -> One (r655)
  | 803 -> One (r656)
  | 1498 -> One (r657)
  | 852 -> One (r658)
  | 805 -> One (r659)
  | 813 -> One (r660)
  | 812 -> One (r661)
  | 811 -> One (r662)
  | 810 -> One (r663)
  | 809 -> One (r664)
  | 818 -> One (r665)
  | 817 -> One (r666)
  | 816 -> One (r667)
  | 815 -> One (r668)
  | 821 -> One (r669)
  | 820 -> One (r670)
  | 829 -> One (r671)
  | 827 -> One (r672)
  | 826 -> One (r673)
  | 834 -> One (r674)
  | 833 -> One (r675)
  | 832 -> One (r676)
  | 837 -> One (r677)
  | 836 -> One (r678)
  | 839 -> One (r679)
  | 841 -> One (r680)
  | 843 -> One (r681)
  | 845 -> One (r682)
  | 850 -> One (r683)
  | 1497 -> One (r684)
  | 1496 -> One (r685)
  | 854 -> One (r686)
  | 856 -> One (r687)
  | 858 -> One (r688)
  | 875 -> One (r689)
  | 874 -> One (r690)
  | 893 -> One (r692)
  | 892 -> One (r693)
  | 891 -> One (r694)
  | 871 -> One (r695)
  | 870 -> One (r696)
  | 869 -> One (r697)
  | 866 -> One (r698)
  | 863 -> One (r699)
  | 862 -> One (r700)
  | 861 -> One (r701)
  | 860 -> One (r702)
  | 865 -> One (r703)
  | 868 -> One (r704)
  | 890 -> One (r705)
  | 881 -> One (r706)
  | 880 -> One (r707)
  | 873 -> One (r708)
  | 879 -> One (r709)
  | 878 -> One (r710)
  | 877 -> One (r711)
  | 887 -> One (r712)
  | 886 -> One (r713)
  | 885 -> One (r714)
  | 884 -> One (r715)
  | 883 -> One (r716)
  | 889 -> One (r717)
  | 1495 -> One (r718)
  | 1494 -> One (r719)
  | 895 -> One (r720)
  | 1490 -> One (r721)
  | 1489 -> One (r722)
  | 897 -> One (r723)
  | 910 -> One (r724)
  | 913 -> One (r726)
  | 912 -> One (r727)
  | 909 -> One (r728)
  | 908 -> One (r729)
  | 904 -> One (r730)
  | 903 -> One (r731)
  | 902 -> One (r732)
  | 901 -> One (r733)
  | 907 -> One (r734)
  | 906 -> One (r735)
  | 965 -> One (r737)
  | 964 -> One (r738)
  | 963 -> One (r739)
  | 958 -> One (r740)
  | 979 -> One (r744)
  | 978 -> One (r745)
  | 977 -> One (r746)
  | 1100 -> One (r747)
  | 1099 -> One (r748)
  | 1098 -> One (r749)
  | 1097 -> One (r750)
  | 957 -> One (r751)
  | 956 -> One (r753)
  | 926 -> One (r754)
  | 941 -> One (r755)
  | 946 -> One (r763)
  | 943 -> One (r765)
  | 942 -> One (r766)
  | 940 -> One (r767)
  | 939 -> One (r768)
  | 938 -> One (r769)
  | 937 -> One (r770)
  | 933 -> One (r771)
  | 932 -> One (r772)
  | 936 -> One (r773)
  | 935 -> One (r774)
  | 948 -> One (r775)
  | 955 -> One (r776)
  | 950 -> One (r777)
  | 952 -> One (r778)
  | 954 -> One (r779)
  | 962 -> One (r780)
  | 976 -> One (r781)
  | 972 -> One (r782)
  | 968 -> One (r783)
  | 971 -> One (r784)
  | 970 -> One (r785)
  | 975 -> One (r786)
  | 974 -> One (r787)
  | 1267 -> One (r788)
  | 1033 -> One (r789)
  | 1048 -> One (r791)
  | 1047 -> One (r792)
  | 1046 -> One (r793)
  | 1045 -> One (r794)
  | 1044 -> One (r795)
  | 1031 -> One (r799)
  | 1030 -> One (r800)
  | 1029 -> One (r801)
  | 1027 -> One (r802)
  | 1026 -> One (r803)
  | 1003 -> One (r805)
  | 1002 -> One (r806)
  | 1001 -> One (r807)
  | 992 -> One (r808)
  | 991 -> One (r809)
  | 997 -> One (r810)
  | 996 -> One (r811)
  | 995 | 1907 -> One (r812)
  | 999 | 1906 -> One (r813)
  | 1020 -> One (r814)
  | 1012 -> One (r815)
  | 1011 -> One (r816)
  | 1010 -> One (r817)
  | 1019 -> One (r818)
  | 1018 -> One (r819)
  | 1040 -> One (r820)
  | 1039 -> One (r821)
  | 1038 -> One (r822)
  | 1266 -> One (r823)
  | 1059 -> One (r824)
  | 1058 -> One (r825)
  | 1057 -> One (r826)
  | 1056 -> One (r827)
  | 1055 -> One (r828)
  | 1054 -> One (r829)
  | 1053 -> One (r830)
  | 1052 -> One (r831)
  | 1092 -> One (r832)
  | 1091 -> One (r833)
  | 1094 -> One (r835)
  | 1093 -> One (r836)
  | 1087 -> One (r837)
  | 1069 -> One (r838)
  | 1068 -> One (r839)
  | 1067 -> One (r840)
  | 1066 -> One (r841)
  | 1065 -> One (r842)
  | 1073 -> One (r846)
  | 1072 -> One (r847)
  | 1086 -> One (r848)
  | 1078 -> One (r849)
  | 1077 -> One (r850)
  | 1076 -> One (r851)
  | 1075 -> One (r852)
  | 1085 -> One (r853)
  | 1084 -> One (r854)
  | 1083 -> One (r855)
  | 1082 -> One (r856)
  | 1081 -> One (r857)
  | 1080 -> One (r858)
  | 1090 -> One (r861)
  | 1089 -> One (r862)
  | 1096 -> One (r863)
  | 1156 | 1210 -> One (r865)
  | 1212 -> One (r867)
  | 1226 -> One (r869)
  | 1216 -> One (r870)
  | 1215 -> One (r871)
  | 1197 -> One (r872)
  | 1196 -> One (r873)
  | 1195 -> One (r874)
  | 1194 -> One (r875)
  | 1193 -> One (r876)
  | 1192 -> One (r877)
  | 1191 -> One (r878)
  | 1181 -> One (r879)
  | 1180 -> One (r880)
  | 1112 -> One (r881)
  | 1111 -> One (r882)
  | 1110 -> One (r883)
  | 1106 -> One (r884)
  | 1104 -> One (r885)
  | 1103 -> One (r886)
  | 1109 -> One (r887)
  | 1108 -> One (r888)
  | 1174 -> One (r889)
  | 1173 -> One (r890)
  | 1118 -> One (r891)
  | 1114 -> One (r892)
  | 1117 -> One (r893)
  | 1116 -> One (r894)
  | 1129 -> One (r895)
  | 1128 -> One (r896)
  | 1127 -> One (r897)
  | 1126 -> One (r898)
  | 1125 -> One (r899)
  | 1120 -> One (r900)
  | 1140 -> One (r901)
  | 1139 -> One (r902)
  | 1138 -> One (r903)
  | 1137 -> One (r904)
  | 1136 -> One (r905)
  | 1131 -> One (r906)
  | 1165 -> One (r907)
  | 1164 -> One (r908)
  | 1142 -> One (r909)
  | 1163 -> One (r910)
  | 1162 -> One (r911)
  | 1161 -> One (r912)
  | 1160 -> One (r913)
  | 1144 -> One (r914)
  | 1158 -> One (r915)
  | 1148 -> One (r916)
  | 1147 -> One (r917)
  | 1146 -> One (r918)
  | 1155 | 1203 -> One (r919)
  | 1152 -> One (r921)
  | 1151 -> One (r922)
  | 1150 -> One (r923)
  | 1149 | 1202 -> One (r924)
  | 1154 -> One (r925)
  | 1170 -> One (r926)
  | 1169 -> One (r927)
  | 1168 -> One (r928)
  | 1172 -> One (r930)
  | 1171 -> One (r931)
  | 1167 -> One (r932)
  | 1176 -> One (r933)
  | 1179 -> One (r934)
  | 1190 -> One (r935)
  | 1189 -> One (r936)
  | 1188 -> One (r937)
  | 1187 -> One (r938)
  | 1186 -> One (r939)
  | 1185 -> One (r940)
  | 1184 -> One (r941)
  | 1183 -> One (r942)
  | 1214 -> One (r943)
  | 1201 -> One (r944)
  | 1200 -> One (r945)
  | 1199 -> One (r946)
  | 1213 -> One (r947)
  | 1205 -> One (r948)
  | 1211 -> One (r949)
  | 1208 -> One (r950)
  | 1207 -> One (r951)
  | 1225 -> One (r952)
  | 1224 -> One (r953)
  | 1223 -> One (r954)
  | 1222 -> One (r955)
  | 1221 -> One (r956)
  | 1220 -> One (r957)
  | 1219 -> One (r958)
  | 1218 -> One (r959)
  | 1235 -> One (r960)
  | 1237 -> One (r961)
  | 1242 -> One (r962)
  | 1241 -> One (r963)
  | 1240 -> One (r964)
  | 1239 -> One (r965)
  | 1252 -> One (r966)
  | 1251 -> One (r967)
  | 1250 -> One (r968)
  | 1249 -> One (r969)
  | 1248 -> One (r970)
  | 1247 -> One (r971)
  | 1246 -> One (r972)
  | 1245 -> One (r973)
  | 1263 -> One (r974)
  | 1262 -> One (r975)
  | 1261 -> One (r976)
  | 1260 -> One (r977)
  | 1259 -> One (r978)
  | 1258 -> One (r979)
  | 1257 -> One (r980)
  | 1256 -> One (r981)
  | 1255 -> One (r982)
  | 1389 -> One (r983)
  | 1438 -> One (r985)
  | 1286 -> One (r986)
  | 1455 -> One (r988)
  | 1446 -> One (r989)
  | 1445 -> One (r990)
  | 1275 -> One (r991)
  | 1274 -> One (r992)
  | 1273 -> One (r993)
  | 1272 -> One (r994)
  | 1271 -> One (r995)
  | 1285 -> One (r996)
  | 1284 -> One (r997)
  | 1283 -> One (r998)
  | 1432 -> One (r999)
  | 1431 -> One (r1000)
  | 1289 -> One (r1001)
  | 1288 -> One (r1002)
  | 1315 -> One (r1003)
  | 1314 -> One (r1004)
  | 1313 -> One (r1005)
  | 1312 -> One (r1006)
  | 1303 -> One (r1007)
  | 1302 -> One (r1009)
  | 1301 -> One (r1010)
  | 1297 -> One (r1011)
  | 1296 -> One (r1012)
  | 1295 -> One (r1013)
  | 1294 -> One (r1014)
  | 1292 -> One (r1015)
  | 1300 -> One (r1016)
  | 1299 -> One (r1017)
  | 1311 -> One (r1018)
  | 1310 -> One (r1019)
  | 1309 -> One (r1020)
  | 1318 -> One (r1021)
  | 1317 -> One (r1022)
  | 1358 -> One (r1023)
  | 1347 -> One (r1024)
  | 1346 -> One (r1025)
  | 1337 -> One (r1026)
  | 1336 -> One (r1028)
  | 1335 -> One (r1029)
  | 1334 -> One (r1030)
  | 1323 -> One (r1031)
  | 1322 -> One (r1032)
  | 1321 -> One (r1033)
  | 1333 -> One (r1034)
  | 1332 -> One (r1035)
  | 1331 -> One (r1036)
  | 1330 -> One (r1037)
  | 1329 -> One (r1038)
  | 1328 -> One (r1039)
  | 1327 -> One (r1040)
  | 1326 -> One (r1041)
  | 1345 -> One (r1042)
  | 1344 -> One (r1043)
  | 1343 -> One (r1044)
  | 1357 -> One (r1045)
  | 1356 -> One (r1046)
  | 1355 -> One (r1047)
  | 1354 -> One (r1048)
  | 1353 -> One (r1049)
  | 1352 -> One (r1050)
  | 1351 -> One (r1051)
  | 1350 -> One (r1052)
  | 1362 -> One (r1053)
  | 1361 -> One (r1054)
  | 1360 -> One (r1055)
  | 1426 -> One (r1056)
  | 1425 -> One (r1057)
  | 1424 -> One (r1058)
  | 1423 -> One (r1059)
  | 1422 -> One (r1060)
  | 1421 -> One (r1061)
  | 1418 -> One (r1062)
  | 1365 -> One (r1063)
  | 1414 -> One (r1064)
  | 1413 -> One (r1065)
  | 1408 -> One (r1066)
  | 1407 -> One (r1067)
  | 1406 -> One (r1068)
  | 1405 -> One (r1069)
  | 1374 -> One (r1070)
  | 1373 -> One (r1071)
  | 1372 -> One (r1072)
  | 1371 -> One (r1073)
  | 1370 -> One (r1074)
  | 1369 -> One (r1075)
  | 1404 -> One (r1076)
  | 1378 -> One (r1077)
  | 1377 -> One (r1078)
  | 1376 -> One (r1079)
  | 1382 -> One (r1080)
  | 1381 -> One (r1081)
  | 1380 -> One (r1082)
  | 1401 -> One (r1083)
  | 1386 -> One (r1084)
  | 1385 -> One (r1085)
  | 1403 -> One (r1087)
  | 1384 -> One (r1088)
  | 1398 -> One (r1089)
  | 1388 -> One (r1090)
  | 1392 -> One (r1091)
  | 1412 -> One (r1092)
  | 1411 -> One (r1093)
  | 1410 -> One (r1094)
  | 1417 -> One (r1095)
  | 1416 -> One (r1096)
  | 1420 -> One (r1097)
  | 1430 -> One (r1098)
  | 1429 -> One (r1099)
  | 1428 -> One (r1100)
  | 1434 -> One (r1101)
  | 1437 -> One (r1102)
  | 1442 -> One (r1103)
  | 1441 -> One (r1104)
  | 1440 -> One (r1105)
  | 1444 -> One (r1106)
  | 1454 -> One (r1107)
  | 1453 -> One (r1108)
  | 1452 -> One (r1109)
  | 1451 -> One (r1110)
  | 1450 -> One (r1111)
  | 1449 -> One (r1112)
  | 1448 -> One (r1113)
  | 1471 -> One (r1114)
  | 1475 -> One (r1115)
  | 1480 -> One (r1116)
  | 1479 -> One (r1117)
  | 1478 -> One (r1118)
  | 1477 -> One (r1119)
  | 1482 -> One (r1120)
  | 1488 -> One (r1121)
  | 1487 -> One (r1122)
  | 1502 | 1545 -> One (r1123)
  | 1501 | 1544 -> One (r1124)
  | 1500 | 1543 -> One (r1125)
  | 1505 | 1550 -> One (r1126)
  | 1504 | 1549 -> One (r1127)
  | 1503 | 1548 -> One (r1128)
  | 1510 | 1557 -> One (r1129)
  | 1509 | 1556 -> One (r1130)
  | 1508 | 1555 -> One (r1131)
  | 1507 | 1554 -> One (r1132)
  | 1516 | 1562 -> One (r1133)
  | 1515 | 1561 -> One (r1134)
  | 1514 | 1560 -> One (r1135)
  | 1519 | 1567 -> One (r1136)
  | 1518 | 1566 -> One (r1137)
  | 1517 | 1565 -> One (r1138)
  | 1526 -> One (r1139)
  | 1529 | 1579 -> One (r1140)
  | 1528 | 1578 -> One (r1141)
  | 1527 | 1577 -> One (r1142)
  | 1531 -> One (r1143)
  | 1534 | 1582 -> One (r1144)
  | 1533 | 1581 -> One (r1145)
  | 1532 | 1580 -> One (r1146)
  | 1536 -> One (r1147)
  | 1542 -> One (r1148)
  | 1547 -> One (r1149)
  | 1552 -> One (r1150)
  | 1559 -> One (r1151)
  | 1564 -> One (r1152)
  | 1569 -> One (r1153)
  | 1572 -> One (r1154)
  | 1586 -> One (r1155)
  | 1585 -> One (r1156)
  | 1591 -> One (r1157)
  | 1595 -> One (r1158)
  | 1597 -> One (r1159)
  | 1599 -> One (r1160)
  | 1613 -> One (r1161)
  | 1612 -> One (r1162)
  | 1611 -> One (r1163)
  | 1610 -> One (r1164)
  | 1609 -> One (r1165)
  | 1622 -> One (r1167)
  | 1607 -> One (r1169)
  | 1602 -> One (r1170)
  | 1605 -> One (r1171)
  | 1604 -> One (r1172)
  | 1620 -> One (r1173)
  | 1619 -> One (r1174)
  | 1618 -> One (r1175)
  | 1615 -> One (r1176)
  | 1625 -> One (r1177)
  | 1627 -> One (r1178)
  | 1635 -> One (r1179)
  | 1630 -> One (r1180)
  | 1633 -> One (r1181)
  | 1632 -> One (r1182)
  | 1640 -> One (r1184)
  | 1639 -> One (r1185)
  | 1653 -> One (r1186)
  | 1652 -> One (r1187)
  | 1644 -> One (r1188)
  | 1643 -> One (r1189)
  | 1674 -> One (r1190)
  | 1673 -> One (r1191)
  | 1672 -> One (r1192)
  | 1671 -> One (r1193)
  | 1670 -> One (r1194)
  | 1669 -> One (r1195)
  | 1686 -> One (r1196)
  | 1679 -> One (r1197)
  | 1678 -> One (r1198)
  | 1683 -> One (r1199)
  | 1682 -> One (r1200)
  | 1681 -> One (r1201)
  | 1685 -> One (r1202)
  | 1699 -> One (r1203)
  | 1705 -> One (r1204)
  | 1708 -> One (r1205)
  | 1721 -> One (r1206)
  | 1738 -> One (r1207)
  | 1737 -> One (r1208)
  | 1736 -> One (r1209)
  | 1735 -> One (r1210)
  | 1734 -> One (r1211)
  | 1733 -> One (r1212)
  | 1746 -> One (r1213)
  | 1745 -> One (r1214)
  | 1744 -> One (r1215)
  | 1743 -> One (r1216)
  | 1742 -> One (r1217)
  | 1741 -> One (r1218)
  | 1740 -> One (r1219)
  | 1766 -> One (r1220)
  | 1767 -> One (r1222)
  | 1760 -> One (r1223)
  | 1759 -> One (r1224)
  | 1758 -> One (r1225)
  | 1765 -> One (r1226)
  | 1764 -> One (r1227)
  | 1769 -> One (r1228)
  | 1775 -> One (r1229)
  | 1774 -> One (r1230)
  | 1773 -> One (r1231)
  | 1772 -> One (r1232)
  | 1778 -> One (r1233)
  | 1777 -> One (r1234)
  | 1786 -> One (r1235)
  | 1794 -> One (r1236)
  | 1793 -> One (r1237)
  | 1792 -> One (r1238)
  | 1791 -> One (r1239)
  | 1797 -> One (r1240)
  | 1796 -> One (r1241)
  | 1801 -> One (r1242)
  | 1812 -> One (r1243)
  | 1811 -> One (r1244)
  | 1815 -> One (r1245)
  | 1814 -> One (r1246)
  | 1818 -> One (r1247)
  | 1817 -> One (r1248)
  | 1827 -> One (r1249)
  | 1826 -> One (r1250)
  | 1834 -> One (r1251)
  | 1833 -> One (r1252)
  | 1832 -> One (r1253)
  | 1836 -> One (r1254)
  | 1844 -> One (r1255)
  | 1852 -> One (r1256)
  | 1849 -> One (r1257)
  | 1848 -> One (r1258)
  | 1847 -> One (r1259)
  | 1846 -> One (r1260)
  | 1851 -> One (r1261)
  | 1869 -> One (r1262)
  | 1866 -> One (r1263)
  | 1865 -> One (r1264)
  | 1863 -> One (r1265)
  | 1860 -> One (r1266)
  | 1859 -> One (r1267)
  | 1858 -> One (r1268)
  | 1857 -> One (r1269)
  | 1862 -> One (r1270)
  | 1868 -> One (r1271)
  | 1874 -> One (r1272)
  | 1884 -> One (r1273)
  | 1881 -> One (r1274)
  | 1880 -> One (r1275)
  | 1879 -> One (r1276)
  | 1878 -> One (r1277)
  | 1883 -> One (r1278)
  | 1893 -> One (r1279)
  | 1898 -> One (r1280)
  | 1897 -> One (r1281)
  | 1910 -> One (r1282)
  | 1909 -> One (r1283)
  | 1922 -> One (r1284)
  | 1921 -> One (r1285)
  | 1945 -> One (r1286)
  | 1944 -> One (r1287)
  | 1954 -> One (r1288)
  | 1956 -> One (r1289)
  | 1958 -> One (r1290)
  | 1971 -> One (r1291)
  | 1975 -> One (r1292)
  | 1980 -> One (r1293)
  | 1987 -> One (r1294)
  | 1986 -> One (r1295)
  | 1985 -> One (r1296)
  | 1984 -> One (r1297)
  | 1994 -> One (r1298)
  | 1998 -> One (r1299)
  | 2002 -> One (r1300)
  | 2005 -> One (r1301)
  | 2010 -> One (r1302)
  | 2014 -> One (r1303)
  | 2018 -> One (r1304)
  | 2022 -> One (r1305)
  | 2026 -> One (r1306)
  | 2029 -> One (r1307)
  | 2033 -> One (r1308)
  | 2039 -> One (r1309)
  | 2049 -> One (r1310)
  | 2051 -> One (r1311)
  | 2054 -> One (r1312)
  | 2053 -> One (r1313)
  | 2056 -> One (r1314)
  | 2066 -> One (r1315)
  | 2062 -> One (r1316)
  | 2061 -> One (r1317)
  | 2065 -> One (r1318)
  | 2064 -> One (r1319)
  | 2071 -> One (r1320)
  | 2070 -> One (r1321)
  | 2069 -> One (r1322)
  | 2073 -> One (r1323)
  | 442 -> Select (function
    | -1 -> [R 117]
    | _ -> S (T T_DOT) :: r391)
  | 682 -> Select (function
    | -1 -> [R 117]
    | _ -> r592)
  | 173 -> Select (function
    | -1 -> r160
    | _ -> R 202 :: r152)
  | 915 -> Select (function
    | -1 -> r750
    | _ -> R 202 :: r743)
  | 982 -> Select (function
    | -1 -> r160
    | _ -> R 202 :: r798)
  | 1061 -> Select (function
    | -1 -> r702
    | _ -> R 202 :: r845)
  | 595 -> Select (function
    | -1 -> r283
    | _ -> [R 234])
  | 460 -> Select (function
    | -1 -> [R 720]
    | _ -> S (N N_pattern) :: r399)
  | 457 -> Select (function
    | -1 -> [R 721]
    | _ -> S (N N_pattern) :: r398)
  | 179 -> Select (function
    | -1 -> r172
    | _ -> R 866 :: r166)
  | 985 -> Select (function
    | -1 -> r172
    | _ -> R 866 :: r804)
  | 959 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (T T_COLONCOLON) :: r407)
  | 89 -> Select (function
    | 256 | 410 | 697 | 805 | 1371 | 1410 | 1461 | 1590 -> r61
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (N N_pattern) :: r56)
  | 246 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> Sub (r1) :: r238)
  | 412 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r362
    | _ -> Sub (r364) :: r366)
  | 636 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r362
    | _ -> Sub (r517) :: r519)
  | 551 -> Select (function
    | 62 | 172 | 213 | 854 | 895 | 897 -> r462
    | _ -> S (T T_OPEN) :: r456)
  | 162 -> Select (function
    | -1 | 289 | 292 | 330 | 345 | 351 | 357 | 1849 | 1860 | 1866 | 1872 | 1881 | 1891 -> S (T T_MODULE) :: r116
    | _ -> Sub (r117) :: r123)
  | 961 -> Select (function
    | -1 -> r510
    | _ -> S (T T_LPAREN) :: r780)
  | 273 -> Select (function
    | -1 -> r285
    | _ -> S (T T_DOT) :: r287)
  | 593 -> Select (function
    | -1 -> r285
    | _ -> S (T T_DOT) :: r505)
  | 207 -> Select (function
    | -1 | 289 | 292 | 330 | 345 | 351 | 357 | 1849 | 1860 | 1866 | 1872 | 1881 | 1891 -> r131
    | _ -> S (T T_COLON) :: r196)
  | 157 -> Select (function
    | 151 | 904 | 933 | 1138 | 1324 | 1344 | 1348 | 1832 -> r108
    | _ -> r106)
  | 154 -> Select (function
    | 151 | 904 | 933 | 1138 | 1324 | 1344 | 1348 | 1832 -> r109
    | _ -> r107)
  | 1924 -> Select (function
    | -1 -> r156
    | _ -> r131)
  | 198 -> Select (function
    | -1 -> r170
    | _ -> r131)
  | 1036 -> Select (function
    | -1 -> r156
    | _ -> r131)
  | 987 -> Select (function
    | -1 -> r170
    | _ -> r131)
  | 1923 -> Select (function
    | -1 -> r157
    | _ -> r150)
  | 175 -> Select (function
    | -1 -> r158
    | _ -> r151)
  | 174 -> Select (function
    | -1 -> r159
    | _ -> r152)
  | 1035 -> Select (function
    | -1 -> r157
    | _ -> r796)
  | 984 -> Select (function
    | -1 -> r158
    | _ -> r797)
  | 983 -> Select (function
    | -1 -> r159
    | _ -> r798)
  | 197 -> Select (function
    | -1 -> r171
    | _ -> r166)
  | 986 -> Select (function
    | -1 -> r171
    | _ -> r804)
  | 280 -> Select (function
    | -1 -> r284
    | _ -> r287)
  | 594 -> Select (function
    | -1 -> r284
    | _ -> r505)
  | 1064 -> Select (function
    | -1 -> r699
    | _ -> r843)
  | 1063 -> Select (function
    | -1 -> r700
    | _ -> r844)
  | 1062 -> Select (function
    | -1 -> r701
    | _ -> r845)
  | 923 -> Select (function
    | -1 -> r747
    | _ -> r741)
  | 917 -> Select (function
    | -1 -> r748
    | _ -> r742)
  | 916 -> Select (function
    | -1 -> r749
    | _ -> r743)
  | _ -> raise Not_found
