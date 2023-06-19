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
    | MenhirInterpreter.T MenhirInterpreter.T_HASH_SUFFIX -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASH_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_HASH_FLOAT -> ("0.",None)
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
    | MenhirInterpreter.T MenhirInterpreter.T_EXCLAVE -> ()
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_parenthesized_type_parameter_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_parenthesized_type_parameter -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_layout -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_include_maybe_functor -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;2;2;1;2;2;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;3;4;1;3;1;2;1;2;3;2;3;1;1;2;5;6;1;1;1;2;1;1;1;2;1;2;3;1;1;1;2;1;1;1;1;1;2;3;4;1;2;3;1;2;3;1;1;2;3;3;1;1;4;1;1;1;2;3;1;2;3;1;1;1;1;1;2;1;2;3;1;4;1;2;1;2;3;1;2;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;2;1;2;3;4;3;4;5;1;2;1;1;3;2;3;2;3;1;2;3;4;4;1;2;1;2;3;4;5;4;5;2;1;3;2;1;2;3;4;3;2;3;4;5;6;7;8;9;8;8;2;3;2;3;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;5;5;2;3;4;5;4;4;3;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;3;4;5;3;4;5;3;4;2;1;2;3;4;1;1;1;1;1;2;1;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;2;2;3;1;1;1;2;3;1;2;3;4;5;3;4;5;6;3;4;5;1;2;1;2;3;4;1;2;3;4;5;5;1;2;6;7;8;9;3;4;5;6;7;8;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;2;3;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;1;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;1;2;3;4;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;1;1;2;3;1;1;2;3;4;1;1;2;8;9;10;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;7;8;9;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;1;2;3;4;1;5;1;1;2;3;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;2;3;4;5;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;2;3;4;2;3;4;5;6;7;1;1;2;3;1;1;2;1;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;3;4;5;1;2;1;1;1;1;2;3;1;1;1;1;3;4;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;4;1;2;1;2;2;1;2;3;4;5;2;1;2;1;2;1;1;3;4;3;3;1;5;6;2;1;3;4;4;5;4;5;6;3;4;4;5;4;5;6;3;4;5;3;1;2;3;1;1;2;3;4;5;1;4;5;1;2;3;3;2;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;3;4;2;2;3;2;3;4;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;5;2;2;5;6;3;4;5;2;1;2;3;4;1;2;1;2;3;1;5;1;2;3;4;5;6;7;8;3;4;5;3;5;6;3;2;2;2;3;2;3;4;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;1;2;3;4;5;6;1;7;1;2;3;2;2;3;4;5;6;7;8;9;10;9;9;3;4;5;6;7;8;9;10;11;10;10;4;5;6;7;6;6;3;4;5;6;5;5;3;4;5;6;7;8;9;8;8;2;2;3;4;5;6;7;8;7;7;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  | T_HASH_SUFFIX -> true
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
  | T_EXCLAVE -> true
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
  let r0 = [R 633] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 141] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 312 :: r6 in
  let r8 = [R 750] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 45] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 204] in
  let r13 = [R 46] in
  let r14 = [R 546] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 47] in
  let r17 = Sub (r15) :: r16 in
  let r18 = [R 48] in
  let r19 = S (T T_RBRACKET) :: r18 in
  let r20 = Sub (r17) :: r19 in
  let r21 = [R 156] in
  let r22 = S (T T_DONE) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = S (T T_DO) :: r23 in
  let r25 = Sub (r1) :: r24 in
  let r26 = R 312 :: r25 in
  let r27 = [R 706] in
  let r28 = [R 381] in
  let r29 = [R 137] in
  let r30 = Sub (r1) :: r29 in
  let r31 = R 312 :: r30 in
  let r32 = [R 350] in
  let r33 = Sub (r1) :: r32 in
  let r34 = S (T T_MINUSGREATER) :: r33 in
  let r35 = S (N N_pattern) :: r34 in
  let r36 = [R 592] in
  let r37 = Sub (r35) :: r36 in
  let r38 = [R 153] in
  let r39 = Sub (r37) :: r38 in
  let r40 = S (T T_WITH) :: r39 in
  let r41 = Sub (r1) :: r40 in
  let r42 = R 312 :: r41 in
  let r43 = [R 206] in
  let r44 = S (T T_UNDERSCORE) :: r27 in
  let r45 = [R 695] in
  let r46 = [R 691] in
  let r47 = S (T T_END) :: r46 in
  let r48 = R 329 :: r47 in
  let r49 = R 73 :: r48 in
  let r50 = R 312 :: r49 in
  let r51 = [R 71] in
  let r52 = S (T T_RPAREN) :: r51 in
  let r53 = [R 735] in
  let r54 = [R 663] in
  let r55 = [R 659] in
  let r56 = [R 113] in
  let r57 = [R 731] in
  let r58 = S (T T_RPAREN) :: r57 in
  let r59 = [R 479] in
  let r60 = S (T T_AMPERAMPER) :: r59 in
  let r61 = [R 908] in
  let r62 = S (T T_RPAREN) :: r61 in
  let r63 = Sub (r60) :: r62 in
  let r64 = [R 403] in
  let r65 = S (T T_UNDERSCORE) :: r64 in
  let r66 = [R 733] in
  let r67 = S (T T_RPAREN) :: r66 in
  let r68 = Sub (r65) :: r67 in
  let r69 = R 312 :: r68 in
  let r70 = [R 734] in
  let r71 = S (T T_RPAREN) :: r70 in
  let r72 = [R 369] in
  let r73 = [R 638] in
  let r74 = R 320 :: r73 in
  let r75 = [R 405] in
  let r76 = S (T T_END) :: r75 in
  let r77 = Sub (r74) :: r76 in
  let r78 = [R 909] in
  let r79 = S (T T_LIDENT) :: r78 in
  let r80 = [R 25] in
  let r81 = S (T T_UNDERSCORE) :: r80 in
  let r82 = [R 882] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 218] in
  let r85 = Sub (r83) :: r84 in
  let r86 = [R 17] in
  let r87 = Sub (r85) :: r86 in
  let r88 = [R 131] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 551] in
  let r91 = Sub (r89) :: r90 in
  let r92 = [R 917] in
  let r93 = R 318 :: r92 in
  let r94 = Sub (r91) :: r93 in
  let r95 = S (T T_COLON) :: r94 in
  let r96 = Sub (r79) :: r95 in
  let r97 = R 312 :: r96 in
  let r98 = [R 453] in
  let r99 = S (T T_RPAREN) :: r98 in
  let r100 = R 240 :: r99 in
  let r101 = [R 241] in
  let r102 = [R 455] in
  let r103 = S (T T_RBRACKET) :: r102 in
  let r104 = [R 457] in
  let r105 = S (T T_RBRACE) :: r104 in
  let r106 = [R 236] in
  let r107 = S (T T_LIDENT) :: r106 in
  let r108 = [R 24] in
  let r109 = Sub (r107) :: r108 in
  let r110 = [R 590] in
  let r111 = Sub (r107) :: r110 in
  let r112 = [R 502] in
  let r113 = S (T T_COLON) :: r112 in
  let r114 = [R 23] in
  let r115 = S (T T_RPAREN) :: r114 in
  let r116 = S (N N_module_type) :: r115 in
  let r117 = R 312 :: r116 in
  let r118 = R 203 :: r117 in
  let r119 = S (T T_QUOTE) :: r111 in
  let r120 = [R 833] in
  let r121 = Sub (r83) :: r120 in
  let r122 = S (T T_MINUSGREATER) :: r121 in
  let r123 = S (T T_RPAREN) :: r122 in
  let r124 = Sub (r89) :: r123 in
  let r125 = S (T T_DOT) :: r124 in
  let r126 = [R 407] in
  let r127 = S (N N_module_expr) :: r126 in
  let r128 = R 312 :: r127 in
  let r129 = S (T T_OF) :: r128 in
  let r130 = [R 393] in
  let r131 = S (T T_END) :: r130 in
  let r132 = S (N N_structure) :: r131 in
  let r133 = [R 367] in
  let r134 = S (T T_LIDENT) :: r133 in
  let r135 = [R 889] in
  let r136 = Sub (r134) :: r135 in
  let r137 = [R 114] in
  let r138 = S (T T_FALSE) :: r137 in
  let r139 = [R 118] in
  let r140 = Sub (r138) :: r139 in
  let r141 = [R 230] in
  let r142 = R 312 :: r141 in
  let r143 = R 223 :: r142 in
  let r144 = Sub (r140) :: r143 in
  let r145 = [R 571] in
  let r146 = Sub (r144) :: r145 in
  let r147 = [R 850] in
  let r148 = R 318 :: r147 in
  let r149 = Sub (r146) :: r148 in
  let r150 = R 557 :: r149 in
  let r151 = S (T T_PLUSEQ) :: r150 in
  let r152 = Sub (r136) :: r151 in
  let r153 = R 891 :: r152 in
  let r154 = R 312 :: r153 in
  let r155 = [R 233] in
  let r156 = R 318 :: r155 in
  let r157 = R 580 :: r156 in
  let r158 = R 887 :: r157 in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = R 891 :: r159 in
  let r161 = R 312 :: r160 in
  let r162 = R 203 :: r161 in
  let r163 = [R 851] in
  let r164 = R 318 :: r163 in
  let r165 = Sub (r146) :: r164 in
  let r166 = R 557 :: r165 in
  let r167 = S (T T_PLUSEQ) :: r166 in
  let r168 = Sub (r136) :: r167 in
  let r169 = [R 234] in
  let r170 = R 318 :: r169 in
  let r171 = R 580 :: r170 in
  let r172 = R 887 :: r171 in
  let r173 = S (T T_LIDENT) :: r172 in
  let r174 = R 891 :: r173 in
  let r175 = [R 895] in
  let r176 = S (T T_UNDERSCORE) :: r175 in
  let r177 = [R 890] in
  let r178 = R 312 :: r177 in
  let r179 = Sub (r176) :: r178 in
  let r180 = R 896 :: r179 in
  let r181 = [R 509] in
  let r182 = Sub (r180) :: r181 in
  let r183 = [R 605] in
  let r184 = Sub (r182) :: r183 in
  let r185 = [R 893] in
  let r186 = S (T T_RPAREN) :: r185 in
  let r187 = [R 894] in
  let r188 = [R 276] in
  let r189 = Sub (r107) :: r188 in
  let r190 = [R 510] in
  let r191 = [R 313] in
  let r192 = [R 606] in
  let r193 = [R 438] in
  let r194 = S (T T_DOTDOT) :: r193 in
  let r195 = [R 888] in
  let r196 = [R 439] in
  let r197 = [R 117] in
  let r198 = S (T T_RPAREN) :: r197 in
  let r199 = [R 839] in
  let r200 = Sub (r83) :: r199 in
  let r201 = S (T T_MINUSGREATER) :: r200 in
  let r202 = [R 827] in
  let r203 = Sub (r83) :: r202 in
  let r204 = S (T T_MINUSGREATER) :: r203 in
  let r205 = Sub (r83) :: r204 in
  let r206 = [R 33] in
  let r207 = [R 205] in
  let r208 = S (T T_RBRACKET) :: r207 in
  let r209 = Sub (r15) :: r208 in
  let r210 = [R 324] in
  let r211 = [R 446] in
  let r212 = R 318 :: r211 in
  let r213 = S (N N_module_expr) :: r212 in
  let r214 = R 312 :: r213 in
  let r215 = [R 447] in
  let r216 = R 318 :: r215 in
  let r217 = S (N N_module_expr) :: r216 in
  let r218 = R 312 :: r217 in
  let r219 = [R 504] in
  let r220 = S (T T_RPAREN) :: r219 in
  let r221 = [R 505] in
  let r222 = S (T T_RPAREN) :: r221 in
  let r223 = S (N N_expr) :: r222 in
  let r224 = [R 379] in
  let r225 = S (T T_LIDENT) :: r224 in
  let r226 = [R 70] in
  let r227 = Sub (r225) :: r226 in
  let r228 = [R 688] in
  let r229 = Sub (r227) :: r228 in
  let r230 = R 312 :: r229 in
  let r231 = [R 380] in
  let r232 = S (T T_LIDENT) :: r231 in
  let r233 = [R 382] in
  let r234 = [R 387] in
  let r235 = [R 136] in
  let r236 = Sub (r37) :: r235 in
  let r237 = S (T T_WITH) :: r236 in
  let r238 = Sub (r1) :: r237 in
  let r239 = R 312 :: r238 in
  let r240 = [R 152] in
  let r241 = Sub (r37) :: r240 in
  let r242 = S (T T_WITH) :: r241 in
  let r243 = Sub (r1) :: r242 in
  let r244 = R 312 :: r243 in
  let r245 = [R 675] in
  let r246 = S (T T_RPAREN) :: r245 in
  let r247 = [R 722] in
  let r248 = [R 201] in
  let r249 = [R 189] in
  let r250 = [R 278] in
  let r251 = Sub (r79) :: r250 in
  let r252 = [R 347] in
  let r253 = R 318 :: r252 in
  let r254 = Sub (r251) :: r253 in
  let r255 = R 564 :: r254 in
  let r256 = R 312 :: r255 in
  let r257 = [R 344] in
  let r258 = Sub (r1) :: r257 in
  let r259 = S (T T_EQUAL) :: r258 in
  let r260 = [R 287] in
  let r261 = Sub (r259) :: r260 in
  let r262 = [R 268] in
  let r263 = [R 250] in
  let r264 = S (T T_LIDENT) :: r263 in
  let r265 = [R 266] in
  let r266 = S (T T_RPAREN) :: r265 in
  let r267 = [R 267] in
  let r268 = S (T T_RPAREN) :: r267 in
  let r269 = [R 251] in
  let r270 = [R 620] in
  let r271 = Sub (r89) :: r270 in
  let r272 = [R 601] in
  let r273 = Sub (r271) :: r272 in
  let r274 = [R 42] in
  let r275 = S (T T_RBRACKET) :: r274 in
  let r276 = Sub (r273) :: r275 in
  let r277 = [R 41] in
  let r278 = [R 40] in
  let r279 = S (T T_RBRACKET) :: r278 in
  let r280 = [R 427] in
  let r281 = Sub (r107) :: r280 in
  let r282 = S (T T_BACKQUOTE) :: r281 in
  let r283 = [R 863] in
  let r284 = R 312 :: r283 in
  let r285 = Sub (r282) :: r284 in
  let r286 = [R 37] in
  let r287 = S (T T_RBRACKET) :: r286 in
  let r288 = [R 99] in
  let r289 = Sub (r134) :: r288 in
  let r290 = [R 34] in
  let r291 = [R 370] in
  let r292 = S (T T_UIDENT) :: r291 in
  let r293 = S (T T_DOT) :: r292 in
  let r294 = [R 368] in
  let r295 = S (T T_LIDENT) :: r294 in
  let r296 = S (T T_UIDENT) :: r72 in
  let r297 = [R 385] in
  let r298 = Sub (r296) :: r297 in
  let r299 = [R 386] in
  let r300 = S (T T_RPAREN) :: r299 in
  let r301 = [R 38] in
  let r302 = S (T T_RBRACKET) :: r301 in
  let r303 = [R 835] in
  let r304 = [R 836] in
  let r305 = [R 840] in
  let r306 = [R 617] in
  let r307 = [R 35] in
  let r308 = [R 618] in
  let r309 = [R 819] in
  let r310 = Sub (r83) :: r309 in
  let r311 = S (T T_MINUSGREATER) :: r310 in
  let r312 = [R 31] in
  let r313 = Sub (r136) :: r312 in
  let r314 = [R 36] in
  let r315 = [R 613] in
  let r316 = [R 18] in
  let r317 = Sub (r107) :: r316 in
  let r318 = [R 817] in
  let r319 = Sub (r83) :: r318 in
  let r320 = S (T T_MINUSGREATER) :: r319 in
  let r321 = S (T T_RPAREN) :: r320 in
  let r322 = Sub (r89) :: r321 in
  let r323 = [R 591] in
  let r324 = [R 818] in
  let r325 = [R 22] in
  let r326 = [R 614] in
  let r327 = [R 823] in
  let r328 = Sub (r83) :: r327 in
  let r329 = S (T T_MINUSGREATER) :: r328 in
  let r330 = [R 821] in
  let r331 = Sub (r83) :: r330 in
  let r332 = S (T T_MINUSGREATER) :: r331 in
  let r333 = S (T T_RPAREN) :: r332 in
  let r334 = Sub (r89) :: r333 in
  let r335 = [R 822] in
  let r336 = [R 824] in
  let r337 = [R 820] in
  let r338 = [R 602] in
  let r339 = [R 595] in
  let r340 = Sub (r87) :: r339 in
  let r341 = [R 862] in
  let r342 = R 312 :: r341 in
  let r343 = Sub (r340) :: r342 in
  let r344 = [R 596] in
  let r345 = [R 39] in
  let r346 = S (T T_RBRACKET) :: r345 in
  let r347 = Sub (r273) :: r346 in
  let r348 = [R 588] in
  let r349 = Sub (r282) :: r348 in
  let r350 = [R 43] in
  let r351 = S (T T_RBRACKET) :: r350 in
  let r352 = [R 252] in
  let r353 = Sub (r89) :: r352 in
  let r354 = [R 262] in
  let r355 = [R 260] in
  let r356 = S (T T_RPAREN) :: r355 in
  let r357 = R 497 :: r356 in
  let r358 = [R 261] in
  let r359 = S (T T_RPAREN) :: r358 in
  let r360 = R 497 :: r359 in
  let r361 = [R 498] in
  let r362 = [R 297] in
  let r363 = Sub (r79) :: r362 in
  let r364 = [R 300] in
  let r365 = Sub (r363) :: r364 in
  let r366 = [R 187] in
  let r367 = Sub (r1) :: r366 in
  let r368 = S (T T_IN) :: r367 in
  let r369 = [R 672] in
  let r370 = [R 670] in
  let r371 = [R 112] in
  let r372 = [R 627] in
  let r373 = S (N N_pattern) :: r372 in
  let r374 = [R 668] in
  let r375 = S (T T_RBRACKET) :: r374 in
  let r376 = [R 253] in
  let r377 = Sub (r225) :: r376 in
  let r378 = [R 338] in
  let r379 = R 495 :: r378 in
  let r380 = R 489 :: r379 in
  let r381 = Sub (r377) :: r380 in
  let r382 = [R 667] in
  let r383 = S (T T_RBRACE) :: r382 in
  let r384 = [R 490] in
  let r385 = [R 496] in
  let r386 = S (T T_UNDERSCORE) :: r53 in
  let r387 = [R 730] in
  let r388 = Sub (r386) :: r387 in
  let r389 = [R 537] in
  let r390 = Sub (r388) :: r389 in
  let r391 = R 312 :: r390 in
  let r392 = [R 740] in
  let r393 = [R 110] in
  let r394 = [R 739] in
  let r395 = S (T T_HASH_INT) :: r393 in
  let r396 = [R 658] in
  let r397 = Sub (r395) :: r396 in
  let r398 = [R 736] in
  let r399 = [R 742] in
  let r400 = S (T T_RBRACKET) :: r399 in
  let r401 = S (T T_LBRACKET) :: r400 in
  let r402 = [R 743] in
  let r403 = [R 528] in
  let r404 = S (N N_pattern) :: r403 in
  let r405 = R 312 :: r404 in
  let r406 = [R 529] in
  let r407 = [R 522] in
  let r408 = [R 536] in
  let r409 = [R 534] in
  let r410 = [R 428] in
  let r411 = S (T T_LIDENT) :: r410 in
  let r412 = [R 535] in
  let r413 = Sub (r388) :: r412 in
  let r414 = S (T T_RPAREN) :: r413 in
  let r415 = [R 122] in
  let r416 = [R 121] in
  let r417 = S (T T_RPAREN) :: r416 in
  let r418 = [R 530] in
  let r419 = [R 745] in
  let r420 = S (T T_RPAREN) :: r419 in
  let r421 = Sub (r89) :: r420 in
  let r422 = [R 527] in
  let r423 = [R 525] in
  let r424 = [R 120] in
  let r425 = S (T T_RPAREN) :: r424 in
  let r426 = [R 744] in
  let r427 = [R 340] in
  let r428 = [R 669] in
  let r429 = [R 671] in
  let r430 = [R 545] in
  let r431 = S (T T_UNDERSCORE) :: r430 in
  let r432 = [R 265] in
  let r433 = [R 263] in
  let r434 = S (T T_RPAREN) :: r433 in
  let r435 = R 497 :: r434 in
  let r436 = [R 264] in
  let r437 = S (T T_RPAREN) :: r436 in
  let r438 = R 497 :: r437 in
  let r439 = [R 294] in
  let r440 = [R 295] in
  let r441 = Sub (r89) :: r440 in
  let r442 = [R 802] in
  let r443 = Sub (r1) :: r442 in
  let r444 = S (T T_EQUAL) :: r443 in
  let r445 = [R 212] in
  let r446 = Sub (r444) :: r445 in
  let r447 = [R 804] in
  let r448 = Sub (r446) :: r447 in
  let r449 = S (T T_RPAREN) :: r448 in
  let r450 = Sub (r411) :: r449 in
  let r451 = [R 269] in
  let r452 = [R 270] in
  let r453 = S (T T_RPAREN) :: r452 in
  let r454 = S (N N_pattern) :: r453 in
  let r455 = [R 274] in
  let r456 = S (T T_RPAREN) :: r455 in
  let r457 = Sub (r89) :: r456 in
  let r458 = S (T T_DOT) :: r457 in
  let r459 = [R 273] in
  let r460 = S (T T_RPAREN) :: r459 in
  let r461 = Sub (r89) :: r460 in
  let r462 = [R 147] in
  let r463 = Sub (r1) :: r462 in
  let r464 = S (T T_IN) :: r463 in
  let r465 = S (N N_module_expr) :: r464 in
  let r466 = R 312 :: r465 in
  let r467 = R 203 :: r466 in
  let r468 = [R 288] in
  let r469 = R 318 :: r468 in
  let r470 = Sub (r251) :: r469 in
  let r471 = R 564 :: r470 in
  let r472 = R 312 :: r471 in
  let r473 = R 203 :: r472 in
  let r474 = [R 148] in
  let r475 = Sub (r1) :: r474 in
  let r476 = S (T T_IN) :: r475 in
  let r477 = S (N N_module_expr) :: r476 in
  let r478 = R 312 :: r477 in
  let r479 = [R 394] in
  let r480 = S (N N_module_expr) :: r479 in
  let r481 = S (T T_MINUSGREATER) :: r480 in
  let r482 = S (N N_functor_args) :: r481 in
  let r483 = [R 220] in
  let r484 = [R 221] in
  let r485 = S (T T_RPAREN) :: r484 in
  let r486 = S (N N_module_type) :: r485 in
  let r487 = [R 408] in
  let r488 = S (T T_RPAREN) :: r487 in
  let r489 = [R 406] in
  let r490 = S (N N_module_type) :: r489 in
  let r491 = S (T T_MINUSGREATER) :: r490 in
  let r492 = S (N N_functor_args) :: r491 in
  let r493 = [R 377] in
  let r494 = Sub (r107) :: r493 in
  let r495 = [R 416] in
  let r496 = Sub (r494) :: r495 in
  let r497 = [R 930] in
  let r498 = S (N N_module_type) :: r497 in
  let r499 = S (T T_EQUAL) :: r498 in
  let r500 = Sub (r496) :: r499 in
  let r501 = S (T T_TYPE) :: r500 in
  let r502 = S (T T_MODULE) :: r501 in
  let r503 = [R 599] in
  let r504 = Sub (r502) :: r503 in
  let r505 = [R 412] in
  let r506 = [R 927] in
  let r507 = Sub (r87) :: r506 in
  let r508 = S (T T_COLONEQUAL) :: r507 in
  let r509 = Sub (r377) :: r508 in
  let r510 = [R 926] in
  let r511 = R 580 :: r510 in
  let r512 = [R 581] in
  let r513 = Sub (r89) :: r512 in
  let r514 = S (T T_EQUAL) :: r513 in
  let r515 = [R 378] in
  let r516 = Sub (r107) :: r515 in
  let r517 = [R 931] in
  let r518 = [R 411] in
  let r519 = [R 928] in
  let r520 = Sub (r298) :: r519 in
  let r521 = S (T T_UIDENT) :: r233 in
  let r522 = [R 929] in
  let r523 = [R 600] in
  let r524 = [R 399] in
  let r525 = [R 503] in
  let r526 = S (T T_RPAREN) :: r525 in
  let r527 = [R 711] in
  let r528 = [R 621] in
  let r529 = S (N N_expr) :: r528 in
  let r530 = [R 714] in
  let r531 = S (T T_RBRACKET) :: r530 in
  let r532 = [R 698] in
  let r533 = [R 624] in
  let r534 = R 491 :: r533 in
  let r535 = [R 492] in
  let r536 = [R 630] in
  let r537 = R 491 :: r536 in
  let r538 = R 499 :: r537 in
  let r539 = Sub (r377) :: r538 in
  let r540 = [R 566] in
  let r541 = Sub (r539) :: r540 in
  let r542 = [R 708] in
  let r543 = S (T T_RBRACE) :: r542 in
  let r544 = [R 674] in
  let r545 = [R 673] in
  let r546 = S (T T_GREATERDOT) :: r545 in
  let r547 = [R 159] in
  let r548 = Sub (r44) :: r547 in
  let r549 = R 312 :: r548 in
  let r550 = [R 687] in
  let r551 = S (T T_END) :: r550 in
  let r552 = R 312 :: r551 in
  let r553 = [R 155] in
  let r554 = S (N N_expr) :: r553 in
  let r555 = S (T T_THEN) :: r554 in
  let r556 = Sub (r1) :: r555 in
  let r557 = R 312 :: r556 in
  let r558 = [R 149] in
  let r559 = Sub (r37) :: r558 in
  let r560 = R 312 :: r559 in
  let r561 = [R 593] in
  let r562 = [R 351] in
  let r563 = Sub (r1) :: r562 in
  let r564 = S (T T_MINUSGREATER) :: r563 in
  let r565 = [R 271] in
  let r566 = Sub (r388) :: r565 in
  let r567 = [R 214] in
  let r568 = Sub (r1) :: r567 in
  let r569 = S (T T_MINUSGREATER) :: r568 in
  let r570 = [R 150] in
  let r571 = Sub (r569) :: r570 in
  let r572 = Sub (r566) :: r571 in
  let r573 = R 312 :: r572 in
  let r574 = [R 272] in
  let r575 = S (T T_RPAREN) :: r574 in
  let r576 = S (N N_let_pattern) :: r575 in
  let r577 = [R 151] in
  let r578 = Sub (r569) :: r577 in
  let r579 = S (T T_RPAREN) :: r578 in
  let r580 = [R 143] in
  let r581 = S (T T_DONE) :: r580 in
  let r582 = Sub (r1) :: r581 in
  let r583 = S (T T_DO) :: r582 in
  let r584 = Sub (r1) :: r583 in
  let r585 = S (T T_IN) :: r584 in
  let r586 = S (N N_pattern) :: r585 in
  let r587 = R 312 :: r586 in
  let r588 = [R 134] in
  let r589 = S (T T_DOWNTO) :: r588 in
  let r590 = [R 157] in
  let r591 = S (T T_DONE) :: r590 in
  let r592 = Sub (r1) :: r591 in
  let r593 = S (T T_DO) :: r592 in
  let r594 = Sub (r1) :: r593 in
  let r595 = Sub (r589) :: r594 in
  let r596 = Sub (r1) :: r595 in
  let r597 = S (T T_EQUAL) :: r596 in
  let r598 = S (N N_pattern) :: r597 in
  let r599 = R 312 :: r598 in
  let r600 = [R 202] in
  let r601 = [R 696] in
  let r602 = [R 707] in
  let r603 = S (T T_RPAREN) :: r602 in
  let r604 = S (T T_LPAREN) :: r603 in
  let r605 = S (T T_DOT) :: r604 in
  let r606 = [R 720] in
  let r607 = S (T T_RPAREN) :: r606 in
  let r608 = S (N N_module_type) :: r607 in
  let r609 = S (T T_COLON) :: r608 in
  let r610 = S (N N_module_expr) :: r609 in
  let r611 = R 312 :: r610 in
  let r612 = [R 298] in
  let r613 = Sub (r1) :: r612 in
  let r614 = S (T T_EQUAL) :: r613 in
  let r615 = [R 158] in
  let r616 = Sub (r44) :: r615 in
  let r617 = R 312 :: r616 in
  let r618 = [R 703] in
  let r619 = [R 704] in
  let r620 = [R 680] in
  let r621 = S (T T_RPAREN) :: r620 in
  let r622 = Sub (r529) :: r621 in
  let r623 = S (T T_LPAREN) :: r622 in
  let r624 = [R 184] in
  let r625 = [R 256] in
  let r626 = [R 884] in
  let r627 = Sub (r89) :: r626 in
  let r628 = S (T T_COLON) :: r627 in
  let r629 = [R 257] in
  let r630 = S (T T_RPAREN) :: r629 in
  let r631 = Sub (r628) :: r630 in
  let r632 = [R 886] in
  let r633 = [R 885] in
  let r634 = [R 258] in
  let r635 = [R 259] in
  let r636 = [R 702] in
  let r637 = [R 677] in
  let r638 = S (T T_RPAREN) :: r637 in
  let r639 = Sub (r1) :: r638 in
  let r640 = S (T T_LPAREN) :: r639 in
  let r641 = [R 615] in
  let r642 = [R 135] in
  let r643 = Sub (r1) :: r642 in
  let r644 = [R 186] in
  let r645 = Sub (r1) :: r644 in
  let r646 = [R 174] in
  let r647 = [R 168] in
  let r648 = [R 185] in
  let r649 = [R 636] in
  let r650 = Sub (r1) :: r649 in
  let r651 = [R 171] in
  let r652 = [R 175] in
  let r653 = [R 167] in
  let r654 = [R 170] in
  let r655 = [R 169] in
  let r656 = [R 179] in
  let r657 = [R 173] in
  let r658 = [R 172] in
  let r659 = [R 177] in
  let r660 = [R 166] in
  let r661 = [R 165] in
  let r662 = [R 188] in
  let r663 = [R 164] in
  let r664 = [R 178] in
  let r665 = [R 176] in
  let r666 = [R 180] in
  let r667 = [R 181] in
  let r668 = [R 182] in
  let r669 = [R 616] in
  let r670 = [R 183] in
  let r671 = [R 19] in
  let r672 = R 318 :: r671 in
  let r673 = Sub (r251) :: r672 in
  let r674 = [R 284] in
  let r675 = Sub (r1) :: r674 in
  let r676 = S (T T_EQUAL) :: r675 in
  let r677 = Sub (r89) :: r676 in
  let r678 = S (T T_DOT) :: r677 in
  let r679 = [R 282] in
  let r680 = Sub (r1) :: r679 in
  let r681 = S (T T_EQUAL) :: r680 in
  let r682 = Sub (r89) :: r681 in
  let r683 = [R 280] in
  let r684 = Sub (r1) :: r683 in
  let r685 = [R 803] in
  let r686 = [R 213] in
  let r687 = Sub (r1) :: r686 in
  let r688 = [R 286] in
  let r689 = Sub (r1) :: r688 in
  let r690 = S (T T_EQUAL) :: r689 in
  let r691 = [R 285] in
  let r692 = Sub (r1) :: r691 in
  let r693 = [R 532] in
  let r694 = [R 538] in
  let r695 = [R 543] in
  let r696 = [R 541] in
  let r697 = [R 531] in
  let r698 = [R 555] in
  let r699 = S (T T_RBRACKET) :: r698 in
  let r700 = Sub (r17) :: r699 in
  let r701 = [R 549] in
  let r702 = [R 550] in
  let r703 = [R 388] in
  let r704 = S (N N_module_expr) :: r703 in
  let r705 = S (T T_EQUAL) :: r704 in
  let r706 = [R 853] in
  let r707 = R 318 :: r706 in
  let r708 = Sub (r705) :: r707 in
  let r709 = Sub (r65) :: r708 in
  let r710 = R 312 :: r709 in
  let r711 = [R 414] in
  let r712 = R 318 :: r711 in
  let r713 = R 493 :: r712 in
  let r714 = Sub (r107) :: r713 in
  let r715 = R 312 :: r714 in
  let r716 = R 203 :: r715 in
  let r717 = [R 494] in
  let r718 = [R 319] in
  let r719 = [R 854] in
  let r720 = R 308 :: r719 in
  let r721 = R 318 :: r720 in
  let r722 = Sub (r705) :: r721 in
  let r723 = [R 389] in
  let r724 = S (N N_module_expr) :: r723 in
  let r725 = S (T T_EQUAL) :: r724 in
  let r726 = [R 309] in
  let r727 = R 308 :: r726 in
  let r728 = R 318 :: r727 in
  let r729 = Sub (r705) :: r728 in
  let r730 = Sub (r65) :: r729 in
  let r731 = [R 390] in
  let r732 = [R 243] in
  let r733 = S (T T_RBRACKET) :: r732 in
  let r734 = Sub (r15) :: r733 in
  let r735 = [R 209] in
  let r736 = S (T T_RBRACKET) :: r735 in
  let r737 = Sub (r17) :: r736 in
  let r738 = [R 430] in
  let r739 = S (T T_STRING) :: r738 in
  let r740 = [R 556] in
  let r741 = R 318 :: r740 in
  let r742 = Sub (r739) :: r741 in
  let r743 = S (T T_EQUAL) :: r742 in
  let r744 = Sub (r91) :: r743 in
  let r745 = S (T T_COLON) :: r744 in
  let r746 = Sub (r79) :: r745 in
  let r747 = R 312 :: r746 in
  let r748 = [R 552] in
  let r749 = Sub (r89) :: r748 in
  let r750 = Sub (r138) :: r415 in
  let r751 = [R 801] in
  let r752 = R 318 :: r751 in
  let r753 = R 312 :: r752 in
  let r754 = Sub (r750) :: r753 in
  let r755 = S (T T_EQUAL) :: r754 in
  let r756 = Sub (r140) :: r755 in
  let r757 = R 312 :: r756 in
  let r758 = [R 637] in
  let r759 = R 318 :: r758 in
  let r760 = R 312 :: r759 in
  let r761 = R 223 :: r760 in
  let r762 = Sub (r140) :: r761 in
  let r763 = R 312 :: r762 in
  let r764 = R 203 :: r763 in
  let r765 = [R 124] in
  let r766 = Sub (r81) :: r765 in
  let r767 = [R 224] in
  let r768 = [R 553] in
  let r769 = Sub (r87) :: r768 in
  let r770 = [R 245] in
  let r771 = R 312 :: r770 in
  let r772 = Sub (r769) :: r771 in
  let r773 = S (T T_COLON) :: r772 in
  let r774 = S (T T_LIDENT) :: r773 in
  let r775 = R 419 :: r774 in
  let r776 = [R 247] in
  let r777 = Sub (r775) :: r776 in
  let r778 = [R 128] in
  let r779 = S (T T_RBRACE) :: r778 in
  let r780 = [R 246] in
  let r781 = R 312 :: r780 in
  let r782 = S (T T_SEMI) :: r781 in
  let r783 = R 312 :: r782 in
  let r784 = Sub (r769) :: r783 in
  let r785 = S (T T_COLON) :: r784 in
  let r786 = [R 554] in
  let r787 = Sub (r87) :: r786 in
  let r788 = [R 125] in
  let r789 = [R 126] in
  let r790 = Sub (r81) :: r789 in
  let r791 = [R 127] in
  let r792 = S (T T_COLONCOLON) :: r425 in
  let r793 = [R 227] in
  let r794 = [R 228] in
  let r795 = Sub (r81) :: r794 in
  let r796 = [R 226] in
  let r797 = Sub (r81) :: r796 in
  let r798 = [R 225] in
  let r799 = Sub (r81) :: r798 in
  let r800 = [R 547] in
  let r801 = [R 577] in
  let r802 = Sub (r144) :: r801 in
  let r803 = [R 645] in
  let r804 = R 318 :: r803 in
  let r805 = Sub (r802) :: r804 in
  let r806 = R 557 :: r805 in
  let r807 = S (T T_PLUSEQ) :: r806 in
  let r808 = Sub (r136) :: r807 in
  let r809 = R 891 :: r808 in
  let r810 = R 312 :: r809 in
  let r811 = [R 646] in
  let r812 = R 318 :: r811 in
  let r813 = Sub (r802) :: r812 in
  let r814 = R 557 :: r813 in
  let r815 = S (T T_PLUSEQ) :: r814 in
  let r816 = Sub (r136) :: r815 in
  let r817 = [R 232] in
  let r818 = R 318 :: r817 in
  let r819 = R 580 :: r818 in
  let r820 = [R 442] in
  let r821 = S (T T_RBRACE) :: r820 in
  let r822 = [R 229] in
  let r823 = R 312 :: r822 in
  let r824 = R 223 :: r823 in
  let r825 = Sub (r140) :: r824 in
  let r826 = [R 440] in
  let r827 = [R 441] in
  let r828 = [R 445] in
  let r829 = S (T T_RBRACE) :: r828 in
  let r830 = [R 444] in
  let r831 = S (T T_RBRACE) :: r830 in
  let r832 = [R 231] in
  let r833 = R 318 :: r832 in
  let r834 = R 580 :: r833 in
  let r835 = [R 321] in
  let r836 = [R 448] in
  let r837 = R 318 :: r836 in
  let r838 = Sub (r298) :: r837 in
  let r839 = R 312 :: r838 in
  let r840 = [R 449] in
  let r841 = R 318 :: r840 in
  let r842 = Sub (r298) :: r841 in
  let r843 = R 312 :: r842 in
  let r844 = [R 391] in
  let r845 = S (N N_module_type) :: r844 in
  let r846 = S (T T_COLON) :: r845 in
  let r847 = [R 648] in
  let r848 = R 318 :: r847 in
  let r849 = Sub (r846) :: r848 in
  let r850 = Sub (r65) :: r849 in
  let r851 = R 312 :: r850 in
  let r852 = [R 415] in
  let r853 = R 318 :: r852 in
  let r854 = S (N N_module_type) :: r853 in
  let r855 = S (T T_COLONEQUAL) :: r854 in
  let r856 = Sub (r107) :: r855 in
  let r857 = R 312 :: r856 in
  let r858 = [R 404] in
  let r859 = R 318 :: r858 in
  let r860 = [R 651] in
  let r861 = R 310 :: r860 in
  let r862 = R 318 :: r861 in
  let r863 = S (N N_module_type) :: r862 in
  let r864 = S (T T_COLON) :: r863 in
  let r865 = [R 311] in
  let r866 = R 310 :: r865 in
  let r867 = R 318 :: r866 in
  let r868 = S (N N_module_type) :: r867 in
  let r869 = S (T T_COLON) :: r868 in
  let r870 = Sub (r65) :: r869 in
  let r871 = S (T T_UIDENT) :: r28 in
  let r872 = Sub (r871) :: r234 in
  let r873 = [R 649] in
  let r874 = R 318 :: r873 in
  let r875 = [R 392] in
  let r876 = S (T T_QUOTED_STRING_EXPR) :: r43 in
  let r877 = [R 84] in
  let r878 = Sub (r876) :: r877 in
  let r879 = [R 94] in
  let r880 = Sub (r878) :: r879 in
  let r881 = [R 655] in
  let r882 = R 304 :: r881 in
  let r883 = R 318 :: r882 in
  let r884 = Sub (r880) :: r883 in
  let r885 = S (T T_COLON) :: r884 in
  let r886 = S (T T_LIDENT) :: r885 in
  let r887 = R 210 :: r886 in
  let r888 = R 918 :: r887 in
  let r889 = R 312 :: r888 in
  let r890 = [R 98] in
  let r891 = R 306 :: r890 in
  let r892 = R 318 :: r891 in
  let r893 = Sub (r878) :: r892 in
  let r894 = S (T T_EQUAL) :: r893 in
  let r895 = S (T T_LIDENT) :: r894 in
  let r896 = R 210 :: r895 in
  let r897 = R 918 :: r896 in
  let r898 = R 312 :: r897 in
  let r899 = [R 607] in
  let r900 = Sub (r180) :: r899 in
  let r901 = [R 211] in
  let r902 = S (T T_RBRACKET) :: r901 in
  let r903 = [R 608] in
  let r904 = [R 85] in
  let r905 = S (T T_END) :: r904 in
  let r906 = R 327 :: r905 in
  let r907 = R 75 :: r906 in
  let r908 = [R 74] in
  let r909 = S (T T_RPAREN) :: r908 in
  let r910 = [R 77] in
  let r911 = R 318 :: r910 in
  let r912 = Sub (r89) :: r911 in
  let r913 = S (T T_COLON) :: r912 in
  let r914 = S (T T_LIDENT) :: r913 in
  let r915 = R 422 :: r914 in
  let r916 = [R 78] in
  let r917 = R 318 :: r916 in
  let r918 = Sub (r91) :: r917 in
  let r919 = S (T T_COLON) :: r918 in
  let r920 = S (T T_LIDENT) :: r919 in
  let r921 = R 559 :: r920 in
  let r922 = [R 76] in
  let r923 = R 318 :: r922 in
  let r924 = Sub (r878) :: r923 in
  let r925 = [R 87] in
  let r926 = Sub (r878) :: r925 in
  let r927 = S (T T_IN) :: r926 in
  let r928 = Sub (r872) :: r927 in
  let r929 = R 312 :: r928 in
  let r930 = [R 88] in
  let r931 = Sub (r878) :: r930 in
  let r932 = S (T T_IN) :: r931 in
  let r933 = Sub (r872) :: r932 in
  let r934 = [R 603] in
  let r935 = Sub (r89) :: r934 in
  let r936 = [R 83] in
  let r937 = Sub (r289) :: r936 in
  let r938 = S (T T_RBRACKET) :: r937 in
  let r939 = Sub (r935) :: r938 in
  let r940 = [R 604] in
  let r941 = [R 123] in
  let r942 = Sub (r89) :: r941 in
  let r943 = S (T T_EQUAL) :: r942 in
  let r944 = Sub (r89) :: r943 in
  let r945 = [R 79] in
  let r946 = R 318 :: r945 in
  let r947 = Sub (r944) :: r946 in
  let r948 = [R 80] in
  let r949 = [R 328] in
  let r950 = [R 307] in
  let r951 = R 306 :: r950 in
  let r952 = R 318 :: r951 in
  let r953 = Sub (r878) :: r952 in
  let r954 = S (T T_EQUAL) :: r953 in
  let r955 = S (T T_LIDENT) :: r954 in
  let r956 = R 210 :: r955 in
  let r957 = R 918 :: r956 in
  let r958 = [R 96] in
  let r959 = Sub (r880) :: r958 in
  let r960 = S (T T_MINUSGREATER) :: r959 in
  let r961 = Sub (r83) :: r960 in
  let r962 = [R 97] in
  let r963 = Sub (r880) :: r962 in
  let r964 = [R 95] in
  let r965 = Sub (r880) :: r964 in
  let r966 = S (T T_MINUSGREATER) :: r965 in
  let r967 = [R 305] in
  let r968 = R 304 :: r967 in
  let r969 = R 318 :: r968 in
  let r970 = Sub (r880) :: r969 in
  let r971 = S (T T_COLON) :: r970 in
  let r972 = S (T T_LIDENT) :: r971 in
  let r973 = R 210 :: r972 in
  let r974 = R 918 :: r973 in
  let r975 = [R 322] in
  let r976 = [R 639] in
  let r977 = [R 657] in
  let r978 = R 318 :: r977 in
  let r979 = S (N N_module_type) :: r978 in
  let r980 = R 312 :: r979 in
  let r981 = [R 643] in
  let r982 = [R 315] in
  let r983 = R 314 :: r982 in
  let r984 = R 318 :: r983 in
  let r985 = R 580 :: r984 in
  let r986 = R 887 :: r985 in
  let r987 = S (T T_LIDENT) :: r986 in
  let r988 = R 891 :: r987 in
  let r989 = [R 644] in
  let r990 = [R 317] in
  let r991 = R 316 :: r990 in
  let r992 = R 318 :: r991 in
  let r993 = R 580 :: r992 in
  let r994 = Sub (r194) :: r993 in
  let r995 = S (T T_COLONEQUAL) :: r994 in
  let r996 = S (T T_LIDENT) :: r995 in
  let r997 = R 891 :: r996 in
  let r998 = [R 56] in
  let r999 = Sub (r876) :: r998 in
  let r1000 = [R 65] in
  let r1001 = Sub (r999) :: r1000 in
  let r1002 = S (T T_EQUAL) :: r1001 in
  let r1003 = [R 857] in
  let r1004 = R 302 :: r1003 in
  let r1005 = R 318 :: r1004 in
  let r1006 = Sub (r1002) :: r1005 in
  let r1007 = S (T T_LIDENT) :: r1006 in
  let r1008 = R 210 :: r1007 in
  let r1009 = R 918 :: r1008 in
  let r1010 = R 312 :: r1009 in
  let r1011 = [R 275] in
  let r1012 = S (T T_RPAREN) :: r1011 in
  let r1013 = Sub (r89) :: r1012 in
  let r1014 = [R 93] in
  let r1015 = S (T T_END) :: r1014 in
  let r1016 = R 329 :: r1015 in
  let r1017 = R 73 :: r1016 in
  let r1018 = [R 913] in
  let r1019 = Sub (r1) :: r1018 in
  let r1020 = S (T T_EQUAL) :: r1019 in
  let r1021 = S (T T_LIDENT) :: r1020 in
  let r1022 = R 417 :: r1021 in
  let r1023 = R 312 :: r1022 in
  let r1024 = [R 59] in
  let r1025 = R 318 :: r1024 in
  let r1026 = [R 914] in
  let r1027 = Sub (r1) :: r1026 in
  let r1028 = S (T T_EQUAL) :: r1027 in
  let r1029 = S (T T_LIDENT) :: r1028 in
  let r1030 = R 417 :: r1029 in
  let r1031 = [R 916] in
  let r1032 = Sub (r1) :: r1031 in
  let r1033 = [R 912] in
  let r1034 = Sub (r89) :: r1033 in
  let r1035 = S (T T_COLON) :: r1034 in
  let r1036 = [R 915] in
  let r1037 = Sub (r1) :: r1036 in
  let r1038 = [R 361] in
  let r1039 = Sub (r444) :: r1038 in
  let r1040 = S (T T_LIDENT) :: r1039 in
  let r1041 = R 557 :: r1040 in
  let r1042 = R 312 :: r1041 in
  let r1043 = [R 60] in
  let r1044 = R 318 :: r1043 in
  let r1045 = [R 362] in
  let r1046 = Sub (r444) :: r1045 in
  let r1047 = S (T T_LIDENT) :: r1046 in
  let r1048 = R 557 :: r1047 in
  let r1049 = [R 364] in
  let r1050 = Sub (r1) :: r1049 in
  let r1051 = S (T T_EQUAL) :: r1050 in
  let r1052 = [R 366] in
  let r1053 = Sub (r1) :: r1052 in
  let r1054 = S (T T_EQUAL) :: r1053 in
  let r1055 = Sub (r89) :: r1054 in
  let r1056 = S (T T_DOT) :: r1055 in
  let r1057 = [R 360] in
  let r1058 = Sub (r91) :: r1057 in
  let r1059 = S (T T_COLON) :: r1058 in
  let r1060 = [R 363] in
  let r1061 = Sub (r1) :: r1060 in
  let r1062 = S (T T_EQUAL) :: r1061 in
  let r1063 = [R 365] in
  let r1064 = Sub (r1) :: r1063 in
  let r1065 = S (T T_EQUAL) :: r1064 in
  let r1066 = Sub (r89) :: r1065 in
  let r1067 = S (T T_DOT) :: r1066 in
  let r1068 = [R 62] in
  let r1069 = R 318 :: r1068 in
  let r1070 = Sub (r1) :: r1069 in
  let r1071 = [R 57] in
  let r1072 = R 318 :: r1071 in
  let r1073 = R 487 :: r1072 in
  let r1074 = Sub (r999) :: r1073 in
  let r1075 = [R 58] in
  let r1076 = R 318 :: r1075 in
  let r1077 = R 487 :: r1076 in
  let r1078 = Sub (r999) :: r1077 in
  let r1079 = [R 89] in
  let r1080 = S (T T_RPAREN) :: r1079 in
  let r1081 = [R 52] in
  let r1082 = Sub (r999) :: r1081 in
  let r1083 = S (T T_IN) :: r1082 in
  let r1084 = Sub (r872) :: r1083 in
  let r1085 = R 312 :: r1084 in
  let r1086 = [R 291] in
  let r1087 = R 318 :: r1086 in
  let r1088 = Sub (r251) :: r1087 in
  let r1089 = R 564 :: r1088 in
  let r1090 = R 312 :: r1089 in
  let r1091 = [R 53] in
  let r1092 = Sub (r999) :: r1091 in
  let r1093 = S (T T_IN) :: r1092 in
  let r1094 = Sub (r872) :: r1093 in
  let r1095 = [R 91] in
  let r1096 = Sub (r227) :: r1095 in
  let r1097 = S (T T_RBRACKET) :: r1096 in
  let r1098 = [R 68] in
  let r1099 = Sub (r999) :: r1098 in
  let r1100 = S (T T_MINUSGREATER) :: r1099 in
  let r1101 = Sub (r566) :: r1100 in
  let r1102 = [R 50] in
  let r1103 = Sub (r1101) :: r1102 in
  let r1104 = [R 51] in
  let r1105 = Sub (r999) :: r1104 in
  let r1106 = [R 255] in
  let r1107 = [R 290] in
  let r1108 = R 318 :: r1107 in
  let r1109 = Sub (r251) :: r1108 in
  let r1110 = [R 92] in
  let r1111 = S (T T_RPAREN) :: r1110 in
  let r1112 = [R 488] in
  let r1113 = [R 61] in
  let r1114 = R 318 :: r1113 in
  let r1115 = Sub (r944) :: r1114 in
  let r1116 = [R 63] in
  let r1117 = [R 330] in
  let r1118 = [R 66] in
  let r1119 = Sub (r999) :: r1118 in
  let r1120 = S (T T_EQUAL) :: r1119 in
  let r1121 = [R 67] in
  let r1122 = [R 303] in
  let r1123 = R 302 :: r1122 in
  let r1124 = R 318 :: r1123 in
  let r1125 = Sub (r1002) :: r1124 in
  let r1126 = S (T T_LIDENT) :: r1125 in
  let r1127 = R 210 :: r1126 in
  let r1128 = R 918 :: r1127 in
  let r1129 = [R 326] in
  let r1130 = [R 845] in
  let r1131 = [R 859] in
  let r1132 = R 318 :: r1131 in
  let r1133 = S (N N_module_expr) :: r1132 in
  let r1134 = R 312 :: r1133 in
  let r1135 = [R 849] in
  let r1136 = [R 842] in
  let r1137 = R 323 :: r1136 in
  let r1138 = [R 679] in
  let r1139 = S (T T_RBRACKET) :: r1138 in
  let r1140 = Sub (r1) :: r1139 in
  let r1141 = [R 678] in
  let r1142 = S (T T_RBRACE) :: r1141 in
  let r1143 = Sub (r1) :: r1142 in
  let r1144 = [R 681] in
  let r1145 = S (T T_RPAREN) :: r1144 in
  let r1146 = Sub (r529) :: r1145 in
  let r1147 = S (T T_LPAREN) :: r1146 in
  let r1148 = [R 685] in
  let r1149 = S (T T_RBRACKET) :: r1148 in
  let r1150 = Sub (r529) :: r1149 in
  let r1151 = [R 683] in
  let r1152 = S (T T_RBRACE) :: r1151 in
  let r1153 = Sub (r529) :: r1152 in
  let r1154 = [R 194] in
  let r1155 = [R 684] in
  let r1156 = S (T T_RBRACKET) :: r1155 in
  let r1157 = Sub (r529) :: r1156 in
  let r1158 = [R 198] in
  let r1159 = [R 682] in
  let r1160 = S (T T_RBRACE) :: r1159 in
  let r1161 = Sub (r529) :: r1160 in
  let r1162 = [R 196] in
  let r1163 = [R 191] in
  let r1164 = [R 193] in
  let r1165 = [R 192] in
  let r1166 = [R 195] in
  let r1167 = [R 199] in
  let r1168 = [R 197] in
  let r1169 = [R 190] in
  let r1170 = [R 299] in
  let r1171 = Sub (r1) :: r1170 in
  let r1172 = [R 301] in
  let r1173 = [R 700] in
  let r1174 = [R 724] in
  let r1175 = [R 723] in
  let r1176 = [R 101] in
  let r1177 = [R 105] in
  let r1178 = S (N N_expr) :: r1177 in
  let r1179 = S (T T_IN) :: r1178 in
  let r1180 = [R 102] in
  let r1181 = Sub (r1179) :: r1180 in
  let r1182 = S (N N_pattern) :: r1181 in
  let r1183 = R 312 :: r1182 in
  let r1184 = [R 597] in
  let r1185 = Sub (r1183) :: r1184 in
  let r1186 = [R 100] in
  let r1187 = [R 598] in
  let r1188 = [R 103] in
  let r1189 = S (N N_expr) :: r1188 in
  let r1190 = S (T T_IN) :: r1189 in
  let r1191 = [R 104] in
  let r1192 = S (N N_expr) :: r1191 in
  let r1193 = Sub (r589) :: r1192 in
  let r1194 = [R 717] in
  let r1195 = [R 713] in
  let r1196 = [R 712] in
  let r1197 = [R 716] in
  let r1198 = [R 719] in
  let r1199 = [R 718] in
  let r1200 = [R 715] in
  let r1201 = S (T T_LIDENT) :: r534 in
  let r1202 = [R 701] in
  let r1203 = S (T T_GREATERRBRACE) :: r1202 in
  let r1204 = [R 709] in
  let r1205 = S (T T_RBRACE) :: r1204 in
  let r1206 = [R 567] in
  let r1207 = Sub (r539) :: r1206 in
  let r1208 = [R 142] in
  let r1209 = S (T T_DONE) :: r1208 in
  let r1210 = Sub (r1) :: r1209 in
  let r1211 = S (T T_DO) :: r1210 in
  let r1212 = Sub (r1) :: r1211 in
  let r1213 = Sub (r589) :: r1212 in
  let r1214 = [R 217] in
  let r1215 = Sub (r569) :: r1214 in
  let r1216 = S (T T_RPAREN) :: r1215 in
  let r1217 = [R 215] in
  let r1218 = Sub (r1) :: r1217 in
  let r1219 = S (T T_MINUSGREATER) :: r1218 in
  let r1220 = [R 216] in
  let r1221 = [R 594] in
  let r1222 = [R 154] in
  let r1223 = [R 686] in
  let r1224 = [R 697] in
  let r1225 = [R 726] in
  let r1226 = [R 710] in
  let r1227 = [R 727] in
  let r1228 = [R 145] in
  let r1229 = Sub (r1) :: r1228 in
  let r1230 = S (T T_IN) :: r1229 in
  let r1231 = Sub (r705) :: r1230 in
  let r1232 = Sub (r65) :: r1231 in
  let r1233 = R 312 :: r1232 in
  let r1234 = [R 146] in
  let r1235 = Sub (r1) :: r1234 in
  let r1236 = S (T T_IN) :: r1235 in
  let r1237 = R 312 :: r1236 in
  let r1238 = R 223 :: r1237 in
  let r1239 = Sub (r140) :: r1238 in
  let r1240 = R 312 :: r1239 in
  let r1241 = [R 342] in
  let r1242 = Sub (r259) :: r1241 in
  let r1243 = [R 346] in
  let r1244 = Sub (r1242) :: r1243 in
  let r1245 = S (T T_RPAREN) :: r1244 in
  let r1246 = Sub (r411) :: r1245 in
  let r1247 = [R 343] in
  let r1248 = Sub (r1) :: r1247 in
  let r1249 = [R 345] in
  let r1250 = [R 283] in
  let r1251 = Sub (r1) :: r1250 in
  let r1252 = S (T T_EQUAL) :: r1251 in
  let r1253 = Sub (r89) :: r1252 in
  let r1254 = [R 281] in
  let r1255 = Sub (r1) :: r1254 in
  let r1256 = [R 721] in
  let r1257 = [R 728] in
  let r1258 = [R 689] in
  let r1259 = S (T T_RPAREN) :: r1258 in
  let r1260 = S (N N_module_expr) :: r1259 in
  let r1261 = R 312 :: r1260 in
  let r1262 = [R 690] in
  let r1263 = S (T T_RPAREN) :: r1262 in
  let r1264 = [R 676] in
  let r1265 = [R 508] in
  let r1266 = S (T T_RPAREN) :: r1265 in
  let r1267 = [R 506] in
  let r1268 = S (T T_RPAREN) :: r1267 in
  let r1269 = [R 507] in
  let r1270 = S (T T_RPAREN) :: r1269 in
  let r1271 = [R 325] in
  let r1272 = R 323 :: r1271 in
  let r1273 = [R 357] in
  let r1274 = R 312 :: r1273 in
  let r1275 = Sub (r769) :: r1274 in
  let r1276 = [R 355] in
  let r1277 = [R 32] in
  let r1278 = [R 825] in
  let r1279 = Sub (r83) :: r1278 in
  let r1280 = S (T T_MINUSGREATER) :: r1279 in
  let r1281 = S (T T_RPAREN) :: r1280 in
  let r1282 = Sub (r89) :: r1281 in
  let r1283 = [R 826] in
  let r1284 = [R 831] in
  let r1285 = Sub (r83) :: r1284 in
  let r1286 = S (T T_MINUSGREATER) :: r1285 in
  let r1287 = [R 829] in
  let r1288 = Sub (r83) :: r1287 in
  let r1289 = S (T T_MINUSGREATER) :: r1288 in
  let r1290 = S (T T_RPAREN) :: r1289 in
  let r1291 = Sub (r89) :: r1290 in
  let r1292 = [R 830] in
  let r1293 = [R 832] in
  let r1294 = [R 828] in
  let r1295 = [R 837] in
  let r1296 = Sub (r83) :: r1295 in
  let r1297 = S (T T_MINUSGREATER) :: r1296 in
  let r1298 = S (T T_RPAREN) :: r1297 in
  let r1299 = Sub (r89) :: r1298 in
  let r1300 = [R 838] in
  let r1301 = [R 834] in
  let r1302 = [R 443] in
  let r1303 = S (T T_RBRACE) :: r1302 in
  let r1304 = [R 207] in
  let r1305 = R 312 :: r1304 in
  let r1306 = [R 208] in
  let r1307 = R 312 :: r1306 in
  let r1308 = [R 72] in
  let r1309 = S (T T_RPAREN) :: r1308 in
  let r1310 = [R 138] in
  let r1311 = [R 140] in
  let r1312 = [R 139] in
  let r1313 = [R 237] in
  let r1314 = [R 242] in
  let r1315 = [R 372] in
  let r1316 = [R 375] in
  let r1317 = S (T T_RPAREN) :: r1316 in
  let r1318 = S (T T_COLONCOLON) :: r1317 in
  let r1319 = S (T T_LPAREN) :: r1318 in
  let r1320 = [R 511] in
  let r1321 = [R 512] in
  let r1322 = [R 513] in
  let r1323 = [R 514] in
  let r1324 = [R 515] in
  let r1325 = [R 516] in
  let r1326 = [R 517] in
  let r1327 = [R 518] in
  let r1328 = [R 519] in
  let r1329 = [R 520] in
  let r1330 = [R 521] in
  let r1331 = [R 871] in
  let r1332 = [R 864] in
  let r1333 = [R 880] in
  let r1334 = [R 332] in
  let r1335 = [R 878] in
  let r1336 = S (T T_SEMISEMI) :: r1335 in
  let r1337 = [R 879] in
  let r1338 = [R 334] in
  let r1339 = [R 337] in
  let r1340 = [R 336] in
  let r1341 = [R 335] in
  let r1342 = R 333 :: r1341 in
  let r1343 = [R 907] in
  let r1344 = S (T T_EOF) :: r1343 in
  let r1345 = R 333 :: r1344 in
  let r1346 = [R 906] in
  function
  | 0 | 2009 | 2013 | 2031 | 2035 | 2039 | 2043 | 2047 | 2051 | 2055 | 2059 | 2063 | 2067 | 2073 | 2101 -> Nothing
  | 2008 -> One ([R 0])
  | 2012 -> One ([R 1])
  | 2018 -> One ([R 2])
  | 2032 -> One ([R 3])
  | 2036 -> One ([R 4])
  | 2042 -> One ([R 5])
  | 2044 -> One ([R 6])
  | 2048 -> One ([R 7])
  | 2052 -> One ([R 8])
  | 2056 -> One ([R 9])
  | 2060 -> One ([R 10])
  | 2066 -> One ([R 11])
  | 2070 -> One ([R 12])
  | 2091 -> One ([R 13])
  | 2111 -> One ([R 14])
  | 229 -> One ([R 15])
  | 228 -> One ([R 16])
  | 2026 -> One ([R 20])
  | 2028 -> One ([R 21])
  | 298 -> One ([R 26])
  | 314 -> One ([R 27])
  | 328 -> One ([R 28])
  | 297 -> One ([R 29])
  | 313 -> One ([R 30])
  | 309 -> One ([R 44])
  | 1413 -> One ([R 49])
  | 1422 -> One ([R 54])
  | 1417 -> One ([R 55])
  | 1458 -> One ([R 64])
  | 1425 -> One ([R 69])
  | 1200 -> One ([R 81])
  | 1180 -> One ([R 82])
  | 1182 -> One ([R 86])
  | 1420 -> One ([R 90])
  | 440 -> One ([R 106])
  | 449 -> One ([R 107])
  | 74 -> One ([R 108])
  | 447 -> One ([R 109])
  | 444 -> One ([R 111])
  | 73 -> One ([R 115])
  | 212 | 943 -> One ([R 116])
  | 980 -> One ([R 119])
  | 1014 -> One ([R 129])
  | 1018 -> One ([R 130])
  | 332 -> One ([R 132])
  | 1640 -> One ([R 133])
  | 736 -> One ([R 144])
  | 1597 -> One ([R 160])
  | 759 -> One ([R 161])
  | 781 -> One ([R 162])
  | 762 -> One ([R 163])
  | 779 -> One ([R 200])
  | 1 -> One (R 203 :: r7)
  | 62 -> One (R 203 :: r26)
  | 67 -> One (R 203 :: r31)
  | 70 -> One (R 203 :: r42)
  | 77 -> One (R 203 :: r50)
  | 101 -> One (R 203 :: r69)
  | 112 -> One (R 203 :: r97)
  | 230 -> One (R 203 :: r214)
  | 231 -> One (R 203 :: r218)
  | 237 -> One (R 203 :: r230)
  | 250 -> One (R 203 :: r239)
  | 253 -> One (R 203 :: r244)
  | 262 -> One (R 203 :: r256)
  | 437 -> One (R 203 :: r391)
  | 464 -> One (R 203 :: r405)
  | 574 -> One (R 203 :: r478)
  | 666 -> One (R 203 :: r549)
  | 669 -> One (R 203 :: r552)
  | 672 -> One (R 203 :: r557)
  | 675 -> One (R 203 :: r560)
  | 681 -> One (R 203 :: r573)
  | 689 -> One (R 203 :: r587)
  | 694 -> One (R 203 :: r599)
  | 711 -> One (R 203 :: r611)
  | 725 -> One (R 203 :: r617)
  | 884 -> One (R 203 :: r710)
  | 925 -> One (R 203 :: r747)
  | 1070 -> One (R 203 :: r839)
  | 1071 -> One (R 203 :: r843)
  | 1080 -> One (R 203 :: r851)
  | 1121 -> One (R 203 :: r889)
  | 1122 -> One (R 203 :: r898)
  | 1261 -> One (R 203 :: r980)
  | 1293 -> One (R 203 :: r1010)
  | 1499 -> One (R 203 :: r1134)
  | 1767 -> One (R 203 :: r1233)
  | 1774 -> One (R 203 :: r1240)
  | 1826 -> One (R 203 :: r1261)
  | 322 -> One ([R 219])
  | 586 -> One ([R 222])
  | 158 -> One ([R 235])
  | 923 -> One ([R 238])
  | 924 -> One ([R 239])
  | 136 -> One (R 240 :: r103)
  | 140 -> One (R 240 :: r105)
  | 227 -> One ([R 244])
  | 966 -> One ([R 248])
  | 967 -> One ([R 249])
  | 1416 -> One ([R 254])
  | 876 -> One ([R 277])
  | 847 -> One ([R 279])
  | 1496 -> One ([R 289])
  | 1423 -> One ([R 292])
  | 534 -> One ([R 293])
  | 1784 -> One ([R 296])
  | 110 -> One (R 312 :: r77)
  | 174 -> One (R 312 :: r132)
  | 199 -> One (R 312 :: r191)
  | 235 -> One (R 312 :: r223)
  | 577 -> One (R 312 :: r482)
  | 584 -> One (R 312 :: r492)
  | 829 -> One (R 312 :: r673)
  | 907 -> One (R 312 :: r730)
  | 1099 -> One (R 312 :: r870)
  | 1136 -> One (R 312 :: r907)
  | 1142 -> One (R 312 :: r915)
  | 1153 -> One (R 312 :: r921)
  | 1164 -> One (R 312 :: r924)
  | 1168 -> One (R 312 :: r933)
  | 1189 -> One (R 312 :: r947)
  | 1205 -> One (R 312 :: r957)
  | 1240 -> One (R 312 :: r974)
  | 1267 -> One (R 312 :: r988)
  | 1277 -> One (R 312 :: r997)
  | 1310 -> One (R 312 :: r1017)
  | 1314 -> One (R 312 :: r1030)
  | 1343 -> One (R 312 :: r1048)
  | 1382 -> One (R 312 :: r1070)
  | 1386 -> One (R 312 :: r1074)
  | 1387 -> One (R 312 :: r1078)
  | 1398 -> One (R 312 :: r1094)
  | 1406 -> One (R 312 :: r1103)
  | 1450 -> One (R 312 :: r1115)
  | 1470 -> One (R 312 :: r1128)
  | 1871 -> One (R 312 :: r1276)
  | 1266 -> One (R 314 :: r981)
  | 1504 -> One (R 314 :: r1135)
  | 1276 -> One (R 316 :: r989)
  | 892 -> One (R 318 :: r718)
  | 1198 -> One (R 318 :: r948)
  | 1259 -> One (R 318 :: r976)
  | 1456 -> One (R 318 :: r1116)
  | 1497 -> One (R 318 :: r1130)
  | 1509 -> One (R 318 :: r1137)
  | 1861 -> One (R 318 :: r1272)
  | 2096 -> One (R 318 :: r1336)
  | 2107 -> One (R 318 :: r1342)
  | 2112 -> One (R 318 :: r1345)
  | 1069 -> One (R 320 :: r835)
  | 1251 -> One (R 320 :: r975)
  | 226 -> One (R 323 :: r210)
  | 1480 -> One (R 323 :: r1129)
  | 1201 -> One (R 327 :: r949)
  | 1459 -> One (R 329 :: r1117)
  | 2094 -> One (R 331 :: r1334)
  | 2102 -> One (R 333 :: r1338)
  | 2103 -> One (R 333 :: r1339)
  | 2104 -> One (R 333 :: r1340)
  | 518 -> One ([R 339])
  | 522 -> One ([R 341])
  | 770 -> One ([R 348])
  | 1493 -> One ([R 349])
  | 1722 -> One ([R 352])
  | 1874 -> One ([R 353])
  | 1877 -> One ([R 354])
  | 1876 -> One ([R 356])
  | 1875 -> One ([R 358])
  | 1873 -> One ([R 359])
  | 2027 -> One ([R 371])
  | 2017 -> One ([R 373])
  | 2025 -> One ([R 374])
  | 2024 -> One ([R 376])
  | 702 -> One ([R 383])
  | 1684 -> One ([R 384])
  | 642 -> One ([R 395])
  | 652 -> One ([R 396])
  | 653 -> One ([R 397])
  | 651 -> One ([R 398])
  | 654 -> One ([R 400])
  | 173 -> One ([R 401])
  | 105 | 1090 -> One ([R 402])
  | 613 -> One ([R 409])
  | 590 -> One ([R 410])
  | 620 -> One ([R 413])
  | 1316 | 1329 -> One ([R 418])
  | 951 -> One ([R 420])
  | 952 -> One ([R 421])
  | 1146 -> One ([R 423])
  | 1144 -> One ([R 424])
  | 1147 -> One ([R 425])
  | 1145 -> One ([R 426])
  | 482 -> One ([R 429])
  | 936 -> One ([R 431])
  | 1026 -> One ([R 432])
  | 1936 -> One ([R 433])
  | 1042 -> One ([R 434])
  | 1937 -> One ([R 435])
  | 1041 -> One ([R 436])
  | 1033 -> One ([R 437])
  | 95 | 257 -> One ([R 450])
  | 119 | 720 -> One ([R 451])
  | 147 -> One ([R 452])
  | 135 -> One ([R 454])
  | 139 -> One ([R 456])
  | 143 -> One ([R 458])
  | 126 -> One ([R 459])
  | 146 | 1617 -> One ([R 460])
  | 125 -> One ([R 461])
  | 124 -> One ([R 462])
  | 123 -> One ([R 463])
  | 122 -> One ([R 464])
  | 121 -> One ([R 465])
  | 98 | 116 | 710 -> One ([R 466])
  | 97 | 709 -> One ([R 467])
  | 96 -> One ([R 468])
  | 118 | 488 | 719 -> One ([R 469])
  | 117 | 718 -> One ([R 470])
  | 93 -> One ([R 471])
  | 99 -> One ([R 472])
  | 128 -> One ([R 473])
  | 120 -> One ([R 474])
  | 127 -> One ([R 475])
  | 100 -> One ([R 476])
  | 145 -> One ([R 477])
  | 148 -> One ([R 478])
  | 144 -> One ([R 480])
  | 385 -> One ([R 481])
  | 384 -> One (R 482 :: r343)
  | 274 -> One (R 483 :: r276)
  | 275 -> One ([R 484])
  | 519 -> One (R 485 :: r427)
  | 520 -> One ([R 486])
  | 1673 -> One ([R 500])
  | 164 -> One ([R 501])
  | 474 -> One ([R 523])
  | 468 -> One ([R 524])
  | 469 -> One ([R 526])
  | 467 | 721 -> One ([R 533])
  | 871 -> One ([R 539])
  | 872 -> One ([R 540])
  | 873 -> One ([R 542])
  | 546 -> One ([R 544])
  | 1292 -> One ([R 548])
  | 1048 | 1363 -> One ([R 558])
  | 1157 -> One ([R 560])
  | 1155 -> One ([R 561])
  | 1158 -> One ([R 562])
  | 1156 -> One ([R 563])
  | 1432 -> One (R 564 :: r1109)
  | 265 -> One ([R 565])
  | 1024 -> One ([R 568])
  | 1025 -> One ([R 569])
  | 1020 -> One ([R 570])
  | 1953 -> One ([R 572])
  | 1952 -> One ([R 573])
  | 1954 -> One ([R 574])
  | 1949 -> One ([R 575])
  | 1950 -> One ([R 576])
  | 1054 -> One ([R 578])
  | 1052 -> One ([R 579])
  | 1650 -> One ([R 582])
  | 1649 -> One ([R 583])
  | 635 -> One ([R 584])
  | 587 -> One ([R 585])
  | 1419 -> One ([R 586])
  | 1418 -> One ([R 587])
  | 407 -> One ([R 589])
  | 377 -> One ([R 619])
  | 1536 -> One ([R 622])
  | 1537 -> One ([R 623])
  | 1745 -> One ([R 625])
  | 1746 -> One ([R 626])
  | 513 -> One ([R 628])
  | 514 -> One ([R 629])
  | 1676 -> One ([R 631])
  | 1677 -> One ([R 632])
  | 784 -> One ([R 634])
  | 788 -> One ([R 635])
  | 1287 -> One ([R 640])
  | 1250 -> One ([R 641])
  | 1253 -> One ([R 642])
  | 1252 -> One ([R 647])
  | 1257 -> One ([R 650])
  | 1256 -> One ([R 652])
  | 1255 -> One ([R 653])
  | 1254 -> One ([R 654])
  | 1288 -> One ([R 656])
  | 91 -> One ([R 660])
  | 89 -> One ([R 661])
  | 90 -> One ([R 662])
  | 86 -> One ([R 664])
  | 84 -> One ([R 665])
  | 85 -> One ([R 666])
  | 701 -> One ([R 692])
  | 765 | 780 -> One ([R 693])
  | 704 | 761 -> One ([R 694])
  | 1544 | 1594 -> One ([R 699])
  | 764 -> One ([R 705])
  | 766 -> One ([R 725])
  | 450 -> One ([R 729])
  | 454 -> One ([R 732])
  | 486 -> One ([R 737])
  | 459 -> One ([R 738])
  | 515 -> One ([R 741])
  | 477 -> One ([R 746])
  | 455 -> One ([R 747])
  | 29 -> One ([R 748])
  | 8 -> One ([R 749])
  | 53 -> One ([R 751])
  | 52 -> One ([R 752])
  | 51 -> One ([R 753])
  | 50 -> One ([R 754])
  | 49 -> One ([R 755])
  | 48 -> One ([R 756])
  | 47 -> One ([R 757])
  | 46 -> One ([R 758])
  | 45 -> One ([R 759])
  | 44 -> One ([R 760])
  | 43 -> One ([R 761])
  | 42 -> One ([R 762])
  | 41 -> One ([R 763])
  | 40 -> One ([R 764])
  | 39 -> One ([R 765])
  | 38 -> One ([R 766])
  | 37 -> One ([R 767])
  | 36 -> One ([R 768])
  | 35 -> One ([R 769])
  | 34 -> One ([R 770])
  | 33 -> One ([R 771])
  | 32 -> One ([R 772])
  | 31 -> One ([R 773])
  | 30 -> One ([R 774])
  | 28 -> One ([R 775])
  | 27 -> One ([R 776])
  | 26 -> One ([R 777])
  | 25 -> One ([R 778])
  | 24 -> One ([R 779])
  | 23 -> One ([R 780])
  | 22 -> One ([R 781])
  | 21 -> One ([R 782])
  | 20 -> One ([R 783])
  | 19 -> One ([R 784])
  | 18 -> One ([R 785])
  | 17 -> One ([R 786])
  | 16 -> One ([R 787])
  | 15 -> One ([R 788])
  | 14 -> One ([R 789])
  | 13 -> One ([R 790])
  | 12 -> One ([R 791])
  | 11 -> One ([R 792])
  | 10 -> One ([R 793])
  | 9 -> One ([R 794])
  | 7 -> One ([R 795])
  | 6 -> One ([R 796])
  | 5 -> One ([R 797])
  | 4 -> One ([R 798])
  | 3 -> One ([R 799])
  | 1488 -> One ([R 800])
  | 347 -> One ([R 805])
  | 374 -> One ([R 806])
  | 362 -> One ([R 807])
  | 368 -> One ([R 808])
  | 1889 -> One ([R 809])
  | 1912 -> One ([R 810])
  | 1900 -> One ([R 811])
  | 1906 -> One ([R 812])
  | 1931 -> One ([R 813])
  | 376 -> One ([R 814])
  | 1921 -> One ([R 815])
  | 319 -> One ([R 816])
  | 1514 -> One ([R 841])
  | 1492 | 1513 -> One ([R 843])
  | 1495 | 1515 -> One ([R 844])
  | 1506 -> One ([R 846])
  | 1489 -> One ([R 847])
  | 1479 -> One ([R 848])
  | 1487 -> One ([R 852])
  | 1491 -> One ([R 855])
  | 1490 -> One ([R 856])
  | 1507 -> One ([R 858])
  | 249 -> One ([R 860])
  | 248 -> One ([R 861])
  | 2085 -> One ([R 865])
  | 2086 -> One ([R 866])
  | 2088 -> One ([R 867])
  | 2089 -> One ([R 868])
  | 2087 -> One ([R 869])
  | 2084 -> One ([R 870])
  | 2077 -> One ([R 872])
  | 2078 -> One ([R 873])
  | 2080 -> One ([R 874])
  | 2081 -> One ([R 875])
  | 2079 -> One ([R 876])
  | 2076 -> One ([R 877])
  | 2090 -> One ([R 881])
  | 306 -> One ([R 883])
  | 593 -> One (R 891 :: r509)
  | 607 -> One ([R 892])
  | 180 -> One ([R 897])
  | 183 -> One ([R 898])
  | 187 -> One ([R 899])
  | 181 -> One ([R 900])
  | 188 -> One ([R 901])
  | 184 -> One ([R 902])
  | 189 -> One ([R 903])
  | 186 -> One ([R 904])
  | 179 -> One ([R 905])
  | 451 -> One ([R 910])
  | 763 -> One ([R 911])
  | 1125 -> One ([R 919])
  | 1327 -> One ([R 920])
  | 1330 -> One ([R 921])
  | 1328 -> One ([R 922])
  | 1361 -> One ([R 923])
  | 1364 -> One ([R 924])
  | 1362 -> One ([R 925])
  | 596 -> One ([R 932])
  | 597 -> One ([R 933])
  | 1669 -> One (S (T T_WITH) :: r1207)
  | 169 -> One (S (T T_TYPE) :: r129)
  | 548 -> One (S (T T_TYPE) :: r450)
  | 1792 -> One (S (T T_TYPE) :: r1246)
  | 971 -> One (S (T T_STAR) :: r790)
  | 2092 -> One (S (T T_SEMISEMI) :: r1333)
  | 2099 -> One (S (T T_SEMISEMI) :: r1337)
  | 2014 -> One (S (T T_RPAREN) :: r56)
  | 323 -> One (S (T T_RPAREN) :: r313)
  | 348 -> One (S (T T_RPAREN) :: r325)
  | 462 -> One (S (T T_RPAREN) :: r402)
  | 506 -> One (S (T T_RPAREN) :: r426)
  | 579 -> One (S (T T_RPAREN) :: r483)
  | 644 -> One (S (T T_RPAREN) :: r524)
  | 1618 -> One (S (T T_RPAREN) :: r1173)
  | 1836 -> One (S (T T_RPAREN) :: r1264)
  | 2015 -> One (S (T T_RPAREN) :: r1315)
  | 277 -> One (S (T T_RBRACKET) :: r277)
  | 947 | 1009 -> One (S (T T_RBRACKET) :: r371)
  | 1658 -> One (S (T T_RBRACKET) :: r1198)
  | 1660 -> One (S (T T_RBRACKET) :: r1199)
  | 1663 -> One (S (T T_RBRACKET) :: r1200)
  | 1753 -> One (S (T T_RBRACKET) :: r1225)
  | 334 -> One (S (T T_QUOTE) :: r317)
  | 1166 -> One (S (T T_OPEN) :: r929)
  | 1390 -> One (S (T T_OPEN) :: r1085)
  | 218 | 221 | 223 | 321 | 353 | 1891 -> One (S (T T_MODULE) :: r118)
  | 989 -> One (S (T T_MINUSGREATER) :: r797)
  | 993 -> One (S (T T_MINUSGREATER) :: r799)
  | 1227 -> One (S (T T_MINUSGREATER) :: r963)
  | 129 -> One (S (T T_LPAREN) :: r100)
  | 553 -> One (S (T T_LOCAL) :: r454)
  | 684 | 1299 | 1704 -> One (S (T T_LOCAL) :: r576)
  | 161 -> One (S (T T_LIDENT) :: r113)
  | 269 -> One (S (T T_LIDENT) :: r262)
  | 418 -> One (S (T T_LIDENT) :: r354)
  | 729 -> One (S (T T_LIDENT) :: r618)
  | 737 -> One (S (T T_LIDENT) :: r625)
  | 738 -> One (S (T T_LIDENT) :: r631)
  | 749 -> One (S (T T_LIDENT) :: r634)
  | 753 -> One (S (T T_LIDENT) :: r636)
  | 953 -> One (S (T T_LIDENT) :: r785)
  | 1331 -> One (S (T T_LIDENT) :: r1035)
  | 1365 -> One (S (T T_LIDENT) :: r1059)
  | 1442 -> One (S (T T_LIDENT) :: r1112)
  | 82 -> One (S (T T_INT) :: r54)
  | 87 -> One (S (T T_INT) :: r55)
  | 767 -> One (S (T T_IN) :: r643)
  | 771 -> One (S (T T_IN) :: r645)
  | 1410 -> One (S (T T_IN) :: r1105)
  | 659 -> One (S (T T_GREATERRBRACE) :: r532)
  | 1748 -> One (S (T T_GREATERRBRACE) :: r1224)
  | 222 -> One (S (T T_GREATER) :: r206)
  | 1879 -> One (S (T T_GREATER) :: r1277)
  | 625 -> One (S (T T_EQUAL) :: r520)
  | 844 -> One (S (T T_EQUAL) :: r684)
  | 850 -> One (S (T T_EQUAL) :: r687)
  | 860 -> One (S (T T_EQUAL) :: r692)
  | 1321 -> One (S (T T_EQUAL) :: r1032)
  | 1339 -> One (S (T T_EQUAL) :: r1037)
  | 1608 -> One (S (T T_EQUAL) :: r1171)
  | 1798 -> One (S (T T_EQUAL) :: r1248)
  | 1811 -> One (S (T T_EQUAL) :: r1255)
  | 2006 -> One (S (T T_EOF) :: r1313)
  | 2010 -> One (S (T T_EOF) :: r1314)
  | 2029 -> One (S (T T_EOF) :: r1320)
  | 2033 -> One (S (T T_EOF) :: r1321)
  | 2037 -> One (S (T T_EOF) :: r1322)
  | 2040 -> One (S (T T_EOF) :: r1323)
  | 2045 -> One (S (T T_EOF) :: r1324)
  | 2049 -> One (S (T T_EOF) :: r1325)
  | 2053 -> One (S (T T_EOF) :: r1326)
  | 2057 -> One (S (T T_EOF) :: r1327)
  | 2061 -> One (S (T T_EOF) :: r1328)
  | 2064 -> One (S (T T_EOF) :: r1329)
  | 2068 -> One (S (T T_EOF) :: r1330)
  | 2116 -> One (S (T T_EOF) :: r1346)
  | 1735 -> One (S (T T_END) :: r1223)
  | 131 -> One (S (T T_DOTDOT) :: r101)
  | 215 -> One (S (T T_DOTDOT) :: r196)
  | 1027 -> One (S (T T_DOTDOT) :: r826)
  | 1028 -> One (S (T T_DOTDOT) :: r827)
  | 241 | 1530 | 1577 -> One (S (T T_DOT) :: r232)
  | 337 -> One (S (T T_DOT) :: r322)
  | 354 -> One (S (T T_DOT) :: r334)
  | 409 -> One (S (T T_DOT) :: r353)
  | 536 -> One (S (T T_DOT) :: r441)
  | 566 -> One (S (T T_DOT) :: r461)
  | 2071 -> One (S (T T_DOT) :: r521)
  | 839 -> One (S (T T_DOT) :: r682)
  | 930 -> One (S (T T_DOT) :: r749)
  | 956 -> One (S (T T_DOT) :: r787)
  | 987 -> One (S (T T_DOT) :: r795)
  | 1305 -> One (S (T T_DOT) :: r1013)
  | 1806 -> One (S (T T_DOT) :: r1253)
  | 1881 -> One (S (T T_DOT) :: r1282)
  | 1892 -> One (S (T T_DOT) :: r1291)
  | 1913 -> One (S (T T_DOT) :: r1299)
  | 2019 -> One (S (T T_DOT) :: r1319)
  | 258 -> One (S (T T_COLONRBRACKET) :: r247)
  | 424 -> One (S (T T_COLONRBRACKET) :: r369)
  | 527 -> One (S (T T_COLONRBRACKET) :: r429)
  | 1620 -> One (S (T T_COLONRBRACKET) :: r1174)
  | 1622 -> One (S (T T_COLONRBRACKET) :: r1175)
  | 1647 -> One (S (T T_COLONRBRACKET) :: r1194)
  | 1820 -> One (S (T T_COLONRBRACKET) :: r1256)
  | 1823 -> One (S (T T_COLONRBRACKET) :: r1257)
  | 216 | 944 -> One (S (T T_COLONCOLON) :: r198)
  | 581 -> One (S (T T_COLON) :: r486)
  | 1221 -> One (S (T T_COLON) :: r961)
  | 1867 -> One (S (T T_COLON) :: r1275)
  | 425 -> One (S (T T_BARRBRACKET) :: r370)
  | 524 -> One (S (T T_BARRBRACKET) :: r428)
  | 657 -> One (S (T T_BARRBRACKET) :: r527)
  | 1651 -> One (S (T T_BARRBRACKET) :: r1195)
  | 1653 -> One (S (T T_BARRBRACKET) :: r1196)
  | 1656 -> One (S (T T_BARRBRACKET) :: r1197)
  | 1756 -> One (S (T T_BARRBRACKET) :: r1226)
  | 1759 -> One (S (T T_BARRBRACKET) :: r1227)
  | 396 -> One (S (T T_BAR) :: r347)
  | 80 -> One (S (N N_pattern) :: r52)
  | 479 -> One (S (N N_pattern) :: r58)
  | 436 -> One (S (N N_pattern) :: r385)
  | 470 -> One (S (N N_pattern) :: r406)
  | 472 -> One (S (N N_pattern) :: r407)
  | 493 -> One (S (N N_pattern) :: r418)
  | 498 -> One (S (N N_pattern) :: r422)
  | 863 -> One (S (N N_pattern) :: r693)
  | 865 -> One (S (N N_pattern) :: r694)
  | 867 -> One (S (N N_pattern) :: r695)
  | 874 -> One (S (N N_pattern) :: r697)
  | 880 -> One (S (N N_pattern) :: r701)
  | 1631 -> One (S (N N_pattern) :: r1190)
  | 108 -> One (S (N N_module_type) :: r71)
  | 583 -> One (S (N N_module_type) :: r488)
  | 621 -> One (S (N N_module_type) :: r517)
  | 623 -> One (S (N N_module_type) :: r518)
  | 648 -> One (S (N N_module_type) :: r526)
  | 889 -> One (S (N N_module_type) :: r717)
  | 901 -> One (S (N N_module_type) :: r725)
  | 1831 -> One (S (N N_module_type) :: r1263)
  | 1846 -> One (S (N N_module_type) :: r1266)
  | 1849 -> One (S (N N_module_type) :: r1268)
  | 1852 -> One (S (N N_module_type) :: r1270)
  | 234 -> One (S (N N_module_expr) :: r220)
  | 532 -> One (S (N N_let_pattern) :: r435)
  | 533 -> One (S (N N_let_pattern) :: r438)
  | 261 -> One (S (N N_expr) :: r249)
  | 661 -> One (S (N N_expr) :: r535)
  | 665 -> One (S (N N_expr) :: r546)
  | 735 -> One (S (N N_expr) :: r624)
  | 760 -> One (S (N N_expr) :: r641)
  | 775 -> One (S (N N_expr) :: r646)
  | 777 -> One (S (N N_expr) :: r647)
  | 782 -> One (S (N N_expr) :: r648)
  | 789 -> One (S (N N_expr) :: r651)
  | 791 -> One (S (N N_expr) :: r652)
  | 793 -> One (S (N N_expr) :: r653)
  | 795 -> One (S (N N_expr) :: r654)
  | 797 -> One (S (N N_expr) :: r655)
  | 799 -> One (S (N N_expr) :: r656)
  | 801 -> One (S (N N_expr) :: r657)
  | 803 -> One (S (N N_expr) :: r658)
  | 805 -> One (S (N N_expr) :: r659)
  | 807 -> One (S (N N_expr) :: r660)
  | 809 -> One (S (N N_expr) :: r661)
  | 811 -> One (S (N N_expr) :: r662)
  | 813 -> One (S (N N_expr) :: r663)
  | 815 -> One (S (N N_expr) :: r664)
  | 817 -> One (S (N N_expr) :: r665)
  | 819 -> One (S (N N_expr) :: r666)
  | 821 -> One (S (N N_expr) :: r667)
  | 823 -> One (S (N N_expr) :: r668)
  | 825 -> One (S (N N_expr) :: r669)
  | 827 -> One (S (N N_expr) :: r670)
  | 1549 -> One (S (N N_expr) :: r1154)
  | 1554 -> One (S (N N_expr) :: r1158)
  | 1559 -> One (S (N N_expr) :: r1162)
  | 1565 -> One (S (N N_expr) :: r1163)
  | 1570 -> One (S (N N_expr) :: r1164)
  | 1575 -> One (S (N N_expr) :: r1165)
  | 1582 -> One (S (N N_expr) :: r1166)
  | 1587 -> One (S (N N_expr) :: r1167)
  | 1592 -> One (S (N N_expr) :: r1168)
  | 1595 -> One (S (N N_expr) :: r1169)
  | 1625 -> One (S (N N_expr) :: r1176)
  | 1638 -> One (S (N N_expr) :: r1193)
  | 1732 -> One (S (N N_expr) :: r1222)
  | 259 -> One (Sub (r1) :: r248)
  | 422 -> One (Sub (r1) :: r361)
  | 680 -> One (Sub (r1) :: r564)
  | 699 -> One (Sub (r1) :: r600)
  | 882 -> One (Sub (r1) :: r702)
  | 1696 -> One (Sub (r1) :: r1213)
  | 1991 -> One (Sub (r1) :: r1311)
  | 1993 -> One (Sub (r1) :: r1312)
  | 2 -> One (Sub (r11) :: r12)
  | 56 -> One (Sub (r11) :: r13)
  | 60 -> One (Sub (r11) :: r20)
  | 224 -> One (Sub (r11) :: r209)
  | 785 -> One (Sub (r11) :: r650)
  | 878 -> One (Sub (r11) :: r700)
  | 919 -> One (Sub (r11) :: r734)
  | 921 -> One (Sub (r11) :: r737)
  | 1391 -> One (Sub (r11) :: r1090)
  | 678 -> One (Sub (r35) :: r561)
  | 1726 -> One (Sub (r35) :: r1221)
  | 1989 -> One (Sub (r37) :: r1310)
  | 76 -> One (Sub (r44) :: r45)
  | 664 -> One (Sub (r44) :: r544)
  | 700 -> One (Sub (r44) :: r601)
  | 731 -> One (Sub (r44) :: r619)
  | 751 -> One (Sub (r44) :: r635)
  | 1414 -> One (Sub (r44) :: r1106)
  | 897 -> One (Sub (r65) :: r722)
  | 1094 -> One (Sub (r65) :: r864)
  | 1001 -> One (Sub (r74) :: r800)
  | 267 -> One (Sub (r79) :: r261)
  | 500 -> One (Sub (r79) :: r423)
  | 869 -> One (Sub (r79) :: r696)
  | 307 -> One (Sub (r81) :: r306)
  | 316 -> One (Sub (r81) :: r308)
  | 969 -> One (Sub (r81) :: r788)
  | 973 -> One (Sub (r81) :: r791)
  | 986 -> One (Sub (r81) :: r793)
  | 1708 -> One (Sub (r81) :: r1219)
  | 217 -> One (Sub (r83) :: r201)
  | 300 -> One (Sub (r83) :: r303)
  | 301 -> One (Sub (r83) :: r304)
  | 304 -> One (Sub (r83) :: r305)
  | 320 -> One (Sub (r83) :: r311)
  | 344 -> One (Sub (r83) :: r324)
  | 352 -> One (Sub (r83) :: r329)
  | 359 -> One (Sub (r83) :: r335)
  | 365 -> One (Sub (r83) :: r336)
  | 371 -> One (Sub (r83) :: r337)
  | 1229 -> One (Sub (r83) :: r966)
  | 1886 -> One (Sub (r83) :: r1283)
  | 1890 -> One (Sub (r83) :: r1286)
  | 1897 -> One (Sub (r83) :: r1292)
  | 1903 -> One (Sub (r83) :: r1293)
  | 1909 -> One (Sub (r83) :: r1294)
  | 1918 -> One (Sub (r83) :: r1300)
  | 1928 -> One (Sub (r83) :: r1301)
  | 388 -> One (Sub (r87) :: r344)
  | 600 -> One (Sub (r87) :: r511)
  | 273 -> One (Sub (r89) :: r269)
  | 329 -> One (Sub (r89) :: r315)
  | 350 -> One (Sub (r89) :: r326)
  | 433 -> One (Sub (r89) :: r384)
  | 535 -> One (Sub (r89) :: r439)
  | 603 -> One (Sub (r89) :: r514)
  | 722 -> One (Sub (r89) :: r614)
  | 740 -> One (Sub (r89) :: r632)
  | 744 -> One (Sub (r89) :: r633)
  | 856 -> One (Sub (r89) :: r690)
  | 1138 -> One (Sub (r89) :: r909)
  | 1176 -> One (Sub (r89) :: r940)
  | 1979 -> One (Sub (r89) :: r1309)
  | 1347 -> One (Sub (r91) :: r1051)
  | 1371 -> One (Sub (r91) :: r1062)
  | 192 -> One (Sub (r107) :: r187)
  | 338 -> One (Sub (r107) :: r323)
  | 2074 -> One (Sub (r107) :: r1331)
  | 2082 -> One (Sub (r107) :: r1332)
  | 557 -> One (Sub (r119) :: r458)
  | 441 -> One (Sub (r136) :: r392)
  | 445 -> One (Sub (r136) :: r394)
  | 1131 -> One (Sub (r180) :: r903)
  | 204 -> One (Sub (r182) :: r192)
  | 185 -> One (Sub (r184) :: r186)
  | 195 -> One (Sub (r189) :: r190)
  | 211 -> One (Sub (r194) :: r195)
  | 1008 -> One (Sub (r194) :: r819)
  | 1057 -> One (Sub (r194) :: r834)
  | 270 -> One (Sub (r264) :: r266)
  | 271 -> One (Sub (r264) :: r268)
  | 419 -> One (Sub (r264) :: r357)
  | 420 -> One (Sub (r264) :: r360)
  | 381 -> One (Sub (r271) :: r338)
  | 279 -> One (Sub (r273) :: r279)
  | 294 -> One (Sub (r273) :: r302)
  | 280 -> One (Sub (r285) :: r287)
  | 281 -> One (Sub (r289) :: r290)
  | 311 -> One (Sub (r289) :: r307)
  | 325 -> One (Sub (r289) :: r314)
  | 284 -> One (Sub (r298) :: r300)
  | 629 -> One (Sub (r298) :: r522)
  | 1091 -> One (Sub (r298) :: r859)
  | 404 -> One (Sub (r349) :: r351)
  | 1614 -> One (Sub (r363) :: r1172)
  | 423 -> One (Sub (r365) :: r368)
  | 428 -> One (Sub (r381) :: r383)
  | 552 -> One (Sub (r388) :: r451)
  | 456 -> One (Sub (r397) :: r398)
  | 480 -> One (Sub (r411) :: r414)
  | 685 -> One (Sub (r411) :: r579)
  | 833 -> One (Sub (r411) :: r678)
  | 1348 -> One (Sub (r411) :: r1056)
  | 1372 -> One (Sub (r411) :: r1067)
  | 1705 -> One (Sub (r411) :: r1216)
  | 530 -> One (Sub (r431) :: r432)
  | 849 -> One (Sub (r446) :: r685)
  | 633 -> One (Sub (r502) :: r523)
  | 592 -> One (Sub (r504) :: r505)
  | 662 -> One (Sub (r541) :: r543)
  | 1668 -> One (Sub (r541) :: r1205)
  | 1712 -> One (Sub (r569) :: r1220)
  | 913 -> One (Sub (r705) :: r731)
  | 1944 -> One (Sub (r750) :: r1305)
  | 1956 -> One (Sub (r750) :: r1307)
  | 949 -> One (Sub (r766) :: r767)
  | 950 -> One (Sub (r777) :: r779)
  | 1010 -> One (Sub (r777) :: r821)
  | 1029 -> One (Sub (r777) :: r829)
  | 1037 -> One (Sub (r777) :: r831)
  | 1932 -> One (Sub (r777) :: r1303)
  | 1115 -> One (Sub (r846) :: r875)
  | 1108 -> One (Sub (r872) :: r874)
  | 1438 -> One (Sub (r880) :: r1111)
  | 1462 -> One (Sub (r880) :: r1120)
  | 1127 -> One (Sub (r900) :: r902)
  | 1402 -> One (Sub (r935) :: r1097)
  | 1389 -> One (Sub (r999) :: r1080)
  | 1466 -> One (Sub (r1002) :: r1121)
  | 1313 -> One (Sub (r1023) :: r1025)
  | 1342 -> One (Sub (r1042) :: r1044)
  | 1629 -> One (Sub (r1183) :: r1187)
  | 1627 -> One (Sub (r1185) :: r1186)
  | 1665 -> One (Sub (r1201) :: r1203)
  | 1803 -> One (Sub (r1242) :: r1249)
  | 774 -> One (r0)
  | 2005 -> One (r2)
  | 2004 -> One (r3)
  | 2003 -> One (r4)
  | 2002 -> One (r5)
  | 2001 -> One (r6)
  | 59 -> One (r7)
  | 54 -> One (r8)
  | 55 -> One (r10)
  | 58 -> One (r12)
  | 57 -> One (r13)
  | 1508 -> One (r14)
  | 1512 -> One (r16)
  | 2000 -> One (r18)
  | 1999 -> One (r19)
  | 61 -> One (r20)
  | 1998 -> One (r21)
  | 1997 -> One (r22)
  | 1996 -> One (r23)
  | 1995 -> One (r24)
  | 64 -> One (r25)
  | 63 -> One (r26)
  | 65 -> One (r27)
  | 66 -> One (r28)
  | 1988 -> One (r29)
  | 69 -> One (r30)
  | 68 -> One (r31)
  | 1723 -> One (r32)
  | 1721 -> One (r33)
  | 679 -> One (r34)
  | 1728 -> One (r36)
  | 1987 -> One (r38)
  | 1986 -> One (r39)
  | 1985 -> One (r40)
  | 72 -> One (r41)
  | 71 -> One (r42)
  | 75 -> One (r43)
  | 1825 -> One (r45)
  | 1984 -> One (r46)
  | 1983 -> One (r47)
  | 1982 -> One (r48)
  | 79 -> One (r49)
  | 78 -> One (r50)
  | 1978 -> One (r51)
  | 1977 -> One (r52)
  | 81 -> One (r53)
  | 83 -> One (r54)
  | 88 -> One (r55)
  | 94 -> One (r56)
  | 492 -> One (r57)
  | 491 | 564 | 1303 -> One (r58)
  | 149 -> One (r59)
  | 151 -> One (r61)
  | 150 -> One (r62)
  | 115 -> One (r63)
  | 104 -> One (r64)
  | 107 -> One (r66)
  | 106 -> One (r67)
  | 103 -> One (r68)
  | 102 -> One (r69)
  | 1976 -> One (r70)
  | 1975 -> One (r71)
  | 109 | 156 -> One (r72)
  | 1291 -> One (r73)
  | 1974 -> One (r75)
  | 1973 -> One (r76)
  | 111 -> One (r77)
  | 152 | 260 | 663 | 1683 -> One (r78)
  | 155 -> One (r80)
  | 315 -> One (r82)
  | 299 -> One (r84)
  | 330 -> One (r86)
  | 333 -> One (r88)
  | 939 -> One (r90)
  | 1972 -> One (r92)
  | 1971 -> One (r93)
  | 154 -> One (r94)
  | 153 -> One (r95)
  | 114 -> One (r96)
  | 113 -> One (r97)
  | 134 -> One (r98)
  | 133 -> One (r99)
  | 130 -> One (r100)
  | 132 -> One (r101)
  | 138 -> One (r102)
  | 137 -> One (r103)
  | 142 -> One (r104)
  | 141 -> One (r105)
  | 159 -> One (r106)
  | 214 -> One (r108)
  | 213 -> One (r109)
  | 559 -> One (r110)
  | 558 -> One (r111)
  | 163 -> One (r112)
  | 162 -> One (r113)
  | 1970 -> One (r114)
  | 1969 -> One (r115)
  | 168 -> One (r116)
  | 167 -> One (r117)
  | 166 -> One (r118)
  | 1930 -> One (r120)
  | 1927 -> One (r121)
  | 1926 -> One (r122)
  | 1925 -> One (r123)
  | 1924 -> One (r124)
  | 1923 -> One (r125)
  | 1968 -> One (r126)
  | 172 -> One (r127)
  | 171 -> One (r128)
  | 170 -> One (r129)
  | 1967 -> One (r130)
  | 1966 -> One (r131)
  | 175 -> One (r132)
  | 282 -> One (r133)
  | 308 -> One (r135)
  | 448 -> One (r137)
  | 1000 -> One (r139)
  | 1036 -> One (r141)
  | 1035 -> One (r142)
  | 1034 | 1955 -> One (r143)
  | 1951 -> One (r145)
  | 1965 -> One (r147)
  | 1964 -> One (r148)
  | 1963 -> One (r149)
  | 1962 -> One (r150)
  | 1961 -> One (r151)
  | 1063 -> One (r155)
  | 1062 -> One (r156)
  | 1061 -> One (r157)
  | 1948 -> One (r163)
  | 1947 -> One (r164)
  | 1941 -> One (r165)
  | 1940 -> One (r166)
  | 1939 -> One (r167)
  | 1045 -> One (r169)
  | 1044 -> One (r170)
  | 1043 -> One (r171)
  | 191 -> One (r175)
  | 198 -> One (r177)
  | 194 | 208 -> One (r178)
  | 190 | 207 -> One (r179)
  | 201 -> One (r181)
  | 206 -> One (r183)
  | 203 -> One (r185)
  | 202 -> One (r186)
  | 193 -> One (r187)
  | 197 -> One (r188)
  | 196 -> One (r190)
  | 200 -> One (r191)
  | 205 -> One (r192)
  | 1013 -> One (r193)
  | 1938 -> One (r195)
  | 1935 -> One (r196)
  | 946 -> One (r197)
  | 945 -> One (r198)
  | 318 -> One (r199)
  | 303 -> One (r200)
  | 1922 -> One (r201)
  | 1911 -> One (r202)
  | 1908 -> One (r203)
  | 1907 -> One (r204)
  | 220 -> One (r205)
  | 1878 -> One (r206)
  | 1866 -> One (r207)
  | 1865 -> One (r208)
  | 225 -> One (r209)
  | 1864 -> One (r210)
  | 1860 -> One (r211)
  | 1859 -> One (r212)
  | 1858 -> One (r213)
  | 1857 -> One (r214)
  | 1856 -> One (r215)
  | 1855 -> One (r216)
  | 233 -> One (r217)
  | 232 -> One (r218)
  | 647 -> One (r219)
  | 646 -> One (r220)
  | 1845 -> One (r221)
  | 1844 -> One (r222)
  | 236 -> One (r223)
  | 240 -> One (r224)
  | 246 -> One (r226)
  | 247 -> One (r228)
  | 239 -> One (r229)
  | 238 -> One (r230)
  | 244 -> One (r231)
  | 242 -> One (r232)
  | 243 -> One (r233)
  | 245 -> One (r234)
  | 1843 -> One (r235)
  | 1842 -> One (r236)
  | 1841 -> One (r237)
  | 252 -> One (r238)
  | 251 -> One (r239)
  | 1840 -> One (r240)
  | 1839 -> One (r241)
  | 1838 -> One (r242)
  | 255 -> One (r243)
  | 254 -> One (r244)
  | 1835 -> One (r245)
  | 1834 -> One (r246)
  | 1819 -> One (r247)
  | 1818 -> One (r248)
  | 1817 -> One (r249)
  | 831 -> One (r250)
  | 1816 -> One (r252)
  | 1815 -> One (r253)
  | 266 -> One (r254)
  | 264 -> One (r255)
  | 263 -> One (r256)
  | 1797 -> One (r257)
  | 1796 -> One (r258)
  | 1814 -> One (r260)
  | 268 -> One (r261)
  | 417 -> One (r262)
  | 272 -> One (r263)
  | 416 -> One (r265)
  | 415 -> One (r266)
  | 414 -> One (r267)
  | 413 -> One (r268)
  | 412 -> One (r269)
  | 393 -> One (r270)
  | 378 -> One (r272)
  | 403 -> One (r274)
  | 402 -> One (r275)
  | 276 -> One (r276)
  | 278 -> One (r277)
  | 401 -> One (r278)
  | 400 -> One (r279)
  | 296 -> One (r280)
  | 295 -> One (r281)
  | 392 -> One (r283)
  | 383 -> One (r284)
  | 395 -> One (r286)
  | 394 -> One (r287)
  | 292 | 1232 -> One (r288)
  | 293 -> One (r290)
  | 288 -> One (r291)
  | 287 -> One (r292)
  | 291 -> One (r294)
  | 289 -> One (r297)
  | 286 -> One (r299)
  | 285 -> One (r300)
  | 380 -> One (r301)
  | 379 -> One (r302)
  | 375 -> One (r303)
  | 302 -> One (r304)
  | 305 -> One (r305)
  | 310 -> One (r306)
  | 312 -> One (r307)
  | 317 -> One (r308)
  | 373 -> One (r309)
  | 370 -> One (r310)
  | 369 -> One (r311)
  | 327 -> One (r312)
  | 324 -> One (r313)
  | 326 -> One (r314)
  | 331 -> One (r315)
  | 336 -> One (r316)
  | 335 -> One (r317)
  | 346 -> One (r318)
  | 343 -> One (r319)
  | 342 -> One (r320)
  | 341 -> One (r321)
  | 340 -> One (r322)
  | 339 -> One (r323)
  | 345 -> One (r324)
  | 349 -> One (r325)
  | 351 -> One (r326)
  | 367 -> One (r327)
  | 364 -> One (r328)
  | 363 -> One (r329)
  | 361 -> One (r330)
  | 358 -> One (r331)
  | 357 -> One (r332)
  | 356 -> One (r333)
  | 355 -> One (r334)
  | 360 -> One (r335)
  | 366 -> One (r336)
  | 372 -> One (r337)
  | 382 -> One (r338)
  | 391 -> One (r339)
  | 390 -> One (r341)
  | 387 -> One (r342)
  | 386 -> One (r343)
  | 389 -> One (r344)
  | 399 -> One (r345)
  | 398 -> One (r346)
  | 397 -> One (r347)
  | 408 -> One (r348)
  | 406 -> One (r350)
  | 405 -> One (r351)
  | 411 -> One (r352)
  | 410 -> One (r353)
  | 1791 -> One (r354)
  | 1790 -> One (r355)
  | 1789 -> One (r356)
  | 1788 -> One (r357)
  | 1787 -> One (r358)
  | 1786 -> One (r359)
  | 421 -> One (r360)
  | 1785 -> One (r361)
  | 529 -> One (r362)
  | 1616 -> One (r364)
  | 1613 -> One (r366)
  | 1612 -> One (r367)
  | 1611 -> One (r368)
  | 526 -> One (r369)
  | 523 -> One (r370)
  | 427 -> One (r371)
  | 512 -> One (r372)
  | 511 -> One (r374)
  | 510 -> One (r375)
  | 429 -> One (r376)
  | 517 -> One (r378)
  | 435 -> One (r379)
  | 432 -> One (r380)
  | 431 -> One (r382)
  | 430 -> One (r383)
  | 434 -> One (r384)
  | 516 -> One (r385)
  | 452 | 855 -> One (r387)
  | 453 -> One (r389)
  | 439 -> One (r390)
  | 438 -> One (r391)
  | 442 -> One (r392)
  | 443 -> One (r393)
  | 446 -> One (r394)
  | 458 -> One (r396)
  | 457 -> One (r398)
  | 509 -> One (r399)
  | 508 -> One (r400)
  | 461 -> One (r401)
  | 463 -> One (r402)
  | 503 -> One (r403)
  | 466 -> One (r404)
  | 465 -> One (r405)
  | 471 -> One (r406)
  | 473 -> One (r407)
  | 476 -> One (r408)
  | 502 -> One (r409)
  | 481 -> One (r410)
  | 485 -> One (r412)
  | 484 -> One (r413)
  | 483 -> One (r414)
  | 487 -> One (r415)
  | 490 -> One (r416)
  | 489 -> One (r417)
  | 494 -> One (r418)
  | 497 -> One (r419)
  | 496 -> One (r420)
  | 495 | 565 | 1304 -> One (r421)
  | 499 -> One (r422)
  | 501 -> One (r423)
  | 505 -> One (r424)
  | 504 -> One (r425)
  | 507 -> One (r426)
  | 521 -> One (r427)
  | 525 -> One (r428)
  | 528 -> One (r429)
  | 531 -> One (r430)
  | 547 -> One (r432)
  | 545 -> One (r433)
  | 544 -> One (r434)
  | 543 -> One (r435)
  | 542 -> One (r436)
  | 541 -> One (r437)
  | 540 -> One (r438)
  | 539 -> One (r439)
  | 538 -> One (r440)
  | 537 -> One (r441)
  | 1782 -> One (r442)
  | 571 -> One (r443)
  | 853 -> One (r445)
  | 1783 -> One (r447)
  | 551 -> One (r448)
  | 550 -> One (r449)
  | 549 -> One (r450)
  | 570 -> One (r451)
  | 556 -> One (r452)
  | 555 -> One (r453)
  | 554 -> One (r454)
  | 563 -> One (r455)
  | 562 -> One (r456)
  | 561 -> One (r457)
  | 560 -> One (r458)
  | 569 -> One (r459)
  | 568 -> One (r460)
  | 567 -> One (r461)
  | 1766 -> One (r462)
  | 1765 -> One (r463)
  | 1764 -> One (r464)
  | 1763 -> One (r465)
  | 1762 -> One (r466)
  | 573 -> One (r467)
  | 1486 -> One (r468)
  | 1485 -> One (r469)
  | 1484 -> One (r470)
  | 1483 -> One (r471)
  | 1482 -> One (r472)
  | 1481 -> One (r473)
  | 1761 -> One (r474)
  | 656 -> One (r475)
  | 655 -> One (r476)
  | 576 -> One (r477)
  | 575 -> One (r478)
  | 643 -> One (r479)
  | 641 -> One (r480)
  | 640 -> One (r481)
  | 578 -> One (r482)
  | 580 -> One (r483)
  | 639 -> One (r484)
  | 638 -> One (r485)
  | 582 -> One (r486)
  | 637 -> One (r487)
  | 636 -> One (r488)
  | 591 -> One (r489)
  | 589 -> One (r490)
  | 588 -> One (r491)
  | 585 -> One (r492)
  | 619 -> One (r493)
  | 618 -> One (r495)
  | 612 -> One (r497)
  | 611 -> One (r498)
  | 610 -> One (r499)
  | 609 -> One (r500)
  | 608 -> One (r501)
  | 631 -> One (r503)
  | 632 -> One (r505)
  | 599 -> One (r506)
  | 598 -> One (r507)
  | 595 -> One (r508)
  | 594 -> One (r509)
  | 602 -> One (r510)
  | 601 -> One (r511)
  | 606 -> One (r512)
  | 605 -> One (r513)
  | 604 -> One (r514)
  | 617 -> One (r515)
  | 622 -> One (r517)
  | 624 -> One (r518)
  | 627 -> One (r519)
  | 626 -> One (r520)
  | 628 | 2072 -> One (r521)
  | 630 -> One (r522)
  | 634 -> One (r523)
  | 645 -> One (r524)
  | 650 -> One (r525)
  | 649 -> One (r526)
  | 1755 -> One (r527)
  | 1535 | 1624 | 1655 | 1662 | 1752 | 1758 | 1822 -> One (r528)
  | 1751 -> One (r530)
  | 1750 -> One (r531)
  | 1747 -> One (r532)
  | 1744 -> One (r533)
  | 660 -> One (r534)
  | 1743 -> One (r535)
  | 1675 -> One (r536)
  | 1674 -> One (r537)
  | 1672 -> One (r538)
  | 1678 -> One (r540)
  | 1742 -> One (r542)
  | 1741 -> One (r543)
  | 1740 -> One (r544)
  | 1739 -> One (r545)
  | 1738 -> One (r546)
  | 1737 -> One (r547)
  | 668 -> One (r548)
  | 667 -> One (r549)
  | 1734 -> One (r550)
  | 671 -> One (r551)
  | 670 -> One (r552)
  | 1731 -> One (r553)
  | 1730 -> One (r554)
  | 1729 -> One (r555)
  | 674 -> One (r556)
  | 673 -> One (r557)
  | 1725 -> One (r558)
  | 677 -> One (r559)
  | 676 -> One (r560)
  | 1724 -> One (r561)
  | 1720 -> One (r562)
  | 1719 -> One (r563)
  | 1718 -> One (r564)
  | 848 -> One (r565)
  | 1703 -> One (r567)
  | 688 -> One (r568)
  | 1717 -> One (r570)
  | 1716 -> One (r571)
  | 683 -> One (r572)
  | 682 -> One (r573)
  | 1302 -> One (r574)
  | 1301 -> One (r575)
  | 1300 -> One (r576)
  | 1715 -> One (r577)
  | 687 -> One (r578)
  | 686 -> One (r579)
  | 1695 -> One (r580)
  | 1694 -> One (r581)
  | 1693 -> One (r582)
  | 1692 -> One (r583)
  | 693 -> One (r584)
  | 692 -> One (r585)
  | 691 -> One (r586)
  | 690 -> One (r587)
  | 1641 -> One (r588)
  | 1691 -> One (r590)
  | 1690 -> One (r591)
  | 1689 -> One (r592)
  | 1688 -> One (r593)
  | 1687 -> One (r594)
  | 1686 -> One (r595)
  | 698 -> One (r596)
  | 697 -> One (r597)
  | 696 -> One (r598)
  | 695 -> One (r599)
  | 1685 -> One (r600)
  | 703 -> One (r601)
  | 708 -> One (r602)
  | 707 -> One (r603)
  | 706 | 1682 -> One (r604)
  | 1681 -> One (r605)
  | 717 -> One (r606)
  | 716 -> One (r607)
  | 715 -> One (r608)
  | 714 -> One (r609)
  | 713 -> One (r610)
  | 712 -> One (r611)
  | 1607 -> One (r612)
  | 724 -> One (r613)
  | 723 -> One (r614)
  | 728 -> One (r615)
  | 727 -> One (r616)
  | 726 -> One (r617)
  | 730 -> One (r618)
  | 732 -> One (r619)
  | 1548 | 1600 -> One (r620)
  | 1547 | 1599 -> One (r621)
  | 734 | 1546 -> One (r622)
  | 733 | 1545 -> One (r623)
  | 1598 -> One (r624)
  | 748 -> One (r625)
  | 743 -> One (r626)
  | 742 | 832 | 1805 -> One (r627)
  | 747 -> One (r629)
  | 746 -> One (r630)
  | 739 -> One (r631)
  | 741 -> One (r632)
  | 745 -> One (r633)
  | 750 -> One (r634)
  | 752 -> One (r635)
  | 754 -> One (r636)
  | 758 | 1564 -> One (r637)
  | 757 | 1563 -> One (r638)
  | 756 | 1562 -> One (r639)
  | 755 | 1561 -> One (r640)
  | 1523 -> One (r641)
  | 769 -> One (r642)
  | 768 -> One (r643)
  | 773 -> One (r644)
  | 772 -> One (r645)
  | 776 -> One (r646)
  | 778 -> One (r647)
  | 783 -> One (r648)
  | 787 -> One (r649)
  | 786 -> One (r650)
  | 790 -> One (r651)
  | 792 -> One (r652)
  | 794 -> One (r653)
  | 796 -> One (r654)
  | 798 -> One (r655)
  | 800 -> One (r656)
  | 802 -> One (r657)
  | 804 -> One (r658)
  | 806 -> One (r659)
  | 808 -> One (r660)
  | 810 -> One (r661)
  | 812 -> One (r662)
  | 814 -> One (r663)
  | 816 -> One (r664)
  | 818 -> One (r665)
  | 820 -> One (r666)
  | 822 -> One (r667)
  | 824 -> One (r668)
  | 826 -> One (r669)
  | 828 -> One (r670)
  | 1522 -> One (r671)
  | 877 -> One (r672)
  | 830 -> One (r673)
  | 838 -> One (r674)
  | 837 -> One (r675)
  | 836 -> One (r676)
  | 835 -> One (r677)
  | 834 -> One (r678)
  | 843 -> One (r679)
  | 842 -> One (r680)
  | 841 -> One (r681)
  | 840 -> One (r682)
  | 846 -> One (r683)
  | 845 -> One (r684)
  | 854 -> One (r685)
  | 852 -> One (r686)
  | 851 -> One (r687)
  | 859 -> One (r688)
  | 858 -> One (r689)
  | 857 -> One (r690)
  | 862 -> One (r691)
  | 861 -> One (r692)
  | 864 -> One (r693)
  | 866 -> One (r694)
  | 868 -> One (r695)
  | 870 -> One (r696)
  | 875 -> One (r697)
  | 1521 -> One (r698)
  | 1520 -> One (r699)
  | 879 -> One (r700)
  | 881 -> One (r701)
  | 883 -> One (r702)
  | 900 -> One (r703)
  | 899 -> One (r704)
  | 918 -> One (r706)
  | 917 -> One (r707)
  | 916 -> One (r708)
  | 896 -> One (r709)
  | 895 -> One (r710)
  | 894 -> One (r711)
  | 891 -> One (r712)
  | 888 -> One (r713)
  | 887 -> One (r714)
  | 886 -> One (r715)
  | 885 -> One (r716)
  | 890 -> One (r717)
  | 893 -> One (r718)
  | 915 -> One (r719)
  | 906 -> One (r720)
  | 905 -> One (r721)
  | 898 -> One (r722)
  | 904 -> One (r723)
  | 903 -> One (r724)
  | 902 -> One (r725)
  | 912 -> One (r726)
  | 911 -> One (r727)
  | 910 -> One (r728)
  | 909 -> One (r729)
  | 908 -> One (r730)
  | 914 -> One (r731)
  | 1519 -> One (r732)
  | 1518 -> One (r733)
  | 920 -> One (r734)
  | 1517 -> One (r735)
  | 1516 -> One (r736)
  | 922 -> One (r737)
  | 935 -> One (r738)
  | 938 -> One (r740)
  | 937 -> One (r741)
  | 934 -> One (r742)
  | 933 -> One (r743)
  | 929 -> One (r744)
  | 928 -> One (r745)
  | 927 -> One (r746)
  | 926 -> One (r747)
  | 932 -> One (r748)
  | 931 -> One (r749)
  | 985 -> One (r751)
  | 984 -> One (r752)
  | 983 -> One (r753)
  | 978 -> One (r754)
  | 999 -> One (r758)
  | 998 -> One (r759)
  | 997 -> One (r760)
  | 1120 -> One (r761)
  | 1119 -> One (r762)
  | 1118 -> One (r763)
  | 1117 -> One (r764)
  | 977 -> One (r765)
  | 976 -> One (r767)
  | 963 -> One (r768)
  | 968 -> One (r776)
  | 965 -> One (r778)
  | 964 -> One (r779)
  | 962 -> One (r780)
  | 961 -> One (r781)
  | 960 -> One (r782)
  | 959 -> One (r783)
  | 955 -> One (r784)
  | 954 -> One (r785)
  | 958 -> One (r786)
  | 957 -> One (r787)
  | 970 -> One (r788)
  | 975 -> One (r789)
  | 972 -> One (r790)
  | 974 -> One (r791)
  | 982 -> One (r792)
  | 996 -> One (r793)
  | 992 -> One (r794)
  | 988 -> One (r795)
  | 991 -> One (r796)
  | 990 -> One (r797)
  | 995 -> One (r798)
  | 994 -> One (r799)
  | 1290 -> One (r800)
  | 1053 -> One (r801)
  | 1068 -> One (r803)
  | 1067 -> One (r804)
  | 1066 -> One (r805)
  | 1065 -> One (r806)
  | 1064 -> One (r807)
  | 1051 -> One (r811)
  | 1050 -> One (r812)
  | 1049 -> One (r813)
  | 1047 -> One (r814)
  | 1046 -> One (r815)
  | 1023 -> One (r817)
  | 1022 -> One (r818)
  | 1021 -> One (r819)
  | 1012 -> One (r820)
  | 1011 -> One (r821)
  | 1017 -> One (r822)
  | 1016 -> One (r823)
  | 1015 | 1943 -> One (r824)
  | 1019 | 1942 -> One (r825)
  | 1040 -> One (r826)
  | 1032 -> One (r827)
  | 1031 -> One (r828)
  | 1030 -> One (r829)
  | 1039 -> One (r830)
  | 1038 -> One (r831)
  | 1060 -> One (r832)
  | 1059 -> One (r833)
  | 1058 -> One (r834)
  | 1289 -> One (r835)
  | 1079 -> One (r836)
  | 1078 -> One (r837)
  | 1077 -> One (r838)
  | 1076 -> One (r839)
  | 1075 -> One (r840)
  | 1074 -> One (r841)
  | 1073 -> One (r842)
  | 1072 -> One (r843)
  | 1112 -> One (r844)
  | 1111 -> One (r845)
  | 1114 -> One (r847)
  | 1113 -> One (r848)
  | 1107 -> One (r849)
  | 1089 -> One (r850)
  | 1088 -> One (r851)
  | 1087 -> One (r852)
  | 1086 -> One (r853)
  | 1085 -> One (r854)
  | 1093 -> One (r858)
  | 1092 -> One (r859)
  | 1106 -> One (r860)
  | 1098 -> One (r861)
  | 1097 -> One (r862)
  | 1096 -> One (r863)
  | 1095 -> One (r864)
  | 1105 -> One (r865)
  | 1104 -> One (r866)
  | 1103 -> One (r867)
  | 1102 -> One (r868)
  | 1101 -> One (r869)
  | 1100 -> One (r870)
  | 1110 -> One (r873)
  | 1109 -> One (r874)
  | 1116 -> One (r875)
  | 1179 | 1233 -> One (r877)
  | 1235 -> One (r879)
  | 1249 -> One (r881)
  | 1239 -> One (r882)
  | 1238 -> One (r883)
  | 1220 -> One (r884)
  | 1219 -> One (r885)
  | 1218 -> One (r886)
  | 1217 -> One (r887)
  | 1216 -> One (r888)
  | 1215 -> One (r889)
  | 1214 -> One (r890)
  | 1204 -> One (r891)
  | 1203 -> One (r892)
  | 1135 -> One (r893)
  | 1134 -> One (r894)
  | 1133 -> One (r895)
  | 1126 -> One (r896)
  | 1124 -> One (r897)
  | 1123 -> One (r898)
  | 1128 -> One (r899)
  | 1130 -> One (r901)
  | 1129 -> One (r902)
  | 1132 -> One (r903)
  | 1197 -> One (r904)
  | 1196 -> One (r905)
  | 1141 -> One (r906)
  | 1137 -> One (r907)
  | 1140 -> One (r908)
  | 1139 -> One (r909)
  | 1152 -> One (r910)
  | 1151 -> One (r911)
  | 1150 -> One (r912)
  | 1149 -> One (r913)
  | 1148 -> One (r914)
  | 1143 -> One (r915)
  | 1163 -> One (r916)
  | 1162 -> One (r917)
  | 1161 -> One (r918)
  | 1160 -> One (r919)
  | 1159 -> One (r920)
  | 1154 -> One (r921)
  | 1188 -> One (r922)
  | 1187 -> One (r923)
  | 1165 -> One (r924)
  | 1186 -> One (r925)
  | 1185 -> One (r926)
  | 1184 -> One (r927)
  | 1183 -> One (r928)
  | 1167 -> One (r929)
  | 1181 -> One (r930)
  | 1171 -> One (r931)
  | 1170 -> One (r932)
  | 1169 -> One (r933)
  | 1178 | 1226 -> One (r934)
  | 1175 -> One (r936)
  | 1174 -> One (r937)
  | 1173 -> One (r938)
  | 1172 | 1225 -> One (r939)
  | 1177 -> One (r940)
  | 1193 -> One (r941)
  | 1192 -> One (r942)
  | 1191 -> One (r943)
  | 1195 -> One (r945)
  | 1194 -> One (r946)
  | 1190 -> One (r947)
  | 1199 -> One (r948)
  | 1202 -> One (r949)
  | 1213 -> One (r950)
  | 1212 -> One (r951)
  | 1211 -> One (r952)
  | 1210 -> One (r953)
  | 1209 -> One (r954)
  | 1208 -> One (r955)
  | 1207 -> One (r956)
  | 1206 -> One (r957)
  | 1237 -> One (r958)
  | 1224 -> One (r959)
  | 1223 -> One (r960)
  | 1222 -> One (r961)
  | 1236 -> One (r962)
  | 1228 -> One (r963)
  | 1234 -> One (r964)
  | 1231 -> One (r965)
  | 1230 -> One (r966)
  | 1248 -> One (r967)
  | 1247 -> One (r968)
  | 1246 -> One (r969)
  | 1245 -> One (r970)
  | 1244 -> One (r971)
  | 1243 -> One (r972)
  | 1242 -> One (r973)
  | 1241 -> One (r974)
  | 1258 -> One (r975)
  | 1260 -> One (r976)
  | 1265 -> One (r977)
  | 1264 -> One (r978)
  | 1263 -> One (r979)
  | 1262 -> One (r980)
  | 1275 -> One (r981)
  | 1274 -> One (r982)
  | 1273 -> One (r983)
  | 1272 -> One (r984)
  | 1271 -> One (r985)
  | 1270 -> One (r986)
  | 1269 -> One (r987)
  | 1268 -> One (r988)
  | 1286 -> One (r989)
  | 1285 -> One (r990)
  | 1284 -> One (r991)
  | 1283 -> One (r992)
  | 1282 -> One (r993)
  | 1281 -> One (r994)
  | 1280 -> One (r995)
  | 1279 -> One (r996)
  | 1278 -> One (r997)
  | 1412 -> One (r998)
  | 1461 -> One (r1000)
  | 1309 -> One (r1001)
  | 1478 -> One (r1003)
  | 1469 -> One (r1004)
  | 1468 -> One (r1005)
  | 1298 -> One (r1006)
  | 1297 -> One (r1007)
  | 1296 -> One (r1008)
  | 1295 -> One (r1009)
  | 1294 -> One (r1010)
  | 1308 -> One (r1011)
  | 1307 -> One (r1012)
  | 1306 -> One (r1013)
  | 1455 -> One (r1014)
  | 1454 -> One (r1015)
  | 1312 -> One (r1016)
  | 1311 -> One (r1017)
  | 1338 -> One (r1018)
  | 1337 -> One (r1019)
  | 1336 -> One (r1020)
  | 1335 -> One (r1021)
  | 1326 -> One (r1022)
  | 1325 -> One (r1024)
  | 1324 -> One (r1025)
  | 1320 -> One (r1026)
  | 1319 -> One (r1027)
  | 1318 -> One (r1028)
  | 1317 -> One (r1029)
  | 1315 -> One (r1030)
  | 1323 -> One (r1031)
  | 1322 -> One (r1032)
  | 1334 -> One (r1033)
  | 1333 -> One (r1034)
  | 1332 -> One (r1035)
  | 1341 -> One (r1036)
  | 1340 -> One (r1037)
  | 1381 -> One (r1038)
  | 1370 -> One (r1039)
  | 1369 -> One (r1040)
  | 1360 -> One (r1041)
  | 1359 -> One (r1043)
  | 1358 -> One (r1044)
  | 1357 -> One (r1045)
  | 1346 -> One (r1046)
  | 1345 -> One (r1047)
  | 1344 -> One (r1048)
  | 1356 -> One (r1049)
  | 1355 -> One (r1050)
  | 1354 -> One (r1051)
  | 1353 -> One (r1052)
  | 1352 -> One (r1053)
  | 1351 -> One (r1054)
  | 1350 -> One (r1055)
  | 1349 -> One (r1056)
  | 1368 -> One (r1057)
  | 1367 -> One (r1058)
  | 1366 -> One (r1059)
  | 1380 -> One (r1060)
  | 1379 -> One (r1061)
  | 1378 -> One (r1062)
  | 1377 -> One (r1063)
  | 1376 -> One (r1064)
  | 1375 -> One (r1065)
  | 1374 -> One (r1066)
  | 1373 -> One (r1067)
  | 1385 -> One (r1068)
  | 1384 -> One (r1069)
  | 1383 -> One (r1070)
  | 1449 -> One (r1071)
  | 1448 -> One (r1072)
  | 1447 -> One (r1073)
  | 1446 -> One (r1074)
  | 1445 -> One (r1075)
  | 1444 -> One (r1076)
  | 1441 -> One (r1077)
  | 1388 -> One (r1078)
  | 1437 -> One (r1079)
  | 1436 -> One (r1080)
  | 1431 -> One (r1081)
  | 1430 -> One (r1082)
  | 1429 -> One (r1083)
  | 1428 -> One (r1084)
  | 1397 -> One (r1085)
  | 1396 -> One (r1086)
  | 1395 -> One (r1087)
  | 1394 -> One (r1088)
  | 1393 -> One (r1089)
  | 1392 -> One (r1090)
  | 1427 -> One (r1091)
  | 1401 -> One (r1092)
  | 1400 -> One (r1093)
  | 1399 -> One (r1094)
  | 1405 -> One (r1095)
  | 1404 -> One (r1096)
  | 1403 -> One (r1097)
  | 1424 -> One (r1098)
  | 1409 -> One (r1099)
  | 1408 -> One (r1100)
  | 1426 -> One (r1102)
  | 1407 -> One (r1103)
  | 1421 -> One (r1104)
  | 1411 -> One (r1105)
  | 1415 -> One (r1106)
  | 1435 -> One (r1107)
  | 1434 -> One (r1108)
  | 1433 -> One (r1109)
  | 1440 -> One (r1110)
  | 1439 -> One (r1111)
  | 1443 -> One (r1112)
  | 1453 -> One (r1113)
  | 1452 -> One (r1114)
  | 1451 -> One (r1115)
  | 1457 -> One (r1116)
  | 1460 -> One (r1117)
  | 1465 -> One (r1118)
  | 1464 -> One (r1119)
  | 1463 -> One (r1120)
  | 1467 -> One (r1121)
  | 1477 -> One (r1122)
  | 1476 -> One (r1123)
  | 1475 -> One (r1124)
  | 1474 -> One (r1125)
  | 1473 -> One (r1126)
  | 1472 -> One (r1127)
  | 1471 -> One (r1128)
  | 1494 -> One (r1129)
  | 1498 -> One (r1130)
  | 1503 -> One (r1131)
  | 1502 -> One (r1132)
  | 1501 -> One (r1133)
  | 1500 -> One (r1134)
  | 1505 -> One (r1135)
  | 1511 -> One (r1136)
  | 1510 -> One (r1137)
  | 1526 | 1569 -> One (r1138)
  | 1525 | 1568 -> One (r1139)
  | 1524 | 1567 -> One (r1140)
  | 1529 | 1574 -> One (r1141)
  | 1528 | 1573 -> One (r1142)
  | 1527 | 1572 -> One (r1143)
  | 1534 | 1581 -> One (r1144)
  | 1533 | 1580 -> One (r1145)
  | 1532 | 1579 -> One (r1146)
  | 1531 | 1578 -> One (r1147)
  | 1540 | 1586 -> One (r1148)
  | 1539 | 1585 -> One (r1149)
  | 1538 | 1584 -> One (r1150)
  | 1543 | 1591 -> One (r1151)
  | 1542 | 1590 -> One (r1152)
  | 1541 | 1589 -> One (r1153)
  | 1550 -> One (r1154)
  | 1553 | 1603 -> One (r1155)
  | 1552 | 1602 -> One (r1156)
  | 1551 | 1601 -> One (r1157)
  | 1555 -> One (r1158)
  | 1558 | 1606 -> One (r1159)
  | 1557 | 1605 -> One (r1160)
  | 1556 | 1604 -> One (r1161)
  | 1560 -> One (r1162)
  | 1566 -> One (r1163)
  | 1571 -> One (r1164)
  | 1576 -> One (r1165)
  | 1583 -> One (r1166)
  | 1588 -> One (r1167)
  | 1593 -> One (r1168)
  | 1596 -> One (r1169)
  | 1610 -> One (r1170)
  | 1609 -> One (r1171)
  | 1615 -> One (r1172)
  | 1619 -> One (r1173)
  | 1621 -> One (r1174)
  | 1623 -> One (r1175)
  | 1626 -> One (r1176)
  | 1637 -> One (r1177)
  | 1636 -> One (r1178)
  | 1644 -> One (r1180)
  | 1635 -> One (r1181)
  | 1630 -> One (r1182)
  | 1646 -> One (r1184)
  | 1628 -> One (r1186)
  | 1645 -> One (r1187)
  | 1634 -> One (r1188)
  | 1633 -> One (r1189)
  | 1632 -> One (r1190)
  | 1643 -> One (r1191)
  | 1642 -> One (r1192)
  | 1639 -> One (r1193)
  | 1648 -> One (r1194)
  | 1652 -> One (r1195)
  | 1654 -> One (r1196)
  | 1657 -> One (r1197)
  | 1659 -> One (r1198)
  | 1661 -> One (r1199)
  | 1664 -> One (r1200)
  | 1667 -> One (r1202)
  | 1666 -> One (r1203)
  | 1680 -> One (r1204)
  | 1679 -> One (r1205)
  | 1671 -> One (r1206)
  | 1670 -> One (r1207)
  | 1702 -> One (r1208)
  | 1701 -> One (r1209)
  | 1700 -> One (r1210)
  | 1699 -> One (r1211)
  | 1698 -> One (r1212)
  | 1697 -> One (r1213)
  | 1714 -> One (r1214)
  | 1707 -> One (r1215)
  | 1706 -> One (r1216)
  | 1711 -> One (r1217)
  | 1710 -> One (r1218)
  | 1709 -> One (r1219)
  | 1713 -> One (r1220)
  | 1727 -> One (r1221)
  | 1733 -> One (r1222)
  | 1736 -> One (r1223)
  | 1749 -> One (r1224)
  | 1754 -> One (r1225)
  | 1757 -> One (r1226)
  | 1760 -> One (r1227)
  | 1773 -> One (r1228)
  | 1772 -> One (r1229)
  | 1771 -> One (r1230)
  | 1770 -> One (r1231)
  | 1769 -> One (r1232)
  | 1768 -> One (r1233)
  | 1781 -> One (r1234)
  | 1780 -> One (r1235)
  | 1779 -> One (r1236)
  | 1778 -> One (r1237)
  | 1777 -> One (r1238)
  | 1776 -> One (r1239)
  | 1775 -> One (r1240)
  | 1801 -> One (r1241)
  | 1802 -> One (r1243)
  | 1795 -> One (r1244)
  | 1794 -> One (r1245)
  | 1793 -> One (r1246)
  | 1800 -> One (r1247)
  | 1799 -> One (r1248)
  | 1804 -> One (r1249)
  | 1810 -> One (r1250)
  | 1809 -> One (r1251)
  | 1808 -> One (r1252)
  | 1807 -> One (r1253)
  | 1813 -> One (r1254)
  | 1812 -> One (r1255)
  | 1821 -> One (r1256)
  | 1824 -> One (r1257)
  | 1830 -> One (r1258)
  | 1829 -> One (r1259)
  | 1828 -> One (r1260)
  | 1827 -> One (r1261)
  | 1833 -> One (r1262)
  | 1832 -> One (r1263)
  | 1837 -> One (r1264)
  | 1848 -> One (r1265)
  | 1847 -> One (r1266)
  | 1851 -> One (r1267)
  | 1850 -> One (r1268)
  | 1854 -> One (r1269)
  | 1853 -> One (r1270)
  | 1863 -> One (r1271)
  | 1862 -> One (r1272)
  | 1870 -> One (r1273)
  | 1869 -> One (r1274)
  | 1868 -> One (r1275)
  | 1872 -> One (r1276)
  | 1880 -> One (r1277)
  | 1888 -> One (r1278)
  | 1885 -> One (r1279)
  | 1884 -> One (r1280)
  | 1883 -> One (r1281)
  | 1882 -> One (r1282)
  | 1887 -> One (r1283)
  | 1905 -> One (r1284)
  | 1902 -> One (r1285)
  | 1901 -> One (r1286)
  | 1899 -> One (r1287)
  | 1896 -> One (r1288)
  | 1895 -> One (r1289)
  | 1894 -> One (r1290)
  | 1893 -> One (r1291)
  | 1898 -> One (r1292)
  | 1904 -> One (r1293)
  | 1910 -> One (r1294)
  | 1920 -> One (r1295)
  | 1917 -> One (r1296)
  | 1916 -> One (r1297)
  | 1915 -> One (r1298)
  | 1914 -> One (r1299)
  | 1919 -> One (r1300)
  | 1929 -> One (r1301)
  | 1934 -> One (r1302)
  | 1933 -> One (r1303)
  | 1946 -> One (r1304)
  | 1945 -> One (r1305)
  | 1958 -> One (r1306)
  | 1957 -> One (r1307)
  | 1981 -> One (r1308)
  | 1980 -> One (r1309)
  | 1990 -> One (r1310)
  | 1992 -> One (r1311)
  | 1994 -> One (r1312)
  | 2007 -> One (r1313)
  | 2011 -> One (r1314)
  | 2016 -> One (r1315)
  | 2023 -> One (r1316)
  | 2022 -> One (r1317)
  | 2021 -> One (r1318)
  | 2020 -> One (r1319)
  | 2030 -> One (r1320)
  | 2034 -> One (r1321)
  | 2038 -> One (r1322)
  | 2041 -> One (r1323)
  | 2046 -> One (r1324)
  | 2050 -> One (r1325)
  | 2054 -> One (r1326)
  | 2058 -> One (r1327)
  | 2062 -> One (r1328)
  | 2065 -> One (r1329)
  | 2069 -> One (r1330)
  | 2075 -> One (r1331)
  | 2083 -> One (r1332)
  | 2093 -> One (r1333)
  | 2095 -> One (r1334)
  | 2098 -> One (r1335)
  | 2097 -> One (r1336)
  | 2100 -> One (r1337)
  | 2110 -> One (r1338)
  | 2106 -> One (r1339)
  | 2105 -> One (r1340)
  | 2109 -> One (r1341)
  | 2108 -> One (r1342)
  | 2115 -> One (r1343)
  | 2114 -> One (r1344)
  | 2113 -> One (r1345)
  | 2117 -> One (r1346)
  | 460 -> Select (function
    | -1 -> [R 119]
    | _ -> S (T T_DOT) :: r401)
  | 705 -> Select (function
    | -1 -> [R 119]
    | _ -> r605)
  | 176 -> Select (function
    | -1 -> r162
    | _ -> R 203 :: r154)
  | 940 -> Select (function
    | -1 -> r764
    | _ -> R 203 :: r757)
  | 1002 -> Select (function
    | -1 -> r162
    | _ -> R 203 :: r810)
  | 1081 -> Select (function
    | -1 -> r716
    | _ -> R 203 :: r857)
  | 616 -> Select (function
    | -1 -> r291
    | _ -> [R 235])
  | 478 -> Select (function
    | -1 -> [R 737]
    | _ -> S (N N_pattern) :: r409)
  | 475 -> Select (function
    | -1 -> [R 738]
    | _ -> S (N N_pattern) :: r408)
  | 182 -> Select (function
    | -1 -> r174
    | _ -> R 891 :: r168)
  | 1005 -> Select (function
    | -1 -> r174
    | _ -> R 891 :: r816)
  | 979 -> Select (function
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> S (T T_COLONCOLON) :: r417)
  | 92 -> Select (function
    | 266 | 423 | 720 | 830 | 1394 | 1433 | 1484 | 1614 -> r63
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> S (N N_pattern) :: r58)
  | 256 -> Select (function
    | -1 -> S (T T_RPAREN) :: r56
    | _ -> Sub (r1) :: r246)
  | 426 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r371
    | _ -> Sub (r373) :: r375)
  | 658 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r371
    | _ -> Sub (r529) :: r531)
  | 572 -> Select (function
    | 61 | 175 | 225 | 879 | 920 | 922 -> r473
    | _ -> S (T T_OPEN) :: r467)
  | 165 -> Select (function
    | -1 | 300 | 303 | 343 | 358 | 364 | 370 | 1885 | 1896 | 1902 | 1908 | 1917 | 1927 -> S (T T_MODULE) :: r118
    | _ -> Sub (r119) :: r125)
  | 981 -> Select (function
    | -1 -> r521
    | _ -> S (T T_LPAREN) :: r792)
  | 283 -> Select (function
    | -1 -> r293
    | _ -> S (T T_DOT) :: r295)
  | 614 -> Select (function
    | -1 -> r293
    | _ -> S (T T_DOT) :: r516)
  | 219 -> Select (function
    | -1 | 300 | 303 | 343 | 358 | 364 | 370 | 1885 | 1896 | 1902 | 1908 | 1917 | 1927 -> r133
    | _ -> S (T T_COLON) :: r205)
  | 160 -> Select (function
    | 154 | 929 | 955 | 1161 | 1347 | 1367 | 1371 | 1868 -> r110
    | _ -> r108)
  | 157 -> Select (function
    | 154 | 929 | 955 | 1161 | 1347 | 1367 | 1371 | 1868 -> r111
    | _ -> r109)
  | 1960 -> Select (function
    | -1 -> r158
    | _ -> r133)
  | 210 -> Select (function
    | -1 -> r172
    | _ -> r133)
  | 1056 -> Select (function
    | -1 -> r158
    | _ -> r133)
  | 1007 -> Select (function
    | -1 -> r172
    | _ -> r133)
  | 1959 -> Select (function
    | -1 -> r159
    | _ -> r152)
  | 178 -> Select (function
    | -1 -> r160
    | _ -> r153)
  | 177 -> Select (function
    | -1 -> r161
    | _ -> r154)
  | 1055 -> Select (function
    | -1 -> r159
    | _ -> r808)
  | 1004 -> Select (function
    | -1 -> r160
    | _ -> r809)
  | 1003 -> Select (function
    | -1 -> r161
    | _ -> r810)
  | 209 -> Select (function
    | -1 -> r173
    | _ -> r168)
  | 1006 -> Select (function
    | -1 -> r173
    | _ -> r816)
  | 290 -> Select (function
    | -1 -> r292
    | _ -> r295)
  | 615 -> Select (function
    | -1 -> r292
    | _ -> r516)
  | 1084 -> Select (function
    | -1 -> r713
    | _ -> r855)
  | 1083 -> Select (function
    | -1 -> r714
    | _ -> r856)
  | 1082 -> Select (function
    | -1 -> r715
    | _ -> r857)
  | 948 -> Select (function
    | -1 -> r761
    | _ -> r755)
  | 942 -> Select (function
    | -1 -> r762
    | _ -> r756)
  | 941 -> Select (function
    | -1 -> r763
    | _ -> r757)
  | _ -> raise Not_found
