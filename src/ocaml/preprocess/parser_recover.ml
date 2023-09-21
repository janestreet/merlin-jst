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
    | MenhirInterpreter.T MenhirInterpreter.T_UNIQUE -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_ONCE -> ()
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
    | MenhirInterpreter.N MenhirInterpreter.N_value_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_unboxed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding_modes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_value_constant -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_layout_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_newtype_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mode_flag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_newtypes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_newtype -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_45_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_layout_string -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_layout_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_layout_annotation -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_params -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_body -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;1;2;2;2;2;1;2;2;2;2;1;1;2;1;1;1;1;1;1;2;3;4;1;1;5;6;6;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;2;2;2;1;1;1;1;1;2;1;2;3;1;1;2;3;4;5;1;2;3;4;5;6;2;3;4;1;2;3;4;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;2;3;4;5;1;2;2;3;4;5;6;1;2;3;2;3;1;1;2;3;2;3;4;5;6;1;2;1;7;1;1;1;2;1;1;2;3;4;5;6;1;2;3;1;1;2;3;1;1;2;1;1;1;1;1;2;3;4;1;2;3;1;1;2;3;1;1;2;3;3;1;1;4;1;1;1;2;3;1;2;3;1;1;1;1;1;2;1;2;3;1;4;1;1;1;2;1;1;2;3;1;1;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;2;1;2;3;3;1;2;1;1;3;2;3;2;3;1;2;1;2;3;4;5;4;5;2;1;2;3;2;3;2;3;4;5;6;7;4;1;5;6;7;8;8;8;9;3;4;4;4;5;1;2;3;2;1;2;3;4;3;4;5;6;7;4;5;6;7;8;2;3;2;3;2;3;3;4;5;6;7;8;8;8;9;2;3;4;4;4;5;2;3;4;5;6;7;8;9;9;9;10;3;4;5;5;5;6;3;4;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;2;1;1;2;3;1;1;2;1;1;1;2;1;1;1;1;1;1;1;1;4;1;1;2;1;1;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;3;1;2;4;5;6;1;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;2;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;2;2;3;1;1;2;1;2;3;4;5;3;3;4;5;3;4;2;1;2;3;4;1;1;1;1;1;2;1;2;3;4;5;3;3;4;5;6;3;4;5;1;2;1;2;1;2;3;4;5;3;4;5;6;1;3;4;1;1;2;2;3;4;5;6;7;2;3;4;1;2;3;4;5;6;7;8;3;4;5;5;1;2;1;2;3;4;5;6;6;7;8;9;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;4;5;2;3;2;3;4;2;3;4;1;3;2;3;3;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;1;1;1;2;4;1;2;5;6;1;2;3;4;2;3;1;1;2;3;4;5;1;2;3;4;5;1;1;1;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;2;3;1;2;3;4;1;1;1;2;3;1;2;1;2;3;4;4;5;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;1;2;3;1;1;2;3;1;1;2;1;2;3;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;4;5;1;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;1;2;3;4;5;6;7;8;3;4;5;3;1;1;2;3;1;4;1;1;3;1;2;1;2;3;4;1;2;3;4;5;1;2;6;1;2;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;2;8;9;1;1;2;3;1;1;2;3;1;4;1;1;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;1;2;3;4;3;2;1;5;1;1;2;3;6;7;8;1;1;2;3;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;2;3;4;5;6;7;8;1;1;2;3;1;1;2;3;4;1;1;2;9;10;11;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;8;9;10;1;6;7;1;8;9;10;2;1;1;4;5;6;7;8;9;10;7;8;9;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;1;2;3;4;1;5;1;1;2;3;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;2;3;4;5;1;1;2;3;4;5;6;7;8;9;2;1;1;2;3;4;5;6;7;8;9;10;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;1;2;3;1;1;2;1;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;1;2;1;2;2;1;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;2;1;2;1;2;1;2;3;4;5;1;2;1;1;1;1;2;3;1;1;1;1;3;4;3;4;3;4;4;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;4;1;2;1;2;2;1;2;3;4;5;2;1;2;1;2;1;1;3;4;3;3;1;5;6;2;1;3;4;4;5;4;5;6;3;4;4;5;4;5;6;3;4;5;3;1;2;3;1;2;3;4;5;1;4;5;1;2;3;3;2;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;7;3;4;5;2;3;3;2;4;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;3;4;2;2;3;2;3;4;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;2;2;2;5;6;3;4;5;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;5;6;3;2;2;2;3;2;3;4;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;2;4;5;5;6;7;5;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;2;3;4;5;6;7;7;7;8;1;2;3;4;5;6;1;7;1;2;3;2;2;3;4;5;6;7;8;9;9;9;10;3;4;5;5;5;6;3;4;5;6;7;8;9;10;10;10;11;4;5;6;6;6;7;2;3;4;2;2;2;2;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;6;5;6;7;8;9;3;4;5;5;6;6;7;3;4;7;8;2;3;3;4;5;4;5;6;4;5;6;4;5;6;7;8;5;6;2;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNIQUE -> true
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
  | T_ONCE -> true
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
  let r0 = [R 730] in
  let r1 = S (T T_UNDERSCORE) :: r0 in
  let r2 = [R 157] in
  let r3 = Sub (r1) :: r2 in
  let r4 = [R 220] in
  let r5 = Sub (r3) :: r4 in
  let r6 = [R 657] in
  let r7 = Sub (r5) :: r6 in
  let r8 = [R 141] in
  let r9 = S (T T_DONE) :: r8 in
  let r10 = Sub (r7) :: r9 in
  let r11 = S (T T_DO) :: r10 in
  let r12 = Sub (r7) :: r11 in
  let r13 = R 320 :: r12 in
  let r14 = [R 774] in
  let r15 = S (T T_AND) :: r14 in
  let r16 = [R 49] in
  let r17 = Sub (r15) :: r16 in
  let r18 = [R 147] in
  let r19 = [R 50] in
  let r20 = [R 566] in
  let r21 = S (N N_structure) :: r20 in
  let r22 = [R 51] in
  let r23 = Sub (r21) :: r22 in
  let r24 = [R 52] in
  let r25 = S (T T_RBRACKET) :: r24 in
  let r26 = Sub (r23) :: r25 in
  let r27 = [R 166] in
  let r28 = S (T T_DONE) :: r27 in
  let r29 = Sub (r7) :: r28 in
  let r30 = S (T T_DO) :: r29 in
  let r31 = Sub (r7) :: r30 in
  let r32 = R 320 :: r31 in
  let r33 = [R 213] in
  let r34 = [R 384] in
  let r35 = [R 137] in
  let r36 = Sub (r7) :: r35 in
  let r37 = R 320 :: r36 in
  let r38 = [R 353] in
  let r39 = Sub (r7) :: r38 in
  let r40 = S (T T_MINUSGREATER) :: r39 in
  let r41 = S (N N_pattern) :: r40 in
  let r42 = [R 616] in
  let r43 = Sub (r41) :: r42 in
  let r44 = [R 163] in
  let r45 = Sub (r43) :: r44 in
  let r46 = S (T T_WITH) :: r45 in
  let r47 = Sub (r7) :: r46 in
  let r48 = R 320 :: r47 in
  let r49 = [R 149] in
  let r50 = [R 719] in
  let r51 = [R 715] in
  let r52 = S (T T_END) :: r51 in
  let r53 = R 337 :: r52 in
  let r54 = R 77 :: r53 in
  let r55 = R 320 :: r54 in
  let r56 = [R 75] in
  let r57 = S (T T_RPAREN) :: r56 in
  let r58 = [R 759] in
  let r59 = [R 689] in
  let r60 = [R 687] in
  let r61 = [R 113] in
  let r62 = [R 755] in
  let r63 = S (T T_RPAREN) :: r62 in
  let r64 = S (N N_pattern) :: r63 in
  let r65 = [R 494] in
  let r66 = S (T T_AMPERAMPER) :: r65 in
  let r67 = [R 932] in
  let r68 = S (T T_RPAREN) :: r67 in
  let r69 = Sub (r66) :: r68 in
  let r70 = [R 406] in
  let r71 = S (T T_UNDERSCORE) :: r70 in
  let r72 = [R 757] in
  let r73 = S (T T_RPAREN) :: r72 in
  let r74 = Sub (r71) :: r73 in
  let r75 = R 320 :: r74 in
  let r76 = [R 758] in
  let r77 = S (T T_RPAREN) :: r76 in
  let r78 = [R 372] in
  let r79 = [R 660] in
  let r80 = R 328 :: r79 in
  let r81 = [R 408] in
  let r82 = S (T T_END) :: r81 in
  let r83 = Sub (r80) :: r82 in
  let r84 = [R 933] in
  let r85 = S (T T_LIDENT) :: r84 in
  let r86 = [R 27] in
  let r87 = S (T T_UNDERSCORE) :: r86 in
  let r88 = [R 905] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 224] in
  let r91 = Sub (r89) :: r90 in
  let r92 = [R 17] in
  let r93 = Sub (r91) :: r92 in
  let r94 = [R 131] in
  let r95 = Sub (r93) :: r94 in
  let r96 = [R 571] in
  let r97 = Sub (r95) :: r96 in
  let r98 = [R 945] in
  let r99 = R 326 :: r98 in
  let r100 = Sub (r97) :: r99 in
  let r101 = S (T T_COLON) :: r100 in
  let r102 = Sub (r85) :: r101 in
  let r103 = R 320 :: r102 in
  let r104 = [R 468] in
  let r105 = S (T T_RPAREN) :: r104 in
  let r106 = R 246 :: r105 in
  let r107 = [R 247] in
  let r108 = [R 470] in
  let r109 = S (T T_RBRACKET) :: r108 in
  let r110 = [R 472] in
  let r111 = S (T T_RBRACE) :: r110 in
  let r112 = [R 242] in
  let r113 = S (T T_LIDENT) :: r112 in
  let r114 = [R 612] in
  let r115 = Sub (r113) :: r114 in
  let r116 = [R 26] in
  let r117 = Sub (r113) :: r116 in
  let r118 = [R 521] in
  let r119 = S (T T_COLON) :: r118 in
  let r120 = S (T T_QUOTE) :: r115 in
  let r121 = [R 856] in
  let r122 = Sub (r89) :: r121 in
  let r123 = S (T T_MINUSGREATER) :: r122 in
  let r124 = S (T T_RPAREN) :: r123 in
  let r125 = Sub (r95) :: r124 in
  let r126 = S (T T_DOT) :: r125 in
  let r127 = Sub (r120) :: r126 in
  let r128 = [R 282] in
  let r129 = Sub (r113) :: r128 in
  let r130 = [R 613] in
  let r131 = S (T T_RPAREN) :: r130 in
  let r132 = Sub (r129) :: r131 in
  let r133 = S (T T_COLON) :: r132 in
  let r134 = Sub (r113) :: r133 in
  let r135 = S (T T_QUOTE) :: r134 in
  let r136 = [R 48] in
  let r137 = S (T T_RPAREN) :: r136 in
  let r138 = [R 47] in
  let r139 = S (T T_RPAREN) :: r138 in
  let r140 = Sub (r129) :: r139 in
  let r141 = [R 25] in
  let r142 = S (T T_RPAREN) :: r141 in
  let r143 = S (N N_module_type) :: r142 in
  let r144 = R 320 :: r143 in
  let r145 = R 146 :: r144 in
  let r146 = [R 410] in
  let r147 = S (N N_module_expr) :: r146 in
  let r148 = R 320 :: r147 in
  let r149 = S (T T_OF) :: r148 in
  let r150 = [R 396] in
  let r151 = S (T T_END) :: r150 in
  let r152 = S (N N_structure) :: r151 in
  let r153 = [R 370] in
  let r154 = S (T T_LIDENT) :: r153 in
  let r155 = [R 912] in
  let r156 = Sub (r154) :: r155 in
  let r157 = [R 114] in
  let r158 = S (T T_FALSE) :: r157 in
  let r159 = [R 118] in
  let r160 = Sub (r158) :: r159 in
  let r161 = [R 236] in
  let r162 = R 320 :: r161 in
  let r163 = R 229 :: r162 in
  let r164 = Sub (r160) :: r163 in
  let r165 = [R 591] in
  let r166 = Sub (r164) :: r165 in
  let r167 = [R 873] in
  let r168 = R 326 :: r167 in
  let r169 = Sub (r166) :: r168 in
  let r170 = R 577 :: r169 in
  let r171 = S (T T_PLUSEQ) :: r170 in
  let r172 = Sub (r156) :: r171 in
  let r173 = R 915 :: r172 in
  let r174 = R 320 :: r173 in
  let r175 = [R 239] in
  let r176 = R 326 :: r175 in
  let r177 = R 600 :: r176 in
  let r178 = R 910 :: r177 in
  let r179 = R 502 :: r178 in
  let r180 = S (T T_LIDENT) :: r179 in
  let r181 = R 915 :: r180 in
  let r182 = R 320 :: r181 in
  let r183 = R 146 :: r182 in
  let r184 = [R 874] in
  let r185 = R 326 :: r184 in
  let r186 = Sub (r166) :: r185 in
  let r187 = R 577 :: r186 in
  let r188 = S (T T_PLUSEQ) :: r187 in
  let r189 = Sub (r156) :: r188 in
  let r190 = [R 240] in
  let r191 = R 326 :: r190 in
  let r192 = R 600 :: r191 in
  let r193 = R 910 :: r192 in
  let r194 = R 502 :: r193 in
  let r195 = S (T T_LIDENT) :: r194 in
  let r196 = R 915 :: r195 in
  let r197 = [R 914] in
  let r198 = R 320 :: r197 in
  let r199 = S (T T_UNDERSCORE) :: r198 in
  let r200 = R 918 :: r199 in
  let r201 = [R 528] in
  let r202 = Sub (r200) :: r201 in
  let r203 = [R 629] in
  let r204 = Sub (r202) :: r203 in
  let r205 = [R 917] in
  let r206 = S (T T_RPAREN) :: r205 in
  let r207 = [R 530] in
  let r208 = [R 321] in
  let r209 = [R 913] in
  let r210 = R 320 :: r209 in
  let r211 = Sub (r113) :: r210 in
  let r212 = [R 529] in
  let r213 = [R 630] in
  let r214 = [R 284] in
  let r215 = Sub (r113) :: r214 in
  let r216 = [R 283] in
  let r217 = [R 453] in
  let r218 = S (T T_DOTDOT) :: r217 in
  let r219 = [R 911] in
  let r220 = [R 454] in
  let r221 = [R 117] in
  let r222 = S (T T_RPAREN) :: r221 in
  let r223 = [R 850] in
  let r224 = Sub (r89) :: r223 in
  let r225 = S (T T_MINUSGREATER) :: r224 in
  let r226 = Sub (r89) :: r225 in
  let r227 = [R 35] in
  let r228 = [R 148] in
  let r229 = S (T T_RBRACKET) :: r228 in
  let r230 = Sub (r21) :: r229 in
  let r231 = [R 332] in
  let r232 = [R 461] in
  let r233 = R 326 :: r232 in
  let r234 = S (N N_module_expr) :: r233 in
  let r235 = R 320 :: r234 in
  let r236 = [R 462] in
  let r237 = R 326 :: r236 in
  let r238 = S (N N_module_expr) :: r237 in
  let r239 = R 320 :: r238 in
  let r240 = [R 523] in
  let r241 = S (T T_RPAREN) :: r240 in
  let r242 = [R 524] in
  let r243 = S (T T_RPAREN) :: r242 in
  let r244 = S (N N_expr) :: r243 in
  let r245 = [R 214] in
  let r246 = [R 382] in
  let r247 = S (T T_LIDENT) :: r246 in
  let r248 = [R 74] in
  let r249 = Sub (r247) :: r248 in
  let r250 = [R 712] in
  let r251 = Sub (r249) :: r250 in
  let r252 = R 320 :: r251 in
  let r253 = [R 383] in
  let r254 = S (T T_LIDENT) :: r253 in
  let r255 = [R 385] in
  let r256 = [R 390] in
  let r257 = [R 136] in
  let r258 = Sub (r43) :: r257 in
  let r259 = S (T T_WITH) :: r258 in
  let r260 = Sub (r7) :: r259 in
  let r261 = R 320 :: r260 in
  let r262 = [R 162] in
  let r263 = Sub (r43) :: r262 in
  let r264 = S (T T_WITH) :: r263 in
  let r265 = Sub (r7) :: r264 in
  let r266 = R 320 :: r265 in
  let r267 = [R 713] in
  let r268 = S (T T_RPAREN) :: r267 in
  let r269 = S (N N_module_expr) :: r268 in
  let r270 = R 320 :: r269 in
  let r271 = R 146 :: r270 in
  let r272 = [R 746] in
  let r273 = [R 212] in
  let r274 = [R 200] in
  let r275 = [R 286] in
  let r276 = Sub (r85) :: r275 in
  let r277 = [R 350] in
  let r278 = R 326 :: r277 in
  let r279 = Sub (r276) :: r278 in
  let r280 = R 584 :: r279 in
  let r281 = R 320 :: r280 in
  let r282 = [R 696] in
  let r283 = [R 694] in
  let r284 = [R 112] in
  let r285 = [R 651] in
  let r286 = S (N N_pattern) :: r285 in
  let r287 = [R 692] in
  let r288 = S (T T_RBRACKET) :: r287 in
  let r289 = [R 259] in
  let r290 = Sub (r247) :: r289 in
  let r291 = [R 346] in
  let r292 = R 514 :: r291 in
  let r293 = R 508 :: r292 in
  let r294 = Sub (r290) :: r293 in
  let r295 = [R 691] in
  let r296 = S (T T_RBRACE) :: r295 in
  let r297 = [R 509] in
  let r298 = [R 644] in
  let r299 = Sub (r95) :: r298 in
  let r300 = [R 625] in
  let r301 = Sub (r299) :: r300 in
  let r302 = [R 44] in
  let r303 = S (T T_RBRACKET) :: r302 in
  let r304 = Sub (r301) :: r303 in
  let r305 = [R 43] in
  let r306 = [R 42] in
  let r307 = S (T T_RBRACKET) :: r306 in
  let r308 = [R 431] in
  let r309 = Sub (r113) :: r308 in
  let r310 = S (T T_BACKQUOTE) :: r309 in
  let r311 = [R 886] in
  let r312 = R 320 :: r311 in
  let r313 = Sub (r310) :: r312 in
  let r314 = [R 39] in
  let r315 = S (T T_RBRACKET) :: r314 in
  let r316 = [R 103] in
  let r317 = Sub (r154) :: r316 in
  let r318 = [R 36] in
  let r319 = [R 373] in
  let r320 = S (T T_UIDENT) :: r319 in
  let r321 = S (T T_DOT) :: r320 in
  let r322 = [R 371] in
  let r323 = S (T T_LIDENT) :: r322 in
  let r324 = S (T T_UIDENT) :: r78 in
  let r325 = [R 388] in
  let r326 = Sub (r324) :: r325 in
  let r327 = [R 389] in
  let r328 = S (T T_RPAREN) :: r327 in
  let r329 = [R 40] in
  let r330 = S (T T_RBRACKET) :: r329 in
  let r331 = [R 858] in
  let r332 = [R 641] in
  let r333 = [R 37] in
  let r334 = [R 842] in
  let r335 = Sub (r89) :: r334 in
  let r336 = S (T T_MINUSGREATER) :: r335 in
  let r337 = [R 33] in
  let r338 = Sub (r156) :: r337 in
  let r339 = [R 38] in
  let r340 = [R 637] in
  let r341 = [R 862] in
  let r342 = Sub (r89) :: r341 in
  let r343 = S (T T_MINUSGREATER) :: r342 in
  let r344 = [R 860] in
  let r345 = Sub (r89) :: r344 in
  let r346 = S (T T_MINUSGREATER) :: r345 in
  let r347 = S (T T_RPAREN) :: r346 in
  let r348 = Sub (r95) :: r347 in
  let r349 = [R 614] in
  let r350 = [R 615] in
  let r351 = S (T T_RPAREN) :: r350 in
  let r352 = Sub (r129) :: r351 in
  let r353 = S (T T_COLON) :: r352 in
  let r354 = Sub (r113) :: r353 in
  let r355 = [R 861] in
  let r356 = [R 863] in
  let r357 = [R 642] in
  let r358 = [R 18] in
  let r359 = Sub (r113) :: r358 in
  let r360 = [R 20] in
  let r361 = S (T T_RPAREN) :: r360 in
  let r362 = Sub (r129) :: r361 in
  let r363 = S (T T_COLON) :: r362 in
  let r364 = [R 19] in
  let r365 = S (T T_RPAREN) :: r364 in
  let r366 = Sub (r129) :: r365 in
  let r367 = S (T T_COLON) :: r366 in
  let r368 = [R 24] in
  let r369 = [R 638] in
  let r370 = [R 840] in
  let r371 = Sub (r89) :: r370 in
  let r372 = S (T T_MINUSGREATER) :: r371 in
  let r373 = S (T T_RPAREN) :: r372 in
  let r374 = Sub (r95) :: r373 in
  let r375 = [R 841] in
  let r376 = [R 843] in
  let r377 = [R 846] in
  let r378 = Sub (r89) :: r377 in
  let r379 = S (T T_MINUSGREATER) :: r378 in
  let r380 = [R 844] in
  let r381 = Sub (r89) :: r380 in
  let r382 = S (T T_MINUSGREATER) :: r381 in
  let r383 = S (T T_RPAREN) :: r382 in
  let r384 = Sub (r95) :: r383 in
  let r385 = [R 845] in
  let r386 = [R 847] in
  let r387 = [R 859] in
  let r388 = [R 626] in
  let r389 = [R 619] in
  let r390 = Sub (r93) :: r389 in
  let r391 = [R 885] in
  let r392 = R 320 :: r391 in
  let r393 = Sub (r390) :: r392 in
  let r394 = [R 620] in
  let r395 = [R 41] in
  let r396 = S (T T_RBRACKET) :: r395 in
  let r397 = Sub (r301) :: r396 in
  let r398 = [R 610] in
  let r399 = Sub (r310) :: r398 in
  let r400 = [R 45] in
  let r401 = S (T T_RBRACKET) :: r400 in
  let r402 = [R 515] in
  let r403 = S (T T_UNDERSCORE) :: r58 in
  let r404 = [R 754] in
  let r405 = Sub (r403) :: r404 in
  let r406 = [R 557] in
  let r407 = Sub (r405) :: r406 in
  let r408 = R 320 :: r407 in
  let r409 = [R 941] in
  let r410 = [R 764] in
  let r411 = [R 763] in
  let r412 = [R 686] in
  let r413 = S (T T_INT) :: r409 in
  let r414 = Sub (r413) :: r412 in
  let r415 = [R 760] in
  let r416 = Sub (r414) :: r415 in
  let r417 = [R 766] in
  let r418 = S (T T_RBRACKET) :: r417 in
  let r419 = S (T T_LBRACKET) :: r418 in
  let r420 = [R 767] in
  let r421 = [R 548] in
  let r422 = S (N N_pattern) :: r421 in
  let r423 = R 320 :: r422 in
  let r424 = [R 549] in
  let r425 = [R 542] in
  let r426 = [R 556] in
  let r427 = [R 554] in
  let r428 = [R 435] in
  let r429 = S (T T_LIDENT) :: r428 in
  let r430 = [R 555] in
  let r431 = Sub (r405) :: r430 in
  let r432 = S (T T_RPAREN) :: r431 in
  let r433 = [R 122] in
  let r434 = [R 121] in
  let r435 = S (T T_RPAREN) :: r434 in
  let r436 = [R 550] in
  let r437 = [R 769] in
  let r438 = S (T T_RPAREN) :: r437 in
  let r439 = Sub (r95) :: r438 in
  let r440 = [R 547] in
  let r441 = [R 545] in
  let r442 = [R 120] in
  let r443 = S (T T_RPAREN) :: r442 in
  let r444 = [R 768] in
  let r445 = [R 348] in
  let r446 = [R 693] in
  let r447 = [R 695] in
  let r448 = [R 274] in
  let r449 = [R 256] in
  let r450 = S (T T_LIDENT) :: r449 in
  let r451 = [R 272] in
  let r452 = S (T T_RPAREN) :: r451 in
  let r453 = [R 257] in
  let r454 = [R 258] in
  let r455 = Sub (r95) :: r454 in
  let r456 = [R 273] in
  let r457 = S (T T_RPAREN) :: r456 in
  let r458 = [R 268] in
  let r459 = [R 266] in
  let r460 = S (T T_RPAREN) :: r459 in
  let r461 = R 516 :: r460 in
  let r462 = [R 267] in
  let r463 = S (T T_RPAREN) :: r462 in
  let r464 = R 516 :: r463 in
  let r465 = [R 517] in
  let r466 = [R 305] in
  let r467 = Sub (r85) :: r466 in
  let r468 = [R 308] in
  let r469 = Sub (r467) :: r468 in
  let r470 = [R 198] in
  let r471 = Sub (r7) :: r470 in
  let r472 = S (T T_IN) :: r471 in
  let r473 = [R 565] in
  let r474 = S (T T_UNDERSCORE) :: r473 in
  let r475 = [R 271] in
  let r476 = [R 269] in
  let r477 = S (T T_RPAREN) :: r476 in
  let r478 = R 516 :: r477 in
  let r479 = [R 302] in
  let r480 = [R 303] in
  let r481 = Sub (r95) :: r480 in
  let r482 = [R 270] in
  let r483 = S (T T_RPAREN) :: r482 in
  let r484 = R 516 :: r483 in
  let r485 = [R 432] in
  let r486 = S (T T_LIDENT) :: r485 in
  let r487 = [R 443] in
  let r488 = Sub (r486) :: r487 in
  let r489 = [R 434] in
  let r490 = Sub (r488) :: r489 in
  let r491 = [R 216] in
  let r492 = S (T T_RPAREN) :: r491 in
  let r493 = [R 433] in
  let r494 = S (T T_RPAREN) :: r493 in
  let r495 = Sub (r129) :: r494 in
  let r496 = S (T T_COLON) :: r495 in
  let r497 = [R 217] in
  let r498 = S (T T_RPAREN) :: r497 in
  let r499 = [R 281] in
  let r500 = S (T T_RPAREN) :: r499 in
  let r501 = Sub (r95) :: r500 in
  let r502 = [R 278] in
  let r503 = S (T T_RPAREN) :: r502 in
  let r504 = [R 275] in
  let r505 = [R 279] in
  let r506 = S (T T_RPAREN) :: r505 in
  let r507 = Sub (r95) :: r506 in
  let r508 = [R 276] in
  let r509 = S (T T_RPAREN) :: r508 in
  let r510 = [R 280] in
  let r511 = S (T T_RPAREN) :: r510 in
  let r512 = Sub (r95) :: r511 in
  let r513 = S (T T_DOT) :: r512 in
  let r514 = [R 826] in
  let r515 = Sub (r7) :: r514 in
  let r516 = [R 296] in
  let r517 = R 326 :: r516 in
  let r518 = Sub (r276) :: r517 in
  let r519 = R 584 :: r518 in
  let r520 = R 320 :: r519 in
  let r521 = R 146 :: r520 in
  let r522 = [R 160] in
  let r523 = Sub (r7) :: r522 in
  let r524 = S (T T_IN) :: r523 in
  let r525 = S (N N_module_expr) :: r524 in
  let r526 = R 320 :: r525 in
  let r527 = R 146 :: r526 in
  let r528 = [R 161] in
  let r529 = Sub (r7) :: r528 in
  let r530 = S (T T_IN) :: r529 in
  let r531 = S (N N_module_expr) :: r530 in
  let r532 = R 320 :: r531 in
  let r533 = [R 397] in
  let r534 = S (N N_module_expr) :: r533 in
  let r535 = S (T T_MINUSGREATER) :: r534 in
  let r536 = S (N N_functor_args) :: r535 in
  let r537 = [R 226] in
  let r538 = [R 227] in
  let r539 = S (T T_RPAREN) :: r538 in
  let r540 = S (N N_module_type) :: r539 in
  let r541 = [R 411] in
  let r542 = S (T T_RPAREN) :: r541 in
  let r543 = [R 409] in
  let r544 = S (N N_module_type) :: r543 in
  let r545 = S (T T_MINUSGREATER) :: r544 in
  let r546 = S (N N_functor_args) :: r545 in
  let r547 = [R 417] in
  let r548 = [R 955] in
  let r549 = Sub (r93) :: r548 in
  let r550 = S (T T_COLONEQUAL) :: r549 in
  let r551 = Sub (r290) :: r550 in
  let r552 = [R 954] in
  let r553 = R 600 :: r552 in
  let r554 = [R 601] in
  let r555 = Sub (r95) :: r554 in
  let r556 = S (T T_EQUAL) :: r555 in
  let r557 = [R 380] in
  let r558 = Sub (r113) :: r557 in
  let r559 = [R 420] in
  let r560 = Sub (r558) :: r559 in
  let r561 = [R 958] in
  let r562 = S (N N_module_type) :: r561 in
  let r563 = S (T T_EQUAL) :: r562 in
  let r564 = Sub (r560) :: r563 in
  let r565 = S (T T_TYPE) :: r564 in
  let r566 = [R 381] in
  let r567 = Sub (r113) :: r566 in
  let r568 = [R 959] in
  let r569 = [R 414] in
  let r570 = [R 956] in
  let r571 = Sub (r326) :: r570 in
  let r572 = S (T T_UIDENT) :: r255 in
  let r573 = [R 957] in
  let r574 = S (T T_MODULE) :: r565 in
  let r575 = [R 624] in
  let r576 = [R 402] in
  let r577 = [R 522] in
  let r578 = S (T T_RPAREN) :: r577 in
  let r579 = [R 735] in
  let r580 = [R 645] in
  let r581 = S (N N_expr) :: r580 in
  let r582 = [R 738] in
  let r583 = S (T T_RBRACKET) :: r582 in
  let r584 = [R 722] in
  let r585 = [R 648] in
  let r586 = R 510 :: r585 in
  let r587 = [R 511] in
  let r588 = [R 654] in
  let r589 = R 510 :: r588 in
  let r590 = R 518 :: r589 in
  let r591 = Sub (r290) :: r590 in
  let r592 = [R 586] in
  let r593 = Sub (r591) :: r592 in
  let r594 = [R 732] in
  let r595 = S (T T_RBRACE) :: r594 in
  let r596 = [R 698] in
  let r597 = [R 697] in
  let r598 = S (T T_GREATERDOT) :: r597 in
  let r599 = [R 169] in
  let r600 = Sub (r1) :: r599 in
  let r601 = R 320 :: r600 in
  let r602 = [R 711] in
  let r603 = S (T T_END) :: r602 in
  let r604 = R 320 :: r603 in
  let r605 = [R 165] in
  let r606 = S (N N_expr) :: r605 in
  let r607 = S (T T_THEN) :: r606 in
  let r608 = Sub (r7) :: r607 in
  let r609 = R 320 :: r608 in
  let r610 = [R 658] in
  let r611 = Sub (r43) :: r610 in
  let r612 = R 320 :: r611 in
  let r613 = [R 617] in
  let r614 = [R 354] in
  let r615 = Sub (r7) :: r614 in
  let r616 = S (T T_MINUSGREATER) :: r615 in
  let r617 = [R 277] in
  let r618 = Sub (r405) :: r617 in
  let r619 = [R 218] in
  let r620 = Sub (r618) :: r619 in
  let r621 = [R 602] in
  let r622 = Sub (r620) :: r621 in
  let r623 = [R 219] in
  let r624 = Sub (r622) :: r623 in
  let r625 = [R 156] in
  let r626 = Sub (r5) :: r625 in
  let r627 = [R 172] in
  let r628 = Sub (r626) :: r627 in
  let r629 = S (T T_MINUSGREATER) :: r628 in
  let r630 = R 506 :: r629 in
  let r631 = Sub (r624) :: r630 in
  let r632 = R 320 :: r631 in
  let r633 = [R 507] in
  let r634 = [R 155] in
  let r635 = Sub (r43) :: r634 in
  let r636 = R 320 :: r635 in
  let r637 = [R 618] in
  let r638 = [R 143] in
  let r639 = S (T T_DONE) :: r638 in
  let r640 = Sub (r7) :: r639 in
  let r641 = S (T T_DO) :: r640 in
  let r642 = Sub (r7) :: r641 in
  let r643 = S (T T_IN) :: r642 in
  let r644 = S (N N_pattern) :: r643 in
  let r645 = R 320 :: r644 in
  let r646 = [R 134] in
  let r647 = S (T T_DOWNTO) :: r646 in
  let r648 = [R 167] in
  let r649 = S (T T_DONE) :: r648 in
  let r650 = Sub (r7) :: r649 in
  let r651 = S (T T_DO) :: r650 in
  let r652 = Sub (r7) :: r651 in
  let r653 = Sub (r647) :: r652 in
  let r654 = Sub (r7) :: r653 in
  let r655 = S (T T_EQUAL) :: r654 in
  let r656 = S (N N_pattern) :: r655 in
  let r657 = R 320 :: r656 in
  let r658 = [R 215] in
  let r659 = [R 720] in
  let r660 = [R 731] in
  let r661 = S (T T_RPAREN) :: r660 in
  let r662 = S (T T_LPAREN) :: r661 in
  let r663 = S (T T_DOT) :: r662 in
  let r664 = [R 744] in
  let r665 = S (T T_RPAREN) :: r664 in
  let r666 = S (N N_module_type) :: r665 in
  let r667 = S (T T_COLON) :: r666 in
  let r668 = S (N N_module_expr) :: r667 in
  let r669 = R 320 :: r668 in
  let r670 = [R 306] in
  let r671 = Sub (r7) :: r670 in
  let r672 = S (T T_EQUAL) :: r671 in
  let r673 = [R 168] in
  let r674 = Sub (r1) :: r673 in
  let r675 = R 320 :: r674 in
  let r676 = [R 727] in
  let r677 = [R 728] in
  let r678 = [R 704] in
  let r679 = S (T T_RPAREN) :: r678 in
  let r680 = Sub (r581) :: r679 in
  let r681 = S (T T_LPAREN) :: r680 in
  let r682 = [R 145] in
  let r683 = Sub (r43) :: r682 in
  let r684 = R 320 :: r683 in
  let r685 = [R 170] in
  let r686 = [R 262] in
  let r687 = [R 907] in
  let r688 = Sub (r95) :: r687 in
  let r689 = S (T T_COLON) :: r688 in
  let r690 = [R 263] in
  let r691 = S (T T_RPAREN) :: r690 in
  let r692 = Sub (r689) :: r691 in
  let r693 = [R 909] in
  let r694 = [R 908] in
  let r695 = [R 264] in
  let r696 = [R 265] in
  let r697 = [R 726] in
  let r698 = [R 723] in
  let r699 = Sub (r290) :: r698 in
  let r700 = [R 701] in
  let r701 = S (T T_RPAREN) :: r700 in
  let r702 = Sub (r7) :: r701 in
  let r703 = [R 639] in
  let r704 = [R 135] in
  let r705 = Sub (r7) :: r704 in
  let r706 = [R 197] in
  let r707 = Sub (r7) :: r706 in
  let r708 = [R 187] in
  let r709 = [R 184] in
  let r710 = [R 171] in
  let r711 = [R 183] in
  let r712 = [R 182] in
  let r713 = [R 188] in
  let r714 = [R 192] in
  let r715 = [R 186] in
  let r716 = [R 185] in
  let r717 = [R 190] in
  let r718 = [R 181] in
  let r719 = [R 180] in
  let r720 = [R 179] in
  let r721 = [R 178] in
  let r722 = [R 177] in
  let r723 = [R 191] in
  let r724 = [R 189] in
  let r725 = [R 196] in
  let r726 = [R 640] in
  let r727 = S (N N_expr) :: r726 in
  let r728 = [R 199] in
  let r729 = [R 193] in
  let r730 = [R 194] in
  let r731 = [R 195] in
  let r732 = [R 223] in
  let r733 = Sub (r7) :: r732 in
  let r734 = [R 21] in
  let r735 = R 326 :: r734 in
  let r736 = Sub (r276) :: r735 in
  let r737 = [R 294] in
  let r738 = Sub (r7) :: r737 in
  let r739 = S (T T_EQUAL) :: r738 in
  let r740 = [R 293] in
  let r741 = Sub (r7) :: r740 in
  let r742 = [R 552] in
  let r743 = [R 558] in
  let r744 = [R 563] in
  let r745 = [R 561] in
  let r746 = [R 551] in
  let r747 = S (T T_EQUAL) :: r515 in
  let r748 = [R 295] in
  let r749 = Sub (r747) :: r748 in
  let r750 = [R 291] in
  let r751 = Sub (r7) :: r750 in
  let r752 = S (T T_EQUAL) :: r751 in
  let r753 = Sub (r95) :: r752 in
  let r754 = [R 289] in
  let r755 = Sub (r7) :: r754 in
  let r756 = [R 827] in
  let r757 = Sub (r626) :: r756 in
  let r758 = S (T T_EQUAL) :: r757 in
  let r759 = [R 575] in
  let r760 = S (T T_RBRACKET) :: r759 in
  let r761 = Sub (r23) :: r760 in
  let r762 = [R 569] in
  let r763 = [R 570] in
  let r764 = [R 391] in
  let r765 = S (N N_module_expr) :: r764 in
  let r766 = S (T T_EQUAL) :: r765 in
  let r767 = [R 876] in
  let r768 = R 326 :: r767 in
  let r769 = Sub (r766) :: r768 in
  let r770 = Sub (r71) :: r769 in
  let r771 = R 320 :: r770 in
  let r772 = [R 418] in
  let r773 = R 326 :: r772 in
  let r774 = R 512 :: r773 in
  let r775 = Sub (r113) :: r774 in
  let r776 = R 320 :: r775 in
  let r777 = R 146 :: r776 in
  let r778 = [R 513] in
  let r779 = [R 327] in
  let r780 = [R 877] in
  let r781 = R 316 :: r780 in
  let r782 = R 326 :: r781 in
  let r783 = Sub (r766) :: r782 in
  let r784 = [R 392] in
  let r785 = S (N N_module_expr) :: r784 in
  let r786 = S (T T_EQUAL) :: r785 in
  let r787 = [R 317] in
  let r788 = R 316 :: r787 in
  let r789 = R 326 :: r788 in
  let r790 = Sub (r766) :: r789 in
  let r791 = Sub (r71) :: r790 in
  let r792 = [R 393] in
  let r793 = [R 249] in
  let r794 = S (T T_RBRACKET) :: r793 in
  let r795 = Sub (r21) :: r794 in
  let r796 = [R 152] in
  let r797 = S (T T_RBRACKET) :: r796 in
  let r798 = Sub (r23) :: r797 in
  let r799 = [R 445] in
  let r800 = S (T T_STRING) :: r799 in
  let r801 = [R 576] in
  let r802 = R 326 :: r801 in
  let r803 = Sub (r800) :: r802 in
  let r804 = S (T T_EQUAL) :: r803 in
  let r805 = Sub (r97) :: r804 in
  let r806 = S (T T_COLON) :: r805 in
  let r807 = Sub (r85) :: r806 in
  let r808 = R 320 :: r807 in
  let r809 = [R 572] in
  let r810 = Sub (r95) :: r809 in
  let r811 = Sub (r158) :: r433 in
  let r812 = [R 825] in
  let r813 = R 326 :: r812 in
  let r814 = R 320 :: r813 in
  let r815 = Sub (r811) :: r814 in
  let r816 = S (T T_EQUAL) :: r815 in
  let r817 = Sub (r160) :: r816 in
  let r818 = R 320 :: r817 in
  let r819 = [R 659] in
  let r820 = R 326 :: r819 in
  let r821 = R 320 :: r820 in
  let r822 = R 229 :: r821 in
  let r823 = Sub (r160) :: r822 in
  let r824 = R 320 :: r823 in
  let r825 = R 146 :: r824 in
  let r826 = [R 124] in
  let r827 = Sub (r87) :: r826 in
  let r828 = [R 230] in
  let r829 = [R 573] in
  let r830 = Sub (r93) :: r829 in
  let r831 = [R 251] in
  let r832 = R 320 :: r831 in
  let r833 = Sub (r830) :: r832 in
  let r834 = S (T T_COLON) :: r833 in
  let r835 = S (T T_LIDENT) :: r834 in
  let r836 = R 423 :: r835 in
  let r837 = [R 253] in
  let r838 = Sub (r836) :: r837 in
  let r839 = [R 128] in
  let r840 = S (T T_RBRACE) :: r839 in
  let r841 = [R 252] in
  let r842 = R 320 :: r841 in
  let r843 = S (T T_SEMI) :: r842 in
  let r844 = R 320 :: r843 in
  let r845 = Sub (r830) :: r844 in
  let r846 = S (T T_COLON) :: r845 in
  let r847 = [R 574] in
  let r848 = Sub (r93) :: r847 in
  let r849 = [R 125] in
  let r850 = [R 126] in
  let r851 = Sub (r87) :: r850 in
  let r852 = [R 127] in
  let r853 = S (T T_COLONCOLON) :: r443 in
  let r854 = [R 233] in
  let r855 = [R 234] in
  let r856 = Sub (r87) :: r855 in
  let r857 = [R 232] in
  let r858 = Sub (r87) :: r857 in
  let r859 = [R 231] in
  let r860 = Sub (r87) :: r859 in
  let r861 = [R 567] in
  let r862 = [R 597] in
  let r863 = Sub (r164) :: r862 in
  let r864 = [R 667] in
  let r865 = R 326 :: r864 in
  let r866 = Sub (r863) :: r865 in
  let r867 = R 577 :: r866 in
  let r868 = S (T T_PLUSEQ) :: r867 in
  let r869 = Sub (r156) :: r868 in
  let r870 = R 915 :: r869 in
  let r871 = R 320 :: r870 in
  let r872 = [R 668] in
  let r873 = R 326 :: r872 in
  let r874 = Sub (r863) :: r873 in
  let r875 = R 577 :: r874 in
  let r876 = S (T T_PLUSEQ) :: r875 in
  let r877 = Sub (r156) :: r876 in
  let r878 = [R 238] in
  let r879 = R 326 :: r878 in
  let r880 = R 600 :: r879 in
  let r881 = [R 457] in
  let r882 = S (T T_RBRACE) :: r881 in
  let r883 = [R 235] in
  let r884 = R 320 :: r883 in
  let r885 = R 229 :: r884 in
  let r886 = Sub (r160) :: r885 in
  let r887 = [R 455] in
  let r888 = [R 456] in
  let r889 = [R 460] in
  let r890 = S (T T_RBRACE) :: r889 in
  let r891 = [R 459] in
  let r892 = S (T T_RBRACE) :: r891 in
  let r893 = [R 237] in
  let r894 = R 326 :: r893 in
  let r895 = R 600 :: r894 in
  let r896 = [R 329] in
  let r897 = [R 463] in
  let r898 = R 326 :: r897 in
  let r899 = Sub (r326) :: r898 in
  let r900 = R 320 :: r899 in
  let r901 = [R 464] in
  let r902 = R 326 :: r901 in
  let r903 = Sub (r326) :: r902 in
  let r904 = R 320 :: r903 in
  let r905 = [R 394] in
  let r906 = S (N N_module_type) :: r905 in
  let r907 = S (T T_COLON) :: r906 in
  let r908 = [R 670] in
  let r909 = R 326 :: r908 in
  let r910 = Sub (r907) :: r909 in
  let r911 = Sub (r71) :: r910 in
  let r912 = R 320 :: r911 in
  let r913 = [R 419] in
  let r914 = R 326 :: r913 in
  let r915 = S (N N_module_type) :: r914 in
  let r916 = S (T T_COLONEQUAL) :: r915 in
  let r917 = Sub (r113) :: r916 in
  let r918 = R 320 :: r917 in
  let r919 = [R 407] in
  let r920 = R 326 :: r919 in
  let r921 = [R 673] in
  let r922 = R 318 :: r921 in
  let r923 = R 326 :: r922 in
  let r924 = S (N N_module_type) :: r923 in
  let r925 = S (T T_COLON) :: r924 in
  let r926 = [R 319] in
  let r927 = R 318 :: r926 in
  let r928 = R 326 :: r927 in
  let r929 = S (N N_module_type) :: r928 in
  let r930 = S (T T_COLON) :: r929 in
  let r931 = Sub (r71) :: r930 in
  let r932 = S (T T_UIDENT) :: r34 in
  let r933 = Sub (r932) :: r256 in
  let r934 = [R 671] in
  let r935 = R 326 :: r934 in
  let r936 = [R 395] in
  let r937 = S (T T_QUOTED_STRING_EXPR) :: r49 in
  let r938 = [R 88] in
  let r939 = Sub (r937) :: r938 in
  let r940 = [R 98] in
  let r941 = Sub (r939) :: r940 in
  let r942 = [R 677] in
  let r943 = R 312 :: r942 in
  let r944 = R 326 :: r943 in
  let r945 = Sub (r941) :: r944 in
  let r946 = S (T T_COLON) :: r945 in
  let r947 = S (T T_LIDENT) :: r946 in
  let r948 = R 153 :: r947 in
  let r949 = R 946 :: r948 in
  let r950 = R 320 :: r949 in
  let r951 = [R 102] in
  let r952 = R 314 :: r951 in
  let r953 = R 326 :: r952 in
  let r954 = Sub (r939) :: r953 in
  let r955 = S (T T_EQUAL) :: r954 in
  let r956 = S (T T_LIDENT) :: r955 in
  let r957 = R 153 :: r956 in
  let r958 = R 946 :: r957 in
  let r959 = R 320 :: r958 in
  let r960 = [R 631] in
  let r961 = Sub (r200) :: r960 in
  let r962 = [R 154] in
  let r963 = S (T T_RBRACKET) :: r962 in
  let r964 = [R 632] in
  let r965 = [R 89] in
  let r966 = S (T T_END) :: r965 in
  let r967 = R 335 :: r966 in
  let r968 = R 79 :: r967 in
  let r969 = [R 78] in
  let r970 = S (T T_RPAREN) :: r969 in
  let r971 = [R 81] in
  let r972 = R 326 :: r971 in
  let r973 = Sub (r95) :: r972 in
  let r974 = S (T T_COLON) :: r973 in
  let r975 = S (T T_LIDENT) :: r974 in
  let r976 = R 426 :: r975 in
  let r977 = [R 82] in
  let r978 = R 326 :: r977 in
  let r979 = Sub (r97) :: r978 in
  let r980 = S (T T_COLON) :: r979 in
  let r981 = S (T T_LIDENT) :: r980 in
  let r982 = R 579 :: r981 in
  let r983 = [R 80] in
  let r984 = R 326 :: r983 in
  let r985 = Sub (r939) :: r984 in
  let r986 = [R 91] in
  let r987 = Sub (r939) :: r986 in
  let r988 = S (T T_IN) :: r987 in
  let r989 = Sub (r933) :: r988 in
  let r990 = R 320 :: r989 in
  let r991 = [R 92] in
  let r992 = Sub (r939) :: r991 in
  let r993 = S (T T_IN) :: r992 in
  let r994 = Sub (r933) :: r993 in
  let r995 = [R 627] in
  let r996 = Sub (r95) :: r995 in
  let r997 = [R 87] in
  let r998 = Sub (r317) :: r997 in
  let r999 = S (T T_RBRACKET) :: r998 in
  let r1000 = Sub (r996) :: r999 in
  let r1001 = [R 628] in
  let r1002 = [R 123] in
  let r1003 = Sub (r95) :: r1002 in
  let r1004 = S (T T_EQUAL) :: r1003 in
  let r1005 = Sub (r95) :: r1004 in
  let r1006 = [R 83] in
  let r1007 = R 326 :: r1006 in
  let r1008 = Sub (r1005) :: r1007 in
  let r1009 = [R 84] in
  let r1010 = [R 336] in
  let r1011 = [R 315] in
  let r1012 = R 314 :: r1011 in
  let r1013 = R 326 :: r1012 in
  let r1014 = Sub (r939) :: r1013 in
  let r1015 = S (T T_EQUAL) :: r1014 in
  let r1016 = S (T T_LIDENT) :: r1015 in
  let r1017 = R 153 :: r1016 in
  let r1018 = R 946 :: r1017 in
  let r1019 = [R 100] in
  let r1020 = Sub (r941) :: r1019 in
  let r1021 = S (T T_MINUSGREATER) :: r1020 in
  let r1022 = Sub (r89) :: r1021 in
  let r1023 = [R 101] in
  let r1024 = Sub (r941) :: r1023 in
  let r1025 = [R 99] in
  let r1026 = Sub (r941) :: r1025 in
  let r1027 = S (T T_MINUSGREATER) :: r1026 in
  let r1028 = [R 313] in
  let r1029 = R 312 :: r1028 in
  let r1030 = R 326 :: r1029 in
  let r1031 = Sub (r941) :: r1030 in
  let r1032 = S (T T_COLON) :: r1031 in
  let r1033 = S (T T_LIDENT) :: r1032 in
  let r1034 = R 153 :: r1033 in
  let r1035 = R 946 :: r1034 in
  let r1036 = [R 330] in
  let r1037 = [R 661] in
  let r1038 = [R 679] in
  let r1039 = R 326 :: r1038 in
  let r1040 = S (N N_module_type) :: r1039 in
  let r1041 = R 320 :: r1040 in
  let r1042 = [R 665] in
  let r1043 = [R 323] in
  let r1044 = R 322 :: r1043 in
  let r1045 = R 326 :: r1044 in
  let r1046 = R 600 :: r1045 in
  let r1047 = R 910 :: r1046 in
  let r1048 = R 502 :: r1047 in
  let r1049 = S (T T_LIDENT) :: r1048 in
  let r1050 = R 915 :: r1049 in
  let r1051 = [R 666] in
  let r1052 = [R 325] in
  let r1053 = R 324 :: r1052 in
  let r1054 = R 326 :: r1053 in
  let r1055 = R 600 :: r1054 in
  let r1056 = Sub (r218) :: r1055 in
  let r1057 = S (T T_COLONEQUAL) :: r1056 in
  let r1058 = R 502 :: r1057 in
  let r1059 = S (T T_LIDENT) :: r1058 in
  let r1060 = R 915 :: r1059 in
  let r1061 = [R 60] in
  let r1062 = Sub (r937) :: r1061 in
  let r1063 = [R 69] in
  let r1064 = Sub (r1062) :: r1063 in
  let r1065 = S (T T_EQUAL) :: r1064 in
  let r1066 = [R 880] in
  let r1067 = R 310 :: r1066 in
  let r1068 = R 326 :: r1067 in
  let r1069 = Sub (r1065) :: r1068 in
  let r1070 = S (T T_LIDENT) :: r1069 in
  let r1071 = R 153 :: r1070 in
  let r1072 = R 946 :: r1071 in
  let r1073 = R 320 :: r1072 in
  let r1074 = [R 97] in
  let r1075 = S (T T_END) :: r1074 in
  let r1076 = R 337 :: r1075 in
  let r1077 = R 77 :: r1076 in
  let r1078 = [R 937] in
  let r1079 = Sub (r7) :: r1078 in
  let r1080 = S (T T_EQUAL) :: r1079 in
  let r1081 = S (T T_LIDENT) :: r1080 in
  let r1082 = R 421 :: r1081 in
  let r1083 = R 320 :: r1082 in
  let r1084 = [R 63] in
  let r1085 = R 326 :: r1084 in
  let r1086 = [R 938] in
  let r1087 = Sub (r7) :: r1086 in
  let r1088 = S (T T_EQUAL) :: r1087 in
  let r1089 = S (T T_LIDENT) :: r1088 in
  let r1090 = R 421 :: r1089 in
  let r1091 = [R 940] in
  let r1092 = Sub (r7) :: r1091 in
  let r1093 = [R 936] in
  let r1094 = Sub (r95) :: r1093 in
  let r1095 = S (T T_COLON) :: r1094 in
  let r1096 = [R 939] in
  let r1097 = Sub (r7) :: r1096 in
  let r1098 = [R 364] in
  let r1099 = Sub (r747) :: r1098 in
  let r1100 = S (T T_LIDENT) :: r1099 in
  let r1101 = R 577 :: r1100 in
  let r1102 = R 320 :: r1101 in
  let r1103 = [R 64] in
  let r1104 = R 326 :: r1103 in
  let r1105 = [R 365] in
  let r1106 = Sub (r747) :: r1105 in
  let r1107 = S (T T_LIDENT) :: r1106 in
  let r1108 = R 577 :: r1107 in
  let r1109 = [R 367] in
  let r1110 = Sub (r7) :: r1109 in
  let r1111 = S (T T_EQUAL) :: r1110 in
  let r1112 = [R 369] in
  let r1113 = Sub (r7) :: r1112 in
  let r1114 = S (T T_EQUAL) :: r1113 in
  let r1115 = Sub (r95) :: r1114 in
  let r1116 = S (T T_DOT) :: r1115 in
  let r1117 = [R 363] in
  let r1118 = Sub (r97) :: r1117 in
  let r1119 = S (T T_COLON) :: r1118 in
  let r1120 = [R 366] in
  let r1121 = Sub (r7) :: r1120 in
  let r1122 = S (T T_EQUAL) :: r1121 in
  let r1123 = [R 368] in
  let r1124 = Sub (r7) :: r1123 in
  let r1125 = S (T T_EQUAL) :: r1124 in
  let r1126 = Sub (r95) :: r1125 in
  let r1127 = S (T T_DOT) :: r1126 in
  let r1128 = [R 66] in
  let r1129 = R 326 :: r1128 in
  let r1130 = Sub (r7) :: r1129 in
  let r1131 = [R 61] in
  let r1132 = R 326 :: r1131 in
  let r1133 = R 504 :: r1132 in
  let r1134 = Sub (r1062) :: r1133 in
  let r1135 = [R 62] in
  let r1136 = R 326 :: r1135 in
  let r1137 = R 504 :: r1136 in
  let r1138 = Sub (r1062) :: r1137 in
  let r1139 = [R 93] in
  let r1140 = S (T T_RPAREN) :: r1139 in
  let r1141 = [R 56] in
  let r1142 = Sub (r1062) :: r1141 in
  let r1143 = S (T T_IN) :: r1142 in
  let r1144 = Sub (r933) :: r1143 in
  let r1145 = R 320 :: r1144 in
  let r1146 = [R 299] in
  let r1147 = R 326 :: r1146 in
  let r1148 = Sub (r276) :: r1147 in
  let r1149 = R 584 :: r1148 in
  let r1150 = R 320 :: r1149 in
  let r1151 = [R 57] in
  let r1152 = Sub (r1062) :: r1151 in
  let r1153 = S (T T_IN) :: r1152 in
  let r1154 = Sub (r933) :: r1153 in
  let r1155 = [R 95] in
  let r1156 = Sub (r249) :: r1155 in
  let r1157 = S (T T_RBRACKET) :: r1156 in
  let r1158 = [R 72] in
  let r1159 = Sub (r1062) :: r1158 in
  let r1160 = S (T T_MINUSGREATER) :: r1159 in
  let r1161 = Sub (r618) :: r1160 in
  let r1162 = [R 54] in
  let r1163 = Sub (r1161) :: r1162 in
  let r1164 = [R 55] in
  let r1165 = Sub (r1062) :: r1164 in
  let r1166 = [R 261] in
  let r1167 = [R 298] in
  let r1168 = R 326 :: r1167 in
  let r1169 = Sub (r276) :: r1168 in
  let r1170 = [R 96] in
  let r1171 = S (T T_RPAREN) :: r1170 in
  let r1172 = [R 505] in
  let r1173 = [R 65] in
  let r1174 = R 326 :: r1173 in
  let r1175 = Sub (r1005) :: r1174 in
  let r1176 = [R 67] in
  let r1177 = [R 338] in
  let r1178 = [R 70] in
  let r1179 = Sub (r1062) :: r1178 in
  let r1180 = S (T T_EQUAL) :: r1179 in
  let r1181 = [R 71] in
  let r1182 = [R 311] in
  let r1183 = R 310 :: r1182 in
  let r1184 = R 326 :: r1183 in
  let r1185 = Sub (r1065) :: r1184 in
  let r1186 = S (T T_LIDENT) :: r1185 in
  let r1187 = R 153 :: r1186 in
  let r1188 = R 946 :: r1187 in
  let r1189 = [R 334] in
  let r1190 = [R 868] in
  let r1191 = [R 882] in
  let r1192 = R 326 :: r1191 in
  let r1193 = S (N N_module_expr) :: r1192 in
  let r1194 = R 320 :: r1193 in
  let r1195 = [R 872] in
  let r1196 = [R 865] in
  let r1197 = R 331 :: r1196 in
  let r1198 = [R 703] in
  let r1199 = S (T T_RBRACKET) :: r1198 in
  let r1200 = Sub (r7) :: r1199 in
  let r1201 = [R 702] in
  let r1202 = S (T T_RBRACE) :: r1201 in
  let r1203 = Sub (r7) :: r1202 in
  let r1204 = [R 705] in
  let r1205 = S (T T_RPAREN) :: r1204 in
  let r1206 = Sub (r581) :: r1205 in
  let r1207 = S (T T_LPAREN) :: r1206 in
  let r1208 = [R 709] in
  let r1209 = S (T T_RBRACKET) :: r1208 in
  let r1210 = Sub (r581) :: r1209 in
  let r1211 = [R 707] in
  let r1212 = S (T T_RBRACE) :: r1211 in
  let r1213 = Sub (r581) :: r1212 in
  let r1214 = [R 205] in
  let r1215 = [R 708] in
  let r1216 = S (T T_RBRACKET) :: r1215 in
  let r1217 = Sub (r581) :: r1216 in
  let r1218 = [R 209] in
  let r1219 = [R 706] in
  let r1220 = S (T T_RBRACE) :: r1219 in
  let r1221 = Sub (r581) :: r1220 in
  let r1222 = [R 207] in
  let r1223 = [R 202] in
  let r1224 = [R 204] in
  let r1225 = [R 203] in
  let r1226 = [R 206] in
  let r1227 = [R 210] in
  let r1228 = [R 208] in
  let r1229 = [R 201] in
  let r1230 = [R 307] in
  let r1231 = Sub (r7) :: r1230 in
  let r1232 = [R 309] in
  let r1233 = [R 724] in
  let r1234 = [R 748] in
  let r1235 = [R 747] in
  let r1236 = [R 105] in
  let r1237 = [R 109] in
  let r1238 = S (N N_expr) :: r1237 in
  let r1239 = S (T T_IN) :: r1238 in
  let r1240 = [R 106] in
  let r1241 = Sub (r1239) :: r1240 in
  let r1242 = S (N N_pattern) :: r1241 in
  let r1243 = R 320 :: r1242 in
  let r1244 = [R 621] in
  let r1245 = Sub (r1243) :: r1244 in
  let r1246 = [R 104] in
  let r1247 = [R 622] in
  let r1248 = [R 107] in
  let r1249 = S (N N_expr) :: r1248 in
  let r1250 = S (T T_IN) :: r1249 in
  let r1251 = [R 108] in
  let r1252 = S (N N_expr) :: r1251 in
  let r1253 = Sub (r647) :: r1252 in
  let r1254 = [R 741] in
  let r1255 = [R 737] in
  let r1256 = [R 736] in
  let r1257 = [R 740] in
  let r1258 = [R 743] in
  let r1259 = [R 742] in
  let r1260 = [R 739] in
  let r1261 = S (T T_LIDENT) :: r586 in
  let r1262 = [R 725] in
  let r1263 = S (T T_GREATERRBRACE) :: r1262 in
  let r1264 = [R 733] in
  let r1265 = S (T T_RBRACE) :: r1264 in
  let r1266 = [R 587] in
  let r1267 = Sub (r591) :: r1266 in
  let r1268 = [R 142] in
  let r1269 = S (T T_DONE) :: r1268 in
  let r1270 = Sub (r7) :: r1269 in
  let r1271 = S (T T_DO) :: r1270 in
  let r1272 = Sub (r7) :: r1271 in
  let r1273 = Sub (r647) :: r1272 in
  let r1274 = [R 164] in
  let r1275 = [R 710] in
  let r1276 = [R 721] in
  let r1277 = [R 750] in
  let r1278 = [R 734] in
  let r1279 = [R 751] in
  let r1280 = [R 158] in
  let r1281 = Sub (r7) :: r1280 in
  let r1282 = S (T T_IN) :: r1281 in
  let r1283 = Sub (r766) :: r1282 in
  let r1284 = Sub (r71) :: r1283 in
  let r1285 = R 320 :: r1284 in
  let r1286 = [R 159] in
  let r1287 = Sub (r7) :: r1286 in
  let r1288 = S (T T_IN) :: r1287 in
  let r1289 = R 320 :: r1288 in
  let r1290 = R 229 :: r1289 in
  let r1291 = Sub (r160) :: r1290 in
  let r1292 = R 320 :: r1291 in
  let r1293 = [R 292] in
  let r1294 = Sub (r7) :: r1293 in
  let r1295 = S (T T_EQUAL) :: r1294 in
  let r1296 = Sub (r95) :: r1295 in
  let r1297 = S (T T_DOT) :: r1296 in
  let r1298 = [R 290] in
  let r1299 = Sub (r7) :: r1298 in
  let r1300 = S (T T_EQUAL) :: r1299 in
  let r1301 = Sub (r95) :: r1300 in
  let r1302 = [R 288] in
  let r1303 = Sub (r7) :: r1302 in
  let r1304 = [R 745] in
  let r1305 = [R 752] in
  let r1306 = [R 714] in
  let r1307 = S (T T_RPAREN) :: r1306 in
  let r1308 = [R 699] in
  let r1309 = [R 700] in
  let r1310 = [R 527] in
  let r1311 = S (T T_RPAREN) :: r1310 in
  let r1312 = [R 525] in
  let r1313 = S (T T_RPAREN) :: r1312 in
  let r1314 = [R 526] in
  let r1315 = S (T T_RPAREN) :: r1314 in
  let r1316 = [R 333] in
  let r1317 = R 331 :: r1316 in
  let r1318 = [R 857] in
  let r1319 = [R 360] in
  let r1320 = R 320 :: r1319 in
  let r1321 = Sub (r830) :: r1320 in
  let r1322 = [R 358] in
  let r1323 = [R 34] in
  let r1324 = [R 848] in
  let r1325 = Sub (r89) :: r1324 in
  let r1326 = S (T T_MINUSGREATER) :: r1325 in
  let r1327 = S (T T_RPAREN) :: r1326 in
  let r1328 = Sub (r95) :: r1327 in
  let r1329 = [R 849] in
  let r1330 = [R 851] in
  let r1331 = [R 854] in
  let r1332 = Sub (r89) :: r1331 in
  let r1333 = S (T T_MINUSGREATER) :: r1332 in
  let r1334 = [R 852] in
  let r1335 = Sub (r89) :: r1334 in
  let r1336 = S (T T_MINUSGREATER) :: r1335 in
  let r1337 = S (T T_RPAREN) :: r1336 in
  let r1338 = Sub (r95) :: r1337 in
  let r1339 = [R 853] in
  let r1340 = [R 855] in
  let r1341 = [R 458] in
  let r1342 = S (T T_RBRACE) :: r1341 in
  let r1343 = [R 150] in
  let r1344 = R 320 :: r1343 in
  let r1345 = [R 151] in
  let r1346 = R 320 :: r1345 in
  let r1347 = [R 76] in
  let r1348 = S (T T_RPAREN) :: r1347 in
  let r1349 = [R 138] in
  let r1350 = [R 140] in
  let r1351 = [R 139] in
  let r1352 = [R 243] in
  let r1353 = [R 248] in
  let r1354 = [R 375] in
  let r1355 = [R 378] in
  let r1356 = S (T T_RPAREN) :: r1355 in
  let r1357 = S (T T_COLONCOLON) :: r1356 in
  let r1358 = S (T T_LPAREN) :: r1357 in
  let r1359 = [R 531] in
  let r1360 = [R 532] in
  let r1361 = [R 533] in
  let r1362 = [R 534] in
  let r1363 = [R 535] in
  let r1364 = [R 536] in
  let r1365 = [R 537] in
  let r1366 = [R 538] in
  let r1367 = [R 539] in
  let r1368 = [R 540] in
  let r1369 = [R 541] in
  let r1370 = [R 894] in
  let r1371 = [R 887] in
  let r1372 = [R 903] in
  let r1373 = [R 340] in
  let r1374 = [R 901] in
  let r1375 = S (T T_SEMISEMI) :: r1374 in
  let r1376 = [R 902] in
  let r1377 = [R 342] in
  let r1378 = [R 345] in
  let r1379 = [R 344] in
  let r1380 = [R 343] in
  let r1381 = R 341 :: r1380 in
  let r1382 = [R 931] in
  let r1383 = S (T T_EOF) :: r1382 in
  let r1384 = R 341 :: r1383 in
  let r1385 = [R 930] in
  function
  | 0 | 2083 | 2087 | 2105 | 2109 | 2113 | 2117 | 2121 | 2125 | 2129 | 2133 | 2137 | 2141 | 2147 | 2175 -> Nothing
  | 2082 -> One ([R 0])
  | 2086 -> One ([R 1])
  | 2092 -> One ([R 2])
  | 2106 -> One ([R 3])
  | 2110 -> One ([R 4])
  | 2116 -> One ([R 5])
  | 2118 -> One ([R 6])
  | 2122 -> One ([R 7])
  | 2126 -> One ([R 8])
  | 2130 -> One ([R 9])
  | 2134 -> One ([R 10])
  | 2140 -> One ([R 11])
  | 2144 -> One ([R 12])
  | 2165 -> One ([R 13])
  | 2185 -> One ([R 14])
  | 261 -> One ([R 15])
  | 260 -> One ([R 16])
  | 2100 -> One ([R 22])
  | 2102 -> One ([R 23])
  | 334 -> One ([R 28])
  | 347 -> One ([R 29])
  | 356 -> One ([R 30])
  | 333 -> One ([R 31])
  | 346 -> One ([R 32])
  | 342 -> One ([R 46])
  | 1520 -> One ([R 53])
  | 1529 -> One ([R 58])
  | 1524 -> One ([R 59])
  | 1565 -> One ([R 68])
  | 1532 -> One ([R 73])
  | 1314 -> One ([R 85])
  | 1294 -> One ([R 86])
  | 1296 -> One ([R 90])
  | 1527 -> One ([R 94])
  | 805 -> One ([R 110])
  | 808 -> One ([R 111])
  | 74 -> One ([R 115])
  | 238 | 1051 -> One ([R 116])
  | 1088 -> One ([R 119])
  | 1126 -> One ([R 129])
  | 1130 -> One ([R 130])
  | 386 -> One ([R 132])
  | 1747 -> One ([R 133])
  | 887 -> One ([R 144])
  | 1 -> One (R 146 :: r13)
  | 62 -> One (R 146 :: r32)
  | 68 -> One (R 146 :: r37)
  | 71 -> One (R 146 :: r48)
  | 78 -> One (R 146 :: r55)
  | 102 -> One (R 146 :: r75)
  | 113 -> One (R 146 :: r103)
  | 262 -> One (R 146 :: r235)
  | 263 -> One (R 146 :: r239)
  | 270 -> One (R 146 :: r252)
  | 283 -> One (R 146 :: r261)
  | 286 -> One (R 146 :: r266)
  | 295 -> One (R 146 :: r281)
  | 476 -> One (R 146 :: r408)
  | 507 -> One (R 146 :: r423)
  | 666 -> One (R 146 :: r532)
  | 759 -> One (R 146 :: r601)
  | 762 -> One (R 146 :: r604)
  | 765 -> One (R 146 :: r609)
  | 768 -> One (R 146 :: r612)
  | 774 -> One (R 146 :: r632)
  | 786 -> One (R 146 :: r636)
  | 793 -> One (R 146 :: r645)
  | 798 -> One (R 146 :: r657)
  | 817 -> One (R 146 :: r669)
  | 831 -> One (R 146 :: r675)
  | 841 -> One (R 146 :: r684)
  | 992 -> One (R 146 :: r771)
  | 1033 -> One (R 146 :: r808)
  | 1184 -> One (R 146 :: r900)
  | 1185 -> One (R 146 :: r904)
  | 1194 -> One (R 146 :: r912)
  | 1235 -> One (R 146 :: r950)
  | 1236 -> One (R 146 :: r959)
  | 1375 -> One (R 146 :: r1041)
  | 1409 -> One (R 146 :: r1073)
  | 1606 -> One (R 146 :: r1194)
  | 1856 -> One (R 146 :: r1285)
  | 1863 -> One (R 146 :: r1292)
  | 1704 -> One ([R 173])
  | 869 -> One ([R 174])
  | 891 -> One ([R 175])
  | 872 -> One ([R 176])
  | 934 -> One ([R 211])
  | 936 -> One ([R 221])
  | 941 -> One ([R 222])
  | 350 -> One ([R 225])
  | 678 -> One ([R 228])
  | 165 -> One ([R 241])
  | 1031 -> One ([R 244])
  | 1032 -> One ([R 245])
  | 137 -> One (R 246 :: r109)
  | 141 -> One (R 246 :: r111)
  | 259 -> One ([R 250])
  | 1074 -> One ([R 254])
  | 1075 -> One ([R 255])
  | 1523 -> One ([R 260])
  | 984 -> One ([R 285])
  | 1895 -> One ([R 287])
  | 1603 -> One ([R 297])
  | 1530 -> One ([R 300])
  | 597 -> One ([R 301])
  | 1872 -> One ([R 304])
  | 111 -> One (R 320 :: r83)
  | 191 -> One (R 320 :: r152)
  | 212 -> One (R 320 :: r208)
  | 267 -> One (R 320 :: r244)
  | 669 -> One (R 320 :: r536)
  | 676 -> One (R 320 :: r546)
  | 942 -> One (R 320 :: r736)
  | 1015 -> One (R 320 :: r791)
  | 1213 -> One (R 320 :: r931)
  | 1250 -> One (R 320 :: r968)
  | 1256 -> One (R 320 :: r976)
  | 1267 -> One (R 320 :: r982)
  | 1278 -> One (R 320 :: r985)
  | 1282 -> One (R 320 :: r994)
  | 1303 -> One (R 320 :: r1008)
  | 1319 -> One (R 320 :: r1018)
  | 1354 -> One (R 320 :: r1035)
  | 1381 -> One (R 320 :: r1050)
  | 1392 -> One (R 320 :: r1060)
  | 1417 -> One (R 320 :: r1077)
  | 1421 -> One (R 320 :: r1090)
  | 1450 -> One (R 320 :: r1108)
  | 1489 -> One (R 320 :: r1130)
  | 1493 -> One (R 320 :: r1134)
  | 1494 -> One (R 320 :: r1138)
  | 1505 -> One (R 320 :: r1154)
  | 1513 -> One (R 320 :: r1163)
  | 1557 -> One (R 320 :: r1175)
  | 1577 -> One (R 320 :: r1188)
  | 1962 -> One (R 320 :: r1322)
  | 1380 -> One (R 322 :: r1042)
  | 1611 -> One (R 322 :: r1195)
  | 1391 -> One (R 324 :: r1051)
  | 1000 -> One (R 326 :: r779)
  | 1312 -> One (R 326 :: r1009)
  | 1373 -> One (R 326 :: r1037)
  | 1563 -> One (R 326 :: r1176)
  | 1604 -> One (R 326 :: r1190)
  | 1616 -> One (R 326 :: r1197)
  | 1943 -> One (R 326 :: r1317)
  | 2170 -> One (R 326 :: r1375)
  | 2181 -> One (R 326 :: r1381)
  | 2186 -> One (R 326 :: r1384)
  | 1183 -> One (R 328 :: r896)
  | 1365 -> One (R 328 :: r1036)
  | 258 -> One (R 331 :: r231)
  | 1587 -> One (R 331 :: r1189)
  | 1315 -> One (R 335 :: r1010)
  | 1566 -> One (R 337 :: r1177)
  | 2168 -> One (R 339 :: r1373)
  | 2176 -> One (R 341 :: r1377)
  | 2177 -> One (R 341 :: r1378)
  | 2178 -> One (R 341 :: r1379)
  | 561 -> One ([R 347])
  | 565 -> One ([R 349])
  | 880 -> One ([R 351])
  | 1600 -> One ([R 352])
  | 1814 -> One ([R 355])
  | 1965 -> One ([R 356])
  | 1968 -> One ([R 357])
  | 1967 -> One ([R 359])
  | 1966 -> One ([R 361])
  | 1964 -> One ([R 362])
  | 2101 -> One ([R 374])
  | 2091 -> One ([R 376])
  | 2099 -> One ([R 377])
  | 2098 -> One ([R 379])
  | 807 -> One ([R 386])
  | 1790 -> One ([R 387])
  | 735 -> One ([R 398])
  | 745 -> One ([R 399])
  | 746 -> One ([R 400])
  | 744 -> One ([R 401])
  | 747 -> One ([R 403])
  | 190 -> One ([R 404])
  | 106 | 1204 -> One ([R 405])
  | 705 -> One ([R 412])
  | 682 -> One ([R 413])
  | 724 -> One ([R 415])
  | 712 -> One ([R 416])
  | 1423 | 1436 -> One ([R 422])
  | 1059 -> One ([R 424])
  | 1060 -> One ([R 425])
  | 1260 -> One ([R 427])
  | 1258 -> One ([R 428])
  | 1261 -> One ([R 429])
  | 1259 -> One ([R 430])
  | 525 -> One ([R 436])
  | 158 -> One ([R 437])
  | 156 -> One ([R 438])
  | 157 -> One ([R 439])
  | 159 -> One ([R 440])
  | 161 -> One ([R 441])
  | 160 -> One ([R 442])
  | 628 -> One ([R 444])
  | 1044 -> One ([R 446])
  | 1138 -> One ([R 447])
  | 2008 -> One ([R 448])
  | 1154 -> One ([R 449])
  | 2009 -> One ([R 450])
  | 1153 -> One ([R 451])
  | 1145 -> One ([R 452])
  | 96 | 290 -> One ([R 465])
  | 120 | 826 -> One ([R 466])
  | 148 -> One ([R 467])
  | 136 -> One ([R 469])
  | 140 -> One ([R 471])
  | 144 -> One ([R 473])
  | 127 -> One ([R 474])
  | 147 | 1724 -> One ([R 475])
  | 126 -> One ([R 476])
  | 125 -> One ([R 477])
  | 124 -> One ([R 478])
  | 123 -> One ([R 479])
  | 122 -> One ([R 480])
  | 99 | 117 | 816 -> One ([R 481])
  | 98 | 815 -> One ([R 482])
  | 97 -> One ([R 483])
  | 119 | 531 | 825 -> One ([R 484])
  | 118 | 824 -> One ([R 485])
  | 94 -> One ([R 486])
  | 100 -> One ([R 487])
  | 129 -> One ([R 488])
  | 121 -> One ([R 489])
  | 128 -> One ([R 490])
  | 101 -> One ([R 491])
  | 146 -> One ([R 492])
  | 149 -> One ([R 493])
  | 145 -> One ([R 495])
  | 449 -> One ([R 496])
  | 448 -> One (R 497 :: r393)
  | 310 -> One (R 498 :: r304)
  | 311 -> One ([R 499])
  | 562 -> One (R 500 :: r445)
  | 563 -> One ([R 501])
  | 1158 -> One ([R 503])
  | 977 -> One (R 518 :: r758)
  | 978 -> One ([R 519])
  | 171 -> One ([R 520])
  | 517 -> One ([R 543])
  | 511 -> One ([R 544])
  | 512 -> One ([R 546])
  | 510 | 827 -> One ([R 553])
  | 960 -> One ([R 559])
  | 961 -> One ([R 560])
  | 962 -> One ([R 562])
  | 610 -> One ([R 564])
  | 1408 -> One ([R 568])
  | 1161 | 1470 -> One ([R 578])
  | 1271 -> One ([R 580])
  | 1269 -> One ([R 581])
  | 1272 -> One ([R 582])
  | 1270 -> One ([R 583])
  | 1539 -> One (R 584 :: r1169)
  | 298 -> One ([R 585])
  | 1136 -> One ([R 588])
  | 1137 -> One ([R 589])
  | 1132 -> One ([R 590])
  | 2025 -> One ([R 592])
  | 2024 -> One ([R 593])
  | 2026 -> One ([R 594])
  | 2021 -> One ([R 595])
  | 2022 -> One ([R 596])
  | 1167 -> One ([R 598])
  | 1165 -> One ([R 599])
  | 780 -> One ([R 603])
  | 1757 -> One ([R 604])
  | 1756 -> One ([R 605])
  | 728 -> One ([R 606])
  | 679 -> One ([R 607])
  | 1526 -> One ([R 608])
  | 1525 -> One ([R 609])
  | 471 -> One ([R 611])
  | 723 -> One ([R 623])
  | 441 -> One ([R 643])
  | 1643 -> One ([R 646])
  | 1644 -> One ([R 647])
  | 1834 -> One ([R 649])
  | 1835 -> One ([R 650])
  | 556 -> One ([R 652])
  | 557 -> One ([R 653])
  | 1782 -> One ([R 655])
  | 1783 -> One ([R 656])
  | 1403 -> One ([R 662])
  | 1364 -> One ([R 663])
  | 1367 -> One ([R 664])
  | 1366 -> One ([R 669])
  | 1371 -> One ([R 672])
  | 1370 -> One ([R 674])
  | 1369 -> One ([R 675])
  | 1368 -> One ([R 676])
  | 1404 -> One ([R 678])
  | 492 -> One ([R 681])
  | 90 -> One ([R 682])
  | 91 -> One ([R 683])
  | 85 -> One ([R 684])
  | 86 -> One ([R 685])
  | 92 -> One ([R 688])
  | 87 -> One ([R 690])
  | 806 -> One ([R 716])
  | 875 | 890 -> One ([R 717])
  | 810 | 871 -> One ([R 718])
  | 874 -> One ([R 729])
  | 876 -> One ([R 749])
  | 490 -> One ([R 753])
  | 495 -> One ([R 756])
  | 529 -> One ([R 761])
  | 502 -> One ([R 762])
  | 558 -> One ([R 765])
  | 520 -> One ([R 770])
  | 501 -> One ([R 771])
  | 29 -> One ([R 772])
  | 8 -> One ([R 773])
  | 53 -> One ([R 775])
  | 52 -> One ([R 776])
  | 51 -> One ([R 777])
  | 50 -> One ([R 778])
  | 49 -> One ([R 779])
  | 48 -> One ([R 780])
  | 47 -> One ([R 781])
  | 46 -> One ([R 782])
  | 45 -> One ([R 783])
  | 44 -> One ([R 784])
  | 43 -> One ([R 785])
  | 42 -> One ([R 786])
  | 41 -> One ([R 787])
  | 40 -> One ([R 788])
  | 39 -> One ([R 789])
  | 38 -> One ([R 790])
  | 37 -> One ([R 791])
  | 36 -> One ([R 792])
  | 35 -> One ([R 793])
  | 34 -> One ([R 794])
  | 33 -> One ([R 795])
  | 32 -> One ([R 796])
  | 31 -> One ([R 797])
  | 30 -> One ([R 798])
  | 28 -> One ([R 799])
  | 27 -> One ([R 800])
  | 26 -> One ([R 801])
  | 25 -> One ([R 802])
  | 24 -> One ([R 803])
  | 23 -> One ([R 804])
  | 22 -> One ([R 805])
  | 21 -> One ([R 806])
  | 20 -> One ([R 807])
  | 19 -> One ([R 808])
  | 18 -> One ([R 809])
  | 17 -> One ([R 810])
  | 16 -> One ([R 811])
  | 15 -> One ([R 812])
  | 14 -> One ([R 813])
  | 13 -> One ([R 814])
  | 12 -> One ([R 815])
  | 11 -> One ([R 816])
  | 10 -> One ([R 817])
  | 9 -> One ([R 818])
  | 7 -> One ([R 819])
  | 6 -> One ([R 820])
  | 5 -> One ([R 821])
  | 4 -> One ([R 822])
  | 3 -> One ([R 823])
  | 1595 -> One ([R 824])
  | 413 -> One ([R 828])
  | 419 -> One ([R 829])
  | 430 -> One ([R 830])
  | 436 -> One ([R 831])
  | 1978 -> One ([R 832])
  | 1984 -> One ([R 833])
  | 1995 -> One ([R 834])
  | 2001 -> One ([R 835])
  | 1955 -> One ([R 836])
  | 338 -> One ([R 837])
  | 375 -> One ([R 838])
  | 380 -> One ([R 839])
  | 1621 -> One ([R 864])
  | 1599 | 1620 -> One ([R 866])
  | 1602 | 1622 -> One ([R 867])
  | 1613 -> One ([R 869])
  | 1596 -> One ([R 870])
  | 1586 -> One ([R 871])
  | 1594 -> One ([R 875])
  | 1598 -> One ([R 878])
  | 1597 -> One ([R 879])
  | 1614 -> One ([R 881])
  | 282 -> One ([R 883])
  | 281 -> One ([R 884])
  | 2159 -> One ([R 888])
  | 2160 -> One ([R 889])
  | 2162 -> One ([R 890])
  | 2163 -> One ([R 891])
  | 2161 -> One ([R 892])
  | 2158 -> One ([R 893])
  | 2151 -> One ([R 895])
  | 2152 -> One ([R 896])
  | 2154 -> One ([R 897])
  | 2155 -> One ([R 898])
  | 2153 -> One ([R 899])
  | 2150 -> One ([R 900])
  | 2164 -> One ([R 904])
  | 339 -> One ([R 906])
  | 685 -> One (R 915 :: r551)
  | 699 -> One ([R 916])
  | 197 -> One ([R 919])
  | 200 -> One ([R 920])
  | 204 -> One ([R 921])
  | 198 -> One ([R 922])
  | 205 -> One ([R 923])
  | 201 -> One ([R 924])
  | 206 -> One ([R 925])
  | 203 -> One ([R 926])
  | 196 -> One ([R 927])
  | 482 -> One ([R 928])
  | 483 -> One ([R 929])
  | 491 -> One ([R 934])
  | 873 -> One ([R 935])
  | 488 -> One ([R 942])
  | 75 -> One ([R 943])
  | 486 -> One ([R 944])
  | 1239 -> One ([R 947])
  | 1434 -> One ([R 948])
  | 1437 -> One ([R 949])
  | 1435 -> One ([R 950])
  | 1468 -> One ([R 951])
  | 1471 -> One ([R 952])
  | 1469 -> One ([R 953])
  | 688 -> One ([R 960])
  | 689 -> One ([R 961])
  | 1776 -> One (S (T T_WITH) :: r1267)
  | 391 -> One (S (T T_UNDERSCORE) :: r363)
  | 186 -> One (S (T T_TYPE) :: r149)
  | 1079 -> One (S (T T_STAR) :: r851)
  | 2166 -> One (S (T T_SEMISEMI) :: r1372)
  | 2173 -> One (S (T T_SEMISEMI) :: r1376)
  | 2088 -> One (S (T T_RPAREN) :: r61)
  | 351 -> One (S (T T_RPAREN) :: r338)
  | 401 -> One (S (T T_RPAREN) :: r368)
  | 505 -> One (S (T T_RPAREN) :: r420)
  | 549 -> One (S (T T_RPAREN) :: r444)
  | 671 -> One (S (T T_RPAREN) :: r537)
  | 737 -> One (S (T T_RPAREN) :: r576)
  | 1725 -> One (S (T T_RPAREN) :: r1233)
  | 1915 -> One (S (T T_RPAREN) :: r1308)
  | 1917 -> One (S (T T_RPAREN) :: r1309)
  | 2089 -> One (S (T T_RPAREN) :: r1354)
  | 1055 | 1121 -> One (S (T T_RBRACKET) :: r284)
  | 313 -> One (S (T T_RBRACKET) :: r305)
  | 1765 -> One (S (T T_RBRACKET) :: r1258)
  | 1767 -> One (S (T T_RBRACKET) :: r1259)
  | 1770 -> One (S (T T_RBRACKET) :: r1260)
  | 1842 -> One (S (T T_RBRACKET) :: r1277)
  | 363 -> One (S (T T_QUOTE) :: r354)
  | 388 -> One (S (T T_QUOTE) :: r359)
  | 1280 -> One (S (T T_OPEN) :: r990)
  | 1497 -> One (S (T T_OPEN) :: r1145)
  | 250 | 252 | 349 | 359 | 423 | 1095 | 1988 -> One (S (T T_MODULE) :: r145)
  | 1100 -> One (S (T T_MINUSGREATER) :: r858)
  | 1104 -> One (S (T T_MINUSGREATER) :: r860)
  | 1341 -> One (S (T T_MINUSGREATER) :: r1024)
  | 130 -> One (S (T T_LPAREN) :: r106)
  | 168 -> One (S (T T_LIDENT) :: r119)
  | 573 -> One (S (T T_LIDENT) :: r448)
  | 587 -> One (S (T T_LIDENT) :: r458)
  | 614 -> One (S (T T_LIDENT) :: r496)
  | 835 -> One (S (T T_LIDENT) :: r676)
  | 847 -> One (S (T T_LIDENT) :: r686)
  | 848 -> One (S (T T_LIDENT) :: r692)
  | 859 -> One (S (T T_LIDENT) :: r695)
  | 863 -> One (S (T T_LIDENT) :: r697)
  | 1061 -> One (S (T T_LIDENT) :: r846)
  | 1438 -> One (S (T T_LIDENT) :: r1095)
  | 1472 -> One (S (T T_LIDENT) :: r1119)
  | 1549 -> One (S (T T_LIDENT) :: r1172)
  | 83 | 498 -> One (S (T T_INT) :: r59)
  | 88 | 499 -> One (S (T T_INT) :: r60)
  | 877 -> One (S (T T_IN) :: r705)
  | 881 -> One (S (T T_IN) :: r707)
  | 1517 -> One (S (T T_IN) :: r1165)
  | 752 -> One (S (T T_GREATERRBRACE) :: r584)
  | 1837 -> One (S (T T_GREATERRBRACE) :: r1276)
  | 251 -> One (S (T T_GREATER) :: r227)
  | 1970 -> One (S (T T_GREATER) :: r1323)
  | 717 -> One (S (T T_EQUAL) :: r571)
  | 949 -> One (S (T T_EQUAL) :: r741)
  | 973 -> One (S (T T_EQUAL) :: r755)
  | 1428 -> One (S (T T_EQUAL) :: r1092)
  | 1446 -> One (S (T T_EQUAL) :: r1097)
  | 1715 -> One (S (T T_EQUAL) :: r1231)
  | 1892 -> One (S (T T_EQUAL) :: r1303)
  | 2080 -> One (S (T T_EOF) :: r1352)
  | 2084 -> One (S (T T_EOF) :: r1353)
  | 2103 -> One (S (T T_EOF) :: r1359)
  | 2107 -> One (S (T T_EOF) :: r1360)
  | 2111 -> One (S (T T_EOF) :: r1361)
  | 2114 -> One (S (T T_EOF) :: r1362)
  | 2119 -> One (S (T T_EOF) :: r1363)
  | 2123 -> One (S (T T_EOF) :: r1364)
  | 2127 -> One (S (T T_EOF) :: r1365)
  | 2131 -> One (S (T T_EOF) :: r1366)
  | 2135 -> One (S (T T_EOF) :: r1367)
  | 2138 -> One (S (T T_EOF) :: r1368)
  | 2142 -> One (S (T T_EOF) :: r1369)
  | 2190 -> One (S (T T_EOF) :: r1385)
  | 1824 -> One (S (T T_END) :: r1275)
  | 132 -> One (S (T T_DOTDOT) :: r107)
  | 241 -> One (S (T T_DOTDOT) :: r220)
  | 1139 -> One (S (T T_DOTDOT) :: r887)
  | 1140 -> One (S (T T_DOTDOT) :: r888)
  | 274 | 1637 | 1684 -> One (S (T T_DOT) :: r254)
  | 360 -> One (S (T T_DOT) :: r348)
  | 407 -> One (S (T T_DOT) :: r374)
  | 424 -> One (S (T T_DOT) :: r384)
  | 577 -> One (S (T T_DOT) :: r455)
  | 599 -> One (S (T T_DOT) :: r481)
  | 631 -> One (S (T T_DOT) :: r501)
  | 642 -> One (S (T T_DOT) :: r507)
  | 2145 -> One (S (T T_DOT) :: r572)
  | 968 -> One (S (T T_DOT) :: r753)
  | 1038 -> One (S (T T_DOT) :: r810)
  | 1064 -> One (S (T T_DOT) :: r848)
  | 1098 -> One (S (T T_DOT) :: r856)
  | 1887 -> One (S (T T_DOT) :: r1301)
  | 1972 -> One (S (T T_DOT) :: r1328)
  | 1989 -> One (S (T T_DOT) :: r1338)
  | 2093 -> One (S (T T_DOT) :: r1358)
  | 940 -> One (S (T T_COMMA) :: r727)
  | 291 -> One (S (T T_COLONRBRACKET) :: r272)
  | 300 -> One (S (T T_COLONRBRACKET) :: r282)
  | 570 -> One (S (T T_COLONRBRACKET) :: r447)
  | 1727 -> One (S (T T_COLONRBRACKET) :: r1234)
  | 1729 -> One (S (T T_COLONRBRACKET) :: r1235)
  | 1754 -> One (S (T T_COLONRBRACKET) :: r1254)
  | 1901 -> One (S (T T_COLONRBRACKET) :: r1304)
  | 1904 -> One (S (T T_COLONRBRACKET) :: r1305)
  | 242 | 1052 -> One (S (T T_COLONCOLON) :: r222)
  | 673 -> One (S (T T_COLON) :: r540)
  | 1335 -> One (S (T T_COLON) :: r1022)
  | 1958 -> One (S (T T_COLON) :: r1321)
  | 301 -> One (S (T T_BARRBRACKET) :: r283)
  | 567 -> One (S (T T_BARRBRACKET) :: r446)
  | 750 -> One (S (T T_BARRBRACKET) :: r579)
  | 1758 -> One (S (T T_BARRBRACKET) :: r1255)
  | 1760 -> One (S (T T_BARRBRACKET) :: r1256)
  | 1763 -> One (S (T T_BARRBRACKET) :: r1257)
  | 1845 -> One (S (T T_BARRBRACKET) :: r1278)
  | 1848 -> One (S (T T_BARRBRACKET) :: r1279)
  | 460 -> One (S (T T_BAR) :: r397)
  | 81 -> One (S (N N_pattern) :: r57)
  | 475 -> One (S (N N_pattern) :: r402)
  | 513 -> One (S (N N_pattern) :: r424)
  | 515 -> One (S (N N_pattern) :: r425)
  | 536 -> One (S (N N_pattern) :: r436)
  | 541 -> One (S (N N_pattern) :: r440)
  | 646 -> One (S (N N_pattern) :: r509)
  | 952 -> One (S (N N_pattern) :: r742)
  | 954 -> One (S (N N_pattern) :: r743)
  | 956 -> One (S (N N_pattern) :: r744)
  | 963 -> One (S (N N_pattern) :: r746)
  | 988 -> One (S (N N_pattern) :: r762)
  | 1738 -> One (S (N N_pattern) :: r1250)
  | 109 -> One (S (N N_module_type) :: r77)
  | 675 -> One (S (N N_module_type) :: r542)
  | 713 -> One (S (N N_module_type) :: r568)
  | 715 -> One (S (N N_module_type) :: r569)
  | 741 -> One (S (N N_module_type) :: r578)
  | 997 -> One (S (N N_module_type) :: r778)
  | 1009 -> One (S (N N_module_type) :: r786)
  | 1912 -> One (S (N N_module_type) :: r1307)
  | 1928 -> One (S (N N_module_type) :: r1311)
  | 1931 -> One (S (N N_module_type) :: r1313)
  | 1934 -> One (S (N N_module_type) :: r1315)
  | 266 -> One (S (N N_module_expr) :: r241)
  | 596 -> One (S (N N_let_pattern) :: r478)
  | 603 -> One (S (N N_let_pattern) :: r484)
  | 635 -> One (S (N N_let_pattern) :: r503)
  | 294 -> One (S (N N_expr) :: r274)
  | 754 -> One (S (N N_expr) :: r587)
  | 758 -> One (S (N N_expr) :: r598)
  | 845 -> One (S (N N_expr) :: r685)
  | 870 -> One (S (N N_expr) :: r703)
  | 886 -> One (S (N N_expr) :: r708)
  | 888 -> One (S (N N_expr) :: r709)
  | 892 -> One (S (N N_expr) :: r710)
  | 894 -> One (S (N N_expr) :: r711)
  | 896 -> One (S (N N_expr) :: r712)
  | 898 -> One (S (N N_expr) :: r713)
  | 900 -> One (S (N N_expr) :: r714)
  | 902 -> One (S (N N_expr) :: r715)
  | 904 -> One (S (N N_expr) :: r716)
  | 906 -> One (S (N N_expr) :: r717)
  | 908 -> One (S (N N_expr) :: r718)
  | 910 -> One (S (N N_expr) :: r719)
  | 912 -> One (S (N N_expr) :: r720)
  | 914 -> One (S (N N_expr) :: r721)
  | 916 -> One (S (N N_expr) :: r722)
  | 918 -> One (S (N N_expr) :: r723)
  | 920 -> One (S (N N_expr) :: r724)
  | 922 -> One (S (N N_expr) :: r725)
  | 926 -> One (S (N N_expr) :: r728)
  | 928 -> One (S (N N_expr) :: r729)
  | 930 -> One (S (N N_expr) :: r730)
  | 932 -> One (S (N N_expr) :: r731)
  | 1656 -> One (S (N N_expr) :: r1214)
  | 1661 -> One (S (N N_expr) :: r1218)
  | 1666 -> One (S (N N_expr) :: r1222)
  | 1672 -> One (S (N N_expr) :: r1223)
  | 1677 -> One (S (N N_expr) :: r1224)
  | 1682 -> One (S (N N_expr) :: r1225)
  | 1689 -> One (S (N N_expr) :: r1226)
  | 1694 -> One (S (N N_expr) :: r1227)
  | 1699 -> One (S (N N_expr) :: r1228)
  | 1702 -> One (S (N N_expr) :: r1229)
  | 1732 -> One (S (N N_expr) :: r1236)
  | 1745 -> One (S (N N_expr) :: r1253)
  | 1821 -> One (S (N N_expr) :: r1274)
  | 77 -> One (Sub (r1) :: r50)
  | 757 -> One (Sub (r1) :: r596)
  | 804 -> One (Sub (r1) :: r659)
  | 837 -> One (Sub (r1) :: r677)
  | 861 -> One (Sub (r1) :: r696)
  | 1521 -> One (Sub (r1) :: r1166)
  | 65 -> One (Sub (r7) :: r33)
  | 269 -> One (Sub (r7) :: r245)
  | 292 -> One (Sub (r7) :: r273)
  | 591 -> One (Sub (r7) :: r465)
  | 773 -> One (Sub (r7) :: r616)
  | 803 -> One (Sub (r7) :: r658)
  | 990 -> One (Sub (r7) :: r763)
  | 1802 -> One (Sub (r7) :: r1273)
  | 2064 -> One (Sub (r7) :: r1350)
  | 2066 -> One (Sub (r7) :: r1351)
  | 2 -> One (Sub (r17) :: r18)
  | 56 -> One (Sub (r17) :: r19)
  | 60 -> One (Sub (r17) :: r26)
  | 256 -> One (Sub (r17) :: r230)
  | 937 -> One (Sub (r17) :: r733)
  | 986 -> One (Sub (r17) :: r761)
  | 1027 -> One (Sub (r17) :: r795)
  | 1029 -> One (Sub (r17) :: r798)
  | 1498 -> One (Sub (r17) :: r1150)
  | 771 -> One (Sub (r41) :: r613)
  | 790 -> One (Sub (r41) :: r637)
  | 2062 -> One (Sub (r43) :: r1349)
  | 1005 -> One (Sub (r71) :: r783)
  | 1208 -> One (Sub (r71) :: r925)
  | 1112 -> One (Sub (r80) :: r861)
  | 543 -> One (Sub (r85) :: r441)
  | 958 -> One (Sub (r85) :: r745)
  | 965 -> One (Sub (r85) :: r749)
  | 340 -> One (Sub (r87) :: r332)
  | 384 -> One (Sub (r87) :: r357)
  | 782 -> One (Sub (r87) :: r633)
  | 1077 -> One (Sub (r87) :: r849)
  | 1081 -> One (Sub (r87) :: r852)
  | 1094 -> One (Sub (r87) :: r854)
  | 336 -> One (Sub (r89) :: r331)
  | 348 -> One (Sub (r89) :: r336)
  | 358 -> One (Sub (r89) :: r343)
  | 376 -> One (Sub (r89) :: r355)
  | 381 -> One (Sub (r89) :: r356)
  | 414 -> One (Sub (r89) :: r375)
  | 420 -> One (Sub (r89) :: r376)
  | 422 -> One (Sub (r89) :: r379)
  | 431 -> One (Sub (r89) :: r385)
  | 437 -> One (Sub (r89) :: r386)
  | 439 -> One (Sub (r89) :: r387)
  | 1343 -> One (Sub (r89) :: r1027)
  | 1956 -> One (Sub (r89) :: r1318)
  | 1979 -> One (Sub (r89) :: r1329)
  | 1985 -> One (Sub (r89) :: r1330)
  | 1987 -> One (Sub (r89) :: r1333)
  | 1996 -> One (Sub (r89) :: r1339)
  | 2002 -> One (Sub (r89) :: r1340)
  | 452 -> One (Sub (r93) :: r394)
  | 692 -> One (Sub (r93) :: r553)
  | 309 -> One (Sub (r95) :: r297)
  | 357 -> One (Sub (r95) :: r340)
  | 403 -> One (Sub (r95) :: r369)
  | 576 -> One (Sub (r95) :: r453)
  | 598 -> One (Sub (r95) :: r479)
  | 695 -> One (Sub (r95) :: r556)
  | 828 -> One (Sub (r95) :: r672)
  | 850 -> One (Sub (r95) :: r693)
  | 854 -> One (Sub (r95) :: r694)
  | 945 -> One (Sub (r95) :: r739)
  | 1252 -> One (Sub (r95) :: r970)
  | 1290 -> One (Sub (r95) :: r1001)
  | 2052 -> One (Sub (r95) :: r1348)
  | 1454 -> One (Sub (r97) :: r1111)
  | 1478 -> One (Sub (r97) :: r1122)
  | 361 -> One (Sub (r113) :: r349)
  | 396 -> One (Sub (r113) :: r367)
  | 2148 -> One (Sub (r113) :: r1370)
  | 2156 -> One (Sub (r113) :: r1371)
  | 649 -> One (Sub (r120) :: r513)
  | 174 -> One (Sub (r129) :: r137)
  | 210 -> One (Sub (r129) :: r207)
  | 217 -> One (Sub (r129) :: r212)
  | 620 -> One (Sub (r129) :: r498)
  | 480 -> One (Sub (r156) :: r410)
  | 484 -> One (Sub (r156) :: r411)
  | 1245 -> One (Sub (r200) :: r964)
  | 222 -> One (Sub (r202) :: r213)
  | 202 -> One (Sub (r204) :: r206)
  | 233 -> One (Sub (r215) :: r216)
  | 237 -> One (Sub (r218) :: r219)
  | 1120 -> One (Sub (r218) :: r880)
  | 1171 -> One (Sub (r218) :: r895)
  | 304 -> One (Sub (r294) :: r296)
  | 445 -> One (Sub (r299) :: r388)
  | 315 -> One (Sub (r301) :: r307)
  | 330 -> One (Sub (r301) :: r330)
  | 316 -> One (Sub (r313) :: r315)
  | 317 -> One (Sub (r317) :: r318)
  | 344 -> One (Sub (r317) :: r333)
  | 353 -> One (Sub (r317) :: r339)
  | 320 -> One (Sub (r326) :: r328)
  | 684 -> One (Sub (r326) :: r547)
  | 721 -> One (Sub (r326) :: r573)
  | 1205 -> One (Sub (r326) :: r920)
  | 468 -> One (Sub (r399) :: r401)
  | 638 -> One (Sub (r405) :: r504)
  | 523 -> One (Sub (r429) :: r432)
  | 574 -> One (Sub (r450) :: r452)
  | 581 -> One (Sub (r450) :: r457)
  | 588 -> One (Sub (r450) :: r461)
  | 589 -> One (Sub (r450) :: r464)
  | 1721 -> One (Sub (r467) :: r1232)
  | 592 -> One (Sub (r469) :: r472)
  | 594 -> One (Sub (r474) :: r475)
  | 613 -> One (Sub (r490) :: r492)
  | 1455 -> One (Sub (r490) :: r1116)
  | 1479 -> One (Sub (r490) :: r1127)
  | 1881 -> One (Sub (r490) :: r1297)
  | 725 -> One (Sub (r574) :: r575)
  | 755 -> One (Sub (r593) :: r595)
  | 1775 -> One (Sub (r593) :: r1265)
  | 1021 -> One (Sub (r766) :: r792)
  | 2016 -> One (Sub (r811) :: r1344)
  | 2028 -> One (Sub (r811) :: r1346)
  | 1057 -> One (Sub (r827) :: r828)
  | 1058 -> One (Sub (r838) :: r840)
  | 1122 -> One (Sub (r838) :: r882)
  | 1141 -> One (Sub (r838) :: r890)
  | 1149 -> One (Sub (r838) :: r892)
  | 2004 -> One (Sub (r838) :: r1342)
  | 1229 -> One (Sub (r907) :: r936)
  | 1222 -> One (Sub (r933) :: r935)
  | 1545 -> One (Sub (r941) :: r1171)
  | 1569 -> One (Sub (r941) :: r1180)
  | 1241 -> One (Sub (r961) :: r963)
  | 1509 -> One (Sub (r996) :: r1157)
  | 1496 -> One (Sub (r1062) :: r1140)
  | 1573 -> One (Sub (r1065) :: r1181)
  | 1420 -> One (Sub (r1083) :: r1085)
  | 1449 -> One (Sub (r1102) :: r1104)
  | 1736 -> One (Sub (r1243) :: r1247)
  | 1734 -> One (Sub (r1245) :: r1246)
  | 1772 -> One (Sub (r1261) :: r1263)
  | 66 -> One (r0)
  | 846 -> One (r2)
  | 885 -> One (r4)
  | 884 -> One (r6)
  | 2079 -> One (r8)
  | 2078 -> One (r9)
  | 2077 -> One (r10)
  | 2076 -> One (r11)
  | 2075 -> One (r12)
  | 59 -> One (r13)
  | 54 -> One (r14)
  | 55 -> One (r16)
  | 58 -> One (r18)
  | 57 -> One (r19)
  | 1615 -> One (r20)
  | 1619 -> One (r22)
  | 2074 -> One (r24)
  | 2073 -> One (r25)
  | 61 -> One (r26)
  | 2072 -> One (r27)
  | 2071 -> One (r28)
  | 2070 -> One (r29)
  | 2069 -> One (r30)
  | 64 -> One (r31)
  | 63 -> One (r32)
  | 2068 -> One (r33)
  | 67 -> One (r34)
  | 2061 -> One (r35)
  | 70 -> One (r36)
  | 69 -> One (r37)
  | 1815 -> One (r38)
  | 1813 -> One (r39)
  | 772 -> One (r40)
  | 792 -> One (r42)
  | 2060 -> One (r44)
  | 2059 -> One (r45)
  | 2058 -> One (r46)
  | 73 -> One (r47)
  | 72 -> One (r48)
  | 76 -> One (r49)
  | 1906 -> One (r50)
  | 2057 -> One (r51)
  | 2056 -> One (r52)
  | 2055 -> One (r53)
  | 80 -> One (r54)
  | 79 -> One (r55)
  | 2051 -> One (r56)
  | 2050 -> One (r57)
  | 82 -> One (r58)
  | 84 -> One (r59)
  | 89 -> One (r60)
  | 95 -> One (r61)
  | 535 -> One (r62)
  | 534 | 629 | 640 -> One (r63)
  | 522 | 612 | 639 | 1415 -> One (r64)
  | 150 -> One (r65)
  | 152 -> One (r67)
  | 151 -> One (r68)
  | 116 -> One (r69)
  | 105 -> One (r70)
  | 108 -> One (r72)
  | 107 -> One (r73)
  | 104 -> One (r74)
  | 103 -> One (r75)
  | 2049 -> One (r76)
  | 2048 -> One (r77)
  | 110 | 163 -> One (r78)
  | 1407 -> One (r79)
  | 2047 -> One (r81)
  | 2046 -> One (r82)
  | 112 -> One (r83)
  | 153 | 293 | 756 | 1789 -> One (r84)
  | 162 | 173 -> One (r86)
  | 383 -> One (r88)
  | 335 -> One (r90)
  | 370 -> One (r92)
  | 387 -> One (r94)
  | 1047 -> One (r96)
  | 2045 -> One (r98)
  | 2044 -> One (r99)
  | 155 -> One (r100)
  | 154 -> One (r101)
  | 115 -> One (r102)
  | 114 -> One (r103)
  | 135 -> One (r104)
  | 134 -> One (r105)
  | 131 -> One (r106)
  | 133 -> One (r107)
  | 139 -> One (r108)
  | 138 -> One (r109)
  | 143 -> One (r110)
  | 142 -> One (r111)
  | 166 -> One (r112)
  | 651 -> One (r114)
  | 650 -> One (r115)
  | 240 | 254 | 1097 -> One (r116)
  | 239 | 253 | 1096 -> One (r117)
  | 170 -> One (r118)
  | 169 -> One (r119)
  | 1954 -> One (r121)
  | 1953 -> One (r122)
  | 1952 -> One (r123)
  | 1951 -> One (r124)
  | 1950 -> One (r125)
  | 1949 -> One (r126)
  | 177 -> One (r128)
  | 657 -> One (r130)
  | 656 -> One (r131)
  | 655 -> One (r132)
  | 654 -> One (r133)
  | 653 -> One (r134)
  | 652 -> One (r135)
  | 176 -> One (r136)
  | 175 -> One (r137)
  | 247 -> One (r138)
  | 246 -> One (r139)
  | 245 -> One (r140)
  | 2043 -> One (r141)
  | 2042 -> One (r142)
  | 185 -> One (r143)
  | 184 -> One (r144)
  | 183 -> One (r145)
  | 2041 -> One (r146)
  | 189 -> One (r147)
  | 188 -> One (r148)
  | 187 -> One (r149)
  | 2040 -> One (r150)
  | 2039 -> One (r151)
  | 192 -> One (r152)
  | 318 -> One (r153)
  | 341 -> One (r155)
  | 487 -> One (r157)
  | 1111 -> One (r159)
  | 1148 -> One (r161)
  | 1147 -> One (r162)
  | 1146 | 2027 -> One (r163)
  | 2023 -> One (r165)
  | 2038 -> One (r167)
  | 2037 -> One (r168)
  | 2036 -> One (r169)
  | 2035 -> One (r170)
  | 2034 -> One (r171)
  | 1177 -> One (r175)
  | 1176 -> One (r176)
  | 1175 -> One (r177)
  | 1170 | 2033 -> One (r178)
  | 2020 -> One (r184)
  | 2019 -> One (r185)
  | 2013 -> One (r186)
  | 2012 -> One (r187)
  | 2011 -> One (r188)
  | 1157 -> One (r190)
  | 1156 -> One (r191)
  | 1155 -> One (r192)
  | 236 | 1119 -> One (r193)
  | 209 | 227 -> One (r197)
  | 208 | 226 -> One (r198)
  | 207 | 225 -> One (r199)
  | 219 -> One (r201)
  | 224 -> One (r203)
  | 221 -> One (r205)
  | 220 -> One (r206)
  | 211 -> One (r207)
  | 213 -> One (r208)
  | 216 | 230 -> One (r209)
  | 215 | 229 -> One (r210)
  | 214 | 228 -> One (r211)
  | 218 -> One (r212)
  | 223 -> One (r213)
  | 235 -> One (r214)
  | 234 -> One (r216)
  | 1125 -> One (r217)
  | 2010 -> One (r219)
  | 2007 -> One (r220)
  | 1054 -> One (r221)
  | 1053 -> One (r222)
  | 1983 -> One (r223)
  | 1982 -> One (r224)
  | 1981 -> One (r225)
  | 249 -> One (r226)
  | 1969 -> One (r227)
  | 1948 -> One (r228)
  | 1947 -> One (r229)
  | 257 -> One (r230)
  | 1946 -> One (r231)
  | 1942 -> One (r232)
  | 1941 -> One (r233)
  | 1940 -> One (r234)
  | 1939 -> One (r235)
  | 1938 -> One (r236)
  | 1937 -> One (r237)
  | 265 -> One (r238)
  | 264 -> One (r239)
  | 740 -> One (r240)
  | 739 -> One (r241)
  | 1927 -> One (r242)
  | 1926 -> One (r243)
  | 268 -> One (r244)
  | 1925 -> One (r245)
  | 273 -> One (r246)
  | 279 -> One (r248)
  | 280 -> One (r250)
  | 272 -> One (r251)
  | 271 -> One (r252)
  | 277 -> One (r253)
  | 275 -> One (r254)
  | 276 -> One (r255)
  | 278 -> One (r256)
  | 1924 -> One (r257)
  | 1923 -> One (r258)
  | 1922 -> One (r259)
  | 285 -> One (r260)
  | 284 -> One (r261)
  | 1921 -> One (r262)
  | 1920 -> One (r263)
  | 1919 -> One (r264)
  | 288 -> One (r265)
  | 287 -> One (r266)
  | 1911 -> One (r267)
  | 1910 -> One (r268)
  | 1909 -> One (r269)
  | 1908 -> One (r270)
  | 1907 -> One (r271)
  | 1900 -> One (r272)
  | 1899 -> One (r273)
  | 1898 -> One (r274)
  | 572 -> One (r275)
  | 1897 -> One (r277)
  | 1896 -> One (r278)
  | 299 -> One (r279)
  | 297 -> One (r280)
  | 296 -> One (r281)
  | 569 -> One (r282)
  | 566 -> One (r283)
  | 303 -> One (r284)
  | 555 -> One (r285)
  | 554 -> One (r287)
  | 553 -> One (r288)
  | 305 -> One (r289)
  | 560 -> One (r291)
  | 474 -> One (r292)
  | 308 -> One (r293)
  | 307 -> One (r295)
  | 306 -> One (r296)
  | 473 -> One (r297)
  | 457 -> One (r298)
  | 442 -> One (r300)
  | 467 -> One (r302)
  | 466 -> One (r303)
  | 312 -> One (r304)
  | 314 -> One (r305)
  | 465 -> One (r306)
  | 464 -> One (r307)
  | 332 -> One (r308)
  | 331 -> One (r309)
  | 456 -> One (r311)
  | 447 -> One (r312)
  | 459 -> One (r314)
  | 458 -> One (r315)
  | 328 | 1346 -> One (r316)
  | 329 -> One (r318)
  | 324 -> One (r319)
  | 323 -> One (r320)
  | 327 -> One (r322)
  | 325 -> One (r325)
  | 322 -> One (r327)
  | 321 -> One (r328)
  | 444 -> One (r329)
  | 443 -> One (r330)
  | 337 -> One (r331)
  | 343 -> One (r332)
  | 345 -> One (r333)
  | 418 -> One (r334)
  | 417 -> One (r335)
  | 416 -> One (r336)
  | 355 -> One (r337)
  | 352 -> One (r338)
  | 354 -> One (r339)
  | 406 -> One (r340)
  | 379 -> One (r341)
  | 378 -> One (r342)
  | 405 -> One (r343)
  | 374 -> One (r344)
  | 373 -> One (r345)
  | 372 -> One (r346)
  | 371 -> One (r347)
  | 369 -> One (r348)
  | 362 -> One (r349)
  | 368 -> One (r350)
  | 367 -> One (r351)
  | 366 -> One (r352)
  | 365 -> One (r353)
  | 364 -> One (r354)
  | 377 -> One (r355)
  | 382 -> One (r356)
  | 385 -> One (r357)
  | 390 -> One (r358)
  | 389 -> One (r359)
  | 395 -> One (r360)
  | 394 -> One (r361)
  | 393 -> One (r362)
  | 392 -> One (r363)
  | 400 -> One (r364)
  | 399 -> One (r365)
  | 398 -> One (r366)
  | 397 -> One (r367)
  | 402 -> One (r368)
  | 404 -> One (r369)
  | 412 -> One (r370)
  | 411 -> One (r371)
  | 410 -> One (r372)
  | 409 -> One (r373)
  | 408 -> One (r374)
  | 415 -> One (r375)
  | 421 -> One (r376)
  | 435 -> One (r377)
  | 434 -> One (r378)
  | 433 -> One (r379)
  | 429 -> One (r380)
  | 428 -> One (r381)
  | 427 -> One (r382)
  | 426 -> One (r383)
  | 425 -> One (r384)
  | 432 -> One (r385)
  | 438 -> One (r386)
  | 440 -> One (r387)
  | 446 -> One (r388)
  | 455 -> One (r389)
  | 454 -> One (r391)
  | 451 -> One (r392)
  | 450 -> One (r393)
  | 453 -> One (r394)
  | 463 -> One (r395)
  | 462 -> One (r396)
  | 461 -> One (r397)
  | 472 -> One (r398)
  | 470 -> One (r400)
  | 469 -> One (r401)
  | 559 -> One (r402)
  | 493 | 944 -> One (r404)
  | 494 -> One (r406)
  | 478 -> One (r407)
  | 477 -> One (r408)
  | 479 -> One (r409)
  | 481 -> One (r410)
  | 485 -> One (r411)
  | 489 -> One (r412)
  | 500 -> One (r415)
  | 497 -> One (r416)
  | 552 -> One (r417)
  | 551 -> One (r418)
  | 504 -> One (r419)
  | 506 -> One (r420)
  | 546 -> One (r421)
  | 509 -> One (r422)
  | 508 -> One (r423)
  | 514 -> One (r424)
  | 516 -> One (r425)
  | 519 -> One (r426)
  | 545 -> One (r427)
  | 524 -> One (r428)
  | 528 -> One (r430)
  | 527 -> One (r431)
  | 526 -> One (r432)
  | 530 -> One (r433)
  | 533 -> One (r434)
  | 532 -> One (r435)
  | 537 -> One (r436)
  | 540 -> One (r437)
  | 539 -> One (r438)
  | 538 | 630 | 641 -> One (r439)
  | 542 -> One (r440)
  | 544 -> One (r441)
  | 548 -> One (r442)
  | 547 -> One (r443)
  | 550 -> One (r444)
  | 564 -> One (r445)
  | 568 -> One (r446)
  | 571 -> One (r447)
  | 586 -> One (r448)
  | 575 -> One (r449)
  | 585 -> One (r451)
  | 584 -> One (r452)
  | 580 -> One (r453)
  | 579 -> One (r454)
  | 578 -> One (r455)
  | 583 -> One (r456)
  | 582 -> One (r457)
  | 1879 -> One (r458)
  | 1878 -> One (r459)
  | 1877 -> One (r460)
  | 1876 -> One (r461)
  | 1875 -> One (r462)
  | 1874 -> One (r463)
  | 590 -> One (r464)
  | 1873 -> One (r465)
  | 593 -> One (r466)
  | 1723 -> One (r468)
  | 1720 -> One (r470)
  | 1719 -> One (r471)
  | 1718 -> One (r472)
  | 595 -> One (r473)
  | 611 -> One (r475)
  | 609 -> One (r476)
  | 608 -> One (r477)
  | 607 -> One (r478)
  | 602 -> One (r479)
  | 601 -> One (r480)
  | 600 -> One (r481)
  | 606 -> One (r482)
  | 605 -> One (r483)
  | 604 -> One (r484)
  | 619 | 627 -> One (r485)
  | 626 -> One (r487)
  | 623 -> One (r489)
  | 625 -> One (r491)
  | 624 -> One (r492)
  | 618 -> One (r493)
  | 617 -> One (r494)
  | 616 -> One (r495)
  | 615 -> One (r496)
  | 622 -> One (r497)
  | 621 -> One (r498)
  | 634 -> One (r499)
  | 633 -> One (r500)
  | 632 -> One (r501)
  | 637 -> One (r502)
  | 636 -> One (r503)
  | 662 -> One (r504)
  | 645 -> One (r505)
  | 644 -> One (r506)
  | 643 -> One (r507)
  | 648 -> One (r508)
  | 647 -> One (r509)
  | 661 -> One (r510)
  | 660 -> One (r511)
  | 659 -> One (r512)
  | 658 -> One (r513)
  | 1871 -> One (r514)
  | 663 -> One (r515)
  | 1593 -> One (r516)
  | 1592 -> One (r517)
  | 1591 -> One (r518)
  | 1590 -> One (r519)
  | 1589 -> One (r520)
  | 1588 -> One (r521)
  | 1855 -> One (r522)
  | 1854 -> One (r523)
  | 1853 -> One (r524)
  | 1852 -> One (r525)
  | 1851 -> One (r526)
  | 665 -> One (r527)
  | 1850 -> One (r528)
  | 749 -> One (r529)
  | 748 -> One (r530)
  | 668 -> One (r531)
  | 667 -> One (r532)
  | 736 -> One (r533)
  | 734 -> One (r534)
  | 733 -> One (r535)
  | 670 -> One (r536)
  | 672 -> One (r537)
  | 732 -> One (r538)
  | 731 -> One (r539)
  | 674 -> One (r540)
  | 730 -> One (r541)
  | 729 -> One (r542)
  | 683 -> One (r543)
  | 681 -> One (r544)
  | 680 -> One (r545)
  | 677 -> One (r546)
  | 727 -> One (r547)
  | 691 -> One (r548)
  | 690 -> One (r549)
  | 687 -> One (r550)
  | 686 -> One (r551)
  | 694 -> One (r552)
  | 693 -> One (r553)
  | 698 -> One (r554)
  | 697 -> One (r555)
  | 696 -> One (r556)
  | 711 -> One (r557)
  | 710 -> One (r559)
  | 704 -> One (r561)
  | 703 -> One (r562)
  | 702 -> One (r563)
  | 701 -> One (r564)
  | 700 -> One (r565)
  | 709 -> One (r566)
  | 714 -> One (r568)
  | 716 -> One (r569)
  | 719 -> One (r570)
  | 718 -> One (r571)
  | 720 | 2146 -> One (r572)
  | 722 -> One (r573)
  | 726 -> One (r575)
  | 738 -> One (r576)
  | 743 -> One (r577)
  | 742 -> One (r578)
  | 1844 -> One (r579)
  | 1642 | 1731 | 1762 | 1769 | 1841 | 1847 | 1903 -> One (r580)
  | 1840 -> One (r582)
  | 1839 -> One (r583)
  | 1836 -> One (r584)
  | 1833 -> One (r585)
  | 753 -> One (r586)
  | 1832 -> One (r587)
  | 1781 -> One (r588)
  | 1780 -> One (r589)
  | 1779 -> One (r590)
  | 1784 -> One (r592)
  | 1831 -> One (r594)
  | 1830 -> One (r595)
  | 1829 -> One (r596)
  | 1828 -> One (r597)
  | 1827 -> One (r598)
  | 1826 -> One (r599)
  | 761 -> One (r600)
  | 760 -> One (r601)
  | 1823 -> One (r602)
  | 764 -> One (r603)
  | 763 -> One (r604)
  | 1820 -> One (r605)
  | 1819 -> One (r606)
  | 1818 -> One (r607)
  | 767 -> One (r608)
  | 766 -> One (r609)
  | 1817 -> One (r610)
  | 770 -> One (r611)
  | 769 -> One (r612)
  | 1816 -> One (r613)
  | 1812 -> One (r614)
  | 1811 -> One (r615)
  | 1810 -> One (r616)
  | 777 -> One (r617)
  | 779 -> One (r619)
  | 983 -> One (r621)
  | 778 -> One (r623)
  | 981 -> One (r625)
  | 1809 -> One (r627)
  | 785 -> One (r628)
  | 784 -> One (r629)
  | 781 -> One (r630)
  | 776 -> One (r631)
  | 775 -> One (r632)
  | 783 -> One (r633)
  | 789 -> One (r634)
  | 788 -> One (r635)
  | 787 -> One (r636)
  | 791 -> One (r637)
  | 1801 -> One (r638)
  | 1800 -> One (r639)
  | 1799 -> One (r640)
  | 1798 -> One (r641)
  | 797 -> One (r642)
  | 796 -> One (r643)
  | 795 -> One (r644)
  | 794 -> One (r645)
  | 1748 -> One (r646)
  | 1797 -> One (r648)
  | 1796 -> One (r649)
  | 1795 -> One (r650)
  | 1794 -> One (r651)
  | 1793 -> One (r652)
  | 1792 -> One (r653)
  | 802 -> One (r654)
  | 801 -> One (r655)
  | 800 -> One (r656)
  | 799 -> One (r657)
  | 1791 -> One (r658)
  | 809 -> One (r659)
  | 814 -> One (r660)
  | 813 -> One (r661)
  | 812 | 1788 -> One (r662)
  | 1787 -> One (r663)
  | 823 -> One (r664)
  | 822 -> One (r665)
  | 821 -> One (r666)
  | 820 -> One (r667)
  | 819 -> One (r668)
  | 818 -> One (r669)
  | 1714 -> One (r670)
  | 830 -> One (r671)
  | 829 -> One (r672)
  | 834 -> One (r673)
  | 833 -> One (r674)
  | 832 -> One (r675)
  | 836 -> One (r676)
  | 838 -> One (r677)
  | 1655 | 1707 -> One (r678)
  | 1654 | 1706 -> One (r679)
  | 840 | 1653 -> One (r680)
  | 839 | 1652 -> One (r681)
  | 844 -> One (r682)
  | 843 -> One (r683)
  | 842 -> One (r684)
  | 1705 -> One (r685)
  | 858 -> One (r686)
  | 853 -> One (r687)
  | 852 | 967 | 1880 -> One (r688)
  | 857 -> One (r690)
  | 856 -> One (r691)
  | 849 -> One (r692)
  | 851 -> One (r693)
  | 855 -> One (r694)
  | 860 -> One (r695)
  | 862 -> One (r696)
  | 864 -> One (r697)
  | 1651 | 1701 -> One (r698)
  | 865 | 1668 -> One (r699)
  | 868 | 1671 -> One (r700)
  | 867 | 1670 -> One (r701)
  | 866 | 1669 -> One (r702)
  | 1630 -> One (r703)
  | 879 -> One (r704)
  | 878 -> One (r705)
  | 883 -> One (r706)
  | 882 -> One (r707)
  | 935 -> One (r708)
  | 889 -> One (r709)
  | 893 -> One (r710)
  | 895 -> One (r711)
  | 897 -> One (r712)
  | 899 -> One (r713)
  | 901 -> One (r714)
  | 903 -> One (r715)
  | 905 -> One (r716)
  | 907 -> One (r717)
  | 909 -> One (r718)
  | 911 -> One (r719)
  | 913 -> One (r720)
  | 915 -> One (r721)
  | 917 -> One (r722)
  | 919 -> One (r723)
  | 921 -> One (r724)
  | 923 -> One (r725)
  | 925 -> One (r726)
  | 924 -> One (r727)
  | 927 -> One (r728)
  | 929 -> One (r729)
  | 931 -> One (r730)
  | 933 -> One (r731)
  | 939 -> One (r732)
  | 938 -> One (r733)
  | 1629 -> One (r734)
  | 985 -> One (r735)
  | 943 -> One (r736)
  | 948 -> One (r737)
  | 947 -> One (r738)
  | 946 -> One (r739)
  | 951 -> One (r740)
  | 950 -> One (r741)
  | 953 -> One (r742)
  | 955 -> One (r743)
  | 957 -> One (r744)
  | 959 -> One (r745)
  | 964 -> One (r746)
  | 976 -> One (r748)
  | 966 -> One (r749)
  | 972 -> One (r750)
  | 971 -> One (r751)
  | 970 -> One (r752)
  | 969 -> One (r753)
  | 975 -> One (r754)
  | 974 -> One (r755)
  | 982 -> One (r756)
  | 980 -> One (r757)
  | 979 -> One (r758)
  | 1628 -> One (r759)
  | 1627 -> One (r760)
  | 987 -> One (r761)
  | 989 -> One (r762)
  | 991 -> One (r763)
  | 1008 -> One (r764)
  | 1007 -> One (r765)
  | 1026 -> One (r767)
  | 1025 -> One (r768)
  | 1024 -> One (r769)
  | 1004 -> One (r770)
  | 1003 -> One (r771)
  | 1002 -> One (r772)
  | 999 -> One (r773)
  | 996 -> One (r774)
  | 995 -> One (r775)
  | 994 -> One (r776)
  | 993 -> One (r777)
  | 998 -> One (r778)
  | 1001 -> One (r779)
  | 1023 -> One (r780)
  | 1014 -> One (r781)
  | 1013 -> One (r782)
  | 1006 -> One (r783)
  | 1012 -> One (r784)
  | 1011 -> One (r785)
  | 1010 -> One (r786)
  | 1020 -> One (r787)
  | 1019 -> One (r788)
  | 1018 -> One (r789)
  | 1017 -> One (r790)
  | 1016 -> One (r791)
  | 1022 -> One (r792)
  | 1626 -> One (r793)
  | 1625 -> One (r794)
  | 1028 -> One (r795)
  | 1624 -> One (r796)
  | 1623 -> One (r797)
  | 1030 -> One (r798)
  | 1043 -> One (r799)
  | 1046 -> One (r801)
  | 1045 -> One (r802)
  | 1042 -> One (r803)
  | 1041 -> One (r804)
  | 1037 -> One (r805)
  | 1036 -> One (r806)
  | 1035 -> One (r807)
  | 1034 -> One (r808)
  | 1040 -> One (r809)
  | 1039 -> One (r810)
  | 1093 -> One (r812)
  | 1092 -> One (r813)
  | 1091 -> One (r814)
  | 1086 -> One (r815)
  | 1110 -> One (r819)
  | 1109 -> One (r820)
  | 1108 -> One (r821)
  | 1234 -> One (r822)
  | 1233 -> One (r823)
  | 1232 -> One (r824)
  | 1231 -> One (r825)
  | 1085 -> One (r826)
  | 1084 -> One (r828)
  | 1071 -> One (r829)
  | 1076 -> One (r837)
  | 1073 -> One (r839)
  | 1072 -> One (r840)
  | 1070 -> One (r841)
  | 1069 -> One (r842)
  | 1068 -> One (r843)
  | 1067 -> One (r844)
  | 1063 -> One (r845)
  | 1062 -> One (r846)
  | 1066 -> One (r847)
  | 1065 -> One (r848)
  | 1078 -> One (r849)
  | 1083 -> One (r850)
  | 1080 -> One (r851)
  | 1082 -> One (r852)
  | 1090 -> One (r853)
  | 1107 -> One (r854)
  | 1103 -> One (r855)
  | 1099 -> One (r856)
  | 1102 -> One (r857)
  | 1101 -> One (r858)
  | 1106 -> One (r859)
  | 1105 -> One (r860)
  | 1406 -> One (r861)
  | 1166 -> One (r862)
  | 1182 -> One (r864)
  | 1181 -> One (r865)
  | 1180 -> One (r866)
  | 1179 -> One (r867)
  | 1178 -> One (r868)
  | 1164 -> One (r872)
  | 1163 -> One (r873)
  | 1162 -> One (r874)
  | 1160 -> One (r875)
  | 1159 -> One (r876)
  | 1135 -> One (r878)
  | 1134 -> One (r879)
  | 1133 -> One (r880)
  | 1124 -> One (r881)
  | 1123 -> One (r882)
  | 1129 -> One (r883)
  | 1128 -> One (r884)
  | 1127 | 2015 -> One (r885)
  | 1131 | 2014 -> One (r886)
  | 1152 -> One (r887)
  | 1144 -> One (r888)
  | 1143 -> One (r889)
  | 1142 -> One (r890)
  | 1151 -> One (r891)
  | 1150 -> One (r892)
  | 1174 -> One (r893)
  | 1173 -> One (r894)
  | 1172 -> One (r895)
  | 1405 -> One (r896)
  | 1193 -> One (r897)
  | 1192 -> One (r898)
  | 1191 -> One (r899)
  | 1190 -> One (r900)
  | 1189 -> One (r901)
  | 1188 -> One (r902)
  | 1187 -> One (r903)
  | 1186 -> One (r904)
  | 1226 -> One (r905)
  | 1225 -> One (r906)
  | 1228 -> One (r908)
  | 1227 -> One (r909)
  | 1221 -> One (r910)
  | 1203 -> One (r911)
  | 1202 -> One (r912)
  | 1201 -> One (r913)
  | 1200 -> One (r914)
  | 1199 -> One (r915)
  | 1207 -> One (r919)
  | 1206 -> One (r920)
  | 1220 -> One (r921)
  | 1212 -> One (r922)
  | 1211 -> One (r923)
  | 1210 -> One (r924)
  | 1209 -> One (r925)
  | 1219 -> One (r926)
  | 1218 -> One (r927)
  | 1217 -> One (r928)
  | 1216 -> One (r929)
  | 1215 -> One (r930)
  | 1214 -> One (r931)
  | 1224 -> One (r934)
  | 1223 -> One (r935)
  | 1230 -> One (r936)
  | 1293 | 1347 -> One (r938)
  | 1349 -> One (r940)
  | 1363 -> One (r942)
  | 1353 -> One (r943)
  | 1352 -> One (r944)
  | 1334 -> One (r945)
  | 1333 -> One (r946)
  | 1332 -> One (r947)
  | 1331 -> One (r948)
  | 1330 -> One (r949)
  | 1329 -> One (r950)
  | 1328 -> One (r951)
  | 1318 -> One (r952)
  | 1317 -> One (r953)
  | 1249 -> One (r954)
  | 1248 -> One (r955)
  | 1247 -> One (r956)
  | 1240 -> One (r957)
  | 1238 -> One (r958)
  | 1237 -> One (r959)
  | 1242 -> One (r960)
  | 1244 -> One (r962)
  | 1243 -> One (r963)
  | 1246 -> One (r964)
  | 1311 -> One (r965)
  | 1310 -> One (r966)
  | 1255 -> One (r967)
  | 1251 -> One (r968)
  | 1254 -> One (r969)
  | 1253 -> One (r970)
  | 1266 -> One (r971)
  | 1265 -> One (r972)
  | 1264 -> One (r973)
  | 1263 -> One (r974)
  | 1262 -> One (r975)
  | 1257 -> One (r976)
  | 1277 -> One (r977)
  | 1276 -> One (r978)
  | 1275 -> One (r979)
  | 1274 -> One (r980)
  | 1273 -> One (r981)
  | 1268 -> One (r982)
  | 1302 -> One (r983)
  | 1301 -> One (r984)
  | 1279 -> One (r985)
  | 1300 -> One (r986)
  | 1299 -> One (r987)
  | 1298 -> One (r988)
  | 1297 -> One (r989)
  | 1281 -> One (r990)
  | 1295 -> One (r991)
  | 1285 -> One (r992)
  | 1284 -> One (r993)
  | 1283 -> One (r994)
  | 1292 | 1340 -> One (r995)
  | 1289 -> One (r997)
  | 1288 -> One (r998)
  | 1287 -> One (r999)
  | 1286 | 1339 -> One (r1000)
  | 1291 -> One (r1001)
  | 1307 -> One (r1002)
  | 1306 -> One (r1003)
  | 1305 -> One (r1004)
  | 1309 -> One (r1006)
  | 1308 -> One (r1007)
  | 1304 -> One (r1008)
  | 1313 -> One (r1009)
  | 1316 -> One (r1010)
  | 1327 -> One (r1011)
  | 1326 -> One (r1012)
  | 1325 -> One (r1013)
  | 1324 -> One (r1014)
  | 1323 -> One (r1015)
  | 1322 -> One (r1016)
  | 1321 -> One (r1017)
  | 1320 -> One (r1018)
  | 1351 -> One (r1019)
  | 1338 -> One (r1020)
  | 1337 -> One (r1021)
  | 1336 -> One (r1022)
  | 1350 -> One (r1023)
  | 1342 -> One (r1024)
  | 1348 -> One (r1025)
  | 1345 -> One (r1026)
  | 1344 -> One (r1027)
  | 1362 -> One (r1028)
  | 1361 -> One (r1029)
  | 1360 -> One (r1030)
  | 1359 -> One (r1031)
  | 1358 -> One (r1032)
  | 1357 -> One (r1033)
  | 1356 -> One (r1034)
  | 1355 -> One (r1035)
  | 1372 -> One (r1036)
  | 1374 -> One (r1037)
  | 1379 -> One (r1038)
  | 1378 -> One (r1039)
  | 1377 -> One (r1040)
  | 1376 -> One (r1041)
  | 1390 -> One (r1042)
  | 1389 -> One (r1043)
  | 1388 -> One (r1044)
  | 1387 -> One (r1045)
  | 1386 -> One (r1046)
  | 1385 -> One (r1047)
  | 1384 -> One (r1048)
  | 1383 -> One (r1049)
  | 1382 -> One (r1050)
  | 1402 -> One (r1051)
  | 1401 -> One (r1052)
  | 1400 -> One (r1053)
  | 1399 -> One (r1054)
  | 1398 -> One (r1055)
  | 1397 -> One (r1056)
  | 1396 -> One (r1057)
  | 1395 -> One (r1058)
  | 1394 -> One (r1059)
  | 1393 -> One (r1060)
  | 1519 -> One (r1061)
  | 1568 -> One (r1063)
  | 1416 -> One (r1064)
  | 1585 -> One (r1066)
  | 1576 -> One (r1067)
  | 1575 -> One (r1068)
  | 1414 -> One (r1069)
  | 1413 -> One (r1070)
  | 1412 -> One (r1071)
  | 1411 -> One (r1072)
  | 1410 -> One (r1073)
  | 1562 -> One (r1074)
  | 1561 -> One (r1075)
  | 1419 -> One (r1076)
  | 1418 -> One (r1077)
  | 1445 -> One (r1078)
  | 1444 -> One (r1079)
  | 1443 -> One (r1080)
  | 1442 -> One (r1081)
  | 1433 -> One (r1082)
  | 1432 -> One (r1084)
  | 1431 -> One (r1085)
  | 1427 -> One (r1086)
  | 1426 -> One (r1087)
  | 1425 -> One (r1088)
  | 1424 -> One (r1089)
  | 1422 -> One (r1090)
  | 1430 -> One (r1091)
  | 1429 -> One (r1092)
  | 1441 -> One (r1093)
  | 1440 -> One (r1094)
  | 1439 -> One (r1095)
  | 1448 -> One (r1096)
  | 1447 -> One (r1097)
  | 1488 -> One (r1098)
  | 1477 -> One (r1099)
  | 1476 -> One (r1100)
  | 1467 -> One (r1101)
  | 1466 -> One (r1103)
  | 1465 -> One (r1104)
  | 1464 -> One (r1105)
  | 1453 -> One (r1106)
  | 1452 -> One (r1107)
  | 1451 -> One (r1108)
  | 1463 -> One (r1109)
  | 1462 -> One (r1110)
  | 1461 -> One (r1111)
  | 1460 -> One (r1112)
  | 1459 -> One (r1113)
  | 1458 -> One (r1114)
  | 1457 -> One (r1115)
  | 1456 -> One (r1116)
  | 1475 -> One (r1117)
  | 1474 -> One (r1118)
  | 1473 -> One (r1119)
  | 1487 -> One (r1120)
  | 1486 -> One (r1121)
  | 1485 -> One (r1122)
  | 1484 -> One (r1123)
  | 1483 -> One (r1124)
  | 1482 -> One (r1125)
  | 1481 -> One (r1126)
  | 1480 -> One (r1127)
  | 1492 -> One (r1128)
  | 1491 -> One (r1129)
  | 1490 -> One (r1130)
  | 1556 -> One (r1131)
  | 1555 -> One (r1132)
  | 1554 -> One (r1133)
  | 1553 -> One (r1134)
  | 1552 -> One (r1135)
  | 1551 -> One (r1136)
  | 1548 -> One (r1137)
  | 1495 -> One (r1138)
  | 1544 -> One (r1139)
  | 1543 -> One (r1140)
  | 1538 -> One (r1141)
  | 1537 -> One (r1142)
  | 1536 -> One (r1143)
  | 1535 -> One (r1144)
  | 1504 -> One (r1145)
  | 1503 -> One (r1146)
  | 1502 -> One (r1147)
  | 1501 -> One (r1148)
  | 1500 -> One (r1149)
  | 1499 -> One (r1150)
  | 1534 -> One (r1151)
  | 1508 -> One (r1152)
  | 1507 -> One (r1153)
  | 1506 -> One (r1154)
  | 1512 -> One (r1155)
  | 1511 -> One (r1156)
  | 1510 -> One (r1157)
  | 1531 -> One (r1158)
  | 1516 -> One (r1159)
  | 1515 -> One (r1160)
  | 1533 -> One (r1162)
  | 1514 -> One (r1163)
  | 1528 -> One (r1164)
  | 1518 -> One (r1165)
  | 1522 -> One (r1166)
  | 1542 -> One (r1167)
  | 1541 -> One (r1168)
  | 1540 -> One (r1169)
  | 1547 -> One (r1170)
  | 1546 -> One (r1171)
  | 1550 -> One (r1172)
  | 1560 -> One (r1173)
  | 1559 -> One (r1174)
  | 1558 -> One (r1175)
  | 1564 -> One (r1176)
  | 1567 -> One (r1177)
  | 1572 -> One (r1178)
  | 1571 -> One (r1179)
  | 1570 -> One (r1180)
  | 1574 -> One (r1181)
  | 1584 -> One (r1182)
  | 1583 -> One (r1183)
  | 1582 -> One (r1184)
  | 1581 -> One (r1185)
  | 1580 -> One (r1186)
  | 1579 -> One (r1187)
  | 1578 -> One (r1188)
  | 1601 -> One (r1189)
  | 1605 -> One (r1190)
  | 1610 -> One (r1191)
  | 1609 -> One (r1192)
  | 1608 -> One (r1193)
  | 1607 -> One (r1194)
  | 1612 -> One (r1195)
  | 1618 -> One (r1196)
  | 1617 -> One (r1197)
  | 1633 | 1676 -> One (r1198)
  | 1632 | 1675 -> One (r1199)
  | 1631 | 1674 -> One (r1200)
  | 1636 | 1681 -> One (r1201)
  | 1635 | 1680 -> One (r1202)
  | 1634 | 1679 -> One (r1203)
  | 1641 | 1688 -> One (r1204)
  | 1640 | 1687 -> One (r1205)
  | 1639 | 1686 -> One (r1206)
  | 1638 | 1685 -> One (r1207)
  | 1647 | 1693 -> One (r1208)
  | 1646 | 1692 -> One (r1209)
  | 1645 | 1691 -> One (r1210)
  | 1650 | 1698 -> One (r1211)
  | 1649 | 1697 -> One (r1212)
  | 1648 | 1696 -> One (r1213)
  | 1657 -> One (r1214)
  | 1660 | 1710 -> One (r1215)
  | 1659 | 1709 -> One (r1216)
  | 1658 | 1708 -> One (r1217)
  | 1662 -> One (r1218)
  | 1665 | 1713 -> One (r1219)
  | 1664 | 1712 -> One (r1220)
  | 1663 | 1711 -> One (r1221)
  | 1667 -> One (r1222)
  | 1673 -> One (r1223)
  | 1678 -> One (r1224)
  | 1683 -> One (r1225)
  | 1690 -> One (r1226)
  | 1695 -> One (r1227)
  | 1700 -> One (r1228)
  | 1703 -> One (r1229)
  | 1717 -> One (r1230)
  | 1716 -> One (r1231)
  | 1722 -> One (r1232)
  | 1726 -> One (r1233)
  | 1728 -> One (r1234)
  | 1730 -> One (r1235)
  | 1733 -> One (r1236)
  | 1744 -> One (r1237)
  | 1743 -> One (r1238)
  | 1751 -> One (r1240)
  | 1742 -> One (r1241)
  | 1737 -> One (r1242)
  | 1753 -> One (r1244)
  | 1735 -> One (r1246)
  | 1752 -> One (r1247)
  | 1741 -> One (r1248)
  | 1740 -> One (r1249)
  | 1739 -> One (r1250)
  | 1750 -> One (r1251)
  | 1749 -> One (r1252)
  | 1746 -> One (r1253)
  | 1755 -> One (r1254)
  | 1759 -> One (r1255)
  | 1761 -> One (r1256)
  | 1764 -> One (r1257)
  | 1766 -> One (r1258)
  | 1768 -> One (r1259)
  | 1771 -> One (r1260)
  | 1774 -> One (r1262)
  | 1773 -> One (r1263)
  | 1786 -> One (r1264)
  | 1785 -> One (r1265)
  | 1778 -> One (r1266)
  | 1777 -> One (r1267)
  | 1808 -> One (r1268)
  | 1807 -> One (r1269)
  | 1806 -> One (r1270)
  | 1805 -> One (r1271)
  | 1804 -> One (r1272)
  | 1803 -> One (r1273)
  | 1822 -> One (r1274)
  | 1825 -> One (r1275)
  | 1838 -> One (r1276)
  | 1843 -> One (r1277)
  | 1846 -> One (r1278)
  | 1849 -> One (r1279)
  | 1862 -> One (r1280)
  | 1861 -> One (r1281)
  | 1860 -> One (r1282)
  | 1859 -> One (r1283)
  | 1858 -> One (r1284)
  | 1857 -> One (r1285)
  | 1870 -> One (r1286)
  | 1869 -> One (r1287)
  | 1868 -> One (r1288)
  | 1867 -> One (r1289)
  | 1866 -> One (r1290)
  | 1865 -> One (r1291)
  | 1864 -> One (r1292)
  | 1886 -> One (r1293)
  | 1885 -> One (r1294)
  | 1884 -> One (r1295)
  | 1883 -> One (r1296)
  | 1882 -> One (r1297)
  | 1891 -> One (r1298)
  | 1890 -> One (r1299)
  | 1889 -> One (r1300)
  | 1888 -> One (r1301)
  | 1894 -> One (r1302)
  | 1893 -> One (r1303)
  | 1902 -> One (r1304)
  | 1905 -> One (r1305)
  | 1914 -> One (r1306)
  | 1913 -> One (r1307)
  | 1916 -> One (r1308)
  | 1918 -> One (r1309)
  | 1930 -> One (r1310)
  | 1929 -> One (r1311)
  | 1933 -> One (r1312)
  | 1932 -> One (r1313)
  | 1936 -> One (r1314)
  | 1935 -> One (r1315)
  | 1945 -> One (r1316)
  | 1944 -> One (r1317)
  | 1957 -> One (r1318)
  | 1961 -> One (r1319)
  | 1960 -> One (r1320)
  | 1959 -> One (r1321)
  | 1963 -> One (r1322)
  | 1971 -> One (r1323)
  | 1977 -> One (r1324)
  | 1976 -> One (r1325)
  | 1975 -> One (r1326)
  | 1974 -> One (r1327)
  | 1973 -> One (r1328)
  | 1980 -> One (r1329)
  | 1986 -> One (r1330)
  | 2000 -> One (r1331)
  | 1999 -> One (r1332)
  | 1998 -> One (r1333)
  | 1994 -> One (r1334)
  | 1993 -> One (r1335)
  | 1992 -> One (r1336)
  | 1991 -> One (r1337)
  | 1990 -> One (r1338)
  | 1997 -> One (r1339)
  | 2003 -> One (r1340)
  | 2006 -> One (r1341)
  | 2005 -> One (r1342)
  | 2018 -> One (r1343)
  | 2017 -> One (r1344)
  | 2030 -> One (r1345)
  | 2029 -> One (r1346)
  | 2054 -> One (r1347)
  | 2053 -> One (r1348)
  | 2063 -> One (r1349)
  | 2065 -> One (r1350)
  | 2067 -> One (r1351)
  | 2081 -> One (r1352)
  | 2085 -> One (r1353)
  | 2090 -> One (r1354)
  | 2097 -> One (r1355)
  | 2096 -> One (r1356)
  | 2095 -> One (r1357)
  | 2094 -> One (r1358)
  | 2104 -> One (r1359)
  | 2108 -> One (r1360)
  | 2112 -> One (r1361)
  | 2115 -> One (r1362)
  | 2120 -> One (r1363)
  | 2124 -> One (r1364)
  | 2128 -> One (r1365)
  | 2132 -> One (r1366)
  | 2136 -> One (r1367)
  | 2139 -> One (r1368)
  | 2143 -> One (r1369)
  | 2149 -> One (r1370)
  | 2157 -> One (r1371)
  | 2167 -> One (r1372)
  | 2169 -> One (r1373)
  | 2172 -> One (r1374)
  | 2171 -> One (r1375)
  | 2174 -> One (r1376)
  | 2184 -> One (r1377)
  | 2180 -> One (r1378)
  | 2179 -> One (r1379)
  | 2183 -> One (r1380)
  | 2182 -> One (r1381)
  | 2189 -> One (r1382)
  | 2188 -> One (r1383)
  | 2187 -> One (r1384)
  | 2191 -> One (r1385)
  | 503 -> Select (function
    | -1 -> [R 119]
    | _ -> S (T T_DOT) :: r419)
  | 811 -> Select (function
    | -1 -> [R 119]
    | _ -> r663)
  | 193 -> Select (function
    | -1 -> r183
    | _ -> R 146 :: r174)
  | 1048 -> Select (function
    | -1 -> r825
    | _ -> R 146 :: r818)
  | 1113 -> Select (function
    | -1 -> r183
    | _ -> R 146 :: r871)
  | 1195 -> Select (function
    | -1 -> r777
    | _ -> R 146 :: r918)
  | 708 -> Select (function
    | -1 -> r319
    | _ -> [R 241])
  | 496 -> Select (function
    | -1 -> [R 680]
    | _ -> S (T T_DOTDOT) :: r416)
  | 521 -> Select (function
    | -1 -> [R 761]
    | _ -> S (N N_pattern) :: r427)
  | 518 -> Select (function
    | -1 -> [R 762]
    | _ -> S (N N_pattern) :: r426)
  | 199 -> Select (function
    | -1 -> r196
    | _ -> R 915 :: r189)
  | 1116 -> Select (function
    | -1 -> r196
    | _ -> R 915 :: r877)
  | 289 -> Select (function
    | -1 -> S (T T_RPAREN) :: r61
    | _ -> S (T T_MODULE) :: r271)
  | 1087 -> Select (function
    | -1 -> S (T T_RPAREN) :: r61
    | _ -> S (T T_COLONCOLON) :: r435)
  | 93 -> Select (function
    | 299 | 592 | 826 | 943 | 1501 | 1540 | 1591 | 1721 -> r69
    | -1 -> S (T T_RPAREN) :: r61
    | _ -> r64)
  | 302 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r284
    | _ -> Sub (r286) :: r288)
  | 751 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r284
    | _ -> Sub (r581) :: r583)
  | 664 -> Select (function
    | -1 | 61 | 192 | 257 | 258 | 987 | 1028 | 1030 | 2176 -> r521
    | _ -> S (T T_OPEN) :: r527)
  | 255 -> Select (function
    | -1 | 336 | 373 | 378 | 411 | 417 | 428 | 434 | 1953 | 1976 | 1982 | 1993 | 1999 -> S (T T_MODULE) :: r145
    | _ -> r127)
  | 1089 -> Select (function
    | -1 -> r572
    | _ -> S (T T_LPAREN) :: r853)
  | 319 -> Select (function
    | -1 -> r321
    | _ -> S (T T_DOT) :: r323)
  | 706 -> Select (function
    | -1 -> r321
    | _ -> S (T T_DOT) :: r567)
  | 248 -> Select (function
    | -1 | 336 | 373 | 378 | 411 | 417 | 428 | 434 | 1953 | 1976 | 1982 | 1993 | 1999 -> r153
    | _ -> S (T T_COLON) :: r226)
  | 167 -> Select (function
    | 576 | 598 | 630 | 641 | 967 | 1094 | 1880 -> r116
    | _ -> r114)
  | 179 -> Select (function
    | 172 | 242 | 250 | 255 | 349 | 359 | 423 | 967 | 1880 | 1988 -> r114
    | _ -> r133)
  | 244 -> Select (function
    | -1 | 249 | 336 | 348 | 358 | 373 | 376 | 378 | 381 | 411 | 414 | 417 | 420 | 422 | 428 | 431 | 434 | 437 | 439 | 1953 | 1956 | 1976 | 1979 | 1982 | 1985 | 1987 | 1993 | 1996 | 1999 | 2002 -> r116
    | _ -> r114)
  | 164 -> Select (function
    | 576 | 598 | 630 | 641 | 967 | 1094 | 1880 -> r117
    | _ -> r115)
  | 178 -> Select (function
    | 172 | 242 | 250 | 255 | 349 | 359 | 423 | 967 | 1880 | 1988 -> r115
    | _ -> r134)
  | 243 -> Select (function
    | -1 | 249 | 336 | 348 | 358 | 373 | 376 | 378 | 381 | 411 | 414 | 417 | 420 | 422 | 428 | 431 | 434 | 437 | 439 | 1953 | 1956 | 1976 | 1979 | 1982 | 1985 | 1987 | 1993 | 1996 | 1999 | 2002 -> r117
    | _ -> r115)
  | 172 -> Select (function
    | 172 | 242 | 250 | 255 | 349 | 359 | 423 | 967 | 1880 | 1988 -> r127
    | _ -> r135)
  | 182 -> Select (function
    | 155 | 1037 | 1063 | 1275 | 1454 | 1474 | 1478 | 1959 -> r130
    | _ -> r138)
  | 181 -> Select (function
    | 155 | 1037 | 1063 | 1275 | 1454 | 1474 | 1478 | 1959 -> r131
    | _ -> r139)
  | 180 -> Select (function
    | 155 | 1037 | 1063 | 1275 | 1454 | 1474 | 1478 | 1959 -> r132
    | _ -> r140)
  | 2032 -> Select (function
    | -1 -> r179
    | _ -> r153)
  | 232 -> Select (function
    | -1 -> r194
    | _ -> r153)
  | 1169 -> Select (function
    | -1 -> r179
    | _ -> r153)
  | 1118 -> Select (function
    | -1 -> r194
    | _ -> r153)
  | 2031 -> Select (function
    | -1 -> r180
    | _ -> r172)
  | 195 -> Select (function
    | -1 -> r181
    | _ -> r173)
  | 194 -> Select (function
    | -1 -> r182
    | _ -> r174)
  | 1168 -> Select (function
    | -1 -> r180
    | _ -> r869)
  | 1115 -> Select (function
    | -1 -> r181
    | _ -> r870)
  | 1114 -> Select (function
    | -1 -> r182
    | _ -> r871)
  | 231 -> Select (function
    | -1 -> r195
    | _ -> r189)
  | 1117 -> Select (function
    | -1 -> r195
    | _ -> r877)
  | 326 -> Select (function
    | -1 -> r320
    | _ -> r323)
  | 707 -> Select (function
    | -1 -> r320
    | _ -> r567)
  | 1198 -> Select (function
    | -1 -> r774
    | _ -> r916)
  | 1197 -> Select (function
    | -1 -> r775
    | _ -> r917)
  | 1196 -> Select (function
    | -1 -> r776
    | _ -> r918)
  | 1056 -> Select (function
    | -1 -> r822
    | _ -> r816)
  | 1050 -> Select (function
    | -1 -> r823
    | _ -> r817)
  | 1049 -> Select (function
    | -1 -> r824
    | _ -> r818)
  | _ -> raise Not_found
