open Std

let log_section = "type-enclosing"
let {Logger.log} = Logger.for_section log_section

type type_info =
  | Modtype of Env.t * Types.module_type
  | Type of Env.t * Types.type_expr * Mode.Value.t option
  | Type_decl of Env.t * Ident.t * Types.type_declaration
  | String of string

type typed_enclosings =
  (Location.t * type_info * Query_protocol.is_tail_position) list

let pat_mode (type pat) env (pat_desc : pat Typedtree.pattern_desc) : Mode.Value.t option =
  match pat_desc with
  | Tpat_var (_, _, _, mode)
  | Tpat_alias (_, _, _, _, mode) -> Some mode
  | _ -> None

let expr_mode env (exp_desc : Typedtree.expression_desc) : Mode.Value.t option =
  match exp_desc with
  | Texp_ident (_, lid, _, _, _) ->
      begin match Env.lookup_value ~loc:lid.loc lid.txt env with
      | _, _, mode, _ -> Some mode
      | exception _ -> None
      end
  | Texp_function { alloc_mode; _ }
  | Texp_tuple (_, alloc_mode)
  | Texp_array (_, _, alloc_mode) ->
      Some (Mode.Value.of_alloc alloc_mode)
  | Texp_construct (_, _, _, alloc_mode)
  | Texp_record { alloc_mode; _ } ->
      Option.map alloc_mode ~f:Mode.Value.of_alloc
  | Texp_variant (_, alloc_mode) ->
      Option.map alloc_mode ~f:(fun (_, mode) -> Mode.Value.of_alloc mode)
  (* [Texp_field] and [Texp_setfield] carry modes, but they're only relevant
     to the boxing of floats. We don't print them to avoid confusion.
  *)
  | Texp_field _ | Texp_setfield _ -> None
  | Texp_list_comprehension _
  | Texp_array_comprehension _
  | Texp_constant _
  | Texp_let _
  | Texp_apply _
  | Texp_match _
  | Texp_try _
  | Texp_ifthenelse _
  | Texp_sequence _
  | Texp_while _
  | Texp_for _
  | Texp_send _
  | Texp_new _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_letmodule _
  | Texp_letexception _
  | Texp_assert _
  | Texp_lazy _
  | Texp_object _
  | Texp_pack _
  | Texp_letop _
  | Texp_unreachable
  | Texp_extension_constructor _
  | Texp_open _
  | Texp_probe _
  | Texp_probe_is_enabled _
  | Texp_exclave _
  | Texp_hole
    -> None

let from_nodes ~path =
  let aux (env, node, tail) =
    let open Browse_raw in
    let ret x = Some (Mbrowse.node_loc node, x, tail) in
    match[@ocaml.warning "-9"] node with
    | Expression {exp_type = t; exp_desc = desc} ->
        let mode = expr_mode env desc in
        ret (Type (env, t, mode))
    | Pattern {pat_type=t; pat_desc = desc} ->
        let mode = pat_mode env desc in
        ret (Type (env, t, mode))
    | Core_type {ctyp_type = t}
    | Value_description { val_desc = { ctyp_type = t } } ->
      ret (Type (env, t, None))
    | Type_declaration { typ_id = id; typ_type = t} ->
      ret (Type_decl (env, id, t))
    | Module_expr {mod_type = Types.Mty_for_hole} -> None
    | Module_expr {mod_type = m}
    | Module_type {mty_type = m}
    | Module_binding {mb_expr = {mod_type = m}}
    | Module_declaration {md_type = {mty_type = m}}
    | Module_type_declaration {mtd_type = Some {mty_type = m}}
    | Module_binding_name {mb_expr = {mod_type = m}}
    | Module_declaration_name {md_type = {mty_type = m}}
    | Module_type_declaration_name {mtd_type = Some {mty_type = m}} ->
      ret (Modtype (env, m))
    | Class_field
        { cf_desc =
            Tcf_method
              (_, _,
               Tcfk_concrete
                 (_, {exp_type})) } ->
      begin match Types.get_desc exp_type with
        | Tarrow (_, _, t, _) -> ret (Type (env, t, None))
        | _ -> None
      end
    | Class_field
        { cf_desc =
            Tcf_val (_, _, _, Tcfk_concrete (_, {exp_type = t }), _) } ->
      ret (Type (env, t, None))
    | Class_field { cf_desc =
                      Tcf_method (_, _, Tcfk_virtual {ctyp_type = t }) } ->
      ret (Type (env, t, None))
    | Class_field { cf_desc =
                      Tcf_val (_, _, _, Tcfk_virtual {ctyp_type = t }, _) } ->
      ret (Type (env, t, None))
    | Binding_op { bop_op_type; _ } -> ret (Type(env, bop_op_type, None))
    | _ -> None
  in
  List.filter_map ~f:aux path

let from_reconstructed ~nodes ~cursor ~verbosity exprs =
  let open Browse_raw in
  let env, node = Mbrowse.leaf_node nodes in
  log ~title:"from_reconstructed" "node = %s\nexprs = [%s]"
    (Browse_raw.string_of_node node)
    (String.concat ~sep:";" (List.map exprs ~f:(fun l ->
         l.Location.txt))
    );
  let include_lident = match node with
    | Pattern _ -> false
    | _ -> true
  in
  let include_uident = match node with
    | Module_binding _
    | Module_binding_name _
    | Module_declaration _
    | Module_declaration_name _
    | Module_type_declaration _
    | Module_type_declaration_name _
      -> false
    | _ -> true
  in

  let get_context lident =
    Context.inspect_browse_tree
      ~cursor
      (Longident.parse lident)
      [nodes]
  in

  let f =
    fun {Location. txt = source; loc} ->
      let context = get_context source in
      Option.iter context ~f:(fun ctx ->
          log ~title:"from_reconstructed" "source = %s; context = %s"
            source (Context.to_string ctx));
      match context with
      (* Retrieve the type from the AST when it is possible *)
      | Some (Context.Constructor (cd, loc)) ->
        log ~title:"from_reconstructed" "ctx: constructor %s"
          cd.cstr_name;
        let ppf, to_string = Format.to_string () in
        Type_utils.print_constr ~verbosity env ppf cd;
        Some (loc, String (to_string ()), `No)
      | Some (Context.Label { lbl_name; lbl_arg; _ }) ->
        log ~title:"from_reconstructed" "ctx: label %s" lbl_name;
        let ppf, to_string = Format.to_string () in
        Type_utils.print_type_with_decl ~verbosity env ppf lbl_arg None;
        Some (loc, String (to_string ()), `No)
      | Some Context.Constant -> None
      | _ ->
        let context = Option.value ~default:Context.Expr context in
        (* Else use the reconstructed identifier *)
        match source with
        | "" ->
          log ~title:"from_reconstructed" "no reconstructed identifier";
          None
        | source when not include_lident && Char.is_lowercase source.[0] ->
          log ~title:"from_reconstructed" "skipping lident";
          None
        | source when not include_uident && Char.is_uppercase source.[0] ->
          log ~title:"from_reconstructed" "skipping uident";
          None
        | source ->
          try
            let ppf, to_string = Format.to_string () in
            if Type_utils.type_in_env ~verbosity ~context env ppf source then (
              log ~title:"from_reconstructed" "typed %s" source;
              Some (loc, String (to_string ()), `No)
            )
            else (
              log ~title:"from_reconstructed" "FAILED to type %s" source;
              None
            )
          with _ ->
            None
  in
  List.filter_map exprs ~f
