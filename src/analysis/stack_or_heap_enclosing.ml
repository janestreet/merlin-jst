open Std

let log_section = "stack-or-heap-enclosing"
let { Logger.log } = Logger.for_section log_section

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.t
  | No_alloc of { reason : string }
  | Unexpected_no_alloc

type stack_or_heap_enclosings = (Location.t * stack_or_heap) list

let from_nodes ~pos ~path =
  let aux (env, node, tail) =
    let open Browse_raw in
    let ret mode_result = Some (Mbrowse.node_loc node, mode_result) in
    let ret_alloc alloc_mode = ret (Alloc_mode alloc_mode) in
    let ret_no_alloc reason = ret (No_alloc { reason }) in
    let ret_maybe_alloc reason = function
      | Some alloc_mode -> ret_alloc alloc_mode
      | None -> ret_no_alloc reason
    in
    match node with
    | Expression { exp_desc; _ } ->
      (match exp_desc with
       | Texp_function { alloc_mode; body; _ } ->
         let body_loc =
           match body with
           | Tfunction_body { exp_loc; _ } -> exp_loc
           | Tfunction_cases { fc_loc; _ } -> fc_loc
         in
         if Lexing.compare_pos pos body_loc.loc_start >= 0
         && Lexing.compare_pos pos body_loc.loc_end <= 0
         then None
         else ret (Alloc_mode alloc_mode)
       | Texp_array (_, _, alloc_mode) -> ret (Alloc_mode alloc_mode)
       | Texp_construct (_, { cstr_repr; _ }, args, maybe_alloc_mode) ->
         (match maybe_alloc_mode with
          | Some alloc_mode -> ret (Alloc_mode alloc_mode)
          | None ->
            (match args with
             | [] -> ret_no_alloc "constructors without arguments don't allocate"
             | _ :: _ ->
               (match cstr_repr with
                | Variant_unboxed -> ret_no_alloc "unboxed constructors don't allocate"
                | Variant_extensible | Variant_boxed _ -> ret Unexpected_no_alloc)))
       | Texp_record { representation; alloc_mode = maybe_alloc_mode; _ } ->
         (match maybe_alloc_mode, representation with
          | _, Record_inlined _ -> None
          | Some alloc_mode, _ -> ret_alloc alloc_mode
          | None, Record_unboxed -> ret_no_alloc "unboxed records don't allocate"
          | None, (Record_boxed _ | Record_float | Record_ufloat) ->
            ret Unexpected_no_alloc)
       | Texp_field (_, _, _, _, maybe_alloc_mode) ->
         Option.bind maybe_alloc_mode ~f:ret_alloc
       | Texp_variant (_, maybe_exp_and_alloc_mode) ->
         maybe_exp_and_alloc_mode
         |> Option.map ~f:snd
         |> ret_maybe_alloc "variants without arguments don't allocate"
       | _ -> None)
    | _ -> None
  in
  List.filter_map ~f:aux path
;;
