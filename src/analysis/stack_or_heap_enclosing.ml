open Std

let log_section = "stack-or-heap-enclosing"
let { Logger.log } = Logger.for_section log_section

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.t
  | No_alloc of string
  | String of string

type stack_or_heap_enclosings = (Location.t * stack_or_heap) list

let from_nodes ~pos ~path =
  let aux (env, node, tail) =
    let open Browse_raw in
    let ret alloc_mode = Some (Mbrowse.node_loc node, alloc_mode) in
    let maybe_ret reason = function
      | Some alloc_mode -> ret (Alloc_mode alloc_mode)
      | None -> ret (No_alloc reason)
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
       | Texp_construct (_, _, args, maybe_alloc_mode) ->
         (match args with
          | [] ->
            maybe_ret "constructors without arguments don't allocate" maybe_alloc_mode
          | _ :: _ -> maybe_ret "unboxed constructors don't allocate" maybe_alloc_mode)
       | Texp_record { alloc_mode = maybe_alloc_mode; _ } ->
         maybe_ret "unboxed records don't allocate" maybe_alloc_mode
       | Texp_field (_, _, _, _, maybe_alloc_mode) ->
         maybe_ret
           "field access only allocates when retrieving [float]s from an unboxed float \
            record"
           maybe_alloc_mode
       | Texp_variant (_, maybe_exp_and_alloc_mode) ->
         maybe_exp_and_alloc_mode
         |> Option.map ~f:snd
         |> maybe_ret "variants without arguments don't allocate"
       | _ -> None)
    | _ -> None
  in
  List.filter_map ~f:aux path
;;
