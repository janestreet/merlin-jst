open Std

let log_section = "stack-or-heap-enclosing"
let { Logger.log } = Logger.for_section log_section

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.t
  | String of string

type typed_enclosings =
  (Location.t * stack_or_heap * Query_protocol.is_tail_position) list

let from_nodes ~path =
  let aux (env, node, tail) =
    let open Browse_raw in
    let ret alloc_mode = Some (Mbrowse.node_loc node, Alloc_mode alloc_mode, tail) in
    let maybe_ret = Option.bind ~f:ret in
    match node with
    | Expression { exp_desc; _ } ->
      (match exp_desc with
       | Texp_function { alloc_mode; _ } | Texp_array (_, _, alloc_mode) -> ret alloc_mode
       | Texp_construct (_, _, _, maybe_alloc_mode)
       | Texp_record { alloc_mode = maybe_alloc_mode; _ }
       | Texp_field (_, _, _, _, maybe_alloc_mode) -> maybe_ret maybe_alloc_mode
       | Texp_variant (_, maybe_exp_and_alloc_mode) ->
         maybe_exp_and_alloc_mode |> Option.map ~f:snd |> maybe_ret
       | _ -> None)
    | _ -> None
  in
  List.filter_map ~f:aux path
;;
