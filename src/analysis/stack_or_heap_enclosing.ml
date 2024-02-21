open Std

let log_section = "stack-or-heap-enclosing"
let {Logger.log} = Logger.for_section log_section

type stack_or_heap =
  | Stack
  | Heap
  | Not_inside_allocating_expr
  | String of string

type typed_enclosings =
  (Location.t * stack_or_heap * Query_protocol.is_tail_position) list

let from_nodes ~path =
  let aux (env, node, tail) =
    (* let open Browse_raw in *)
    let ret x = Some (Mbrowse.node_loc node, x, tail) in
    let _ : _ = ret in
    match[@ocaml.warning "-9"] node with
    | _ -> None
  in
  List.filter_map ~f:aux path
