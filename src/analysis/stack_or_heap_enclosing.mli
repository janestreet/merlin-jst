(** Provides allocation information around the cursor.

    TODO: more docs
*)

val log_section : string

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.t
  | No_alloc
  | String of string

type typed_enclosings =
  (Location.t * stack_or_heap * Query_protocol.is_tail_position) list

val from_nodes :
  path:(Env.t * Browse_raw.node * Query_protocol.is_tail_position) list ->
  typed_enclosings
