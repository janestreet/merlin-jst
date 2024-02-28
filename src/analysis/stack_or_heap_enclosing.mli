(** Provides allocation information around the cursor.

    TODO: more docs
*)

val log_section : string

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.t
  | No_alloc of { reason : string }
  | Unexpected_no_alloc
  | String of string

type stack_or_heap_enclosings = (Location.t * stack_or_heap) list

val from_nodes
  :  pos:Lexing.position
  -> path:(Env.t * Browse_raw.node * Query_protocol.is_tail_position) list
  -> stack_or_heap_enclosings
