type t
val builddir : t -> string
val sourcefile : t -> string option
val source_digest : t -> string option
val comments : t -> (string * Location.t) list
val impl_shape : t -> Shape.t option
val uid_to_loc : Shape.Uid.t -> t -> Location.t option

(** When we look for docstring in external compilation unit we can perform
    a uid-based search and return the attached comment in the attributes.
    This is a more sound way to get documentation than resorting on the
    [Ocamldoc.associate_comment] heuristic *)
val uid_to_attributes : Shape.Uid.t -> t -> Parsetree.attributes option

val read : string -> t
