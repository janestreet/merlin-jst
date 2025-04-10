exception Not_an_index of string

val ext : string
val magic_number : string

module Lid : sig
  include Set.OrderedType
  val of_lid : Longident.t Location.loc -> t
  val to_lid : t -> Longident.t Location.loc
end
module Lid_set : Granular_set.S with type elt = Lid.t
module Stats : Map.S with type key = String.t
module Uid_set = Shape.Uid.Set
module Uid_map : Granular_map.S with type key = Shape.Uid.t
module Union_find : sig
  type t

  val make : Uid_set.t -> t
  val get : t -> Uid_set.t
  val union : t -> t -> t
end

type stat = { mtime : float; size : int; source_digest : string option }

type index =
  { defs : Lid_set.t Uid_map.t;
    approximated : Lid_set.t Uid_map.t;
    cu_shape : (Compilation_unit.t, Shape.t) Hashtbl.t;
    stats : stat Stats.t;
    root_directory : string option;
    related_uids : Union_find.t Uid_map.t
  }

val pp : Format.formatter -> index -> unit

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this
    key is already present the locations are merged. *)
val add : Lid_set.t Uid_map.t -> Shape.Uid.t -> Lid_set.t -> Lid_set.t Uid_map.t

type file_content =
  | Cmt of Cmt_format.cmt_infos
  | Cms of Cms_format.cms_infos
  | Index of index
  | Unknown

val write : file:string -> index -> unit
val read : file:string -> file_content

val read_exn : file:string -> index
