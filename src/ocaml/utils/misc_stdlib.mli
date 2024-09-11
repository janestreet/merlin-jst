(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module Option : sig
  type 'a t = 'a option

  (* short circuits if the first argument really is a [Some] *)
  val first_some : 'a option -> (unit -> 'a option) -> 'a option

  val print
     : (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit
end

module String : sig
  include module type of String
  module Set : Set.S with type elt = string
  module Map : Map.S with type key = string
  module Tbl : Hashtbl.S with type key = string

  val print : Format.formatter -> t -> unit

  val for_all : (char -> bool) -> t -> bool

  val begins_with : ?from:int -> string -> prefix:string -> bool

  val split_on_string : string -> split_on:string -> string list

  val split_on_chars : string -> split_on:char list -> string list

  (** Splits on the last occurrence of the given character. *)
  val split_last_exn : string -> split_on:char -> string * string

  val starts_with : prefix:string -> string -> bool
  val ends_with : suffix:string -> string -> bool
end

module Int : sig
  include module type of Int

  val min : t -> t -> t
  val max : t -> t -> t
end

val format_as_unboxed_literal : string -> string
(** [format_as_unboxed_literal constant_literal] converts [constant_literal] to its
    corresponding unboxed literal by either adding "#" in front or changing
    "-" to "-#".

    Examples:

      [0.1] to [#0.1]
      [-3] to [-#3]
      [0xa.cp-1] to [#0xa.cp-1] *)

(** The result of a less-than-or-equal comparison *)
module Le_result : sig
  type t =
    | Equal
    | Less
    | Not_le

  val combine : t -> t -> t
  val combine_list : t list -> t

  val is_le : t -> bool
  val is_equal : t -> bool
end

(** Propositional equality *)
type (_, _) eq = Refl : ('a, 'a) eq

(** Utilities for module-level programming *)
module type T = sig
  type t
end

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module type T3 = sig
  type ('a, 'b, 'c) t
end

module type T4 = sig
  type ('a, 'b, 'c, 'd) t
end
