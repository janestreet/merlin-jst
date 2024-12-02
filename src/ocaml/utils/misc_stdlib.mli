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

val pp_parens_if :
     bool
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit
(** [pp_parens_if bool formatter ppf arg] prints [formatter ppf arg], wrapping it with
    [()] if [bool] is true. *)

val pp_nested_list :
     nested:bool
  -> pp_element:(nested:bool -> Format.formatter -> 'a -> unit)
  -> pp_sep:(Format.formatter -> unit -> unit)
  -> Format.formatter
  -> 'a list
  -> unit
(** [pp_nested_list ~nested ~pp_element ~pp_sep ppf args] prints the list [args]
    with [pp_element] on [ppf]. The elements are separated by [pp_sep]. If
    [~nested] is true, the list is wrapped in parens. The element printer is
    always called with [nested:true], indicating that any inner lists are nested
    and need parens. *)

val to_string_of_print :
  (Format.formatter -> 'a -> unit) -> 'a -> string
(** [to_string_of_print print] produces a string conversion function from a
    pretty printer. This is similar but preferable to [Format.asprintf "%a"]
    when the output may be large, since [to_string] functions don't usually
    return embedded newlines. *)

module List : sig
  val map_option : ('a -> 'b option) -> 'a list -> 'b list option
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
  val some_if_all_elements_are_some : 'a option list -> 'a list option
  val iter_until_error
       : f:('a -> (unit, 'b) Result.t)
      -> 'a list
      -> (unit, 'b) Result.t

  val merge_iter
      : cmp:('a -> 'b -> int)
    -> left_only:('a -> unit)
    -> right_only:('b -> unit)
    -> both:('a -> 'b -> unit)
    -> 'a list
    -> 'b list
    -> unit
end

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

(** {2 Extensions to the Array module} *)
module Array : sig
  val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  (** Same as [Array.exists2] from the standard library. *)

  val fold_left2 :
    ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a array -> 'b array -> 'acc
  (** [fold_left2 f init [|a1; ...; an|] [|b1; ...; bn|]] is
      [f (... (f (f init a1 b1) a2 b2) ...) an bn].
      @raise Invalid_argument if the two arrays are determined
      to have different lengths.
  *)

  val for_alli : (int -> 'a -> bool) -> 'a array -> bool
  (** Same as [Array.for_all] from the standard library, but the
      function is applied with the index of the element as first argument,
      and the element itself as second argument. *)

  val all_somes : 'a option array -> 'a array option

  val equal : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
  (** Compare two arrays for equality, using the supplied predicate for
      element equality *)

  val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int
  (** Compare two arrays, using the supplied predicate for element equality *)

  val map_sharing : ('a -> 'a) -> 'a array -> 'a array
  (** [map_sharing f a] is [map f a]. If for all elements of the array
      [f e == e] then [map_sharing f a == a] *)

  val of_list_map : ('a -> 'b) -> 'a list -> 'b array
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

module Monad : sig
  module type Basic = sig
    type 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t

    (** The following identities ought to hold (for some value of =):

        - [return x >>= f = f x]
        - [t >>= fun x -> return x = t]
        - [(t >>= f) >>= g = t >>= fun x -> (f x >>= g)]

        Note: [>>=] is the infix notation for [bind]) *)
  end

  module type Basic2 = sig
    (** Multi parameter monad. The second parameter gets unified across all
        the computation.  This is used to encode monads working on a multi
        parameter data structure like ([('a,'b) result]). *)

    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val return : 'a -> ('a, _) t
  end

  module type S = sig
    type 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    (** [>>=] is a synonym for [bind] *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    (** [return v] returns the (trivial) computation that returns v. *)
    val return : 'a -> 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [join t] is [t >>= (fun t' -> t')]. *)
    val join : 'a t t -> 'a t

    (** [ignore_m t] is [map (fun _ -> ()) t]. *)
    val ignore_m : 'a t -> unit t

    val all : 'a t list -> 'a list t

    (** Like [all], but ensures that every monadic value in the list produces
        a unit value, all of which are discarded rather than being collected
        into a list. *)
    val all_unit : unit t list -> unit t
  end

  module type S2 = sig
    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val return : 'a -> ('a, _) t
    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
    val ignore_m : (_, 'e) t -> (unit, 'e) t
    val all : ('a, 'e) t list -> ('a list, 'e) t
    val all_unit : (unit, 'e) t list -> (unit, 'e) t
  end

  module Make (X : Basic) : S with type 'a t = 'a X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t = ('a, 'e) X.t

  module Option : S with type 'a t = 'a option
  module Result : S2 with type ('a, 'e) t = ('a, 'e) result
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

type (_, _) eq = Refl : ('a, 'a) eq

module type T1 = sig
  type 'a t
end
