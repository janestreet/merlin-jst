(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Re-export *)
module type Axis_ops = sig
  include Mode_intf.Lattice

  val less_or_equal : t -> t -> Misc_stdlib.Le_result.t

  val equal : t -> t -> bool
end

(** The jkind axis of Externality *)
module Externality : sig
  type t =
    | External
    | External64
    | Internal

  include Axis_ops with type t := t
end

(** The jkind axis of nullability *)
module Nullability : sig
  type t =
    | Non_null
    | Maybe_null

  include Axis_ops with type t := t
end

module Axis : sig
  module Nonmodal : sig
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  (** Represents an axis of a jkind *)
  type 'a t =
    | Modal : ('m, 'a, 'd) Mode.Alloc.axis -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed [@@unboxed]

  (* CR zqian: push ['a t] into the module to avoid first-class module. *)

  (** Given a jkind axis, get its interface *)
  val get : 'a t -> (module Axis_ops with type t = 'a)

  val all : packed list

  val name : _ t -> string

  (** Is this a modal axis? Includes externality, because that will one
      day be modal (it is a deep property). *)
  val is_modal : _ t -> bool
end

<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-8
(** A collection with one item for each jkind axis.
    [T] parametizes what element is being held for each axis. *)
module Axis_collection (T : Misc_stdlib.T1) : sig
  type t =
    { locality : Mode.Locality.Const.t T.t;
      linearity : Mode.Linearity.Const.t T.t;
      uniqueness : Mode.Uniqueness.Const.t T.t;
      portability : Mode.Portability.Const.t T.t;
      contention : Mode.Contention.Const.t T.t;
      yielding : Mode.Yielding.Const.t T.t;
      externality : Externality.t T.t;
      nullability : Nullability.t T.t
    }

  val get : axis:'a Axis.t -> t -> 'a T.t

  val set : axis:'a Axis.t -> t -> 'a T.t -> t

  module Create_f : sig
    (** This record type is used to pass a polymorphic function to [create] *)
    type t = { f : 'a. axis:'a Axis.t -> 'a T.t }
  end
||||||| ocaml-flambda/flambda-backend:9af08951c69b6ab8be73ee9c53b8b29a1a6e5c66
(** A collection with one item for each jkind axis.
    [T] parametizes what element is being held for each axis. *)
module Axis_collection (T : Misc.T1) : sig
  type t =
    { locality : Mode.Locality.Const.t T.t;
      linearity : Mode.Linearity.Const.t T.t;
      uniqueness : Mode.Uniqueness.Const.t T.t;
      portability : Mode.Portability.Const.t T.t;
      contention : Mode.Contention.Const.t T.t;
      yielding : Mode.Yielding.Const.t T.t;
      externality : Externality.t T.t;
      nullability : Nullability.t T.t
    }

  val get : axis:'a Axis.t -> t -> 'a T.t

  val set : axis:'a Axis.t -> t -> 'a T.t -> t

  module Create_f : sig
    (** This record type is used to pass a polymorphic function to [create] *)
    type t = { f : 'a. axis:'a Axis.t -> 'a T.t }
  end
=======
module Axis_set : sig
  (** A set of [Axis.t], represented as a bitfield for efficiency. *)
  type t [@@immediate]

  val empty : t

  val singleton : _ Axis.t -> t
>>>>>>> ocaml-flambda/flambda-backend:dc108ccc92da9f9ded43ff047d8dc27a42e2079f

  val is_empty : t -> bool

  val add : t -> _ Axis.t -> t

  val remove : t -> _ Axis.t -> t

  val mem : t -> _ Axis.t -> bool

  val equal : t -> t -> bool

  val union : t -> t -> t

  val intersection : t -> t -> t

  val diff : t -> t -> t

  val is_subset : t -> t -> bool

  val complement : t -> t

  val to_seq : t -> Axis.packed Seq.t

  val to_list : t -> Axis.packed list

  (** Create a [t], specify for each axis whether it should be included *)
  val create : f:(axis:Axis.packed -> bool) -> t

  (** A set of all axes *)
  val all : t

  (** A set of all modal axes *)
  val all_modal_axes : t

  (** A set of all nonmodal axes *)
  val all_nonmodal_axes : t

  val print : Format.formatter -> t -> unit
end
