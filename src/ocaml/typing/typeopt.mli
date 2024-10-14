(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val is_function_type :
      Env.t -> Types.type_expr -> (Types.type_expr * Types.type_expr) option
val is_base_type : Env.t -> Types.type_expr -> Path.t -> bool

val maybe_pointer_type : Env.t -> Types.type_expr
  -> Lambda.immediate_or_pointer
val maybe_pointer : Typedtree.expression -> Lambda.immediate_or_pointer

val array_type_kind :
  elt_sort:(Jkind.Sort.t option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-1

||||||| ocaml-flambda/flambda-backend:efe8f8dfb491f8e0fae4fbe8788f1c740b5b3b06
=======
val array_type_mut : Env.t -> Types.type_expr -> Lambda.mutable_flag
>>>>>>> ocaml-flambda/flambda-backend:5.2.0minus-1
val array_kind :
  Typedtree.expression -> Jkind.Sort.t -> Lambda.array_kind
(*
val array_pattern_kind :
  Typedtree.pattern -> Jkind.Sort.t -> Lambda.array_kind

(* If [kind] or [layout] is unknown, attempt to specialize it by examining the
   type parameters of the bigarray. If [kind] or [length] is not unknown, returns
   it unmodified. *)
val bigarray_specialize_kind_and_layout :
  Env.t -> kind:Lambda.bigarray_kind -> layout:Lambda.bigarray_layout ->
  Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

val value_kind : Env.t -> Types.type_expr -> Lambda.value_kind
*)

val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-1
                             | `Other]
||||||| ocaml-flambda/flambda-backend:efe8f8dfb491f8e0fae4fbe8788f1c740b5b3b06
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_base] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_base_sort : Jkind.Sort.base -> Lambda.layout
=======
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_base] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_const_sort : Jkind.Sort.Const.t -> Lambda.layout
>>>>>>> ocaml-flambda/flambda-backend:5.2.0minus-1

(*
val value_kind_union :
      Lambda.value_kind -> Lambda.value_kind -> Lambda.value_kind
  (** [value_kind_union k1 k2] is a value_kind at least as general as
      [k1] and [k2] *)
*)
