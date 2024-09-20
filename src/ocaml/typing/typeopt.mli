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

val array_kind :
  Typedtree.expression -> Jkind.Sort.t -> Lambda.array_kind
(*
val array_pattern_kind :
  Typedtree.pattern -> Jkind.Sort.t -> Lambda.array_kind
<<<<<<< janestreet/merlin-jst:merge-with-upstream-merlin-round-2-of-conflict-fixing
val bigarray_type_kind_and_layout :
      Env.t -> Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout
||||||| ocaml-flambda/flambda-backend:2d672b4f4ed9e63c57aef3925cc5a74a9a00b6a4
val bigarray_type_kind_and_layout :
      Env.t -> Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

(* CR layouts v7: [layout], [function_return_layout], [function2_return_layout],
   and [layout_of_sort] have had location arguments added just to support the
=======

(* If [kind] or [layout] is unknown, attempt to specialize it by examining the
   type parameters of the bigarray. If [kind] or [length] is not unknown, returns
   it unmodified. *)
val bigarray_specialize_kind_and_layout :
  Env.t -> kind:Lambda.bigarray_kind -> layout:Lambda.bigarray_layout ->
  Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

(* CR layouts v7: [layout], [function_return_layout], [function2_return_layout],
   and [layout_of_sort] have had location arguments added just to support the
>>>>>>> ocaml-flambda/flambda-backend:cbc35f98fe9785b315ed09c5cd7268c579d08945
val value_kind : Env.t -> Types.type_expr -> Lambda.value_kind
*)

val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
<<<<<<< janestreet/merlin-jst:merge-with-upstream-merlin-round-2-of-conflict-fixing
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]
||||||| ocaml-flambda/flambda-backend:2d672b4f4ed9e63c57aef3925cc5a74a9a00b6a4
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_const_sort] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_const_sort : Jkind.Sort.const -> Lambda.layout
=======
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_base] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_base_sort : Jkind.Sort.base -> Lambda.layout
>>>>>>> ocaml-flambda/flambda-backend:cbc35f98fe9785b315ed09c5cd7268c579d08945

(*
val value_kind_union :
      Lambda.value_kind -> Lambda.value_kind -> Lambda.value_kind
  (** [value_kind_union k1 k2] is a value_kind at least as general as
      [k1] and [k2] *)
*)
