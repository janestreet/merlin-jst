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
  elt_sort:(Jkind.Sort.Const.t option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
(*
val array_type_mut : Env.t -> Types.type_expr -> Lambda.mutable_flag
val array_kind_of_elt :
  elt_sort:(Jkind.Sort.Const.t option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
*)
val array_kind :
<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-5
  Typedtree.expression -> Jkind.Sort.t -> Lambda.array_kind
(*
||||||| ocaml-flambda/flambda-backend:581b385a59911c05d91e2de7868e16f791e0c67a
  Typedtree.expression -> Jkind.Sort.t -> Lambda.array_kind
=======
  Typedtree.expression -> Jkind.Sort.Const.t -> Lambda.array_kind
>>>>>>> ocaml-flambda/flambda-backend:df4a6e0ba4f74dc790e0ad79f15ea73be1225c4b
val array_pattern_kind :
  Typedtree.pattern -> Jkind.Sort.Const.t -> Lambda.array_kind

(* If [kind] or [layout] is unknown, attempt to specialize it by examining the
   type parameters of the bigarray. If [kind] or [length] is not unknown, returns
   it unmodified. *)
val bigarray_specialize_kind_and_layout :
  Env.t -> kind:Lambda.bigarray_kind -> layout:Lambda.bigarray_layout ->
  Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

val value_kind : Env.t -> Types.type_expr -> Lambda.value_kind
*)
<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-5
||||||| ocaml-flambda/flambda-backend:581b385a59911c05d91e2de7868e16f791e0c67a
val layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

(* These translate a type system sort to a lambda layout.  The function [layout]
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_base] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_const_sort : Jkind.Sort.Const.t -> Lambda.layout

(* Given a function type and the sort of its return type, compute the layout of
   its return type. *)
val function_return_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout
=======
val layout :
  Env.t -> Location.t -> Jkind.Sort.Const.t -> Types.type_expr -> Lambda.layout

(* These translate a type system sort to a lambda layout.  The function [layout]
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_non_void_sort] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.Sort.Const.t -> Lambda.layout
val layout_of_non_void_sort : Jkind.Sort.Const.t -> Lambda.layout

(* Given a function type and the sort of its return type, compute the layout of
   its return type. *)
val function_return_layout :
  Env.t -> Location.t -> Jkind.Sort.Const.t -> Types.type_expr -> Lambda.layout
>>>>>>> ocaml-flambda/flambda-backend:df4a6e0ba4f74dc790e0ad79f15ea73be1225c4b

<<<<<<< janestreet/merlin-jst:merge-5.2.0minus-5
val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]
||||||| ocaml-flambda/flambda-backend:581b385a59911c05d91e2de7868e16f791e0c67a
(* Given a function type with two arguments and the sort of its return type,
   compute the layout of its return type. *)
val function2_return_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

(* Given a function type and the sort of its argument, compute the layout
   of its argument.  Fails loudly if the type isn't a function type. *)
val function_arg_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

val value_kind : Env.t -> Location.t -> Types.type_expr -> Lambda.value_kind
=======
(* Given a function type with two arguments and the sort of its return type,
   compute the layout of its return type. *)
val function2_return_layout :
  Env.t -> Location.t -> Jkind.Sort.Const.t -> Types.type_expr -> Lambda.layout

(* Given a function type and the sort of its argument, compute the layout
   of its argument.  Fails loudly if the type isn't a function type. *)
val function_arg_layout :
  Env.t -> Location.t -> Jkind.Sort.Const.t -> Types.type_expr -> Lambda.layout

val value_kind : Env.t -> Location.t -> Types.type_expr -> Lambda.value_kind
>>>>>>> ocaml-flambda/flambda-backend:df4a6e0ba4f74dc790e0ad79f15ea73be1225c4b

(*
val value_kind_union :
      Lambda.value_kind -> Lambda.value_kind -> Lambda.value_kind
  (** [value_kind_union k1 k2] is a value_kind at least as general as
      [k1] and [k2] *)
*)
