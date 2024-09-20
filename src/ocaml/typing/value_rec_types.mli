(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Vincent Laviron, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro, SAS                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types related to the compilation of value let-recs (non-functional
     recursive definitions) *)

(** The kind of recursive bindings, as computed by
    [Value_rec_check.classify_expression] *)
    type recursive_binding_kind =
<<<<<<< janestreet/merlin-jst:merge-with-upstream-merlin-round-2-of-conflict-fixing
    | Static
||||||| ocaml-flambda/flambda-backend:2d672b4f4ed9e63c57aef3925cc5a74a9a00b6a4
| Static
  (** Bindings for which some kind of pre-allocation scheme is possible.
      The expression is allowed to be recursive, as long as its definition does
      not inspect recursively defined values. *)
| Dynamic
  (** Bindings for which pre-allocation is not possible.
      The expression is not allowed to refer to any recursive variable. *)
=======
| Static
  (** Bindings for which some kind of pre-allocation scheme is possible.
      The expression is allowed to be recursive, as long as its definition does
      not inspect recursively defined values.
      See [Value_rec_compiler] for more details on the compilation scheme. *)
| Dynamic
  (** Bindings for which pre-allocation is not possible.
      The expression is not allowed to refer to any recursive variable. *)
>>>>>>> ocaml-flambda/flambda-backend:cbc35f98fe9785b315ed09c5cd7268c579d08945
      (** Bindings for which some kind of pre-allocation scheme is possible.
          The expression is allowed to be recursive, as long as its definition does
          not inspect recursively defined values. *)
    | Dynamic
      (** Bindings for which pre-allocation is not possible.
          The expression is not allowed to refer to any recursive variable. *)
