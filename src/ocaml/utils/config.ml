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

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)

(* merlin-jst: All magic numbers ending in "500" should not change when we
   upgrade the upstream Merlin version.  Otherwise, this should track
   "utils/config.mlp" from the compiler.  Also, the above warning comment about
   "config.mlbuild" is outdated and can be ignored. *)

(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let flambda = false

let exec_magic_number = "Caml1999X532"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number =
  (* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
  "Caml1999I532"
and cmo_magic_number = "Caml1999O532"
and cma_magic_number = "Caml1999A532"
and cmx_magic_number =
  if flambda then
    "Caml2021y533"
  else
    "Caml2021Y532"
and cmxa_magic_number =
  if flambda then
    "Caml2021z533"
  else
    "Caml2021Z532"
and ast_impl_magic_number = "Caml1999M033"
and ast_intf_magic_number = "Caml1999N033"
and cmxs_magic_number = "Caml1999D532"
and cmt_magic_number = "Caml1999T532"
and cms_magic_number = "Caml1999S532"

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let reserved_header_bits = 8
let runtime5 = true

let merlin = true
