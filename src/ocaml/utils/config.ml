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

let exec_magic_number = "Caml1999X550"
let ext_obj = ".o_The boot compiler cannot process C objects"

    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number =
  (* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
  "Caml1999I550"
and cmo_magic_number = "Caml1999O550"
and cma_magic_number = "Caml1999A550"
and cmx_magic_number =
  if flambda then
    "Caml1999y550"
  else
    "Caml1999Y550"
and cmxa_magic_number =
  if flambda then
    "Caml1999z550"
  else
    "Caml1999Z550"
and ast_impl_magic_number = "Caml1999M550"
and ast_intf_magic_number = "Caml1999N550"
and cmxs_magic_number = "Caml1999D550"
and cmt_magic_number = "Caml1999T550"
and cms_magic_number = "Caml1999S550"
and index_magic_number = "Merl2023I501"

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let reserved_header_bits = 8
let runtime5 = true

let merlin = true
