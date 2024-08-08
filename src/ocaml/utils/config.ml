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

<<<<<<< HEAD
let exec_magic_number = "Caml1999X534"
||||||| 7b73c6aa3
let exec_magic_number = "Caml1999X031"
=======
let ext_obj = ".o_The boot compiler cannot process C objects"

let exec_magic_number = "Caml1999X034"
>>>>>>> upstream/main
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
<<<<<<< HEAD
and cmi_magic_number =
  (* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
  "Caml1999I534"
and cmo_magic_number = "Caml1999O534"
and cma_magic_number = "Caml1999A534"
||||||| 7b73c6aa3
and cmi_magic_number = "Caml1999I031"
and cmo_magic_number = "Caml1999O031"
and cma_magic_number = "Caml1999A031"
=======
and cmi_magic_number = "Caml1999I034"
and cmo_magic_number = "Caml1999O034"
and cma_magic_number = "Caml1999A034"
>>>>>>> upstream/main
and cmx_magic_number =
  if flambda then
<<<<<<< HEAD
    "Caml2021y535"
||||||| 7b73c6aa3
    "Caml1999y031"
=======
    "Caml1999y034"
>>>>>>> upstream/main
  else
<<<<<<< HEAD
    "Caml2021Y534"
||||||| 7b73c6aa3
    "Caml1999Y031"
=======
    "Caml1999Y034"
>>>>>>> upstream/main
and cmxa_magic_number =
  if flambda then
<<<<<<< HEAD
    "Caml2021z535"
||||||| 7b73c6aa3
    "Caml1999z031"
=======
    "Caml1999z034"
>>>>>>> upstream/main
  else
<<<<<<< HEAD
    "Caml2021Z534"
and ast_impl_magic_number = "Caml1999M534"
and ast_intf_magic_number = "Caml1999N534"
and cmxs_magic_number = "Caml1999D534"
and cmt_magic_number = "Caml1999T534"
and cms_magic_number = "Caml1999S534"
||||||| 7b73c6aa3
    "Caml1999Z031"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D031"
and cmt_magic_number = "Caml1999T031"
=======
    "Caml1999Z034"
and ast_impl_magic_number = "Caml1999M034"
and ast_intf_magic_number = "Caml1999N034"
and cmxs_magic_number = "Caml1999D034"
and cmt_magic_number = "Caml1999T034"
and index_magic_number = "Merl2023I001"
>>>>>>> upstream/main

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let reserved_header_bits = 8
let runtime5 = true

let merlin = true
