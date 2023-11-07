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
let exec_magic_number = "Caml1999X513"
||||||| b01e78e20
let exec_magic_number = "Caml1999X031"
=======
let exec_magic_number = "Caml1999X033"
>>>>>>> ups/501
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
<<<<<<< HEAD
and cmi_magic_number =
  (* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
  "Caml1999I514"
and cmo_magic_number = "Caml1999O514"
and cma_magic_number = "Caml1999A514"
||||||| b01e78e20
and cmi_magic_number = "Caml1999I031"
and cmo_magic_number = "Caml1999O031"
and cma_magic_number = "Caml1999A031"
=======
and cmi_magic_number = "Caml1999I033"
and cmo_magic_number = "Caml1999O033"
and cma_magic_number = "Caml1999A033"
>>>>>>> ups/501
and cmx_magic_number =
  if flambda then
<<<<<<< HEAD
    "Caml2021y516"
||||||| b01e78e20
    "Caml1999y031"
=======
    "Caml1999y033"
>>>>>>> ups/501
  else
<<<<<<< HEAD
    "Caml2021Y516"
||||||| b01e78e20
    "Caml1999Y031"
=======
    "Caml1999Y033"
>>>>>>> ups/501
and cmxa_magic_number =
  if flambda then
<<<<<<< HEAD
    "Caml2021z516"
||||||| b01e78e20
    "Caml1999z031"
=======
    "Caml1999z033"
>>>>>>> ups/501
  else
<<<<<<< HEAD
    "Caml2021Z516"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D515"
and cmt_magic_number = "Caml1999T514"
and cms_magic_number = "Caml1999S512"
||||||| b01e78e20
    "Caml1999Z031"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D031"
and cmt_magic_number = "Caml1999T031"
=======
    "Caml1999Z033"
and ast_impl_magic_number = "Caml1999M033"
and ast_intf_magic_number = "Caml1999N033"
and cmxs_magic_number = "Caml1999D033"
and cmt_magic_number = "Caml1999T033"
>>>>>>> ups/501

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let merlin = true
