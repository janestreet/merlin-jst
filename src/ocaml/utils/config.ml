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

(* merlin-jst: All magic numbers ending in `500` (or higher) should not change when we
   upgrade the upstream Merlin version.  Otherwise, this should track
   `utils/config.mlp` from the compiler.  Also, the above warning comment about
   `config.mlbuild` is outdated and can be ignored. *)

(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

(* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
let cmi_magic_number = "Caml1999I551"

let ast_impl_magic_number = "Caml1999M551"
let ast_intf_magic_number = "Caml1999N551"
let cmt_magic_number = "Caml1999T551"
let cms_magic_number = "Caml1999S551"
let index_magic_number = "Merl2023I501"

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let reserved_header_bits = 8
let runtime5 = true

let merlin = true
