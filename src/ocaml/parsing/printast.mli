(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Raw printer for {!Parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Parsetree
open Format

<<<<<<< HEAD
val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit
val constant: formatter -> constant -> unit
||||||| 7b73c6aa3
val interface : formatter -> signature_item list -> unit;;
val implementation : formatter -> structure_item list -> unit;;
val top_phrase : formatter -> toplevel_phrase -> unit;;
=======
val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit
>>>>>>> upstream/main

val expression: int -> formatter -> expression -> unit
val pattern: int -> formatter -> pattern -> unit
val structure: int -> formatter -> structure -> unit
val payload: int -> formatter -> payload -> unit
val core_type: int -> formatter -> core_type -> unit
val extension_constructor: int -> formatter -> extension_constructor -> unit

