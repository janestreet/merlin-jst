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

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
open Parsetree
open Format
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
open Parsetree;;
open Format;;

val interface : formatter -> signature_item list -> unit;;
val implementation : formatter -> structure_item list -> unit;;
val top_phrase : formatter -> toplevel_phrase -> unit;;
val constant: formatter -> constant -> unit;;
=======
open Parsetree
open Format

val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit
val constant: formatter -> constant -> unit
>>>>>>> ocaml-flambda/flambda-backend:main

val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit
val constant: formatter -> constant -> unit

val expression: int -> formatter -> expression -> unit
val pattern: int -> formatter -> pattern -> unit
val structure: int -> formatter -> structure -> unit
val payload: int -> formatter -> payload -> unit
val core_type: int -> formatter -> core_type -> unit
val extension_constructor: int -> formatter -> extension_constructor -> unit

val tyvar: Format.formatter -> string -> unit
  (** Print a type variable name, taking care of the special treatment
      required for the single quote character in second position. *)
