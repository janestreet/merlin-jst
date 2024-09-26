(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

module Artifact : sig
  type t

  val read : string -> t

  val impl_shape : t -> Shape.t option
end

val log : 'a Logger.printf

type config = {
  mconfig: Mconfig.t;
  ml_or_mli: [ `ML | `MLI ];
  traverse_aliases: bool;
}

type result = {
  uid: Shape.Uid.t;
  decl_uid: Shape.Uid.t;
  file: string;
  location: Location.t;
  approximated: bool;
}

module Namespace_resolution : sig
  type t = 
    | From_context of Query_protocol.Locate_context.t
    (** Choose the namespaces based on a [Query_protocol.Locate_context.t] *)

    | Explicit of Env_lookup.Namespace.inferred_basic list
    (** Explicitly choose which namespaces to search in. The namespaces are prioritized
        based on the list order (with the first element being highest priority) *)

    | Inferred
    (** Infer which namespaces to search in *)
end

val uid_of_result
  : traverse_aliases:bool
  -> Shape_reduce.result
  -> Shape.Uid.t option * bool

val find_source
  : config: Mconfig.t
  -> Warnings.loc
  -> string
  -> [> `File_not_found of string
      | `Found of string * Location.t ]

val from_path
  : config:config
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> namespace:Env_lookup.Namespace.t
  -> Path.t
  -> [> `File_not_found of string
     | `Found of result
     | `Builtin of Shape.Uid.t * string
     | `Not_in_env of string
     | `Not_found of string * string option ]

val from_string
  :  config:config
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> pos:Lexing.position
  -> ?let_pun_behavior:Mbrowse.Let_pun_behavior.t
  -> ?namespaces:Namespace_resolution.t
  -> string
  -> [> `File_not_found of string
      | `Found of result
      | `Builtin of Shape.Uid.t * string
      | `Missing_labels_namespace
      | `Not_found of string * string option
      | `Not_in_env of string
      | `At_origin ]

val get_doc
  :  config:Mconfig.t
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> comments:(string * Location.t) list
  -> pos:Lexing.position
  -> [ `User_input of string
      | `Completion_entry of Env_lookup.Namespace.t * Path.t * Location.t ]
  -> [> `File_not_found of string
      | `Found of string
      | `Builtin of string
      | `Not_found of string * string option
      | `Not_in_env of string
      | `No_documentation ]
