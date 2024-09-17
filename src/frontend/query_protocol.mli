module Compl :
  sig
    type 'desc raw_entry = {
      name : string;
      kind :
        [ `Constructor
        | `Keyword
        | `Label
        | `MethodCall
        | `Modtype
        | `Module
        | `Type
        | `Value
        | `Variant ];
      desc : 'desc;
      info : 'desc;
      deprecated : bool;
    }
    type entry = string raw_entry
    type application_context = {
      argument_type : string;
      labels : (string * string) list;
    }
    type t = {
      entries : entry list;
      context : [ `Application of application_context | `Unknown ];
    }
    type kind =
        [ `Constructor
        | `Keywords
        | `Labels
        | `Modules
        | `Modules_type
        | `Types
        | `Values
        | `Variants ]
  end
type completions = Compl.t
type outline = item list
and item = {
  outline_name : string;
  outline_kind :
    [ `Class
    | `Constructor
    | `Exn
    | `Label
    | `Method
    | `Modtype
    | `Module
    | `Type
    | `Value ];
  outline_type : string option;
  deprecated : bool;
  location : Location.t;
  children : outline;
}
type shape = { shape_loc : Location.t; shape_sub : shape list; }
type error_filter = { lexing : bool; parsing : bool; typing : bool; }
type syntax_doc_result = {
  name : string;
  description : string;
  documentation : string;
}
type is_tail_position = [ `No | `Tail_call | `Tail_position ]
type _ _bool = bool
type occurrences_status =
    [ `Included | `No_def | `Not_requested | `Out_of_sync of string list ]
module Locate_context :
  sig
    type t =
        Expr
      | Module_path
      | Module_type
      | Patt
      | Type
      | Constant
      | Constructor
      | Label
      | Unknown
    val to_string : t -> string
    val of_string : string -> t option
    val all : t list
  end
type _ t =
    Type_expr : string * Msource.position -> string t
  | Stack_or_heap_enclosing : Msource.position *
      int option -> (Location.t * [ `Index of int | `String of string ]) list
                    t
  | Type_enclosing : (string * int) option * Msource.position *
      int option -> (Location.t * [ `Index of int | `String of string ] *
                     is_tail_position)
                    list t
  | Enclosing : Msource.position -> Location.t list t
  | Complete_prefix : string * Msource.position * Compl.kind list * bool *
      bool -> completions t
  | Expand_prefix : string * Msource.position * Compl.kind list *
      bool -> completions t
  | Polarity_search : string * Msource.position -> completions t
  | Refactor_open : [ `Qualify | `Unqualify ] *
      Msource.position -> (string * Location.t) list t
  | Document : string option *
      Msource.position -> [ `Builtin of string
                          | `File_not_found of string
                          | `Found of string
                          | `Invalid_context
                          | `No_documentation
                          | `Not_found of string * string option
                          | `Not_in_env of string ] t
  | Syntax_document :
      Msource.position -> [ `Found of syntax_doc_result | `No_documentation ]
                          t
  | Locate_type :
      Msource.position -> [ `At_origin
                          | `Builtin of string
                          | `File_not_found of string
                          | `Found of string option * Lexing.position
                          | `Invalid_context
                          | `Not_found of string * string option
                          | `Not_in_env of string ] t
  | Locate : string option * [ `ML | `MLI ] * Msource.position *
      Locate_context.t option -> [ `At_origin
                                 | `Builtin of string
                                 | `File_not_found of string
                                 | `Found of string option * Lexing.position
                                 | `Invalid_context
                                 | `Not_found of string * string option
                                 | `Not_in_env of string ] t
  | Jump : string *
      Msource.position -> [ `Error of string | `Found of Lexing.position ] t
  | Phrase : [ `Next | `Prev ] * Msource.position -> Lexing.position t
  | Case_analysis : Msource.position *
      Msource.position -> (Location.t * string) t
  | Holes : (Location.t * string) list t
  | Construct : Msource.position * [ `Local | `None ] option *
      int option -> (Location.t * string list) t
  | Outline : outline t
  | Shape : Msource.position -> shape list t
  | Errors : error_filter -> Location.report list t
  | Dump : Std.json list -> Std.json t
  | Path_of_source : string list -> string t
  | List_modules : string list -> string list t
  | Findlib_list : string list t
  | Extension_list : [ `All | `Disabled | `Enabled ] -> string list t
  | Path_list : [ `Build | `Source ] -> string list t
  | Occurrences : [ `Ident_at of Msource.position ] *
      [ `Buffer | `Project ] -> (Location.t list * occurrences_status) t
  | Version : string t
