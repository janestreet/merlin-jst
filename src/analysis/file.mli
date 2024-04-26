type t = private
  | ML   of string
  | MLL  of string
  | MLI  of string
  | CMT  of string
  | CMTI of string
  | CMS of string
  | CMSI of string

val ml : string -> t
val mli : string -> t
val cmt : string -> t
val cmti : string -> t
val cms : string -> t
val cmsi : string -> t

val of_filename : string -> t option

val alternate : t -> t

val name : t -> string

val to_legacy : t -> t option

val with_ext : ?src_suffix_pair:(string * string) -> t -> string

val explain_not_found :
  ?doc_from:string -> string -> t -> [> `File_not_found of string ]
