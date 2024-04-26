open Std

type t =
  | ML   of string
  | MLL  of string
  | MLI  of string
  | CMT  of string
  | CMTI of string
  | CMS  of string
  | CMSI of string

let file_path_to_mod_name f =
  Misc.unitname (Filename.basename f)

let ml   s = ML   (file_path_to_mod_name s)
let mll  s = MLL  (file_path_to_mod_name s)
let mli  s = MLI  (file_path_to_mod_name s)
let cmt  s = CMT  (file_path_to_mod_name s)
let cmti s = CMTI (file_path_to_mod_name s)
let cms  s = CMS  (file_path_to_mod_name s)
let cmsi s = CMSI (file_path_to_mod_name s)

let of_filename fn =
  match Misc.rev_string_split ~on:'.' fn with
  | []
  | [ _ ] -> None
  | ext :: _ ->
    let ext = String.lowercase ext in
    Some (
      match ext with
      | "cmti" -> cmti fn
      | "cmt"  -> cmt fn
      | "mll"  -> mll fn
      | "cms" -> cms fn
      | "cmsi" -> cmsi fn
      | _ -> if Filename.check_suffix ext "i" then mli fn else ml fn
    )

let alternate = function
  | ML  s
  | MLL s -> MLI s
  | MLI s -> ML s
  | CMT s  -> CMTI s
  | CMTI s -> CMT s
  | CMS s -> CMSI s
  | CMSI s -> CMS s

let to_legacy = function
  | ML _ | MLI _ | MLL _ | CMT _ | CMTI _ -> None
  | CMS s -> Some (CMT s)
  | CMSI s -> Some (CMTI s)

let name = function
  | ML name
  | MLL name
  | MLI name
  | CMT name
  | CMTI name
  | CMS name
  | CMSI name -> name

let ext src_suffix_pair = function
  | ML _  -> fst src_suffix_pair
  | MLI _  -> snd src_suffix_pair
  | MLL _ -> ".mll"
  | CMT _ -> ".cmt"
  | CMTI _ -> ".cmti"
  | CMS _ -> ".cms"
  | CMSI _ -> ".cmsi"

let with_ext ?(src_suffix_pair=(".ml",".mli")) t =
  name t ^ ext src_suffix_pair t

let explain_not_found ?(doc_from="") str_ident path =
  let msg =
    match path with
    | ML file ->
      sprintf "'%s' seems to originate from '%s' whose ML file could not be \
               found" str_ident file
    | MLL file ->
      sprintf "'%s' seems to originate from '%s' whose MLL file could not be \
               found" str_ident file
    | MLI file ->
      sprintf "'%s' seems to originate from '%s' whose MLI file could not be \
               found" str_ident file
    | CMT file ->
      sprintf "Needed cmt file of module '%s' to locate '%s' but it is not \
               present" file str_ident
    | CMTI file when file <> doc_from ->
      sprintf "Needed cmti file of module '%s' to locate '%s' but it is not \
               present" file str_ident
    | CMTI _ ->
      sprintf "The documentation for '%s' originates in the current file, \
               but no cmt is available" str_ident
    | CMS file ->
      sprintf "Needed cms file of module '%s' to locate '%s' but it is not \
               present" file str_ident
    | CMSI file when file <> doc_from ->
      sprintf "Needed cmsi file of module '%s' to locate '%s' but it is not \
               present" file str_ident
    | CMSI _ ->
      sprintf "The documentation for '%s' originates in the current file, \
               but no cms is available" str_ident

  in
  `File_not_found msg

