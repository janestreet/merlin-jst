(** {0 OCaml compiler compatible command-line parameters} *)
let cmi_file = ref None
let include_dirs        = ref []
let hidden_include_dirs = ref []
let fast                = ref false
let classic             = ref false
let principal           = ref false
let real_paths          = ref true
let recursive_types     = ref false
let strict_sequence     = ref false
let applicative_functors = ref true

let nopervasives        = ref false
let strict_formats      = ref true
let open_modules        = ref []
let as_parameter        = ref false

module Annotations = struct
  type t = Check_default | Check_all | Check_opt_only | No_check

  let all = [ Check_default; Check_all; Check_opt_only; No_check ]

  let to_string = function
    | Check_default -> "default"
    | Check_all -> "all"
    | Check_opt_only -> "opt"
    | No_check -> "none"

  let equal t1 t2 =
    match t1, t2 with
    | Check_default, Check_default -> true
    | Check_all, Check_all -> true
    | No_check, No_check -> true
    | Check_opt_only, Check_opt_only -> true
    | (Check_default | Check_all | Check_opt_only | No_check), _ -> false

  let of_string v =
    let f t =
      if String.equal (to_string t) v then Some t else None
    in
    List.find_map f all

  let doc =
    "\n\    The argument specifies which annotations to check: \n\
     \      \"opt\" means attributes with \"opt\" payload and is intended for debugging;\n\
     \      \"default\" means attributes without \"opt\" payload; \n\
     \      \"all\" covers both \"opt\" and \"default\" and is intended for optimized builds."
end

let zero_alloc_check = ref Annotations.Check_default

let annotations         = ref false
let binary_annotations  = ref true
let binary_annotations_cms  = ref false
let print_types         = ref false
let native_code         = ref false
let error_size          = ref 500
let dont_write_files    = ref true
let keep_locs           = ref true
let keep_docs           = ref false
let transparent_modules = ref true
let for_package         = ref None
let debug               = ref false
let opaque              = ref false
let unboxed_types       = ref false

let locations = ref true
let all_ppx = ref []
