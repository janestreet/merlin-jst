type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]

(** {0 OCaml compiler compatible command-line parameters} *)
let cmi_file = ref None
let include_dirs        = ref []
let hidden_include_dirs = ref []
let fast                = ref false
let classic             = ref false
let all_ppx             = ref []
let principal           = ref false
let real_paths          = ref true
let recursive_types     = ref false
let strict_sequence     = ref false
let applicative_functors = ref true

let nopervasives        = ref false
let strict_formats      = ref true
let open_modules        = ref []
let parameters          = ref ([] : string list)
let as_parameter        = ref false
let as_argument_for     = ref None
let zero_alloc_check    = ref Zero_alloc_annotations.Check_default
let zero_alloc_check_assert_all = ref false
let infer_with_bounds   = ref false

let annotations         = ref false
let binary_annotations  = ref true
let binary_annotations_cms  = ref false
let store_occurrences   = ref true
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
let profile_columns     = ref []

let locations = ref true
