type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]

(** {0 OCaml compiler compatible command-line parameters}

    For compatibility with typechecker.
    Argument parsing / build environment construction happens elsewhere.
*)

(** {1 Relevant settings}
    Parameters from OCaml compiler which affect Merlin behavior. *)
val cmi_file             : string option ref
val include_dirs         : string list ref
val hidden_include_dirs  : string list ref
val fast                 : bool ref
val classic              : bool ref
val all_ppx              : string list ref
val principal            : bool ref
val real_paths           : bool ref
val recursive_types      : bool ref
val strict_sequence      : bool ref
val applicative_functors : bool ref
val nopervasives         : bool ref
val strict_formats       : bool ref
val open_modules         : string list ref
val parameters           : string list ref
val as_parameter         : bool ref
val as_argument_for      : string option ref
val zero_alloc_check : Zero_alloc_annotations.t ref
val zero_alloc_check_assert_all : bool ref
val allow_illegal_crossing : bool ref

(** {1 Dummy values}
    Ignored by merlin but kept for compatibility with upstream code. *)
val annotations          : bool ref
val binary_annotations   : bool ref
<<<<<<< HEAD
val binary_annotations_cms   : bool ref
val store_occurrences    : bool ref
||||||| fcc3157ab0
=======
val store_occurrences    : bool ref
>>>>>>> 501-plus-upstream-main-9fa77db
val print_types          : bool ref
val native_code          : bool ref
val dont_write_files     : bool ref
val error_size           : int ref (* max size of module related errors *)
val keep_locs            : bool ref
val keep_docs            : bool ref
val transparent_modules  : bool ref
val for_package          : string option ref
val debug                : bool ref
val opaque               : bool ref
val unboxed_types        : bool ref
val profile_columns : profile_column list ref

val locations            : bool ref
val all_ppx              : string list ref
