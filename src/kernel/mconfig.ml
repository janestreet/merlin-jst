open Std

(** {1 OCaml commandline parsing} *)

let {Logger. log} = Logger.for_section "Mconfig"

type ocaml = {
  include_dirs         : string list;
  hidden_include_dirs  : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : bool;
  threads              : [ `None | `Threads | `Vmthreads ];
  recursive_types      : bool;
  strict_sequence      : bool;
  applicative_functors : bool;
  nopervasives         : bool;
  strict_formats       : bool;
  open_modules         : string list;
  ppx                  : string with_workdir list;
  pp                   : string with_workdir option;
  warnings             : Warnings.state;
  cmi_file             : string option;
  as_parameter         : bool;
}

let dump_warnings st =
  let st' = Warnings.backup () in
  Warnings.restore st;
  Misc.try_finally Warnings.dump
    ~always:(fun () -> Warnings.restore st')

let dump_ocaml x = `Assoc [
    "include_dirs"         , `List (List.map ~f:Json.string x.include_dirs);
    "hidden_include_dirs"  , `List (List.map ~f:Json.string x.hidden_include_dirs);
    "no_std_include"       , `Bool x.no_std_include;
    "unsafe"               , `Bool x.unsafe;
    "classic"              , `Bool x.classic;
    "principal"            , `Bool x.principal;
    "real_paths"           , `Bool x.real_paths;
    "recursive_types"      , `Bool x.recursive_types;
    "strict_sequence"      , `Bool x.strict_sequence;
    "applicative_functors" , `Bool x.applicative_functors;
    "nopervasives"         , `Bool x.nopervasives;
    "strict_formats"       , `Bool x.strict_formats;
    "open_modules"         , Json.list Json.string x.open_modules;
    "ppx"                  , Json.list (dump_with_workdir Json.string) x.ppx;
    "pp"                   , Json.option (dump_with_workdir Json.string) x.pp;
    "warnings"             , dump_warnings x.warnings;
    "cmi_file"             , Json.option Json.string x.cmi_file;
    "as_parameter"         , `Bool x.as_parameter;
  ]

(** Some paths can be resolved relative to a current working directory *)

let cwd = ref None

let unsafe_get_cwd () = match !cwd with
  | None -> assert false
  | Some cwd -> cwd

let canonicalize_filename path =
  Misc.canonicalize_filename ?cwd:!cwd path

let marg_path f =
  Marg.param "path" (fun path acc -> f (canonicalize_filename path) acc)

let marg_commandline f =
  Marg.param "command"
    (fun workval acc -> f {workdir = unsafe_get_cwd (); workval} acc)

(** {1 Merlin high-level settings} *)

type include_paths =
  { visible : string list;
    hidden : string list }

type merlin = {
  build_path  : include_paths;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  extensions  : string list;
  suffixes    : (string * string) list;
  stdlib      : string option;
  reader      : string list;
  protocol    : [`Json | `Sexp];
  log_file    : string option;
  log_sections : string list;
  config_path : string option;

  use_ppx_cache : bool;

  exclude_query_dir : bool;

  flags_to_apply : string list with_workdir list;

  flags_applied : string list with_workdir list;

  failures : string list;
  extension_to_reader : (string * string) list

}

let dump_merlin x =
  let dump_flag_list flags =
    dump_with_workdir (Json.list Json.string) flags
  in
  `Assoc [
    "build_path"   ,
      `Assoc [
        "visible", `List (List.map ~f:Json.string x.build_path.visible);
        "hidden", `List (List.map ~f:Json.string x.build_path.hidden)
      ];
    "source_path"  , `List (List.map ~f:Json.string x.source_path);
    "cmi_path"     , `List (List.map ~f:Json.string x.cmi_path);
    "cmt_path"     , `List (List.map ~f:Json.string x.cmt_path);
    "flags_applied", `List (List.map ~f:dump_flag_list x.flags_applied);
    "extensions"   , `List (List.map ~f:Json.string x.extensions);
    "suffixes"     , `List (
      List.map ~f:(fun (impl,intf) -> `Assoc [
          "impl", `String impl;
          "intf", `String intf;
        ]) x.suffixes
    );
    "stdlib"       , Json.option Json.string x.stdlib;
    "reader"       , `List (List.map ~f:Json.string x.reader);
    "protocol"     , (match x.protocol with
        | `Json -> `String "json"
        | `Sexp -> `String "sexp"
      );
    "log_file"     , Json.option Json.string x.log_file;
    "log_sections" , Json.list Json.string x.log_sections;
    "flags_to_apply"   , `List (List.map ~f:dump_flag_list x.flags_to_apply);

    "failures"         , `List (List.map ~f:Json.string x.failures);
    "assoc_suffixes"   , `List (
      List.map ~f:(fun (suffix,reader) -> `Assoc [
          "extension", `String suffix;
          "reader", `String reader;
        ]) x.extension_to_reader
    )
  ]

module Verbosity = struct
  type t = Smart | Lvl of int

  let default = Lvl 0

  let to_int t ~for_smart =
    match t with
    | Smart -> for_smart
    | Lvl v -> v

  let param_spec = "\"smart\" | <integer>"

  let of_string = function
    | "smart" -> Smart
    | maybe_int ->
      try Lvl (int_of_string maybe_int)
      with _ -> invalid_arg ("argument should be: " ^ param_spec)

  let to_string = function
    | Smart -> "smart"
    | Lvl v -> "lvl " ^ (string_of_int v)

  let to_json t = `String (to_string t)
end

type query = {
  filename  : string;
  directory : string;
  printer_width : int;
  verbosity : Verbosity.t;
}

let dump_query x = `Assoc [
    "filename"  , `String x.filename;
    "directory" , `String x.directory;
    "printer_width", `Int x.printer_width;
    "verbosity" , Verbosity.to_json x.verbosity;
  ]

type t = {
  ocaml   : ocaml;
  merlin  : merlin;
  query   : query;
}

let dump x = `Assoc [
    "ocaml"   , dump_ocaml x.ocaml;
    "merlin"  , dump_merlin x.merlin;
    "query"   , dump_query x.query;
  ]

let arguments_table = Hashtbl.create 67

let stdlib =
  let env =
    try Some (Sys.getenv "OCAMLLIB")
    with Not_found ->
    try Some (Sys.getenv "CAMLLIB")
    with Not_found -> None
  in
  fun config ->
    match config.merlin.stdlib with
    | Some stdlib -> stdlib
    | None -> match env with
      | Some stdlib -> stdlib
      | None -> Standard_library.path

let normalize_step t =
  let merlin = t.merlin in
  if merlin.flags_to_apply <> [] then
    let flagss = merlin.flags_to_apply in
    let t = {t with merlin = { merlin with
                               flags_to_apply = [];
                               flags_applied = flagss @ merlin.flags_applied;
                             } }
    in
    let failures = ref [] in
    let warning failure = failures := failure :: !failures in
    let t = List.fold_left ~f:(fun t {workdir; workval} -> fst (
        let_ref cwd (Some workdir)
          (Marg.parse_all ~warning arguments_table [] workval t)
      )) ~init:t flagss
    in
    {t with merlin = {t.merlin with failures = !failures @ t.merlin.failures}}
  else
    t

let is_normalized t =
  let merlin = t.merlin in
  merlin.flags_to_apply = []

let rec normalize t =
  if is_normalized t then (
    log ~title:"normalize" "%a" Logger.json (fun () -> dump t);
    t
  ) else
    normalize (normalize_step t)

let merge_build_paths
      (dot_bp : Mconfig_dot.include_paths) (merlin_bp : include_paths) =
  { visible = dot_bp.visible @ merlin_bp.visible;
    hidden = dot_bp.hidden @ merlin_bp.hidden }

let get_external_config path t =
  let path = Misc.canonicalize_filename path in
  let directory = Filename.dirname path in
  match Mconfig_dot.find_project_context directory with
  | None -> t
  | Some (ctxt, config_path) ->
    let dot, failures = Mconfig_dot.get_config ctxt path in
    let merlin = t.merlin in
    let merlin = {
      merlin with
      build_path = merge_build_paths dot.build_path merlin.build_path;
      source_path = dot.source_path @ merlin.source_path;
      cmi_path = dot.cmi_path @ merlin.cmi_path;
      cmt_path = dot.cmt_path @ merlin.cmt_path;
      exclude_query_dir = dot.exclude_query_dir || merlin.exclude_query_dir;
      use_ppx_cache = dot.use_ppx_cache || merlin.use_ppx_cache;
      extensions = dot.extensions @ merlin.extensions;
      suffixes = dot.suffixes @ merlin.suffixes;
      stdlib = (if dot.stdlib = None then merlin.stdlib else dot.stdlib);
      reader =
        if dot.reader = []
        then merlin.reader
        else dot.reader;
      flags_to_apply = dot.flags @ merlin.flags_to_apply;
      failures = failures @ merlin.failures;
      config_path = Some config_path;
    } in
    normalize { t with merlin }

let merlin_flags = [
  (
    "-build-path",
    marg_path (fun dir merlin ->
      let build_path =
        { merlin.build_path with visible = dir :: merlin.build_path.visible }
      in
        {merlin with build_path = build_path}),
    "<dir> Add <dir> to merlin build path"
  );
  (* CR ccasinghino: Do we want a similar flag for hidden includes? *)
  (
    "-source-path",
    marg_path (fun dir merlin ->
        {merlin with source_path = dir :: merlin.source_path}),
    "<dir> Add <dir> to merlin source path"
  );
  (
    "-cmi-path",
    marg_path (fun dir merlin ->
        {merlin with cmi_path = dir :: merlin.cmi_path}),
    "<dir> Add <dir> to merlin cmi path"
  );
  (
    "-cmt-path",
    marg_path (fun dir merlin ->
        {merlin with cmt_path = dir :: merlin.cmt_path}),
    "<dir> Add <dir> to merlin cmt path"
  );
  (
    "-reader",
    Marg.param "command" (fun reader merlin ->
        {merlin with reader = Shell.split_command reader }),
    "<command> Use <command> as a merlin reader"
  );
  (
    "-assocsuffix",
    Marg.param "suffix:reader"
      (fun assoc_pair merlin ->
         match Misc.rev_string_split ~on:':' assoc_pair with
         | [reader;suffix] ->
              {merlin with
               extension_to_reader = (suffix,reader)::merlin.extension_to_reader}
         | _ -> merlin
      ),
    "Associate suffix with reader"
  );
  (
    "-addsuffix",
    Marg.param "implementation Suffix, interface Suffix"
    (fun suffix_pair merlin ->
      match Misc.rev_string_split ~on:':' suffix_pair with
      | [intf;impl] ->
        {merlin with suffixes = (impl,intf)::merlin.suffixes}
      | _ -> merlin
    ),
    "Add a suffix implementation,interface pair"
  );
  (
    "-merlin-extension",
    Marg.param "extension" (fun extension merlin ->
        match Extension.lookup extension with
        | None -> invalid_arg "Unknown extension"
        | Some _ ->
          {merlin with extensions = extension :: merlin.extensions}),
    "<extension> Load merlin syntax extension"
  );
  (
    "-flags",
    Marg.param "string" (fun flags merlin ->
        let flags =
          { workdir = unsafe_get_cwd (); workval = Shell.split_command flags }
        in
        {merlin with flags_to_apply = flags :: merlin.flags_to_apply}),
    "<quoted flags> Unescape argument and interpret it as more flags"
  );
  (
    "-protocol",
    Marg.param "protocol" (fun prot merlin ->
        match prot with
        | "json" -> {merlin with protocol = `Json}
        | "sexp" -> {merlin with protocol = `Sexp}
        | _ -> invalid_arg "Valid protocols are 'json' and 'sexp'";
      ),
    "<protocol> Select frontend protocol ('json' or 'sexp')"
  );
  (
    "-log-file",
    Marg.param "file" (fun file merlin -> {merlin with log_file = Some file}),
    "<file> Log messages to specified file ('' for disabling, '-' for stderr)"
  );
  (
    "-log-section",
    Marg.param "file" (fun section merlin ->
        let sections = String.split_on_char_ ',' section in
        {merlin with log_sections = sections @ merlin.log_sections}),
    "<section,...> Only log specific sections (separated by comma)"
  );
  (
    "-ocamllib-path",
    marg_path (fun path merlin -> {merlin with stdlib = Some path}),
    "<path> Change path of ocaml standard library"
  );
  (
    (* Legacy support for janestreet. Ignored. To be removed soon. *)
    "-attributes-allowed",
    Marg.unit_ignore,
    " DEPRECATED"
  );
]

let query_flags = [
  (
    "-verbosity",
    Marg.param Verbosity.param_spec (fun verbosity query ->
        let verbosity =
          Verbosity.of_string verbosity
        in
        {query with verbosity}),
    "\"smart\" | <integer> Verbosity determines the number of \
      expansions of aliases in answers. \"smart\" is equivalent to \
      verbosity=0 but expands module types."
  );
  (
    "-printer-width",
    Marg.param "integer" (fun width query ->
        let printer_width =
          try int_of_string width
          with _ -> invalid_arg "argument should be an integer"
        in
        {query with printer_width}),
    "<integer> Optimal width for formatting types, signatures, etc"
  )
]

let ocaml_ignored_flags = [
  "-a"; "-absname"; "-alias-deps"; "-annot"; "-app-funct"; "-bin-annot";
  "-bin-annot-cms"; "-c"; "-compact"; "-compat-32"; "-config"; "-custom";
  "-dalloc"; "-dclambda"; "-dcmm"; "-dcombine"; "-dcse"; "-dflambda";
  "-dflambda-no-invariants"; "-dflambda-verbose"; "-dinstr"; "-dinterf";
  "-dlambda"; "-dlinear"; "-dlive"; "-dparsetree"; "-dprefer"; "-dshape";
  "-drawclambda"; "-drawflambda"; "-drawlambda"; "-dreload"; "-dscheduling";
  "-dsel"; "-dsource"; "-dspill"; "-dsplit"; "-dstartup"; "-dtimings";
  "-dtypedtree"; "-dtypes"; "-dump-pass"; "-fno-PIC"; "-fPIC"; "-g"; "-i";
  "-inlining-report"; "-keep-docs"; "-keep-docs"; "-keep-locs"; "-linkall";
  "-make_runtime"; "-make-runtime"; "-modern"; "-no-alias-deps"; "-noassert";
  "-noautolink"; "-no-check-prims"; "-nodynlink"; "-no-float-const-prop";
  "-no-keep-locs"; "-no-principal"; "-no-rectypes"; "-no-strict-formats";
  "-no-strict-sequence"; "-no-unbox-free-vars-of-clos";
  "-no-unbox-specialised-args"; "-O2"; "-O3"; "-Oclassic";
  "-only-erasable-extensions"; "-opaque";
  "-output-complete-obj"; "-output-obj"; "-p"; "-pack";
  "-remove-unused-arguments"; "-S"; "-shared"; "-unbox-closures"; "-v";
  "-verbose"; "-where";
  "-no-absname"; "-no-g"; "-safe-matching";

  (* flambda-backend specific *)
  "-basic-block-sections";
  "-caml-apply-inline-fast-path";
  "-debug-ocaml";
  "-dgc-timings";
  "-flambda2-backend-cse-at-toplevel";
  "-flambda2-debug";
  "-flambda2-debug-concrete-types-only-on-canonicals";
  "-flambda2-debug-keep-invalid-handlers";
  "-flambda2-debug-permute-every-name";
  "-flambda2-expert-can-inline-recursive-functions";
  "-flambda2-expert-code-id-and-symbol-scoping-checks";
  "-flambda2-expert-fallback-inlining-heuristic";
  "-flambda2-expert-inline-effects-in-cmm";
  "-flambda2-expert-phantom-lets";
  "-flambda2-inlining-report-bin";
  "-flambda2-join-points";
  "-flambda2-result-types";
  "-flambda2-result-types-all-functions";
  "-flambda2-result-types-functors-only";
  "-flambda2-speculative-inlining-only-if-arguments-useful";
  "-flambda2-unbox-along-intra-function-control-flow";
  "-flambda2-unicode";
  "-no-flambda2-backend-cse-at-toplevel";
  "-no-flambda2-debug";
  "-no-flambda2-debug-concrete-types-only-on-canonicals";
  "-no-flambda2-debug-keep-invalid-handlers";
  "-no-flambda2-debug-permute-every-name";
  "-no-flambda2-expert-can-inline-recursive-functions";
  "-no-flambda2-expert-code-id-and-symbol-scoping-checks";
  "-no-flambda2-expert-fallback-inlining-heuristic";
  "-no-flambda2-expert-inline-effects-in-cmm";
  "-no-flambda2-expert-phantom-lets";
  "-no-flambda2-join-points";
  "-no-flambda2-speculative-inlining-only-if-arguments-useful";
  "-no-flambda2-unbox-along-intra-function-control-flow";
  "-ocamlcfg";
  "-no-ocamlcfg";
  "-regalloc-validate";
  "-no-regalloc-validate";
  "-checkmach-details-cutoff";
  "-use-cached-generic-functions";
  "-flambda2-expert-shorten-symbol-names";
  "-no-flambda2-expert-shorten-symbol-names";

  (* Jane Street specific *)
  "-disable-poll-insertion";
  "-gdwarf-may-alter-codegen";
  "-gno-dwarf-may-alter-codegen";
  "-davail";
  "-dranges";
  "-ddebug-invariants";
  "-cfg-peephole-optimize";
  "-no-cfg-peephole-optimize";
  "-verbose-types";
  "-no-verbose-types";
  "-fsse3";
  "-fno-sse3";
  "-fssse3";
  "-fno-ssse3";
  "-fsse41";
  "-fno-sse41";
  "-fsse42";
  "-fno-sse42";
  "-fsimd-regalloc";
  "-fno-simd-regalloc";
  "-fclmul";
  "-fno-clmul";
  "-no-auto-include-otherlibs";
]

let ocaml_ignored_parametrized_flags = [
  "-cc"; "-cclib"; "-ccopt"; "-color"; "-dflambda-let"; "-dllib"; "-dllpath";
  "-for-pack"; "-impl"; "-inline-alloc-cost"; "-inline-branch-cost";
  "-inline-branch-factor"; "-inline-call-cost"; "-inline-indirect-cost";
  "-inline-lifting-benefit"; "-inline-max-depth"; "-inline-max-unroll";
  "-inline"; "-inline-prim-cost"; "-inline-toplevel"; "-intf";
  "-intf_suffix"; "-intf-suffix"; "-o"; "-rounds"; "-runtime-variant";
  "-unbox-closures-factor"; "-use-prims"; "-use_runtime"; "-use-runtime";
  "-error-style"; "-dump-dir" ;

  (* flambda-backend specific *)
  "-extension";
  "-drawfexpr-to";
  "-dfexpr-to";
  "-dflexpect-to";
  "-reorder-blocks-random";
  "-heap-reduction-threshold";
  "-flambda2-cse-depth";
  "-flambda2-expert-max-block-size-for-projections";
  "-flambda2-expert-max-unboxing-depth";
  "-flambda2-join-depth";
  "-flambda2-inline-alloc-cost";
  "-flambda2-inline-branch-cost";
  "-flambda2-inline-call-cost";
  "-flambda2-inline-indirect-cost";
  "-flambda2-inline-large-function-size";
  "-flambda2-inline-max-depth";
  "-flambda2-inline-max-rec-depth";
  "-flambda2-inline-poly-compare-cost";
  "-flambda2-inline-prim-cost";
  "-flambda2-inline-small-function-size";
  "-flambda2-inline-threshold";
  "-zero-alloc-check";
  "-regalloc";
  "-regalloc-param";
  "-cached-generic-functions-path";
  "-gdwarf-max-function-complexity";
]

let ocaml_warnings_spec ~error =
  Marg.param "warning specification" (fun spec ocaml ->
      let b' = Warnings.backup () in
      Warnings.restore ocaml.warnings;
      Misc.try_finally (fun () ->
          ignore @@ Warnings.parse_options error spec;
          { ocaml with warnings = Warnings.backup () })
        ~always:(fun () -> Warnings.restore b'))

let ocaml_alert_spec =
  Marg.param "alert specification" (fun spec ocaml ->
      let b' = Warnings.backup () in
      Warnings.restore ocaml.warnings;
      Misc.try_finally (fun () ->
          Warnings.parse_alert_option spec;
          { ocaml with warnings = Warnings.backup () })
        ~always:(fun () -> Warnings.restore b'))

let ocaml_flags = [
  (
    "-I",
    marg_path (fun dir ocaml ->
        {ocaml with include_dirs = dir :: ocaml.include_dirs}),
    "<dir> Add <dir> to the list of include directories"
  );
  (
    "-H",
    marg_path (fun dir ocaml ->
        {ocaml with hidden_include_dirs = dir :: ocaml.hidden_include_dirs}),
    "<dir> Add <dir> to the list of hidden include directories"
  );
  (
    "-nostdlib",
    Marg.unit (fun ocaml -> {ocaml with no_std_include = true}),
    " Do not add default directory to the list of include directories"
  );
  (
    "-unsafe",
    Marg.unit (fun ocaml -> {ocaml with unsafe = true}),
    " Do not compile bounds checking on array and string access"
  );
  (
    "-labels",
    Marg.unit (fun ocaml -> {ocaml with classic = false}),
    " Use commuting label mode"
  );
  (
    "-nolabels",
    Marg.unit (fun ocaml -> {ocaml with classic = true}),
    " Ignore non-optional labels in types"
  );
  (
    "-principal",
    Marg.unit (fun ocaml -> {ocaml with principal = true}),
    " Check principality of type inference"
  );
  (
    "-real-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = true}),
    " Display real paths in types rather than short ones"
  );
  (
    "-short-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = false}),
    " Shorten paths in types"
  );
  (
    "-rectypes",
    Marg.unit (fun ocaml -> {ocaml with recursive_types = true}),
    " Allow arbitrary recursive types"
  );
  (
    "-strict-sequence",
    Marg.unit (fun ocaml -> {ocaml with strict_sequence = true}),
    " Left-hand part of a sequence must have type unit"
  );
  (
    "-no-app-funct",
    Marg.unit (fun ocaml -> {ocaml with applicative_functors = false}),
    " Deactivate applicative functors"
  );
  (
    "-thread",
    Marg.unit (fun ocaml -> {ocaml with threads = `Threads}),
    " Add support for system threads library"
  );
  (
    "-vmthread",
    Marg.unit (fun ocaml -> {ocaml with threads = `None}),
    " Add support for VM-scheduled threads library"
  );
  (
    "-safe-string",
    Marg.unit (fun ocaml -> ocaml),
    " Default to true unconditionally since 5.00"
  );
  (
    "-nopervasives",
    Marg.unit (fun ocaml -> {ocaml with nopervasives = true}),
    " Don't open Pervasives module (advanced)"
  );
  (
    "-strict-formats",
    Marg.unit (fun ocaml -> {ocaml with strict_formats = true}),
    " Reject invalid formats accepted by legacy implementations"
  );
  (
    "-open",
    Marg.param "module" (fun md ocaml ->
        {ocaml with open_modules = md :: ocaml.open_modules}),
    "<module>  Opens the module <module> before typing"
  );
  (
    "-ppx",
    marg_commandline (fun command ocaml ->
        {ocaml with ppx = command :: ocaml.ppx}),
    "<command> Pipe abstract syntax trees through preprocessor <command>"
  );
  (
    "-pp",
    marg_commandline (fun pp ocaml -> {ocaml with pp = Some pp}),
    "<command> Pipe sources through preprocessor <command>"
  );
  ( "-w",
    ocaml_warnings_spec ~error:false,
    Printf.sprintf
      "<list>  Enable or disable warnings according to <list>:\n\
      \        +<spec>   enable warnings in <spec>\n\
      \        -<spec>   disable warnings in <spec>\n\
      \        @<spec>   enable warnings in <spec> and treat them as errors\n\
      \     <spec> can be:\n\
      \        <num>             a single warning number\n\
      \        <num1>..<num2>    a range of consecutive warning numbers\n\
      \        <letter>          a predefined set\n\
      \     default setting is %S"
      Warnings.defaults_w
  );
  ( "-warn-error",
    ocaml_warnings_spec ~error:true,
    Printf.sprintf
      "<list> Enable or disable error status for warnings according\n\
      \     to <list>.  See option -w for the syntax of <list>.\n\
      \     Default setting is %S"
      Warnings.defaults_warn_error
  );
  ( "-alert",
    ocaml_alert_spec,
    Printf.sprintf
      "<list>  Enable or disable alerts according to <list>:\n\
      \        +<alertname>  enable alert <alertname>\n\
      \        -<alertname>  disable alert <alertname>\n\
      \        ++<alertname> treat <alertname> as fatal error\n\
      \        --<alertname> treat <alertname> as non-fatal\n\
      \        @<alertname>  enable <alertname> and treat it as fatal error\n\
      \    <alertname> can be 'all' to refer to all alert names"
  );
  ( "-cmi-file",
    Marg.param "file" (fun cmi_file ocaml ->
        {ocaml with cmi_file = Some cmi_file}),
    "<file>  Use the <file> interface to type-check"
  );
  ( "-as-parameter",
    Marg.unit (fun ocaml -> {ocaml with as_parameter = true}),
    " Compiles the interface as a parameter for an open module."
  );
]

(** {1 Main configuration} *)

let initial = {
  ocaml = {
    include_dirs         = [];
    hidden_include_dirs  = [];
    no_std_include       = false;
    unsafe               = false;
    classic              = false;
    principal            = false;
    real_paths           = true;
    threads              = `None;
    recursive_types      = false;
    strict_sequence      = false;
    applicative_functors = true;
    nopervasives         = false;
    strict_formats       = false;
    open_modules         = [];
    ppx                  = [];
    pp                   = None;
    warnings             = Warnings.backup ();
    cmi_file             = None;
    as_parameter         = false;
  };
  merlin = {
    build_path  = {visible = []; hidden = []};
    source_path = [];
    cmi_path    = [];
    cmt_path    = [];
    extensions  = [];
    suffixes    = [(".ml", ".mli"); (".re", ".rei")];
    stdlib      = None;
    reader      = [];
    protocol    = `Json;
    log_file    = None;
    log_sections = [];
    config_path = None;

    exclude_query_dir = false;

    use_ppx_cache     = false;

    flags_to_apply    = [];
    flags_applied     = [];

    failures = [];
    extension_to_reader = [(".re","reason");(".rei","reason")];
  };
  query = {
    filename = "*buffer*";
    directory = Sys.getcwd ();
    verbosity = Verbosity.default;
    printer_width = 0;
  }
}

let parse_arguments ~wd ~warning local_spec args t local =
  let_ref cwd (Some wd) @@ fun () ->
  Marg.parse_all ~warning arguments_table local_spec args t local

let global_flags = [
  (
    "-filename",
    marg_path (fun path t ->
        let query = t.query in
        let path = Misc.canonicalize_filename path in
        let filename = Filename.basename path in
        let directory = Filename.dirname path in
        let t = {t with query = {query with filename; directory}} in
        Logger.with_log_file t.merlin.log_file
          ~sections:t.merlin.log_sections @@ fun () ->
        get_external_config path t),
    "<path> Path of the buffer; \
     extension determines the kind of file (interface or implementation), \
     basename is used as name of the module being definer, \
     directory is used to resolve other relative paths"
  );
  (
    "-dot-merlin",
    marg_path (fun dotmerlin t -> get_external_config dotmerlin t),
    "<path> Load <path> as a .merlin; if it is a directory, \
     look for .merlin here or in a parent directory"
  );
]

let () =
  List.iter ~f:(fun name -> Hashtbl.add arguments_table name Marg.unit_ignore)
    ocaml_ignored_flags;
  List.iter ~f:(fun name -> Hashtbl.add arguments_table name Marg.param_ignore)
    ocaml_ignored_parametrized_flags;
  let lens prj upd flag : _ Marg.t = fun args a ->
    let cwd' = match !cwd with
      | None when a.query.directory <> "" -> Some a.query.directory
      | cwd -> cwd
    in
    let_ref cwd cwd' @@ fun () ->
    let args, b = flag args (prj a) in
    args, (upd a b)
  in
  let add prj upd (name,flag,_doc) =
    assert (not (Hashtbl.mem arguments_table name));
    Hashtbl.add arguments_table name (lens prj upd flag)
  in
  List.iter
    ~f:(add (fun x -> x.ocaml) (fun x ocaml -> {x with ocaml}))
    ocaml_flags;
  List.iter
    ~f:(add (fun x -> x.merlin) (fun x merlin -> {x with merlin}))
    merlin_flags;
  List.iter
    ~f:(add (fun x -> x.query) (fun x query -> {x with query}))
    query_flags;
  List.iter
    ~f:(add (fun x -> x) (fun _ x -> x))
    global_flags

let flags_for_completion () =
  List.sort ~cmp:compare (
    "-dot-merlin" :: "-reader" ::
    List.map ~f:(fun (x,_,_) -> x) ocaml_flags
  )

let document_arguments oc =
  let print_doc flags =
    List.iter ~f:(fun (name,_flag,doc) -> Printf.fprintf oc "  %s\t%s\n" name doc)
      flags
  in
  output_string oc "Flags affecting Merlin:\n";
  print_doc merlin_flags;
  print_doc query_flags;
  output_string oc "Flags affecting OCaml frontend:\n";
  print_doc ocaml_flags;
  output_string oc "Flags accepted by ocamlc and ocamlopt but not affecting merlin will be ignored.\n"

let source_path config =
  let stdlib = if config.ocaml.no_std_include then [] else [stdlib config] in
  List.concat
    [[config.query.directory];
     stdlib;
     config.merlin.source_path]
  |> List.filter_dup

let build_path config = (
  let visible =
    match config.ocaml.threads with
    | `None -> config.ocaml.include_dirs
    | `Threads -> "+threads" :: config.ocaml.include_dirs
    | `Vmthreads -> "+vmthreads" :: config.ocaml.include_dirs
  in
  let visible =
    config.merlin.cmi_path @
    config.merlin.build_path.visible @
    visible
  in
  let hidden =
    config.merlin.build_path.hidden @
    config.ocaml.hidden_include_dirs
  in
  let stdlib = stdlib config in
  let visible =
    List.map ~f:(Misc.expand_directory stdlib) visible
  in
  let hidden =
    List.rev_map ~f:(Misc.expand_directory stdlib) hidden
  in
  let stdlib = if config.ocaml.no_std_include then [] else [stdlib] in
  let visible = List.rev_append visible stdlib in
  let visible =
    if config.merlin.exclude_query_dir
    then visible
    else config.query.directory :: visible
  in
  let visible' = List.filter_dup visible in
  let hidden' = List.filter_dup hidden in
  log ~title:"build_path" "%d items in path, %d after deduplication"
    (List.length visible) (List.length visible');
  log ~title:"hidden_build_path" "%d items in path, %d after deduplication"
    (List.length hidden) (List.length hidden');
  { visible = visible'; hidden = hidden' }
)

(* CR -H: What is `cmt_path` and does it need to consider hidden_includes?  *)
let cmt_path config = (
  let dirs =
    match config.ocaml.threads with
    | `None -> config.ocaml.include_dirs
    | `Threads -> "+threads" :: config.ocaml.include_dirs
    | `Vmthreads -> "+vmthreads" :: config.ocaml.include_dirs
  in
  let dirs =
    config.merlin.cmt_path @
    config.merlin.build_path.visible @
    dirs
  in
  let stdlib = stdlib config in
  let exp_dirs =
    List.map ~f:(Misc.expand_directory stdlib) dirs
  in
  let stdlib = if config.ocaml.no_std_include then [] else [stdlib] in
  config.query.directory :: List.rev_append exp_dirs stdlib
)

let global_modules ?(include_current=false) config = (
  (* CR -H: I took a look at a couple uses of `global_modules` and the `.visible` below
     seems fine, but I'm not confident *)
  let modules = Misc.modules_in_path ~ext:".cmi" (build_path config).visible in
  if include_current then modules
  else match config.query.filename with
    | "" -> modules
    | filename -> List.remove (Misc.unitname filename) modules
)

(** {1 Accessors for other information} *)

let filename t = t.query.filename

let unitname t = Misc.unitname t.query.filename
