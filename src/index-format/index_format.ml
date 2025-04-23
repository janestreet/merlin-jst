exception Not_an_index of string

module Lid = Lid
module Lid_set = Granular_set.Make (Lid)
module Uid_map = Granular_map.Make (Shape.Uid)
module Stats = Map.Make (String)

let add map uid locs =
  Uid_map.update uid
    (function
      | None -> Some locs
      | Some locs' -> Some (Lid_set.union locs' locs))
    map

type stat = { mtime : float; size : int; source_digest : string option }

type index =
  { defs : Lid_set.t Uid_map.t;
    approximated : Lid_set.t Uid_map.t;
    cu_shape : (Compilation_unit.t, Shape.t) Hashtbl.t;
    stats : stat Stats.t;
    root_directory : string option
  }

let lidset_schema iter lidset = Lid_set.schema iter Lid.schema lidset

let type_setmap : Lid_set.t Uid_map.t Type.Id.t = Type.Id.make ()

let index_schema (iter : Granular_marshal.iter) index =
  Uid_map.schema type_setmap iter
    (fun iter _ v -> lidset_schema iter v)
    index.defs;
  Uid_map.schema type_setmap iter
    (fun iter _ v -> lidset_schema iter v)
    index.approximated

let compress index =
  let cache = Lid.cache () in
  let compress_map_set =
    Uid_map.iter (fun _ -> Lid_set.iter (Lid.deduplicate cache))
  in
  compress_map_set index.defs;
  compress_map_set index.approximated;
 index

let pp_lidset fmt locs =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
    Lid.pp fmt (Lid_set.elements locs)

let pp_partials (fmt : Format.formatter) (partials : Lid_set.t Uid_map.t) =
  Format.fprintf fmt "{@[";
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid pp_lidset locs)
    partials;
  Format.fprintf fmt "@]}"

let pp (fmt : Format.formatter) pl =
  Format.fprintf fmt "%i uids:@ {@[" (Uid_map.cardinal pl.defs);
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid pp_lidset locs)
    pl.defs;
  Format.fprintf fmt "@]},@ ";
  Format.fprintf fmt "%i approx shapes:@ @[%a@],@ "
    (Uid_map.cardinal pl.approximated)
    pp_partials pl.approximated;
  Format.fprintf fmt "and shapes for CUS %s.@ "
    (String.concat ";@,"
       (Hashtbl.to_seq_keys pl.cu_shape
       |> List.of_seq
       |> List.map Compilation_unit.full_path_as_string))

let ext = "ocaml-index"

let magic_number = Config.index_magic_number

let write ~file index =
  let index = compress index in
  Misc.output_to_file_via_temporary ~mode:[ Open_binary ] file
    (fun _temp_file_name oc ->
      output_string oc magic_number;
      Granular_marshal.write oc index_schema (index : index))

type file_content =
  | Cmt of Cmt_format.cmt_infos
  | Cms of Cms_format.cms_infos
  | Index of index
  | Unknown

let read ~file =
  let ic = open_in_bin file in
  Merlin_utils.Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
      let file_magic_number = ref (Cmt_format.read_magic_number ic) in
      let cmi_magic_number = Ocaml_utils.Config.cmi_magic_number in
      let cmt_magic_number = Ocaml_utils.Config.cmt_magic_number in
      let cms_magic_number = Ocaml_utils.Config.cms_magic_number in
      (if String.equal !file_magic_number cmi_magic_number then
         let _ = Cmi_format.input_cmi ic in
         file_magic_number := Cms_format.read_magic_number ic);
      if String.equal !file_magic_number cmt_magic_number then
        Cmt (input_value ic : Cmt_format.cmt_infos)
      else if String.equal !file_magic_number cms_magic_number then
        Cms (input_value ic : Cms_format.cms_infos)
      else if String.equal !file_magic_number magic_number then
        Index (Granular_marshal.read file ic index_schema)
      else Unknown)

let read_exn ~file =
  match read ~file with
  | Index index -> index
  | _ -> raise (Not_an_index file)
