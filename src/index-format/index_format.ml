exception Not_an_index of string

module Lid : Set.OrderedType with type t = Longident.t Location.loc = struct
  type t = Longident.t Location.loc

  let compare_pos (p1 : Lexing.position) (p2 : Lexing.position) =
    let p1f, p2f = Filename.(basename p1.pos_fname, basename p2.pos_fname) in
    match String.compare p1f p2f with
    | 0 -> Int.compare p1.pos_cnum p2.pos_cnum
    | n -> n

  let compare (t1 : t) (t2 : t) =
    match compare_pos t1.loc.loc_start t2.loc.loc_start with
    | 0 -> compare_pos t1.loc.loc_end t2.loc.loc_end
    | n -> n
end

module LidSet = Set.Make (Lid)
module Stats = Map.Make (String)

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this key is
    already present the locations are merged. *)
let add tbl uid locs =
  try
    let locations = Hashtbl.find tbl uid in
    Hashtbl.replace tbl uid (LidSet.union locs locations)
  with Not_found -> Hashtbl.add tbl uid locs

type stat = { mtime : float; size : int; source_digest: string option }
type index = {
  defs : (Shape.Uid.t, LidSet.t) Hashtbl.t;
  approximated : (Shape.Uid.t, LidSet.t) Hashtbl.t;
  load_path : Load_path.paths;
  cu_shape : (string, Shape.t) Hashtbl.t;
  stats : stat Stats.t;
}

let pp_partials (fmt : Format.formatter)
    (partials : (Shape.Uid.t, LidSet.t) Hashtbl.t) =
  Format.fprintf fmt "{@[";
  Hashtbl.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (LidSet.elements locs))
    partials;
  Format.fprintf fmt "@]}"

let pp (fmt : Format.formatter) pl =
  Format.fprintf fmt "%i uids:@ {@[" (Hashtbl.length pl.defs);
  Hashtbl.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (LidSet.elements locs))
    pl.defs;
  Format.fprintf fmt "@]},@ ";
  Format.fprintf fmt "%i approx shapes:@ @[%a@],@ "
    (Hashtbl.length pl.approximated)
    pp_partials pl.approximated;
  Format.fprintf fmt "and shapes for CUS %s.@ "
    (String.concat ";@," (Hashtbl.to_seq_keys pl.cu_shape |> List.of_seq))

let ext = "ocaml-index"

(* [magic_number] Must be the same lenght as cmt's magic numbers *)
let magic_number = "Merl2023I001"

let write ~file index =
  Misc.output_to_file_via_temporary ~mode:[ Open_binary ] file
    (fun _temp_file_name oc ->
      output_string oc magic_number;
      output_value oc (index : index))

type file_content = Cmt of Cmt_format.cmt_infos | Index of index | Unknown

let read ~file =
  let ic = open_in_bin file in
  Merlin_utils.Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
      let file_magic_number = ref (Cmt_format.read_magic_number ic) in
      let cmi_magic_number = Ocaml_utils.Config.cmi_magic_number in
      let cmt_magic_number = Ocaml_utils.Config.cmt_magic_number in
      (if String.equal !file_magic_number cmi_magic_number then
         let _ = Cmi_format.input_cmi ic in
         file_magic_number := Cmt_format.read_magic_number ic);
      if String.equal !file_magic_number cmt_magic_number then
        Cmt (input_value ic : Cmt_format.cmt_infos)
      else if String.equal !file_magic_number magic_number then
        Index (input_value ic : index)
      else Unknown)

let read_exn ~file =
  match read ~file with Index index -> index | _ -> raise (Not_an_index file)
