module Kind = Shape.Sig_component_kind
open Index_format

let with_root ?root file =
  match root with
  | None -> file
  | Some root -> Filename.concat root file

let add_root ~root (lid : Longident.t Location.loc) =
  match root with
  | None -> lid
  | Some root ->
    let pos_fname = Filename.concat root lid.loc.loc_start.pos_fname in
    { lid with
      loc =
        { lid.loc with
          loc_start = { lid.loc.loc_start with pos_fname };
          loc_end = { lid.loc.loc_end with pos_fname }
        }
    }

let merge m m' =
  Shape.Uid.Map.union
    (fun _uid locs locs' -> Some (Lid_set.union locs locs'))
    m m'

(** Cmt files contains a table of declarations' Uids associated to a typedtree
    fragment. [add_locs_from_fragments] gather locations from these *)
let gather_locs_from_fragments ~root ~rewrite_root map fragments =
  let to_located_lid (name : string Location.loc) =
    { name with txt = Longident.Lident name.txt }
  in
  let add_loc uid fragment acc =
    match fragment with
    | None -> acc
    | Some lid ->
      let lid = to_located_lid lid in
      let lid = if rewrite_root then add_root ~root lid else lid in
      Shape.Uid.Map.add uid (Lid_set.singleton lid) acc
  in
  Shape.Uid.Tbl.fold add_loc fragments map

module Reduce_conf (Loaded_shapes : sig
  val shapes : (Compilation_unit.t, Shape.t) Hashtbl.t
end) =
struct
  let fuel = 10

  let try_load ~unit_name () =
    match
      Hashtbl.find_opt Loaded_shapes.shapes
        (Compilation_unit.of_string unit_name)
    with
    | Some shape ->
      Log.debug "Used loaded shape for %s" unit_name;
      Some shape
    | None -> begin
      let artifact =
        let cms = Format.sprintf "%s.cms" unit_name in
        match Locate.Artifact.read (Load_path.find_normalized cms) with
        | artifact ->
          Log.debug "Loaded CMS %s" cms;
          Some artifact
        | exception Not_found -> (
          let cmt = Format.sprintf "%s.cmt" unit_name in
          match Locate.Artifact.read (Load_path.find_normalized cmt) with
          | artifact ->
            Log.debug "Loaded CMT %s" cmt;
            Some artifact
          | exception Not_found ->
            Log.warn "Failed to load file %S in load_path: @[%s@]\n%!" cmt
            @@ String.concat "; " (Load_path.get_path_list ());
            None)
      in
      match artifact with
      | None -> None
      | Some artifact -> Merlin_analysis.Locate.Artifact.impl_shape artifact
    end

  let read_unit_shape ~unit_name =
    Log.debug "Read unit shape: %s\n%!" unit_name;
    try_load ~unit_name ()
end

let init_load_path_once ~do_not_use_cmt_loadpath =
  let loaded = ref false in
  fun ~(dirs : Load_path.paths) cmt_loadpath ->
    if not !loaded then (
      let cmt_visible, cmt_hidden =
        if do_not_use_cmt_loadpath then ([], [])
        else (cmt_loadpath.Load_path.visible, cmt_loadpath.Load_path.hidden)
      in
      let visible = List.concat [ cmt_visible; dirs.visible ] in
      let hidden = List.concat [ cmt_hidden; dirs.hidden ] in
      Load_path.(init ~auto_include:no_auto_include ~visible ~hidden);
      loaded := true)

let index_of_artifact ~root ~rewrite_root ~build_path ~do_not_use_cmt_loadpath
    ~shapes ~cmt_loadpath ~cmt_impl_shape ~cmt_modname ~uid_to_loc
    ~cmt_ident_occurrences ~cmt_initial_env ~cmt_sourcefile ~cmt_source_digest =
  init_load_path_once ~do_not_use_cmt_loadpath ~dirs:build_path cmt_loadpath;
  let module Reduce = Shape_reduce.Make (Reduce_conf (struct
    let shapes = shapes
  end)) in
  let defs =
    if Option.is_none cmt_impl_shape then Shape.Uid.Map.empty
    else
      gather_locs_from_fragments ~root ~rewrite_root Shape.Uid.Map.empty
        uid_to_loc
  in
  (* The list [cmt_ident_occurrences] associate each ident usage location in the
     module with its (partially) reduced shape. We finish the reduction and
     group together all the locations that share the same definition uid. *)
  let defs, approximated =
    Array.fold_left
      (fun ((acc_defs, acc_apx) as acc) (lid, (item : Shape_reduce.result)) ->
        let lid = if rewrite_root then add_root ~root lid else lid in
        let resolved =
          match item with
          | Unresolved shape -> Reduce.reduce_for_uid cmt_initial_env shape
          | Resolved _ when Option.is_none cmt_impl_shape ->
            (* Right now, without additional information we cannot take the
               risk to mix uids from interfaces with the ones from
               implementations. We simply ignore items defined in an interface. *)
            Internal_error_missing_uid
          | result -> result
        in
        match Locate.uid_of_result ~traverse_aliases:false resolved with
        | Some uid, false -> (add acc_defs uid (Lid_set.singleton lid), acc_apx)
        | Some uid, true -> (acc_defs, add acc_apx uid (Lid_set.singleton lid))
        | None, _ -> acc)
      (defs, Shape.Uid.Map.empty)
      cmt_ident_occurrences
  in
  let cu_shape = Hashtbl.create 1 in
  Option.iter (Hashtbl.add cu_shape cmt_modname) cmt_impl_shape;
  let stats =
    match cmt_sourcefile with
    | None -> Stats.empty
    | Some src -> (
      let rooted_src = with_root ?root src in
      try
        let stats = Unix.stat rooted_src in
        let src = if rewrite_root then rooted_src else src in
        Stats.singleton src
          { mtime = stats.st_mtime;
            size = stats.st_size;
            source_digest = cmt_source_digest
          }
      with Unix.Unix_error _ -> Stats.empty)
  in
  { defs; approximated; cu_shape; stats; root_directory = None }

let shape_of_artifact ~impl_shape ~modname =
  let cu_shape = Hashtbl.create 1 in
  Option.iter (Hashtbl.add cu_shape modname) impl_shape;
  { defs = Shape.Uid.Map.empty;
    approximated = Shape.Uid.Map.empty;
    cu_shape;
    stats = Stats.empty;
    root_directory = None
  }

let shape_of_cmt { Cmt_format.cmt_impl_shape; cmt_modname; _ } =
  shape_of_artifact ~impl_shape:cmt_impl_shape ~modname:cmt_modname

let shape_of_cms { Cms_format.cms_impl_shape; cms_modname; _ } =
  shape_of_artifact ~impl_shape:cms_impl_shape ~modname:cms_modname

let index_of_cmt ~root ~build_path ~shapes cmt_infos =
  let { Cmt_format.cmt_loadpath;
        cmt_impl_shape;
        cmt_modname;
        cmt_uid_to_decl;
        cmt_ident_occurrences;
        cmt_initial_env;
        cmt_sourcefile;
        cmt_source_digest;
        _
      } =
    cmt_infos
  in
  let uid_to_loc =
    Shape.Uid.Tbl.to_list cmt_uid_to_decl
    |> List.map (fun (uid, fragment) ->
           (uid, Typedtree_utils.location_of_declaration ~uid fragment))
    |> Shape.Uid.Tbl.of_list
  in
  index_of_artifact ~root ~build_path ~shapes ~cmt_loadpath ~cmt_impl_shape
    ~cmt_modname ~uid_to_loc ~cmt_ident_occurrences ~cmt_initial_env
    ~cmt_sourcefile ~cmt_source_digest

let index_of_cms ~root ~build_path ~shapes cms_infos =
  let { Cms_format.cms_impl_shape;
        cms_modname;
        cms_uid_to_loc;
        cms_ident_occurrences;
        cms_sourcefile;
        cms_source_digest;
        cms_initial_env;
        _
      } =
    cms_infos
  in
  let uid_to_loc =
    Shape.Uid.Tbl.to_list cms_uid_to_loc
    |> List.map (fun (uid, l) -> (uid, Some l))
    |> Shape.Uid.Tbl.of_list
  in
  index_of_artifact ~root ~build_path ~shapes
    ~cmt_loadpath:{ visible = []; hidden = [] }
    ~cmt_impl_shape:cms_impl_shape ~cmt_modname:cms_modname ~uid_to_loc
    ~cmt_ident_occurrences:cms_ident_occurrences
    ~cmt_initial_env:(Option.value cms_initial_env ~default:Env.empty)
    ~cmt_sourcefile:cms_sourcefile ~cmt_source_digest:cms_source_digest

let merge_index ~store_shapes ~into index =
  let defs = merge index.defs into.defs in
  let approximated = merge index.approximated into.approximated in
  let stats = Stats.union (fun _ f1 _f2 -> Some f1) into.stats index.stats in
  if store_shapes then
    Hashtbl.add_seq into.cu_shape (Hashtbl.to_seq index.cu_shape);
  { into with defs; approximated; stats }

let from_files ~store_shapes ~output_file ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath files =
  Log.debug "Debug log is enabled";
  let initial_index =
    { defs = Shape.Uid.Map.empty;
      approximated = Shape.Uid.Map.empty;
      cu_shape = Hashtbl.create 64;
      stats = Stats.empty;
      root_directory = root
    }
  in
  let final_index =
    Ocaml_utils.Local_store.with_store (Ocaml_utils.Local_store.fresh ())
    @@ fun () ->
    List.fold_left
      (fun into file ->
        Log.debug "Indexing from file: %s" file;
        let index =
          match Cms_cache.read file with
          | cms_item ->
            index_of_cms ~root ~rewrite_root ~build_path
              ~do_not_use_cmt_loadpath ~shapes:into.cu_shape cms_item.cms_infos
          | exception _ -> (
            match Cmt_cache.read file with
            | cmt_item ->
              index_of_cmt ~root ~rewrite_root ~build_path
                ~do_not_use_cmt_loadpath ~shapes:into.cu_shape
                cmt_item.cmt_infos
            | exception _ -> (
              match read ~file with
              | Index index -> index
              | _ ->
                Log.error "Unknown file type: %s" file;
                exit 1))
        in
        (* We add the shapes into `into` because we need to collect them so we can use
           them for shape reduction, regardless of whether store_shapes is true *)
        merge_index ~store_shapes:true index ~into)
      initial_index files
  in
  let final_index =
    (* Don't save the collected shapes if store_shapes is false *)
    if store_shapes then final_index
    else { final_index with cu_shape = Hashtbl.create 0 }
  in
  write ~file:output_file final_index

let gather_shapes ~output_file files =
  let initial_index =
    { defs = Shape.Uid.Map.empty;
      approximated = Shape.Uid.Map.empty;
      cu_shape = Hashtbl.create 64;
      stats = Stats.empty;
      root_directory = None
    }
  in
  let final_index =
    List.fold_left
      (fun into file ->
        let index =
          match Cache.read file with
          | Cmt cmt_infos -> Some (shape_of_cmt cmt_infos)
          | Cms cmt_infos -> Some (shape_of_cms cmt_infos)
          | Index index -> Some index
          | Unknown | (exception _) ->
            Log.error "Not a valid file %S" file;
            None
        in
        match index with
        | None -> into
        | Some index -> merge_index ~store_shapes:true index ~into)
      initial_index files
  in
  write ~file:output_file final_index
