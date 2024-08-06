open Std
module LidSet = Index_format.LidSet

let {Logger. log} = Logger.for_section "occurrences"

type res = { locs: Warnings.loc list; synced: bool }

let set_fname ~file (loc : Location.t) =
  let pos_fname = file in
  { loc with
      loc_start = { loc.loc_start with pos_fname };
      loc_end   = { loc.loc_end with pos_fname }}

let decl_of_path_or_lid env namespace path lid =
  match (namespace : Shape.Sig_component_kind.t) with
  | Constructor ->
    begin match Env.find_constructor_by_name lid env with
    | exception Not_found -> None
    | {cstr_uid; cstr_loc; _ } ->
      Some { Env_lookup.uid = cstr_uid; loc = cstr_loc; namespace }
    end
  | Label ->
    begin match Env.find_label_by_name lid env with
    | exception Not_found -> None
    | {lbl_uid; lbl_loc; _ } ->
      Some { Env_lookup.uid = lbl_uid; loc = lbl_loc; namespace }
    end
  | _ -> Env_lookup.loc path namespace env

let index_buffer_ ~scope ~current_buffer_path ~local_defs () =
  let {Logger. log} = Logger.for_section "index" in
  let defs = Hashtbl.create 64 in
  let module Shape_reduce =
    Shape_reduce.Make (struct
      let fuel = 10

      let read_unit_shape ~unit_name =
          log ~title:"read_unit_shape" "inspecting %s" unit_name;
          let read unit_name =
            let cms = Format.sprintf "%s.cms" unit_name in
            match Locate.Artifact.read (Load_path.find_uncap cms) with
            | artifact -> Some artifact
            | exception _ ->
              let cmt = Format.sprintf "%s.cmt" unit_name in
              match Locate.Artifact.read (Load_path.find_uncap cmt) with
              | artifact -> Some artifact
              | exception _ ->
                None
          in
          match read unit_name with
          | Some artifact ->
            log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
            Locate.Artifact.impl_shape artifact
          | None ->
            log ~title:"read_unit_shape" "failed to find %s" unit_name;
            None
    end)
  in
  let f ~namespace env path (lid : Longident.t Location.loc)  =
    log ~title:"index_buffer" "Path: %a" Logger.fmt (Fun.flip Path.print path);
    let not_ghost { Location.loc = { loc_ghost; _ }; _ } = not loc_ghost in
    let lid = { lid with loc = set_fname ~file:current_buffer_path lid.loc } in
    let index_decl () =
      begin match decl_of_path_or_lid env namespace path lid.txt with
      | exception _ |  None -> log ~title:"index_buffer" "Declaration not found"
      | Some decl ->
        log ~title:"index_buffer" "Found declaration: %a"
          Logger.fmt (Fun.flip Location.print_loc decl.loc);
        Index_format.(add defs decl.uid (LidSet.singleton lid))
      end
    in
     if not_ghost lid then
      match Env.shape_of_path ~namespace env path with
      | exception Not_found -> ()
      | path_shape ->
        log ~title:"index_buffer" "Shape of path: %a"
          Logger.fmt (Fun.flip Shape.print path_shape);
        let result =  Shape_reduce.reduce_for_uid env path_shape in
        begin match Locate.uid_of_result ~traverse_aliases:false result with
        | Some uid, false ->
          log ~title:"index_buffer" "Found %s (%a) wiht uid %a"
            (Longident.head lid.txt)
            Logger.fmt (Fun.flip Location.print_loc lid.loc)
            Logger.fmt (Fun.flip Shape.Uid.print uid);
            Index_format.(add defs uid (LidSet.singleton lid))
          | Some uid, true ->
            log ~title:"index_buffer" "Shape is approximative, found uid: %a"
              Logger.fmt (Fun.flip Shape.Uid.print uid);
            index_decl ()
          | None, _ ->
            log ~title:"index_buffer" "Reduction failed: missing uid";
            index_decl ()
        end
  in
  let f ~namespace env path (lid : Longident.t Location.loc)  =
    (* The compiler lacks sufficient location information to precisely hihglight
       modules in paths. This function hacks around that issue when looking for
       occurrences in the current buffer only. *)
    let rec iter_on_path ~namespace path ({Location.txt; loc} as lid) =
      let () = f ~namespace env path lid in
      if scope = `Buffer then
      match path, txt with
      | Pdot (path, _), Ldot (lid, s) ->
        let length_with_dot = String.length s + 1 in
        let lid =
          { Location.txt = lid; loc = { loc with loc_end = {loc.loc_end with
            pos_cnum = loc.loc_end.pos_cnum - length_with_dot}} }
        in
        iter_on_path ~namespace:Module path lid
      | Papply _, _ -> ()
      | _, _ -> ()
    in
    iter_on_path ~namespace path lid
  in
  Ast_iterators.iter_on_usages ~f local_defs;
  defs


let index_buffer =
  (* Right now, we only cache the last used index. We could do better by caching
      the index for every known buffer. *)
  let cache = ref None in
  fun ~scope ~current_buffer_path ~stamp ~local_defs () ->
    let {Logger. log} = Logger.for_section "index" in
    match !cache with
    | Some (path, stamp', scope', value) when
        String.equal path current_buffer_path
        && Int.equal stamp' stamp
        && scope' = scope ->
      log ~title:"index_cache" "Reusing cached value for path %s and stamp %i."
        path stamp';
      value
    | _ ->
      log ~title:"index_cache" "No valid cache found, reindexing.";
      let result = index_buffer_ ~current_buffer_path ~scope ~local_defs () in
      cache := Some (current_buffer_path, stamp, scope, result);
      result

(* A longident can have the form: A.B.x Right now we are only interested in
   values, but we will eventually want to index all occurrences of modules in
   such longidents. However there is an issue with that: we only have the
   location of the complete longident which might span multiple lines. This is
   enough to get the last component since it will always be on the last line,
   but will prevent us to find the location of previous components. *)
let last_loc (loc : Location.t) lid =
  if lid = Longident.Lident "*unknown*" then loc
  else
    let last_size = Longident.last lid |> String.length in
    { loc with
      loc_start = { loc.loc_end with
        pos_cnum = loc.loc_end.pos_cnum - last_size;
      }
    }

let uid_and_loc_of_node env node =
  let open Browse_raw in
  log ~title:"occurrences" "Looking for uid of node %s"
    @@ string_of_node node;
  match node with
  | Module_binding_name { mb_id = Some ident; mb_name; _ } ->
    let md = Env.find_module (Pident ident) env in
    Some (md.md_uid, mb_name.loc)
  | Pattern { pat_desc =
      Tpat_var (_, name, uid, _) | Tpat_alias (_, _, name, uid, _); _ } ->
      Some (uid, name.loc)
  | Type_declaration { typ_type; typ_name; _ } ->
      Some (typ_type.type_uid, typ_name.loc)
  | Label_declaration { ld_uid; ld_loc ; _ } ->
      Some (ld_uid, ld_loc)
  | Constructor_declaration { cd_uid; cd_loc ; _ } ->
      Some (cd_uid, cd_loc)
  | Value_description { val_val; val_name; _ } ->
      Some (val_val.val_uid, val_name.loc)
  | _ -> None

let comp_unit_of_uid = function
  | Shape.Uid.Compilation_unit comp_unit
  | Item { comp_unit; _ } -> Some comp_unit
  | Internal | Predef _ -> None

let check Index_format.{ stats; _ } file =
  let open Index_format in
  match Stats.find_opt file stats with
  | None -> log ~title:"stat_check" "No mtime found for file %S." file; true
  | Some { size; _ } ->
    try
      let stats = Unix.stat file in
      let equal =
        (* This is fast but approximative. A better option would be to check
            [mtime] and then [source_digest] if the times differ. *)
          Int.equal stats.st_size size
      in
      log ~title:"stat_check"
        "File %s has been modified since the index was built." file;
      equal
    with Unix.Unix_error _ -> false

let locs_of ~config ~env ~typer_result ~pos ~scope path =
  log ~title:"occurrences" "Looking for occurences of %s (pos: %s)"
    path
    (Lexing.print_position () pos);
  let local_defs = Mtyper.get_typedtree typer_result in
  let locate_result =
    Locate.from_string
    ~config:{ mconfig = config; traverse_aliases=false; ml_or_mli = `ML}
    ~env ~local_defs ~pos path
  in
  (* When we fail to find an exact definition we restrict scope to `Buffer *)
  let def, scope =
    match locate_result with
    | `At_origin ->
      log ~title:"locs_of" "Cursor is on definition / declaration";
      (* We are on  a definition / declaration so we look for the node's uid  *)
      let browse = Mbrowse.of_typedtree local_defs in
      let env, node = Mbrowse.leaf_node (Mbrowse.enclosing pos [browse]) in
      uid_and_loc_of_node env node, scope
    | `Found { uid; location; approximated = false; _ } ->
        log ~title:"locs_of" "Found definition uid using locate: %a "
          Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
        Some (uid, location), scope
    | `Found { decl_uid; location; approximated = true; _ } ->
        log ~title:"locs_of" "Approx. definition: %a "
          Logger.fmt (fun fmt -> Shape.Uid.print fmt decl_uid);
        Some (decl_uid, location), `Buffer
    | `Builtin (uid, s) ->
        log ~title:"locs_of" "Locate found a builtin: %s" s;
        Some (uid, Location.none), scope
    | _ ->
        log ~title:"locs_of" "Locate failed to find a definition.";
        None, `Buffer
  in
  let current_buffer_path =
    Filename.concat config.query.directory config.query.filename
  in
  match def with
  | Some (def_uid, def_loc) ->
    log ~title:"locs_of" "Definition has uid %a (%a)"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt def_uid)
      Logger.fmt (fun fmt -> Location.print_loc fmt def_loc);
    log ~title:"locs_of" "Indexing current buffer";
    let buffer_index =
      let stamp = Mtyper.get_stamp typer_result in
      index_buffer ~scope ~current_buffer_path ~stamp ~local_defs ()
    in
    let buffer_locs = Hashtbl.find_opt buffer_index def_uid in
    let external_locs, desync =
      if scope = `Buffer then [], false else begin
      let exception File_changed in
      try
        let locs = List.filter_map config.merlin.index_files ~f:(fun file ->
          let external_index = Index_cache.read file in
          Hashtbl.find_opt external_index.defs def_uid
          |> Option.map ~f:(fun locs -> LidSet.filter (fun {loc; _} ->
            (* We ignore external results that concern the current buffer *)
            let fname = loc.Location.loc_start.Lexing.pos_fname in
            if String.equal fname current_buffer_path then false
            else begin
              (* We ignore external results if the index is not up-to-date *)
              (* We could return partial results from up-to-date file *)
              if not (check external_index fname) then begin
                log ~title:"locs_of" "File %s might be out-of-sync." fname;
                raise File_changed
              end;
              true
            end) locs))
          in
          locs, false
      with File_changed -> [], true
      end
    in
    if desync then log ~title:"locs_of" "External index might be out-of-sync.";
    let locs =
      let all_locs =
        match buffer_locs with
        | Some buffer_locs -> buffer_locs :: external_locs
        | None -> external_locs
      in
      List.fold_left ~init:LidSet.empty ~f:LidSet.union all_locs
    in
    let locs =
      log ~title:"occurrences" "Found %i locs" (LidSet.cardinal locs);
      LidSet.elements locs
      |> List.filter_map ~f:(fun {Location.txt; loc} ->
        log ~title:"occurrences" "Found occ: %s %a"
          (Longident.head txt) Logger.fmt (Fun.flip Location.print_loc loc);
        let loc = last_loc loc txt in
        let fname = loc.Location.loc_start.Lexing.pos_fname in
        if Filename.is_relative fname then begin
          match Locate.find_source ~config loc fname with
          | `Found (file, _) -> Some (set_fname ~file loc)
          | `File_not_found msg ->
              log ~title:"occurrences" "%s" msg;
              None
        end else Some loc)
      in
    let def_uid_is_in_current_unit =
      let uid_comp_unit = comp_unit_of_uid def_uid in
      Option.value_map ~default:false uid_comp_unit
        ~f:(Misc_utils.is_current_unit)
    in
    let synced = not desync in
    if not def_uid_is_in_current_unit then Ok { locs; synced }
    else
      let locs = set_fname ~file:current_buffer_path def_loc :: locs in
      Ok { locs;  synced }
  | None -> Error "Could not find the definition [uid]"
