(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

let last_location = ref Location.none

let {Logger. log} = Logger.for_section "locate"

type config = {
  mconfig: Mconfig.t;
  ml_or_mli: [ `ML | `MLI ];
  traverse_aliases: bool;
}

type result = {
  uid: Shape.Uid.t;
  decl_uid: Shape.Uid.t;
  file: string;
  location: Location.t;
  approximated: bool;
}

module File : sig
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
  val cms : string -> t
  val cmsi : string -> t

  val of_filename : string -> t option

  val alternate : t -> t

  val name : t -> string

  val to_legacy : t -> t option

  val with_ext : ?src_suffix_pair:(string * string) -> t -> string

  val explain_not_found :
    ?doc_from:string -> string -> t -> [> `File_not_found of string ]
end = struct
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
end

module Artifact : sig
  type t
  val builddir : t -> string
  val sourcefile : t -> string option
  val source_digest : t -> string option
  val comments : t -> (string * Location.t) list
  val impl_shape : t -> Shape.t option
  val uid_to_loc : Shape.Uid.t -> t -> Location.t option

  (** When we look for docstring in external compilation unit we can perform
      a uid-based search and return the attached comment in the attributes.
      This is a more sound way to get documentation than resorting on the
      [Ocamldoc.associate_comment] heuristic *)
  val uid_to_attributes : Shape.Uid.t -> t -> Parsetree.attributes option

  val read : string -> t
end = struct
  type t = Cmt of Cmt_format.cmt_infos
         | Cms of Cms_format.cms_infos

  let builddir = function
    | Cmt cmt_infos -> cmt_infos.cmt_builddir
    | Cms cms_infos -> cms_infos.cms_builddir
  let sourcefile = function
    | Cmt cmt_infos -> cmt_infos.cmt_sourcefile
    | Cms cms_infos -> cms_infos.cms_sourcefile
  let source_digest = function
    | Cmt cmt_infos -> cmt_infos.cmt_source_digest
    | Cms cms_infos -> cms_infos.cms_source_digest
  let comments = function
    | Cmt cmt_infos -> cmt_infos.cmt_comments
    | Cms cms_infos -> cms_infos.cms_comments
  let impl_shape = function
    | Cmt cmt_infos -> cmt_infos.cmt_impl_shape
    | Cms cms_infos -> cms_infos.cms_impl_shape

  let uid_to_loc uid = function
    | Cmt cmt_infos ->
      Shape.Uid.Tbl.find_opt cmt_infos.cmt_uid_to_decl uid
      |> Option.bind ~f:(Misc_utils.loc_of_decl ~uid)
      |> Option.map ~f:(fun { Location.loc; _ } -> loc)
    | Cms cms_infos -> Shape.Uid.Tbl.find_opt cms_infos.cms_uid_to_loc uid

  let uid_to_attributes uid = function
    | Cms cms_infos -> Shape.Uid.Tbl.find_opt cms_infos.cms_uid_to_attributes uid
    | Cmt cmt_infos ->
    let exception Found of Typedtree.attributes in
    let test elt_uid attributes =
      if Shape.Uid.equal uid elt_uid then raise (Found attributes)
    in
    let iterator =
      let first_item = ref true in
      let uid_is_comp_unit = match uid with
        | Shape.Uid.Compilation_unit _ -> true
        | _ -> false
      in
      fun env -> { Tast_iterator.default_iterator with

        (* Needed to return top-level module doc (when the uid is a compunit).
          The module docstring must be the first signature or structure item *)
        signature_item = (fun sub ({ sig_desc; _} as si) ->
          begin match sig_desc, !first_item, uid_is_comp_unit with
          | Tsig_attribute attr, true, true -> raise (Found [attr])
          | _, false, true -> raise Not_found
          | _, _, _ -> first_item := false end;
          Tast_iterator.default_iterator.signature_item sub si);

        structure_item = (fun sub ({ str_desc; _} as sti) ->
          begin match str_desc, !first_item, uid_is_comp_unit with
          | Tstr_attribute attr, true, true -> raise (Found [attr])
          | _, false, true -> raise Not_found
          | _, _, _ -> first_item := false end;
          Tast_iterator.default_iterator.structure_item sub sti);

        value_description = (fun sub ({ val_val; val_attributes; _ } as vd) ->
          test val_val.val_uid val_attributes;
          Tast_iterator.default_iterator.value_description sub vd);

        type_declaration = (fun sub ({ typ_type; typ_attributes; _ } as td) ->
          test typ_type.type_uid typ_attributes;
          Tast_iterator.default_iterator.type_declaration sub td);

        value_binding = (fun sub ({ vb_pat; vb_attributes; _ } as vb) ->
          begin match vb_pat.pat_desc with
          | Tpat_var (id, _, _, _) ->
              begin try
                let vd = Env.find_value (Pident id) env in
                test vd.val_uid vb_attributes
              with Not_found -> () end
          | _ -> () end;
          Tast_iterator.default_iterator.value_binding sub vb)
      }
    in
    let typedtree =
      match cmt_infos.Cmt_format.cmt_annots with
      | Interface s -> Some (`Interface { s with
          sig_final_env = Envaux.env_of_only_summary s.sig_final_env})
      | Implementation str -> Some (`Implementation { str with
          str_final_env = Envaux.env_of_only_summary str.str_final_env})
      | _ -> None
    in
    try begin match typedtree with
      | Some (`Interface s) ->
          let iterator = iterator s.sig_final_env in
          iterator.signature iterator s;
          log ~title:"doc_from_uid" "uid not found in the signature"
      | Some (`Implementation str) ->
          let iterator = iterator str.str_final_env in
          iterator.structure iterator str;
          log ~title:"doc_from_uid" "uid not found in the implementation"
      | _ -> () end;
      None
    with
      | Found attrs -> Some attrs
      | Not_found -> None

  let read file =
    match File.of_filename file with
    | Some (CMT _ | CMTI _) -> Cmt (Cmt_cache.read file).cmt_infos
    | Some (CMS _ | CMSI _) -> Cms (Cms_cache.read file).cms_infos
    | Some (ML _ | MLL _ | MLI _) | None -> assert false
end

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val src : string -> File.t
  val build : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  let src   file = if !prioritize_impl then File.ml  file else File.mli  file
  let build file = if !prioritize_impl then File.cms file else File.cmsi file

  let is_preferred fn =
    match File.of_filename fn with
    | Some ML _ -> !prioritize_impl
    | Some MLI _ -> not !prioritize_impl
    | _ -> false
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : digest:Digest.t -> string -> unit

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string;
    digest : Digest.t;
  }

  let last_file_visited t = t.last_file_visited
  let digest t = t.digest

  let state = ref None

  let reset () = state := None

  let move_to ~digest file =
    log ~title:"File_switching.move_to" "file: %s\ndigest: %s" file
    @@ Digest.to_hex digest;

    state := Some { last_file_visited = file ; digest }

  let where_am_i () = Option.map !state ~f:last_file_visited

  let source_digest () = Option.map !state ~f:digest
end


module Utils = struct
  (* Reuse the code of [Misc.find_in_path_uncap] but returns all the files
     matching, instead of the first one. This is only used when looking for ml
     files, not cmts. Indeed for cmts we know that the load path will only ever
     contain files with uniq names; this in not the case for the "source path"
     however. We therefore get all matching files and use an heuristic at the
     call site to choose the appropriate file.

     Note: We do not refine the load path for module path as we used too. *)
  let find_all_in_path_uncap ?src_suffix_pair ~with_fallback path file =
    let name = File.with_ext ?src_suffix_pair file in
    log ~title:"find_all_in_path_uncap" "Looking for file %S in path:\n%a" name
      Logger.fmt (fun fmt -> Format.pp_print_list Format.pp_print_string fmt path);
    let uname = String.uncapitalize name in
    let fallback, ufallback =
      let alt = File.alternate file in
      let fallback = File.with_ext ?src_suffix_pair alt in
      fallback, String.uncapitalize fallback
    in
    let try_file dirname basename acc =
      if Misc.exact_file_exists ~dirname ~basename
      then Misc.canonicalize_filename (Filename.concat dirname basename) :: acc
      else acc
    in
    let try_dir acc dirname =
      let acc = try_file dirname uname acc in
      let acc = try_file dirname name acc in
      let acc =
        if with_fallback then
          let acc = try_file dirname ufallback acc in
          let acc = try_file dirname fallback acc in
          acc
        else
          acc
      in
      acc
    in
    List.fold_left ~f:try_dir ~init:[] path

  let find_all_matches ~config ?(with_fallback=false) file =
    let files =
      List.concat_map ~f:(fun synonym_pair ->
        find_all_in_path_uncap ~src_suffix_pair:synonym_pair ~with_fallback
          (Mconfig.source_path config @ Mconfig.hidden_source_path config) file
      ) Mconfig.(config.merlin.suffixes)
    in
    List.dedup_adjacent files ~cmp:String.compare

  let find_file_with_path ~config ?(with_fallback=false) file path =
    if File.name file = Misc.unitname Mconfig.(config.query.filename) then
      Some Mconfig.(config.query.filename)
    else
      let attempt_search src_suffix_pair =
        let try_one file =
          let fallback =
            if with_fallback then
              Some (File.with_ext ~src_suffix_pair (File.alternate file))
            else
              None
          in
          let fname = File.with_ext ~src_suffix_pair file in
          try Some (Misc.find_in_path_normalized ?fallback path fname)
          with Not_found ->  None
        in
<<<<<<< HEAD
        match try_one file with
        | Some _ as f -> f
        | None -> Option.bind ~f:try_one (File.to_legacy file)
||||||| fcc3157ab0
        let fname = File.with_ext ~src_suffix_pair file in
        try Some (Misc.find_in_path_uncap ?fallback path fname)
        with Not_found -> None
=======
        let fname = File.with_ext ~src_suffix_pair file in
        try Some (Misc.find_in_path_normalized ?fallback path fname)
        with Not_found -> None
>>>>>>> 501-plus-upstream-main-9fa77db
      in
      try
        Some (List.find_map Mconfig.(config.merlin.suffixes) ~f:attempt_search)
      with Not_found ->
        None

  let find_file ~config ?with_fallback (file : File.t) =
    find_file_with_path ~config ?with_fallback file @@
        match file with
<<<<<<< HEAD
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config @ Mconfig.hidden_source_path config
        | CMT _ | CMTI _         -> Mconfig.build_path config @ Mconfig.hidden_build_path config
        | CMS _ | CMSI _         -> Mconfig.build_path config @ Mconfig.hidden_build_path config
||||||| fcc3157ab0
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config
        | CMT _ | CMTI _         -> Mconfig.build_path config
=======
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config
        | CMT _ | CMTI _         -> Mconfig.cmt_path config
>>>>>>> 501-plus-upstream-main-9fa77db
end

let move_to filename artifact =
  let digest =
    (* [None] only for packs, and we wouldn't have a trie if the cmt was for a
       pack. *)
    let sourcefile_in_builddir =
      Filename.concat
        (Artifact.builddir artifact)
        (Option.get (Artifact.sourcefile artifact))
    in
    match sourcefile_in_builddir |> String.split_on_char ~sep:'.' |> List.rev with
    | ext :: "pp" :: rev_path ->
      (* If the source file was a post-processed file (.pp.mli?), use the
         regular .mli? file for locate. *)
      let sourcefile_in_builddir =
        (ext :: rev_path) |> List.rev |> String.concat ~sep:"."
      in
      (match
         Misc.exact_file_exists
           ~dirname:(Filename.dirname sourcefile_in_builddir)
           ~basename:(Filename.basename sourcefile_in_builddir)
       with
       | true -> Digest.file sourcefile_in_builddir
       | false -> Option.get (Artifact.source_digest artifact))
    | _ -> Option.get (Artifact.source_digest artifact)
  in
  File_switching.move_to ~digest filename

<<<<<<< HEAD

let load_cmt ~config ?with_fallback:(_ = true) comp_unit =
  Preferences.set config.ml_or_mli;
  let file = Preferences.build comp_unit in
  match Utils.find_file ~config:config.mconfig ~with_fallback:true file with
||||||| fcc3157ab0

let load_cmt ~config comp_unit ml_or_mli =
  Preferences.set ml_or_mli;
  let file =
    Preferences.build comp_unit
  in
  match Utils.find_file ~config ~with_fallback:true file with
=======
let load_cmt ~config ?(with_fallback = true) comp_unit =
  Preferences.set config.ml_or_mli;
  let file =
    Preferences.build comp_unit
  in
  match Utils.find_file ~config:config.mconfig ~with_fallback file with
>>>>>>> 501-plus-upstream-main-9fa77db
  | Some path ->
    let artifact = Artifact.read path in
    let source_file = Artifact.sourcefile artifact in
    let source_file = Option.value ~default:"*pack*" source_file in
    move_to path artifact;
    Ok (source_file, artifact)
  | None -> Error ()

let scrape_alias ~env ~fallback_uid ~namespace path =
  let find_type_and_uid ~env ~namespace path =
    match namespace with
    | Shape.Sig_component_kind.Module ->
      let { Types.md_type; md_uid; _ } = Env.find_module path env in
      md_type, md_uid
    | Module_type ->
      begin match Env.find_modtype path env with
      | { Types.mtd_type = Some mtd_type; mtd_uid; _ } ->
        mtd_type, mtd_uid
      | _ -> raise Not_found
    end
    | _ -> raise Not_found
  in
  let rec non_alias_declaration_uid ~fallback_uid path =
    match find_type_and_uid ~env ~namespace path with
    | Mty_alias path, fallback_uid ->
        non_alias_declaration_uid ~fallback_uid path
    | Mty_ident alias_path, fallback_uid
      when namespace = Shape.Sig_component_kind.Module_type ->
        (* This case is necessary to traverse module type aliases *)
        non_alias_declaration_uid ~fallback_uid alias_path
    | _, md_uid -> md_uid
    | exception Not_found -> fallback_uid
  in
  non_alias_declaration_uid ~fallback_uid path

<<<<<<< HEAD
(* merlin-jst: Supports the addition of [Compilation_unit], replacing
   comparisons of [Env.get_unit_name ()] with [comp_unit]; we don't use
   [Option.equal] because [Std.Option] doesn't have it so the patch would be
   less local in order to add a module alias before [open Std]. *)

||||||| fcc3157ab0
let uid_of_path ~config ~env ~ml_or_mli ~decl_uid path namespace =
  let module Shape_reduce =
    Shape.Make_reduce (struct
      type env = Env.t

      let fuel = 10

      let read_unit_shape ~unit_name =
          log ~title:"read_unit_shape" "inspecting %s" unit_name;
          match load_cmt ~config unit_name `ML with
          | Ok (filename, cmt_infos) ->
            move_to filename cmt_infos;
            log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
            cmt_infos.cmt_impl_shape
          | Error () ->
            log ~title:"read_unit_shape" "failed to find %s" unit_name;
            None

      let find_shape env id = Env.shape_of_path
        ~namespace:Shape.Sig_component_kind.Module env (Pident id)
    end)
  in
  match ml_or_mli with
  | `MLI ->
    let uid = scrape_alias ~fallback_uid:decl_uid ~env ~namespace path in
    log ~title:"uid_of_path" "Declaration uid: %a"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt decl_uid);
    log ~title:"uid_of_path" "Alias scrapped: %a"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    Some uid
  | `ML ->
    let shape = Env.shape_of_path ~namespace env path in
    log ~title:"shape_of_path" "initial: %a"
      Logger.fmt (fun fmt -> Shape.print fmt shape);
    let r = Shape_reduce.weak_reduce env shape in
    log ~title:"shape_of_path" "reduced: %a"
      Logger.fmt (fun fmt -> Shape.print fmt r);
    r.uid

let from_uid ~config ~ml_or_mli uid loc path =
  let loc_of_comp_unit comp_unit =
    match load_cmt ~config comp_unit ml_or_mli with
    | Ok (pos_fname, _cmt) ->
      let pos = Std.Lexing.make_pos ~pos_fname (1, 0) in
      let loc = { Location.loc_start=pos; loc_end=pos; loc_ghost=true } in
      Some loc
    | _ -> None
  in
  let title = "from_uid" in
  match uid with
  | Some (Shape.Uid.Item { comp_unit; _ } as uid) ->
    let locopt =
      if Env.get_unit_name () = comp_unit then begin
        log ~title "We look for %a in the current compilation unit."
          Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
        let tbl = Env.get_uid_to_loc_tbl () in
        match Shape.Uid.Tbl.find_opt tbl uid with
        | Some loc ->
          log ~title "Found location: %a"
            Logger.fmt (fun fmt -> Location.print_loc fmt loc);
          Some (uid, loc)
        | None ->
          log ~title
            "Uid not found in the local table.\
            Fallbacking to the node's location: %a"
          Logger.fmt (fun fmt -> Location.print_loc fmt loc);
          Some (uid, loc)
      end else begin
        log ~title "Loading the shapes for unit %S" comp_unit;
        match load_cmt ~config comp_unit ml_or_mli with
        | Ok (_pos_fname, cmt) ->
          log ~title "Shapes successfully loaded, looking for %a"
            Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
          begin match Shape.Uid.Tbl.find_opt cmt.cmt_uid_to_loc uid with
            | Some loc ->
              log ~title "Found location: %a"
                Logger.fmt (fun fmt -> Location.print_loc fmt loc);
              Some (uid, loc)
            | None ->
              log ~title "Uid not found in the cmt table. \
                Fallbacking to the node's location: %a"
                Logger.fmt (fun fmt -> Location.print_loc fmt loc);
              Some (uid, loc)
          end
        | _ ->
          log ~title "Failed to load the shapes";
          None
      end
    in
    begin match locopt with
    | Some (uid, loc) -> `Found (Some uid, loc)
    | None -> `Not_found (Path.name path, None)
    end
  | Some (Compilation_unit comp_unit as uid) ->
    begin
      log ~title "Got the uid of a compilation unit: %a"
        Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
      match loc_of_comp_unit comp_unit with
      | Some loc -> `Found (Some uid, loc)
      | _ -> log ~title "Failed to load the shapes";
        `Not_found (Path.name path, None)
    end
  | Some (Predef _ | Internal) -> assert false
  | None -> log ~title "No UID found, fallbacking to lookup location.";
      `Found (None, loc)

let locate ~config ~env ~ml_or_mli decl_uid loc path ns =
  let uid = uid_of_path ~config ~env ~ml_or_mli ~decl_uid path ns in
  from_uid ~config ~ml_or_mli uid loc path

let path_and_loc_of_cstr desc _ =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, _) -> path, desc.cstr_loc
  | _ ->
    match get_desc desc.cstr_res with
    | Tconstr (path, _, _) -> path, desc.cstr_loc
    | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match get_desc desc.lbl_res with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

=======
>>>>>>> 501-plus-upstream-main-9fa77db
type find_source_result =
  | Found of string
  | Not_found of File.t
  | Multiple_matches of string list

let find_source ~config loc =
  log ~title:"find_source" "attempt to find %S"
  loc.Location.loc_start.Lexing.pos_fname ;
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let file =
    match File.of_filename fname with
    | Some file -> file
    | None ->
      (* no extension? we have to decide. *)
      Preferences.src fname
  in
  let filename = File.name file in
  let initial_path =
    match File_switching.where_am_i () with
    | None -> fname
    | Some s -> s
  in
  log ~title:"find_source" "initial path: %S" initial_path;
  let dir = Filename.dirname initial_path in
  let dir =
    match config.Mconfig.query.directory with
    | "" -> dir
    | cwd -> Misc.canonicalize_filename ~cwd dir
  in
  match Utils.find_all_matches ~config ~with_fallback file with
  | [] ->
    log ~title:"find_source" "failed to find %S in source path (fallback = %b)"
       filename with_fallback ;
    log ~title:"find_source" "looking for %S in %S" (File.name file) dir ;
    begin match
      Utils.find_file_with_path ~config ~with_fallback file [dir]
    with
    | Some source -> Found source
    | None ->
      log ~title:"find_source" "Trying to find %S in %S directly" fname dir;
      try Found (Misc.find_in_path [dir] fname)
      with _ -> Not_found file
    end
  | [ x ] -> Found x
  | files ->
    log ~title:(sprintf "find_source(%s)" filename)
      "multiple matches in the source path : %s"
      (String.concat ~sep:" , " files);
    try
      match File_switching.source_digest () with
      | None ->
        log ~title:"find_source"
          "... no source digest available to select the right one" ;
        raise Not_found
      | Some digest ->
        log ~title:"find_source"
          "... trying to use source digest to find the right one" ;
        log ~title:"find_source" "Source digest: %s" (Digest.to_hex digest) ;
        Found (
          List.find files ~f:(fun f ->
            let fdigest = Digest.file f in
            log ~title:"find_source" "  %s (%s)" f (Digest.to_hex fdigest) ;
            fdigest = digest
          )
        )
    with Not_found ->
      log ~title:"find_source" "... using heuristic to select the right one" ;
      log ~title:"find_source" "we are looking for a file named %s in %s" fname dir ;
      let rev = String.reverse (Misc.canonicalize_filename ~cwd:dir fname) in
      let lst =
        List.map files ~f:(fun path ->
          let path' = String.reverse path in
          let priority = (String.common_prefix_len rev path') * 2 +
                          if Preferences.is_preferred path
                          then 1
                          else 0
          in
          priority, path
        )
      in
      let lst =
        (* TODO: remove duplicates in [source_path] instead of using
          [sort_uniq] here. *)
        List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
          let tmp = compare j i in
          if tmp <> 0 then tmp else
          match compare s t with
          | 0 -> 0
          | n ->
            (* Check if we are referring to the same files.
                Especially useful on OSX case-insensitive FS.
                FIXME: May be able handle symlinks and non-existing files,
                CHECK *)
            match File_id.get s, File_id.get t with
            | s', t' when File_id.check s' t' ->
              0
            | _ -> n
        ) lst
      in
      match lst with
      | (i1, _) :: (i2, _) :: _ when i1 = i2 ->
        Multiple_matches files
      | (_, s) :: _ -> Found s
      | _ -> assert false

(* Well, that's just another hack.
   [find_source] doesn't like the "-o" option of the compiler. This hack handles
   Jane Street specific use case where "-o" is used to prefix a unit name by the
   name of the library which contains it. *)
let find_source ~config loc path =
  let result =
    match find_source ~config loc with
    | Found _ as result -> result
    | failure ->
      let fname = loc.Location.loc_start.Lexing.pos_fname in
      match
        let i = String.first_double_underscore_end fname in
        let pos = i + 1 in
        let fname = String.sub fname ~pos ~len:(String.length fname - pos) in
        let loc =
          let lstart = { loc.Location.loc_start with Lexing.pos_fname = fname } in
          { loc with Location.loc_start = lstart }
        in
        find_source ~config loc
      with
      | Found _ as result -> result
      | _ -> failure
      | exception _ -> failure
  in
  match (result : find_source_result) with
  | Found src -> `Found (src, loc)
  | Not_found f -> File.explain_not_found path f
  | Multiple_matches lst ->
    let matches = String.concat lst ~sep:", " in
    `File_not_found (
      sprintf "Several source files in your path have the same name, and \
               merlin doesn't know which is the right one: %s"
        matches)

<<<<<<< HEAD
(** [find_loc_of_uid] uid's location are given by tables stored int he cmt files
  for external compilation units or computed by Merlin for the current buffer.
  This function lookups a uid's location in the appropriate table. *)
let find_loc_of_uid ~config ~local_defs uid comp_unit =
  let title = "find_loc_of_uid" in
  if Misc_utils.is_current_unit comp_unit then begin
    log ~title "We look for %a in the current compilation unit."
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    log ~title "Looking for %a in the uid_to_loc table"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    let tbl = Ast_iterators.build_uid_to_locs_tbl ~local_defs () in
    match Shape.Uid.Tbl.find_opt tbl uid with
    | Some { Location.loc; _ } -> `Some (uid, loc)
    | None -> log ~title "Uid not found in the local table."; `None
  end else begin
    log ~title "Loading the cmt file for unit %S" comp_unit;
    match load_cmt ~config comp_unit with
    | Ok (_pos_fname, artifact) ->
      log ~title "Shapes successfully loaded, looking for %a"
        Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
      begin match Artifact.uid_to_loc uid artifact with
        | Some decl -> `Some (uid, decl)
        | None -> log ~title "Uid not found in the cmt's table."; `None
      end
    | _ -> log ~title "Failed to load the cmt file"; `None
  end
||||||| fcc3157ab0
module Namespace = struct
  type all = Namespace.t
=======
(** [find_loc_of_uid] uid's location are given by tables stored int he cmt files
  for external compilation units or computed by Merlin for the current buffer.
  This function lookups a uid's location in the appropriate table. *)
let find_loc_of_uid ~config ~local_defs uid comp_unit =
  let title = "find_loc_of_uid" in
  let loc_of_decl ~uid def =
    match Misc_utils.loc_of_decl ~uid def  with
    | Some loc ->
      log ~title "Found location: %a"
        Logger.fmt (fun fmt -> Location.print_loc fmt loc.loc);
      `Some (uid, loc.loc)
    | None -> log ~title "The declaration has no location."; `None
  in
  if Env.get_unit_name () = comp_unit then begin
    log ~title "We look for %a in the current compilation unit."
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    log ~title "Looking for %a in the uid_to_loc table"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    let tbl = Ast_iterators.build_uid_to_locs_tbl ~local_defs () in
    match Shape.Uid.Tbl.find_opt tbl uid with
    | Some { Location.loc; _ } -> `Some (uid, loc)
    | None -> log ~title "Uid not found in the local table."; `None
  end else begin
    log ~title "Loading the cmt file for unit %S" comp_unit;
    match load_cmt ~config comp_unit with
    | Ok (_pos_fname, cmt) ->
      log ~title "Shapes successfully loaded, looking for %a"
        Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
      begin match Shape.Uid.Tbl.find_opt cmt.cmt_uid_to_decl uid with
        | Some decl -> loc_of_decl ~uid decl
        | None -> log ~title "Uid not found in the cmt's table."; `None
      end
    | _ -> log ~title "Failed to load the cmt file"; `None
  end
>>>>>>> 501-plus-upstream-main-9fa77db

let find_loc_of_comp_unit ~config uid comp_unit =
  let title = "find_loc_of_comp_unit" in
  log ~title "Got the uid of a compilation unit: %s" comp_unit;
  match load_cmt ~config comp_unit with
  | Ok (pos_fname, _cmt) ->
    let pos = Std.Lexing.make_pos ~pos_fname (1, 0) in
    let loc = { Location.loc_start=pos; loc_end=pos; loc_ghost=true } in
    `Some (uid, loc)
    | _ -> log ~title "Failed to load the CU's cmt"; `None

<<<<<<< HEAD
let find_definition_uid ~config ~env ~(decl : Env_lookup.item) path =
  let namespace = decl.namespace in
  let module Reduce = Shape_reduce.Make (struct
      let fuel = 10
      let read_unit_shape ~unit_name =
        log ~title:"read_unit_shape" "inspecting %s" unit_name;
        match
          load_cmt ~config:({config with ml_or_mli = `ML})
                   ~with_fallback:false unit_name
        with
        | Ok (filename, artifact) ->
          move_to filename artifact;
          log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
          (Artifact.impl_shape artifact)
        | Error () ->
          log ~title:"read_unit_shape" "failed to find %s" unit_name;
          None
  end)
  in
  let shape = Env.shape_of_path ~namespace env path in
  log ~title:"shape_of_path" "initial: %a"
    Logger.fmt (Fun.flip Shape.print shape);
  let _keep_aliases =
    if config.traverse_aliases
    then (fun _ -> false)
    else (function
    | Shape. { uid = Some (Item { comp_unit; _ });
              desc = Alias { desc = Comp_unit alias_cu; _ };
              _ }
      when let by = comp_unit ^ "__" in
        Merlin_utils.Std.String.is_prefixed ~by alias_cu ->
      false
    | _ -> true)
  in
  let reduced = Reduce.reduce_for_uid env shape
  in
  log ~title:"shape_of_path" "reduced: %a"
    Logger.fmt (fun fmt -> Shape_reduce.print_result fmt reduced);
  reduced
||||||| fcc3157ab0
  type t =(* TODO: share with [Namespace.t] *)
    [ `Type | `Mod | `Modtype | `Vals | under_type ]
=======
let find_definition_uid ~config ~env ~(decl : Env_lookup.item) path =
  let namespace = decl.namespace in
  let module Reduce = Shape_reduce.Make (struct
      let fuel = 10
>>>>>>> 501-plus-upstream-main-9fa77db

<<<<<<< HEAD
let rec uid_of_result ~traverse_aliases = function
  | Shape_reduce.Resolved uid ->
      Some uid, false
  | Resolved_alias (Item { comp_unit; _ },
      (Resolved_alias (Compilation_unit comp_unit', _) as rest))
      when let by = comp_unit ^ "__" in String.is_prefixed ~by comp_unit' ->
      (* Always traverse dune-wrapper aliases *)
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (_alias, rest) when traverse_aliases ->
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (alias, _rest) ->
      Some alias, false
  | Unresolved { uid = Some uid; desc = Comp_unit _; approximated; hash = _ } ->
      Some uid, approximated
  | Approximated _ | Unresolved _ | Internal_error_missing_uid ->
      None, true
||||||| fcc3157ab0
  type inferred =
    [ t
    | `This_label of Types.label_description
    | `This_cstr of Types.constructor_description ]
=======
      let read_unit_shape ~unit_name =
          log ~title:"read_unit_shape" "inspecting %s" unit_name;
          match
            load_cmt ~config:({config with ml_or_mli = `ML})
                     ~with_fallback:false unit_name
          with
          | Ok (filename, cmt_infos) ->
            move_to filename cmt_infos;
            log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
            cmt_infos.cmt_impl_shape
          | Error () ->
            log ~title:"read_unit_shape" "failed to find %s" unit_name;
            None
    end)
  in
  let shape = Env.shape_of_path ~namespace env path in
  log ~title:"shape_of_path" "initial: %a"
    Logger.fmt (Fun.flip Shape.print shape);
  let reduced = Reduce.reduce_for_uid env shape
  in
  log ~title:"shape_of_path" "reduced: %a"
    Logger.fmt (fun fmt -> Shape_reduce.print_result fmt reduced);
  reduced
>>>>>>> 501-plus-upstream-main-9fa77db

<<<<<<< HEAD
(** This is the main function here *)
let from_path ~config ~env ~local_defs ~decl path =
  let title = "from_path" in
  let unalias (decl : Env_lookup.item) =
    if not config.traverse_aliases then decl.uid else
    let namespace = decl.namespace in
    let uid = scrape_alias ~fallback_uid:decl.uid ~env ~namespace path in
    if uid <> decl.uid then
      log ~title:"uid_of_path" "Unaliased declaration uid: %a -> %a"
        Logger.fmt (Fun.flip Shape.Uid.print decl.uid)
        Logger.fmt (Fun.flip Shape.Uid.print uid);
    uid
  in
  (* Step 1:  Path => Uid *)
  let decl : Env_lookup.item = { decl with uid = (unalias decl) } in
  let uid, approximated = match config.ml_or_mli with
    | `MLI -> decl.uid, false
    | `ML ->
      let traverse_aliases = config.traverse_aliases in
      let result = find_definition_uid ~config ~env ~decl path in
      match uid_of_result ~traverse_aliases result with
      | Some uid, approx -> uid, approx
      | None, _approx ->
          log ~title "No definition uid, falling back to the declaration uid: %a"
            Logger.fmt (Fun.flip Shape.Uid.print decl.uid);
          decl.uid, true
  in
  (* Step 2:  Uid => Location *)
  let loc = match uid with
    | Predef s -> `Builtin (uid, s)
    | Internal -> `Builtin (uid, "<internal>")
    | Item {comp_unit; _} -> find_loc_of_uid ~config ~local_defs uid comp_unit
    | Compilation_unit comp_unit -> find_loc_of_comp_unit ~config uid comp_unit
  in
  let loc = match loc with
    | `None ->
      log ~title "Falling back to the declaration's location: %a"
        Logger.fmt (Fun.flip Location.print_loc decl.loc);
      `Some (decl.uid, decl.loc)
    | other -> other
  in
  (* Step 3:  Location => Source *)
  match loc with
  | `None -> assert false
  | `Builtin _ as err -> err
  | `Some (uid, loc) ->
    match find_source ~config:config.mconfig loc (Path.name path) with
    | `Found (file, location) ->
      log ~title:"find_source" "Found file: %s (%a)" file
        Logger.fmt (Fun.flip Location.print_loc location);
      `Found {
        uid;
        decl_uid = decl.uid;
        file; location; approximated }
    | `File_not_found _ as otherwise -> otherwise
||||||| fcc3157ab0
  let from_context : Context.t -> inferred list = function
    | Type          -> [ `Type ; `Mod ; `Modtype ; `Constr ; `Labels ; `Vals ]
    | Module_type   -> [ `Modtype ; `Mod ; `Type ; `Constr ; `Labels ; `Vals ]
    | Expr | Constant ->
      [ `Vals ; `Mod ; `Modtype ; `Constr ; `Labels ; `Type ]
    | Patt          -> [ `Mod ; `Modtype ; `Type ; `Constr ; `Labels ; `Vals ]
    | Unknown       -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]
    | Label lbl     -> [ `This_label lbl ]
    | Module_path   -> [ `Mod ]
    | Constructor (c, _) -> [ `This_cstr c ]
end
=======
let rec uid_of_result ~traverse_aliases = function
  | Shape_reduce.Resolved uid ->
      Some uid, false
  | Resolved_alias ((Item { comp_unit; _ } | Compilation_unit comp_unit),
      ((Resolved_alias (Compilation_unit comp_unit', _)
      | Resolved (Compilation_unit comp_unit') ) as rest))
      when let by = comp_unit ^ "__" in String.is_prefixed ~by comp_unit' ->
      (* Always traverse dune-wrapper aliases *)
      log ~title:"uid_of_result"
        "Traversing wrapping alias: %s__ %s" comp_unit comp_unit';
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (_alias, rest) when traverse_aliases ->
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (alias, _rest) ->
      Some alias, false
  | Unresolved { uid = Some uid; desc = Comp_unit _; approximated } ->
      Some uid, approximated
  | Approximated _ | Unresolved _ | Internal_error_missing_uid ->
      None, true
>>>>>>> 501-plus-upstream-main-9fa77db

<<<<<<< HEAD
let from_longident ~config ~env ~local_defs nss ident =
||||||| fcc3157ab0
module Env_lookup : sig

  val loc
    : Path.t
    -> Namespace.all
    -> Env.t
    -> (Location.t * Shape.Uid.t * Shape.Sig_component_kind.t) option

  val in_namespaces
     : Namespace.inferred list
    -> Longident.t
    -> Env.t
    -> (Path.t * Shape.Sig_component_kind.t * Shape.Uid.t * Location.t) option

end = struct

  let loc path (namespace : Namespace.all) env =
    try
      Some (
        match namespace with
        | `Unknown
        | `Apply
        | `Vals ->
          let vd = Env.find_value path env in
          vd.val_loc, vd.val_uid, Shape.Sig_component_kind.Value
        | `Constr
        | `Labels
        | `Type ->
          let td = Env.find_type path env in
          td.type_loc, td.type_uid, Shape.Sig_component_kind.Type
        | `Functor
        | `Mod ->
          let md = Env.find_module path env in
          md.md_loc, md.md_uid, Shape.Sig_component_kind.Module
        | `Modtype ->
          let mtd = Env.find_modtype path env in
          mtd.mtd_loc, mtd.mtd_uid, Shape.Sig_component_kind.Module_type
      )
    with
      Not_found -> None

  exception Found of
    (Path.t * Shape.Sig_component_kind.t * Shape.Uid.t * Location.t)

  let in_namespaces (nss : Namespace.inferred list) ident env =
    let open Shape.Sig_component_kind in
    try
      List.iter nss ~f:(fun namespace ->
        try
          match namespace with
          | `This_cstr ({ Types.cstr_tag = Cstr_extension _; _ } as cd) ->
            log ~title:"lookup"
              "got extension constructor";
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Extension_constructor, cd.cstr_uid, loc))
          | `This_cstr cd ->
            log ~title:"lookup"
              "got constructor, fetching path and loc in type namespace";
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Type, cd.cstr_uid,loc))
          | `Constr ->
            log ~title:"lookup" "lookup in constructor namespace" ;
            let cd = Env.find_constructor_by_name ident env in
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Type,cd.cstr_uid, loc))
          | `Mod ->
            log ~title:"lookup" "lookup in module namespace" ;
            let path, md = Env.find_module_by_name ident env in
            raise (Found (path, Module, md.md_uid, md.Types.md_loc))
          | `Modtype ->
            log ~title:"lookup" "lookup in module type namespace" ;
            let path, mtd = Env.find_modtype_by_name ident env in
            raise (Found (path, Module_type, mtd.mtd_uid, mtd.Types.mtd_loc))
          | `Type ->
            log ~title:"lookup" "lookup in type namespace" ;
            let path, typ_decl = Env.find_type_by_name ident env in
            raise (
              Found (path, Type, typ_decl.type_uid, typ_decl.Types.type_loc)
            )
          | `Vals ->
            log ~title:"lookup" "lookup in value namespace" ;
            let path, val_desc = Env.find_value_by_name ident env in
            raise (
              Found (path, Value, val_desc.val_uid, val_desc.Types.val_loc)
            )
          | `This_label lbl ->
            log ~title:"lookup"
              "got label, fetching path and loc in type namespace";
            let path, loc = path_and_loc_from_label lbl env in
            (* TODO: Use [`Labels] here instead of [`Type] *)
            raise (Found (path, Type, lbl.lbl_uid, loc))
          | `Labels ->
            log ~title:"lookup" "lookup in label namespace" ;
            let lbl = Env.find_label_by_name ident env in
            let path, loc = path_and_loc_from_label lbl env in
            (* TODO: Use [`Labels] here instead of [`Type] *)
            raise (Found (path, Type, lbl.lbl_uid, loc))
        with Not_found -> ()
      ) ;
      log ~title:"lookup" "   ... not in the environment" ;
      None
    with Found ((path, namespace, decl_uid, _loc) as x) ->
      log ~title:"env_lookup" "found: '%a' in namespace %s with uid %a"
        Logger.fmt (fun fmt -> Path.print fmt path)
        (Shape.Sig_component_kind.to_string namespace)
        Logger.fmt (fun fmt -> Shape.Uid.print fmt decl_uid);
      Some x
end

let uid_from_longident ~config ~env nss ml_or_mli ident =
=======
(** This is the main function here *)
let from_path ~config ~env ~local_defs ~decl path =
  let title = "from_path" in
  let unalias (decl : Env_lookup.item) =
    if not config.traverse_aliases then decl.uid else
    let namespace = decl.namespace in
    let uid = scrape_alias ~fallback_uid:decl.uid ~env ~namespace path in
    if uid <> decl.uid then
      log ~title:"uid_of_path" "Unaliased declaration uid: %a -> %a"
        Logger.fmt (Fun.flip Shape.Uid.print decl.uid)
        Logger.fmt (Fun.flip Shape.Uid.print uid);
    uid
  in
  (* Step 1:  Path => Uid *)
  let decl : Env_lookup.item = { decl with uid = (unalias decl) } in
  let uid, approximated = match config.ml_or_mli with
    | `MLI -> decl.uid, false
    | `ML ->
      let traverse_aliases = config.traverse_aliases in
      let result = find_definition_uid ~config ~env ~decl path in
      match uid_of_result ~traverse_aliases result with
      | Some uid, approx -> uid, approx
      | None, _approx ->
          log ~title "No definition uid, falling back to the declaration uid: %a"
            Logger.fmt (Fun.flip Shape.Uid.print decl.uid);
          decl.uid, true
  in
  (* Step 2:  Uid => Location *)
  let loc = match uid with
    | Predef s -> `Builtin (uid, s)
    | Internal -> `Builtin (uid, "<internal>")
    | Item {comp_unit; _} -> find_loc_of_uid ~config ~local_defs uid comp_unit
    | Compilation_unit comp_unit -> find_loc_of_comp_unit ~config uid comp_unit
  in
  let loc = match loc with
    | `None ->
      log ~title "Falling back to the declaration's location: %a"
        Logger.fmt (Fun.flip Location.print_loc decl.loc);
      `Some (decl.uid, decl.loc)
    | other -> other
  in
  (* Step 3:  Location => Source *)
  match loc with
  | `None -> assert false
  | `Builtin _ as err -> err
  | `Some (uid, loc) ->
    match find_source ~config:config.mconfig loc (Path.name path) with
    | `Found (file, location) ->
      log ~title:"find_source" "Found file: %s (%a)" file
        Logger.fmt (Fun.flip Location.print_loc location);
      `Found {
        uid;
        decl_uid = decl.uid;
        file; location; approximated }
    | `File_not_found _ as otherwise -> otherwise

let from_longident ~config ~env ~local_defs nss ident =
>>>>>>> 501-plus-upstream-main-9fa77db
  let str_ident =
    try String.concat ~sep:"." (Longident.flatten ident)
    with _-> "Not a flat longident"
  in
  match Env_lookup.by_longident nss ident env with
  | None -> `Not_in_env str_ident
  | Some (path, decl) -> from_path ~config ~env ~local_defs ~decl path

let from_path ~config ~env ~local_defs ~namespace path =
  File_switching.reset ();
<<<<<<< HEAD
  match Env_lookup.loc path namespace env with
  | None -> `Not_in_env (Path.name path)
  | Some decl -> from_path ~config ~env ~local_defs ~decl path
||||||| fcc3157ab0
  if Utils.is_builtin_path path then
    `Builtin
  else
    match Env_lookup.loc path namespace env with
    | None -> `Not_in_env (Path.name path)
    | Some (loc, uid, namespace) ->
      match locate ~config ~env ~ml_or_mli uid loc path namespace with
      | `Not_found _
      | `File_not_found _ as err -> err
      | `Found (uid, loc) ->
        match find_source ~config loc (Path.name path) with
        | `Found (file, loc) -> `Found (uid, file, loc)
        | `File_not_found _ as otherwise -> otherwise
=======
  match Env_lookup.by_path path namespace env with
  | None -> `Not_in_env (Path.name path)
  | Some decl -> from_path ~config ~env ~local_defs ~decl path
>>>>>>> 501-plus-upstream-main-9fa77db

let infer_namespace ?namespaces ~pos lid browse is_label =
  match namespaces with
  | Some nss ->
    if not is_label
    then `Ok (nss :> Env_lookup.Namespace.inferred list)
    else if List.mem `Labels ~set:nss then (
      log ~title:"from_string" "restricting namespaces to labels";
      `Ok [ `Labels ]
    ) else (
      log ~title:"from_string"
        "input is clearly a label, but the given namespaces don't cover that";
      `Error `Missing_labels_namespace
    )
  | None ->
    match Context.inspect_browse_tree ~cursor:pos lid [browse], is_label with
    | None, _ ->
      log ~title:"from_string" "already at origin, doing nothing" ;
      `Error `At_origin
    | Some (Label _ as ctxt), true
    | Some ctxt, false ->
      log ~title:"from_string"
        "inferred context: %s" (Context.to_string ctxt);
      `Ok (Env_lookup.Namespace.from_context ctxt)
    | _, true ->
      log ~title:"from_string"
        "dropping inferred context, it is not precise enough";
      `Ok [ `Labels ]

let from_string ~config ~env ~local_defs ~pos ?namespaces path =
  File_switching.reset ();
  let browse = Mbrowse.of_typedtree local_defs in
  let lid = Type_utils.parse_longident path in
  let from_lid lid =
    let ident, is_label = Longident.keep_suffix lid in
    match infer_namespace ?namespaces ~pos lid browse is_label with
    | `Error e -> e
    | `Ok nss ->
      log ~title:"from_string"
        "looking for the source of '%s' (prioritizing %s files)"
        path (match config.ml_or_mli with `ML -> ".ml" | `MLI -> ".mli");
      from_longident ~config ~env ~local_defs nss ident
  in
  Option.value_map ~f:from_lid ~default:(`Not_found (path, None)) lid

<<<<<<< HEAD
(** When we look for docstring in external compilation unit we can perform
    a uid-based search and return the attached comment in the attributes.
    This is a more sound way to get documentation than resorting on the
    [Ocamldoc.associate_comment] heuristic *)
(* In a future release of OCaml the cmt's uid_to_loc table will contain
   fragments of the typedtree that might be used to get the docstrings without
   relying on this iteration *)
let find_doc_attributes_in_typedtree ~config ~comp_unit uid =
  log ~title:"doc_from_uid" "Loading the cmt for unit %S" comp_unit;
  match load_cmt ~config:({config with ml_or_mli = `MLI}) comp_unit with
  | Ok (_, artifact) ->
    log ~title:"doc_from_uid" "Cmt loaded, itering on the typedtree";
    begin
      match Artifact.uid_to_attributes uid artifact with
      | Some attrs ->
        log ~title:"doc_from_uid" "Found attributes for this uid";
        let parse_attributes attrs =
          let open Parsetree in
          try Some (List.find_map attrs ~f:(fun attr ->
            if List.exists ["ocaml.doc"; "ocaml.text"]
              ~f:(String.equal attr.attr_name.txt)
            then Ast_helper.extract_str_payload attr.attr_payload
            else None))
          with Not_found -> None
        in
        begin match parse_attributes attrs with
        | Some (doc, _) -> `Found_attributes (doc |> String.trim)
        | None -> `No_documentation end
      | None -> `No_documentation
    end
  | Error _ -> `No_documentation
||||||| fcc3157ab0
(** When we look for docstring in external compilation unit we can perform
    a uid-based search and return the attached comment in the attributes.
    This is a more sound way to get documentation than resorting on the
    [Ocamldoc.associate_comment] heuristic *)
(* In a future release of OCaml the cmt's uid_to_loc table will contain
   fragments of the typedtree that might be used to get the docstrings without
   relying on this iteration *)
let find_doc_attributes_in_typedtree ~config ~comp_unit uid =
  let exception Found_attributes of Typedtree.attributes in
  let test elt_uid attributes =
    if Shape.Uid.equal uid elt_uid then raise (Found_attributes attributes)
  in
  let iterator =
    let first_item = ref true in
    let uid_is_comp_unit = match uid with
      | Shape.Uid.Compilation_unit _ -> true
      | _ -> false
    in
    fun env -> { Tast_iterator.default_iterator with

      (* Needed to return top-level module doc (when the uid is a compunit).
         The module docstring must be the first signature or structure item *)
      signature_item = (fun sub ({ sig_desc; _} as si) ->
        begin match sig_desc, !first_item, uid_is_comp_unit with
        | Tsig_attribute attr, true, true -> raise (Found_attributes [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.signature_item sub si);

      structure_item = (fun sub ({ str_desc; _} as sti) ->
        begin match str_desc, !first_item, uid_is_comp_unit with
        | Tstr_attribute attr, true, true -> raise (Found_attributes [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.structure_item sub sti);

      value_description = (fun sub ({ val_val; val_attributes; _ } as vd) ->
        test val_val.val_uid val_attributes;
        Tast_iterator.default_iterator.value_description sub vd);

      type_declaration = (fun sub ({ typ_type; typ_attributes; _ } as td) ->
        test typ_type.type_uid typ_attributes;
        Tast_iterator.default_iterator.type_declaration sub td);

      value_binding = (fun sub ({ vb_pat; vb_attributes; _ } as vb) ->
        let pat_var_iter ~f pat =
          let rec aux pat =
            let open Typedtree in
            match pat.pat_desc with
            | Tpat_var (id, _) -> f id
            | Tpat_alias (pat, _, _)
            | Tpat_variant (_, Some pat, _)
            | Tpat_lazy pat
            | Tpat_or (pat, _, _) ->
                aux pat
            | Tpat_tuple pats
            | Tpat_construct (_, _, pats, _)
            | Tpat_array pats ->
                List.iter ~f:aux pats
            | Tpat_record (pats, _) ->
                List.iter ~f:(fun (_, _, pat) -> aux pat) pats
            | _ -> ()
          in
          aux pat
        in
        pat_var_iter vb_pat ~f:(fun id ->
          try
            let vd = Env.find_value (Pident id) env in
            test vd.val_uid vb_attributes
          with Not_found -> ());
        Tast_iterator.default_iterator.value_binding sub vb)
    }
  in
  let typedtree =
    log ~title:"doc_from_uid" "Loading the cmt for unit %S" comp_unit;
    match load_cmt ~config comp_unit `MLI with
    | Ok (_, cmt_infos) ->
      log ~title:"doc_from_uid" "Cmt loaded, itering on the typedtree";
      begin match cmt_infos.cmt_annots with
      | Interface s -> Some (`Interface { s with
          sig_final_env = Envaux.env_of_only_summary s.sig_final_env})
      | Implementation str -> Some (`Implementation { str with
          str_final_env = Envaux.env_of_only_summary str.str_final_env})
      | _ -> None
      end
    | Error _ -> None
  in
  try begin match typedtree with
    | Some (`Interface s) ->
        let iterator = iterator s.sig_final_env in
        iterator.signature iterator s;
        log ~title:"doc_from_uid" "uid not found in the signature"
    | Some (`Implementation str) ->
        let iterator = iterator str.str_final_env in
        iterator.structure iterator str;
        log ~title:"doc_from_uid" "uid not found in the implementation"
    | _ -> () end;
    `No_documentation
  with
    | Found_attributes attrs ->
        log ~title:"doc_from_uid" "Found attributes for this uid";
        let parse_attributes attrs =
          let open Parsetree in
          try Some (List.find_map attrs ~f:(fun attr ->
            if List.exists ["ocaml.doc"; "ocaml.text"]
              ~f:(String.equal attr.attr_name.txt)
            then Ast_helper.extract_str_payload attr.attr_payload
            else None))
          with Not_found -> None
        in
        begin match parse_attributes attrs with
        | Some (doc, _) -> `Found (doc |> String.trim)
        | None -> `No_documentation end
    | Not_found -> `No_documentation
=======

let find_doc_attribute attrs =
  let open Parsetree in
  try Some (List.find_map attrs ~f:(fun attr ->
    if List.exists ["ocaml.doc"; "ocaml.text"]
      ~f:(String.equal attr.attr_name.txt)
    then Ast_helper.extract_str_payload attr.attr_payload
    else None))
  with Not_found -> None

let find_compunit_doc_in_typedtree cmt_infos =
  let first_item_attribute =
    log ~title:"doc_from_uid" "Itering on the typedtree";
    match cmt_infos.Cmt_format.cmt_annots with
    | Interface
        { sig_items = { sig_desc = Tsig_attribute attr; _} :: _; _} -> Some attr
    | Implementation
        { str_items = { str_desc = Tstr_attribute attr; _} :: _; _} -> Some attr
    | _ -> None
  in
  match first_item_attribute with
  | None -> `No_documentation
  | Some attr ->
    log ~title:"doc_from_uid" "Found attributes for this uid";
    begin match find_doc_attribute [attr] with
    | Some (doc, _) -> `Found_doc (doc |> String.trim)
    | None -> `No_documentation end

let doc_of_item_declaration decl =
  let attributes = match decl with
    | Typedtree.Value { val_attributes; _ } -> val_attributes
    | Value_binding { vb_attributes; _ } -> vb_attributes
    | Type { typ_attributes; _ } -> typ_attributes
    | Constructor { cd_attributes; _ } -> cd_attributes
    | Extension_constructor { ext_attributes; _ }  -> ext_attributes
    | Label { ld_attributes; _ } -> ld_attributes
    | Module { md_attributes; _ } -> md_attributes
    | Module_substitution { ms_attributes; _ } -> ms_attributes
    | Module_binding { mb_attributes; _ } -> mb_attributes
    | Module_type { mtd_attributes; _ } -> mtd_attributes
    | Class { ci_attributes; _ }
    | Class_type { ci_attributes; _ } -> ci_attributes
  in
  match find_doc_attribute attributes with
  | Some (doc, _) -> `Found_doc (doc |> String.trim)
  | None -> `No_documentation

(** When we look for docstring in an external compilation unit we can perform a
    uid-based search and return the attached comment in the attributes. This is
    a more sound way to get documentation than resorting on the
    [Ocamldoc.associate_comment] heuristic. *)
let find_uid_doc_in_cmt cmt_infos uid =
  match uid with
  | Shape.Uid.Compilation_unit _ ->
    (* For module doc we need to look at the first items in the typedtree *)
    find_compunit_doc_in_typedtree cmt_infos
  | _ -> begin
      let decl =
          Shape.Uid.Tbl.find_opt cmt_infos.Cmt_format.cmt_uid_to_decl uid
      in
      match decl with
      | None -> `No_documentation
      | Some decl ->
        begin match doc_of_item_declaration decl with
        | `Found_doc d -> `Found_doc d
        | `No_documentation -> `Found_decl (uid, decl, cmt_infos.cmt_comments)
        end
    end
>>>>>>> 501-plus-upstream-main-9fa77db

let doc_from_uid ~config ~loc uid =
  begin match uid with
<<<<<<< HEAD
  | Shape.Uid.Item { comp_unit; _ }
  | Shape.Uid.Compilation_unit comp_unit
      when not (Misc_utils.is_current_unit comp_unit) ->
        log ~title:"get_doc" "the doc (%a) you're looking for is in another
          compilation unit (%s)"
          Logger.fmt (fun fmt -> Shape.Uid.print fmt uid) comp_unit;
        (match find_doc_attributes_in_typedtree ~config ~comp_unit uid with
        | `Found_attributes doc -> `Found_doc doc
        | `No_documentation ->
            (* We fallback on the legacy heuristic to handle some unproper
               doc placement. See test [unattached-comment.t] *)
            `Found_loc loc)
||||||| fcc3157ab0
  | Some (Shape.Uid.Item { comp_unit; _ } as uid)
  | Some (Shape.Uid.Compilation_unit comp_unit as uid)
      when Env.get_unit_name () <> comp_unit ->
        log ~title:"get_doc" "the doc (%a) you're looking for is in another
          compilation unit (%s)"
          Logger.fmt (fun fmt -> Shape.Uid.print fmt uid) comp_unit;
        (match find_doc_attributes_in_typedtree ~config ~comp_unit uid with
        | `Found doc -> `Found_doc doc
        | `No_documentation ->
            (* We fallback on the legacy heuristic to handle some unproper
               doc placement. See test [unattached-comment.t] *)
            `Found_loc loc)
=======
  | Shape.Uid.Item { comp_unit; _ }
  | Shape.Uid.Compilation_unit comp_unit
    when Env.get_unit_name () <> comp_unit ->
    log ~title:"get_doc" "the doc (%a) you're looking for is in another
      compilation unit (%s)"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid) comp_unit;
      log ~title:"doc_from_uid" "Loading the cmt for unit %S" comp_unit;
    begin match load_cmt ~config:({config with ml_or_mli = `MLI}) comp_unit with
    | Error _ -> `No_documentation
    | Ok (_, cmt_infos) ->
      log ~title:"doc_from_uid" "Cmt loaded for %s" (Option.value ~default:"<>" cmt_infos.cmt_sourcefile);
      find_uid_doc_in_cmt cmt_infos uid
      end
>>>>>>> 501-plus-upstream-main-9fa77db
  | _ ->
    (* Uid based search doesn't works in the current CU since Merlin's parser
       does not attach doc comments to the typedtree *)
    `Found_loc loc
  end

let doc_from_comment_list ~after_only ~buffer_comments loc =
  (* When the doc we look for is in the current buffer or if search by uid
    has failed we use an alternative heuristic since Merlin's pure parser
    does not poulates doc attributes in the typedtree. *)
  let comments =
    match File_switching.where_am_i () with
    | None ->
      log ~title:"get_doc" "Using reader's comment (current buffer)";
      buffer_comments
    | Some cmt_path ->
      log ~title:"get_doc" "File switching: actually in %s" cmt_path;
      let artifact = Artifact.read cmt_path in
      Artifact.comments artifact
  in
  log ~title:"get_doc" "%a" Logger.fmt (fun fmt ->
      Format.fprintf fmt "looking around %a inside: [\n"
        Location.print_loc !last_location;
      List.iter comments ~f:(fun (c, l) ->
          Format.fprintf fmt "  (%S, %a);\n" c
            Location.print_loc l);
      Format.fprintf fmt "]\n"
    );
  match
    Ocamldoc.associate_comment ~after_only comments loc !last_location
  with
  | None, _     -> `No_documentation
  | Some doc, _ -> `Found doc

<<<<<<< HEAD
let get_doc ~config:mconfig ~env ~local_defs ~comments ~pos =
||||||| fcc3157ab0
let get_doc ~config ~env ~local_defs ~comments ~pos =
=======
(* Get doc relies on different heuristics depending on the situation:
  - First it locates the declaration.
  - If a Uid is found that belongs to another compilation unit:
    - [doc_from_uid] The cmt file for that compilation unit is loaded
    - If the Uid is the one of a compilation unit we look in the typetree
    - else a lookup is performed in the [uid_to_decl] table
  - If the uid-based search failed we fallback on the [doc_from_comment_list]
    heuristic that uses location to select comments in a list. *)
let get_doc ~config:mconfig ~env ~local_defs ~comments ~pos =
>>>>>>> 501-plus-upstream-main-9fa77db
  File_switching.reset ();
  fun path ->
  let_ref last_location Location.none @@ fun () ->
  let config = { mconfig; ml_or_mli = `MLI; traverse_aliases = true; } in
  let doc_from_uid_result =
    match path with
    | `Completion_entry (namespace, path, _loc) ->
      log ~title:"get_doc" "completion: looking for the doc of '%a'"
        Logger.fmt (fun fmt -> Path.print fmt path) ;
<<<<<<< HEAD
      let from_path =
        from_path ~config ~env ~local_defs ~namespace path
      in
||||||| fcc3157ab0
      let from_path = from_path ~config ~env ~namespace `MLI path in
=======

      let from_path = from_path ~config ~env ~local_defs ~namespace path in
>>>>>>> 501-plus-upstream-main-9fa77db
      begin match from_path with
      | `Found { uid; location = loc; _ } ->
        doc_from_uid ~config ~loc uid
      | (`Builtin _ |`Not_in_env _|`File_not_found _|`Not_found _)
        as otherwise -> otherwise
      end
    | `User_input path ->
      log ~title:"get_doc" "looking for the doc of '%s'" path;
      begin match from_string ~config ~env ~local_defs ~pos path with
      | `Found { uid; location = loc; _ } ->
        doc_from_uid ~config ~loc uid
      | `At_origin ->
        `Found_loc { Location.loc_start = pos; loc_end = pos; loc_ghost = true }
      | `Missing_labels_namespace -> `No_documentation
      | (`Builtin _ | `Not_in_env _ | `Not_found _ |`File_not_found _ )
        as otherwise -> otherwise
      end
  in
  match doc_from_uid_result with
  | `Found_doc doc -> `Found doc
  | `Found_decl (uid, decl, comments) ->
      (match Misc_utils.loc_of_decl ~uid decl with
      | None -> `No_documentation
      | Some loc ->
        let after_only = match decl with
          | Typedtree.Constructor _ | Label _ -> true
          | _ -> false
        in
        doc_from_comment_list ~after_only ~buffer_comments:comments loc.loc)
  | `Found_loc loc ->
<<<<<<< HEAD
      doc_from_comment_list ~local_defs ~buffer_comments:comments loc
  | `Builtin _ ->
||||||| fcc3157ab0
      doc_from_comment_list ~local_defs ~buffer_comments:comments loc
  | `Builtin ->
=======
      (* based on https://v2.ocaml.org/manual/doccomments.html#ss:label-comments: *)
      let browse = Mbrowse.of_typedtree local_defs in
      let (_, deepest_before) =
        Mbrowse.(leaf_node @@ deepest_before loc.Location.loc_start [browse])
      in
      let after_only = begin match deepest_before with
        | Browse_raw.Constructor_declaration _ -> true
        (* The remaining `true` cases are currently not reachable *)
        | Label_declaration _ | Record_field _ | Row_field _  -> true
        | _ -> false end
      in
      doc_from_comment_list ~after_only ~buffer_comments:comments loc
  | `Builtin _ ->
>>>>>>> 501-plus-upstream-main-9fa77db
    begin match path with
    | `User_input path -> `Builtin path
    | `Completion_entry (_, path, _) -> `Builtin (Path.name path)
    end
  | `File_not_found _
  | `Not_found _
  | `No_documentation
  | `Not_in_env _ as otherwise -> otherwise
