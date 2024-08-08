(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Local_store

module Dir : sig
  type entry = {
    basename : string;
    path : string
  }

  type t

  val path : t -> string
  val files : t -> entry list
  val basenames : t -> string list
  val hidden : t -> bool

  val create : hidden:bool -> string -> t

  val find : t -> string -> string option
<<<<<<< janestreet/merlin-jst:merge-with-flambda-backend-5.2-merge
  val find_uncap : t -> string -> string option

  val check : hidden:bool -> t -> bool
||||||| ocaml-flambda/flambda-backend:1cc52ed5fa73a88abe59baf3058df23ee48e105d
  val find_uncap : t -> string -> string option
=======
  val find_normalized : t -> string -> string option
>>>>>>> ocaml-flambda/flambda-backend:33aedfc93c38ccad7a4d89974405c05123a18932
end = struct
  type entry = {
    basename : string;
    path : string
  }

  type t = {
    path : string;
    files : entry list;
    hidden : bool
  }

  let path t = t.path
  let files t = t.files
  let basenames t = List.map (fun { basename; _ } -> basename) t.files
  let hidden t = t.hidden

  let find t fn =
    List.find_map (fun { basename; path } ->
      if String.equal basename fn then
        Some path
      else
        None) t.files

  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search { basename; path } =
      if Misc.normalized_unit_filename basename = fn then
        Some path
      else
        None
    in
    List.find_map search t.files

  let check ~hidden t =
    hidden = t.hidden && Directory_content_cache.check t.path

  let create ~hidden path =
    let files = Array.to_list (Directory_content_cache.read path)
      |> List.map (fun basename -> { basename; path = Filename.concat path basename }) in
    { path; files; hidden }
end

type visibility = Visible | Hidden

(** Stores cached paths to files *)
module Path_cache : sig
  (* Clear cache *)
  val reset : unit -> unit

  (* Same as [add] below, but will replace existing entries.

     [prepend_add] is faster than [add] and intended for use in [init] and [remove_dir]:
     since we are starting from an empty cache, we can avoid checking whether a unit name
     already exists in the cache simply by adding entries in reverse order. *)
  val prepend_add : Dir.t -> unit

  (* Add path to cache. If path with same basename is already in cache, skip adding. *)
  val add : Dir.t -> unit

  (* Search for a basename in cache. Ignore case if [uncap] is true *)
  val find : uncap:bool -> string -> string * visibility
end = struct
  module STbl = Misc.String.Tbl

  (* Mapping from basenames to full filenames *)
  type registry = string STbl.t

  let visible_files : registry ref = s_table STbl.create 42
  let visible_files_uncap : registry ref = s_table STbl.create 42

  let hidden_files : registry ref = s_table STbl.create 42
  let hidden_files_uncap : registry ref = s_table STbl.create 42

  let reset () =
    STbl.clear !hidden_files;
    STbl.clear !hidden_files_uncap;
    STbl.clear !visible_files;
    STbl.clear !visible_files_uncap

  let prepend_add dir =
    List.iter (fun ({ basename = base; path = fn } : Dir.entry) ->
        if Dir.hidden dir then begin
          STbl.replace !hidden_files base fn;
          STbl.replace !hidden_files_uncap (Misc.normalized_unit_filename base) fn
        end else begin
          STbl.replace !visible_files base fn;
          STbl.replace !visible_files_uncap (String.uncapitalize_ascii base) fn
        end
      ) (Dir.files dir)

  let add dir =
    let update base fn visible_files hidden_files =
      if (Dir.hidden dir) && not (STbl.mem !hidden_files base) then
        STbl.replace !hidden_files base fn
      else if not (STbl.mem !visible_files base) then
        STbl.replace !visible_files base fn
    in
    List.iter
      (fun ({ basename = base; path = fn }: Dir.entry) ->
         update base fn visible_files hidden_files;
         let ubase = Misc.normalized_unit_filename base in
         update ubase fn visible_files_uncap hidden_files_uncap)
      (Dir.files dir)

  let find fn visible_files hidden_files =
    try (STbl.find !visible_files fn, Visible) with
    | Not_found -> (STbl.find !hidden_files fn, Hidden)

  let find ~uncap fn =
    if uncap then
      find (String.uncapitalize_ascii fn) visible_files_uncap hidden_files_uncap
    else
      find fn visible_files hidden_files
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

let visible_dirs = s_ref []
let hidden_dirs = s_ref []
let no_auto_include _ _ = raise Not_found
let auto_include_callback = ref no_auto_include

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.reset ();
  hidden_dirs := [];
  visible_dirs := [];
  auto_include_callback := no_auto_include

let get_visible () = List.rev !visible_dirs

let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)

type paths =
  { visible : string list;
    hidden : string list }

let get_paths () =
  { visible = List.rev_map Dir.path !visible_dirs;
    hidden = List.rev_map Dir.path !hidden_dirs }

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let init ~auto_include ~visible ~hidden =
  assert (not Config.merlin || Local_store.is_bound ());
  let rec loop_changed ~hidden acc = function
    | [] -> Some acc
    | new_path :: new_rest ->
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
  in
  let rec loop_unchanged ~hidden acc new_paths old_dirs =
    match new_paths, old_dirs with
    | [], [] -> None
    | new_path :: new_rest, [] ->
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
    | [], _ :: _ -> Some acc
    | new_path :: new_rest, old_dir :: old_rest ->
      if String.equal new_path (Dir.path old_dir) then begin
        if Dir.check ~hidden old_dir then begin
          loop_unchanged ~hidden (old_dir :: acc) new_rest old_rest
        end else begin
          loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
        end
      end else begin
        loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
      end
  in
  let new_visible =
    loop_unchanged ~hidden:false [] visible (List.rev !visible_dirs)
  in
  let new_hidden =
    loop_unchanged ~hidden:true [] hidden (List.rev !hidden_dirs)
  in
  let update =
    match new_visible, new_hidden with
    | None, None -> None
    | Some v, None -> Some (v, !hidden_dirs)
    | None, Some h -> Some (!visible_dirs, h)
    | Some v, Some h -> Some (v, h)
  in
  match update with
  | None -> ()
  | Some (new_visible, new_hidden) ->
    reset ();
    visible_dirs := new_visible;
    hidden_dirs := new_hidden;
    List.iter Path_cache.prepend_add new_hidden;
    List.iter Path_cache.prepend_add new_visible;
    auto_include_callback := auto_include

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
  let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
  if    List.compare_lengths visible !visible_dirs <> 0
     || List.compare_lengths hidden !hidden_dirs <> 0 then begin
    reset ();
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter Path_cache.prepend_add hidden;
    List.iter Path_cache.prepend_add visible
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present, in order to enforce
   left-to-right precedence. *)
let add (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.add dir;
  if (Dir.hidden dir) then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs

let append_dir = add

let add_dir ~hidden dir = add (Dir.create ~hidden dir)

(* Add the directory at the start of load path - so basenames are
   unconditionally added. *)
let prepend_dir (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.prepend_add dir;
  if (Dir.hidden dir) then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]

let is_basename fn = Filename.basename fn = fn

(* let auto_include_libs libs alert find_in_dir fn =
  let scan (lib, lazy dir) =
    let file = find_in_dir dir fn in
    let alert_and_add_dir _ =
      alert lib;
      append_dir dir
    in
    Option.iter alert_and_add_dir file;
    file
  in
  match List.find_map scan libs with
  | Some base -> base
  | None -> raise Not_found *)

(* let auto_include_otherlibs =
  (* Ensure directories are only ever scanned once *)
  let expand = Misc.expand_directory Config.standard_library in
  let otherlibs =
    let read_lib lib = lazy (Dir.create ~hidden:false (expand ("+" ^ lib))) in
    List.map (fun lib -> (lib, read_lib lib)) ["dynlink"; "str"; "unix"] in
  auto_include_libs otherlibs *)

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
      fst (Path_cache.find ~uncap:false fn)
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
      Path_cache.find ~uncap:true fn
    else
      try
        (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
    let fn_uncap = String.uncapitalize_ascii fn in
    (!auto_include_callback Dir.find_uncap fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)
