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

<<<<<<< HEAD
  val path : t -> string
  val files : t -> entry list
  val basenames : t -> string list
  val hidden : t -> bool

  val create : hidden:bool -> string -> t

  val find : t -> string -> string option
  val find_normalized : t -> string -> string option

  val check : hidden:bool -> t -> bool
end = struct
  type entry = {
    basename : string;
    path : string
  }
||||||| fcc3157ab0
let files : registry ref = s_table STbl.create 42
let files_uncap : registry ref = s_table STbl.create 42
=======
let visible_files : registry ref = s_table STbl.create 42
let visible_files_uncap : registry ref = s_table STbl.create 42

let hidden_files : registry ref = s_table STbl.create 42
let hidden_files_uncap : registry ref = s_table STbl.create 42
>>>>>>> 501-plus-upstream-main-9fa77db

  type t = {
    path : string;
<<<<<<< HEAD
    files : entry list;
    hidden : bool
||||||| fcc3157ab0
    files : string list;
=======
    files : string list;
    hidden : bool;
>>>>>>> 501-plus-upstream-main-9fa77db
  }

  let path t = t.path
  let files t = t.files
<<<<<<< HEAD
  let basenames t = List.map (fun { basename; _ } -> basename) t.files
  let hidden t = t.hidden
||||||| fcc3157ab0
=======
  let hidden t = t.hidden
>>>>>>> 501-plus-upstream-main-9fa77db

  let find t fn =
    List.find_map (fun { basename; path } ->
      if String.equal basename fn then
        Some path
      else
        None) t.files

<<<<<<< HEAD
  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search { basename; path } =
      if Misc.normalized_unit_filename basename = fn then
        Some path
||||||| fcc3157ab0
  let find_uncap t fn =
    let fn = String.uncapitalize_ascii fn in
    let search base =
      if String.uncapitalize_ascii base = fn then
        Some (Filename.concat t.path base)
=======
  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search base =
      if Misc.normalized_unit_filename base = fn then
        Some (Filename.concat t.path base)
>>>>>>> 501-plus-upstream-main-9fa77db
      else
        None
    in
    List.find_map search t.files

<<<<<<< HEAD
  let check ~hidden t =
    hidden = t.hidden && Directory_content_cache.check t.path
||||||| fcc3157ab0
  let create path =
    { path; files = Array.to_list (Directory_content_cache.read path) }
=======
  let create ~hidden path =
    { path; files = Array.to_list (Directory_content_cache.read path); hidden }
>>>>>>> 501-plus-upstream-main-9fa77db

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

let reset_visible () =
  assert (not Config.merlin || Local_store.is_bound ());
<<<<<<< HEAD
  Path_cache.reset ();
  hidden_dirs := [];
  visible_dirs := [];
||||||| fcc3157ab0
  STbl.clear !files;
  STbl.clear !files_uncap;
  dirs := [];
=======
  STbl.clear !visible_files;
  STbl.clear !visible_files_uncap;
  visible_dirs := []

let reset_hidden () =
  assert (not Config.merlin || Local_store.is_bound ());
  STbl.clear !hidden_files;
  STbl.clear !hidden_files_uncap;
  hidden_dirs := []

let reset ?(only_hidden = false) ?(only_visible = false) () =
  if not only_visible then reset_hidden ();
  if not only_hidden then reset_visible ();
>>>>>>> 501-plus-upstream-main-9fa77db
  auto_include_callback := no_auto_include

<<<<<<< HEAD
let get_visible () = List.rev !visible_dirs
||||||| fcc3157ab0
let get () = List.rev !dirs
let get_paths () = List.rev_map Dir.path !dirs
=======
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
>>>>>>> 501-plus-upstream-main-9fa77db

<<<<<<< HEAD
let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)
||||||| fcc3157ab0
(* Optimized version of [add] below, for use in [init] and [remove_dir]: since
   we are starting from an empty cache, we can avoid checking whether a unit
   name already exists in the cache simply by adding entries in reverse
   order. *)
let prepend_add dir =
  List.iter (fun base ->
      let fn = Filename.concat dir.Dir.path base in
      STbl.replace !files base fn;
      STbl.replace !files_uncap (String.uncapitalize_ascii base) fn
    ) dir.Dir.files
=======
(* Optimized version of [add] below, for use in [init] and [remove_dir]: since
   we are starting from an empty cache, we can avoid checking whether a unit
   name already exists in the cache simply by adding entries in reverse
   order. *)
let prepend_add dir =
  List.iter (fun base ->
      let fn = Filename.concat dir.Dir.path base in
      let filename = Misc.normalized_unit_filename base in
      if dir.Dir.hidden then begin
        STbl.replace !hidden_files base fn;
        STbl.replace !hidden_files_uncap filename fn
      end else begin
        STbl.replace !visible_files base fn;
        STbl.replace !visible_files_uncap filename fn
      end
    ) dir.Dir.files
>>>>>>> 501-plus-upstream-main-9fa77db

<<<<<<< HEAD
type paths =
  { visible : string list;
    hidden : string list }

let get_paths () =
  { visible = List.rev_map Dir.path !visible_dirs;
    hidden = List.rev_map Dir.path !hidden_dirs }

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let init ~auto_include ~visible ~hidden =
||||||| fcc3157ab0
let init ~auto_include l =
=======
let init ~auto_include ~visible ~hidden =
>>>>>>> 501-plus-upstream-main-9fa77db
  assert (not Config.merlin || Local_store.is_bound ());
  let rec loop_changed ~hidden acc = function
    | [] -> Some acc
    | new_path :: new_rest ->
<<<<<<< HEAD
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| fcc3157ab0
      loop_changed (Dir.create new_path :: acc) new_rest
=======
      loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> 501-plus-upstream-main-9fa77db
  in
  let rec loop_unchanged ~hidden acc new_paths old_dirs =
    match new_paths, old_dirs with
    | [], [] -> None
    | new_path :: new_rest, [] ->
<<<<<<< HEAD
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| fcc3157ab0
      loop_changed (Dir.create new_path :: acc) new_rest
=======
      loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> 501-plus-upstream-main-9fa77db
    | [], _ :: _ -> Some acc
    | new_path :: new_rest, old_dir :: old_rest ->
      if String.equal new_path (Dir.path old_dir) then begin
<<<<<<< HEAD
        if Dir.check ~hidden old_dir then begin
          loop_unchanged ~hidden (old_dir :: acc) new_rest old_rest
||||||| fcc3157ab0
        if Dir.check old_dir then begin
          loop_unchanged (old_dir :: acc) new_rest old_rest
=======
        if Dir.check old_dir then begin
          loop_unchanged ~hidden (old_dir :: acc) new_rest old_rest
>>>>>>> 501-plus-upstream-main-9fa77db
        end else begin
<<<<<<< HEAD
          loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| fcc3157ab0
          loop_changed (Dir.create new_path :: acc) new_rest
=======
          loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> 501-plus-upstream-main-9fa77db
        end
      end else begin
<<<<<<< HEAD
        loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| fcc3157ab0
        loop_changed (Dir.create new_path :: acc) new_rest
=======
        loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> 501-plus-upstream-main-9fa77db
      end
  in
<<<<<<< HEAD
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
||||||| fcc3157ab0
  match loop_unchanged [] l (List.rev !dirs) with
=======
  let () =
    match loop_unchanged ~hidden:false [] visible (List.rev !visible_dirs) with
    | None -> ()
    | Some new_dirs ->
      reset ~only_visible:true ();
      visible_dirs := new_dirs;
      List.iter prepend_add new_dirs;
      auto_include_callback := auto_include
  in
  match loop_unchanged ~hidden:true [] hidden (List.rev !hidden_dirs) with
>>>>>>> 501-plus-upstream-main-9fa77db
  | None -> ()
<<<<<<< HEAD
  | Some (new_visible, new_hidden) ->
    reset ();
    visible_dirs := new_visible;
    hidden_dirs := new_hidden;
    List.iter Path_cache.prepend_add new_hidden;
    List.iter Path_cache.prepend_add new_visible;
||||||| fcc3157ab0
  | Some new_dirs ->
    reset ();
    dirs := new_dirs;
    List.iter prepend_add new_dirs;
=======
  | Some new_dirs ->
    reset ~only_hidden:true ();
    hidden_dirs := new_dirs;
    List.iter prepend_add new_dirs;
>>>>>>> 501-plus-upstream-main-9fa77db
    auto_include_callback := auto_include

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
  let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
  if    List.compare_lengths visible !visible_dirs <> 0
     || List.compare_lengths hidden !hidden_dirs <> 0 then begin
    reset ();
<<<<<<< HEAD
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter Path_cache.prepend_add hidden;
    List.iter Path_cache.prepend_add visible
||||||| fcc3157ab0
    List.iter prepend_add new_dirs;
    dirs := new_dirs
=======
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter prepend_add hidden;
    List.iter prepend_add visible
>>>>>>> 501-plus-upstream-main-9fa77db
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present, in order to enforce
   left-to-right precedence. *)
let add (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
<<<<<<< HEAD
  Path_cache.add dir;
  if (Dir.hidden dir) then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs
||||||| fcc3157ab0
  List.iter
    (fun base ->
       let fn = Filename.concat dir.Dir.path base in
       if not (STbl.mem !files base) then
         STbl.replace !files base fn;
       let ubase = String.uncapitalize_ascii base in
       if not (STbl.mem !files_uncap ubase) then
         STbl.replace !files_uncap ubase fn)
    dir.Dir.files;
  dirs := dir :: !dirs
=======
  let update base fn visible_files hidden_files =
    if dir.hidden && not (STbl.mem !hidden_files base) then
      STbl.replace !hidden_files base fn
    else if not (STbl.mem !visible_files base) then
      STbl.replace !visible_files base fn
  in
  List.iter
    (fun base ->
       let fn = Filename.concat dir.Dir.path base in
       update base fn visible_files hidden_files;
       let ubase = Misc.normalized_unit_filename base in
       update ubase fn visible_files_uncap hidden_files_uncap)
    dir.files;
  if dir.hidden then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs
>>>>>>> 501-plus-upstream-main-9fa77db

let append_dir = add

let add_dir ~hidden dir = add (Dir.create ~hidden dir)

(* Add the directory at the start of load path - so basenames are
   unconditionally added. *)
let prepend_dir (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
<<<<<<< HEAD
  Path_cache.prepend_add dir;
  if (Dir.hidden dir) then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]
||||||| fcc3157ab0
  prepend_add dir;
  dirs := !dirs @ [dir]
=======
  prepend_add dir;
  if dir.hidden then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]
>>>>>>> 501-plus-upstream-main-9fa77db

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

type visibility = Visible | Hidden

let find_file_in_cache fn visible_files hidden_files =
  try (STbl.find !visible_files fn, Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Hidden)

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
<<<<<<< HEAD
      fst (Path_cache.find ~uncap:false fn)
||||||| fcc3157ab0
      STbl.find !files fn
=======
      fst (find_file_in_cache fn visible_files hidden_files)
>>>>>>> 501-plus-upstream-main-9fa77db
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
<<<<<<< HEAD
      Path_cache.find ~uncap:true fn
||||||| fcc3157ab0
      STbl.find !files_uncap (String.uncapitalize_ascii fn)
=======
      find_file_in_cache (Misc.normalized_unit_filename fn)
        visible_files_uncap hidden_files_uncap
>>>>>>> 501-plus-upstream-main-9fa77db
    else
      try
        (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
<<<<<<< HEAD
    let fn_uncap = String.uncapitalize_ascii fn in
    (!auto_include_callback Dir.find_normalized fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)
||||||| fcc3157ab0
    let fn_uncap = String.uncapitalize_ascii fn in
    !auto_include_callback Dir.find_uncap fn_uncap
=======
    let fn_uncap = Misc.normalized_unit_filename fn in
    (!auto_include_callback Dir.find_normalized fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)

(* Merlin: expose standard reset function *)
let reset () = reset ()
>>>>>>> 501-plus-upstream-main-9fa77db
