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
  val find_uncap : t -> string -> string option

  val check : hidden:bool -> t -> bool
end = struct
  type entry = {
    basename : string;
    path : string
  }
||||||| 7b73c6aa3
let files : registry ref = s_table STbl.create 42
let files_uncap : registry ref = s_table STbl.create 42
=======
let visible_files : registry ref = s_table STbl.create 42
let visible_files_uncap : registry ref = s_table STbl.create 42

let hidden_files : registry ref = s_table STbl.create 42
let hidden_files_uncap : registry ref = s_table STbl.create 42
>>>>>>> upstream/main

  type t = {
    path : string;
<<<<<<< HEAD
    files : entry list;
    hidden : bool
||||||| 7b73c6aa3
    files : string list;
=======
    files : string list;
    hidden : bool;
>>>>>>> upstream/main
  }

  let path t = t.path
  let files t = t.files
<<<<<<< HEAD
  let basenames t = List.map (fun { basename; _ } -> basename) t.files
  let hidden t = t.hidden
||||||| 7b73c6aa3
=======
  let hidden t = t.hidden
>>>>>>> upstream/main

<<<<<<< HEAD
  let find t fn =
    List.find_map (fun { basename; path } ->
      if String.equal basename fn then
        Some path
      else
        None) t.files
||||||| 7b73c6aa3
  let create path =
    { path; files = Array.to_list (Directory_content_cache.read path) }
=======
  let find t fn =
    if List.mem fn t.files then
      Some (Filename.concat t.path fn)
    else
      None

  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search base =
      if Misc.normalized_unit_filename base = fn then
        Some (Filename.concat t.path base)
      else
        None
    in
    List.find_map search t.files

  let create ~hidden path =
    { path; files = Array.to_list (Directory_content_cache.read path); hidden }
>>>>>>> upstream/main

  let find_uncap t fn =
    let fn = String.uncapitalize_ascii fn in
    let search { basename; path } =
      if String.uncapitalize_ascii basename = fn then
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

<<<<<<< HEAD
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
          STbl.replace !hidden_files_uncap (String.uncapitalize_ascii base) fn
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
         let ubase = String.uncapitalize_ascii base in
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
||||||| 7b73c6aa3
let dirs = s_ref []
=======
type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string
>>>>>>> upstream/main

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
  auto_include_callback := no_auto_include
||||||| 7b73c6aa3
  STbl.clear !files;
  STbl.clear !files_uncap;
  dirs := []
=======
  STbl.clear !visible_files;
  STbl.clear !visible_files_uncap;
  visible_dirs := []
>>>>>>> upstream/main

<<<<<<< HEAD
let get_visible () = List.rev !visible_dirs
||||||| 7b73c6aa3
let get () = List.rev !dirs
let get_paths () = List.rev_map Dir.path !dirs
=======
let reset_hidden () =
  assert (not Config.merlin || Local_store.is_bound ());
  STbl.clear !hidden_files;
  STbl.clear !hidden_files_uncap;
  hidden_dirs := []

let reset ?(only_hidden = false) ?(only_visible = false) () =
  if not only_visible then reset_hidden ();
  if not only_hidden then reset_visible ();
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
>>>>>>> upstream/main

<<<<<<< HEAD
let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)
||||||| 7b73c6aa3
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
>>>>>>> upstream/main

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
||||||| 7b73c6aa3
let init l =
=======
let init ~auto_include ~visible ~hidden =
>>>>>>> upstream/main
  assert (not Config.merlin || Local_store.is_bound ());
  let rec loop_changed ~hidden acc = function
    | [] -> Some acc
    | new_path :: new_rest ->
<<<<<<< HEAD
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| 7b73c6aa3
      loop_changed (Dir.create new_path :: acc) new_rest
=======
      loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> upstream/main
  in
  let rec loop_unchanged ~hidden acc new_paths old_dirs =
    match new_paths, old_dirs with
    | [], [] -> None
    | new_path :: new_rest, [] ->
<<<<<<< HEAD
      loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| 7b73c6aa3
      loop_changed (Dir.create new_path :: acc) new_rest
=======
      loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> upstream/main
    | [], _ :: _ -> Some acc
    | new_path :: new_rest, old_dir :: old_rest ->
      if String.equal new_path (Dir.path old_dir) then begin
<<<<<<< HEAD
        if Dir.check ~hidden old_dir then begin
          loop_unchanged ~hidden (old_dir :: acc) new_rest old_rest
||||||| 7b73c6aa3
        if Dir.check old_dir then begin
          loop_unchanged (old_dir :: acc) new_rest old_rest
=======
        if Dir.check old_dir then begin
          loop_unchanged ~hidden (old_dir :: acc) new_rest old_rest
>>>>>>> upstream/main
        end else begin
<<<<<<< HEAD
          loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| 7b73c6aa3
          loop_changed (Dir.create new_path :: acc) new_rest
=======
          loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> upstream/main
        end
      end else begin
<<<<<<< HEAD
        loop_changed ~hidden (Dir.create new_path ~hidden :: acc) new_rest
||||||| 7b73c6aa3
        loop_changed (Dir.create new_path :: acc) new_rest
=======
        loop_changed ~hidden (Dir.create ~hidden new_path :: acc) new_rest
>>>>>>> upstream/main
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
||||||| 7b73c6aa3
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
>>>>>>> upstream/main
  | None -> ()
<<<<<<< HEAD
  | Some (new_visible, new_hidden) ->
    reset ();
    visible_dirs := new_visible;
    hidden_dirs := new_hidden;
    List.iter Path_cache.prepend_add new_hidden;
    List.iter Path_cache.prepend_add new_visible;
    auto_include_callback := auto_include
||||||| 7b73c6aa3
  | Some new_dirs ->
    reset ();
    dirs := new_dirs;
    List.iter prepend_add new_dirs
=======
  | Some new_dirs ->
    reset ~only_hidden:true ();
    hidden_dirs := new_dirs;
    List.iter prepend_add new_dirs;
    auto_include_callback := auto_include
>>>>>>> upstream/main

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
||||||| 7b73c6aa3
    List.iter prepend_add new_dirs;
    dirs := new_dirs
=======
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter prepend_add hidden;
    List.iter prepend_add visible
>>>>>>> upstream/main
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
||||||| 7b73c6aa3
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
>>>>>>> upstream/main

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
||||||| 7b73c6aa3
  prepend_add dir;
  dirs := !dirs @ [dir]
=======
  prepend_add dir;
  if dir.hidden then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]
>>>>>>> upstream/main

let is_basename fn = Filename.basename fn = fn

<<<<<<< HEAD
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

||||||| 7b73c6aa3
=======
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

>>>>>>> upstream/main
let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
<<<<<<< HEAD
  try
    if is_basename fn && not !Sys.interactive then
      fst (Path_cache.find ~uncap:false fn)
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn
||||||| 7b73c6aa3
  if is_basename fn && not !Sys.interactive then
    STbl.find !files fn
  else
    Misc.find_in_path (get_paths ()) fn
=======
  try
    if is_basename fn && not !Sys.interactive then
      fst (find_file_in_cache fn visible_files hidden_files)
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn
>>>>>>> upstream/main

<<<<<<< HEAD
let find_uncap_with_visibility fn =
||||||| 7b73c6aa3
let find_uncap fn =
=======
let find_normalized_with_visibility fn =
>>>>>>> upstream/main
  assert (not Config.merlin || Local_store.is_bound ());
<<<<<<< HEAD
  try
    if is_basename fn && not !Sys.interactive then
      Path_cache.find ~uncap:true fn
    else
      try
        (Misc.find_in_path_uncap (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_uncap (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
    let fn_uncap = String.uncapitalize_ascii fn in
    (!auto_include_callback Dir.find_uncap fn_uncap, Visible)

let find_uncap fn = fst (find_uncap_with_visibility fn)
||||||| 7b73c6aa3
  if is_basename fn && not !Sys.interactive then
    STbl.find !files_uncap (String.uncapitalize_ascii fn)
  else
    Misc.find_in_path_uncap (get_paths ()) fn
=======
  try
    if is_basename fn && not !Sys.interactive then
      find_file_in_cache (Misc.normalized_unit_filename fn)
        visible_files_uncap hidden_files_uncap
    else
      try
        (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
    let fn_uncap = Misc.normalized_unit_filename fn in
    (!auto_include_callback Dir.find_normalized fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)

(* Merlin: expose standard reset function *)
let reset () = reset ()
>>>>>>> upstream/main
