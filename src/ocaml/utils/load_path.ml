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

module STbl = Misc.String.Tbl

(* Mapping from basenames to full filenames *)
type registry = string STbl.t

let files : registry ref = s_table STbl.create 42
let files_uncap : registry ref = s_table STbl.create 42

module Dir = struct
  type t = {
    path : string;
    files : string list;
  }

  let path t = t.path
  let files t = t.files

  let create path =
    { path; files = Array.to_list (Directory_content_cache.read path) }

  let check t = Directory_content_cache.check t.path

end

let dirs = s_ref []

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  files := SMap.empty;
  files_uncap := SMap.empty;
  dirs := []

let get () = List.rev !dirs
let get_paths () = List.rev_map Dir.path !dirs

let add_to_maps fn basenames files files_uncap =
  List.fold_left (fun (files, files_uncap) base ->
      let fn = fn base in
      SMap.add base fn files,
      SMap.add (String.uncapitalize_ascii base) fn files_uncap
    ) (files, files_uncap) basenames

(* Optimized version of [add] below, for use in [init] and [remove_dir]: since
   we are starting from an empty cache, we can avoid checking whether a unit
   name already exists in the cache simply by adding entries in reverse
   order. *)
let add dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path)
      dir.Dir.files !files !files_uncap
  in
  files := new_files;
  files_uncap := new_files_uncap

let init l =
  assert (not Config.merlin || Local_store.is_bound ());
  let rec loop_changed acc = function
    | [] -> Some acc
    | new_path :: new_rest ->
      loop_changed (Dir.create new_path :: acc) new_rest
  in
  let rec loop_unchanged acc new_paths old_dirs =
    match new_paths, old_dirs with
    | [], [] -> None
    | new_path :: new_rest, [] ->
      loop_changed (Dir.create new_path :: acc) new_rest
    | [], _ :: _ -> Some acc
    | new_path :: new_rest, old_dir :: old_rest ->
      if String.equal new_path (Dir.path old_dir) then begin
        if Dir.check old_dir then begin
          loop_unchanged (old_dir :: acc) new_rest old_rest
        end else begin
          loop_changed (Dir.create new_path :: acc) new_rest
        end
      end else begin
        loop_changed (Dir.create new_path :: acc) new_rest
      end
  in
  match loop_unchanged [] l (List.rev !dirs) with
  | None -> ()
  | Some new_dirs ->
    reset ();
    dirs := new_dirs;
    List.iter prepend_add new_dirs

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_dirs = List.filter (fun d -> Dir.path d <> dir) !dirs in
  if List.compare_lengths new_dirs !dirs <> 0 then begin
    reset ();
    List.iter add new_dirs;
    dirs := new_dirs
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present in the cache, in
   order to enforce left-to-right precedence. *)
let add dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path) dir.Dir.files
      SMap.empty SMap.empty
  in
  let first _ fn _ = Some fn in
  files := SMap.union first !files new_files;
  files_uncap := SMap.union first !files_uncap new_files_uncap;
  dirs := dir :: !dirs

let add_dir dir = add (Dir.create dir)

let is_basename fn = Filename.basename fn = fn

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  if is_basename fn then
    SMap.find fn !files
  else
    Misc.find_in_path (get_paths ()) fn

let find_uncap fn =
  assert (not Config.merlin || Local_store.is_bound ());
  if is_basename fn then
    SMap.find (String.uncapitalize_ascii fn) !files_uncap
  else
    Misc.find_in_path_uncap (get_paths ()) fn
