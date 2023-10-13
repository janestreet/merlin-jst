(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Option = struct
  type 'a t = 'a option

  let first_some a b = match a with
    | Some _ -> a
    | None -> b ()

  let print print_contents ppf t =
    match t with
    | None -> Format.pp_print_string ppf "None"
    | Some contents ->
      Format.fprintf ppf "@[(Some@ %a)@]" print_contents contents
end

module String = struct
  include String
  module Set = Set.Make(String)
  module Map = Map.Make(String)
  module Tbl = Hashtbl.Make(struct
    include String
    let hash = Hashtbl.hash
  end)

  let for_all f t =
    let len = String.length t in
    let rec loop i =
      i = len || (f t.[i] && loop (i + 1))
    in
    loop 0

  let print ppf t =
    Format.pp_print_string ppf t

  let begins_with ?(from = 0) str ~prefix =
    let rec helper idx =
      if idx < 0 then true
      else
        String.get str (from + idx) = String.get prefix idx && helper (idx-1)
    in
    let n = String.length str in
    let m = String.length prefix in
    if n >= from + m then helper (m-1) else false

  let split_on_string str ~split_on =
    let n = String.length str in
    let m = String.length split_on in
    let rec helper acc last_idx idx =
      if idx = n then
        let cur = String.sub str last_idx (idx - last_idx) in
        List.rev (cur :: acc)
      else if begins_with ~from:idx str ~prefix:split_on then
        let cur = String.sub str last_idx (idx - last_idx) in
        helper (cur :: acc) (idx + m) (idx + m)
      else
        helper acc last_idx (idx + 1)
    in
    helper [] 0 0

  let split_on_chars str ~split_on:chars =
    let rec helper chars_left s acc =
      match chars_left with
      | [] -> s :: acc
      | c :: cs ->
        List.fold_right (helper cs) (String.split_on_char c s) acc
    in
    helper chars str []

  let split_last_exn str ~split_on =
    let n = String.length str in
    let ridx = String.rindex str split_on in
    String.sub str 0 ridx, String.sub str (ridx + 1) (n - ridx - 1)

  let starts_with ~prefix s =
    let len_s = length s
    and len_pre = length prefix in
    let rec aux i =
      if i = len_pre then true
      else if unsafe_get s i <> unsafe_get prefix i then false
      else aux (i + 1)
    in len_s >= len_pre && aux 0

  let ends_with ~suffix s =
    let len_s = length s
    and len_suf = length suffix in
    let diff = len_s - len_suf in
    let rec aux i =
      if i = len_suf then true
      else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
      else aux (i + 1)
    in diff >= 0 && aux 0
end
