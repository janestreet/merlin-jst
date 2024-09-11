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

let pp_parens_if condition printer ppf arg =
  Format.fprintf ppf "%s%a%s"
    (if condition then "(" else "")
    printer arg
    (if condition then ")" else "")

let pp_nested_list ~nested ~pp_element ~pp_sep ppf arg =
  Format.fprintf ppf "@[%a@]"
    (pp_parens_if nested
       (Format.pp_print_list ~pp_sep (pp_element ~nested:true)))
    arg

module List = struct
  include List (* for references to Stdlib.List later in this file *)

  let map_option f l =
    let rec aux l acc =
      match l with
      | [] -> Some (List.rev acc)
      | x :: xs ->
        match f x with
        | None -> None
        | Some x -> aux xs (x :: acc)
    in
    aux l []

  let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

  let rec iter_until_error ~f l =
      match l with
      | [] -> Ok ()
      | x :: xs ->
        match f x with
        | Ok () -> iter_until_error ~f xs
        | Error _ as e -> e
end

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

module Int = struct
  include Int
  let min (a : int) (b : int) = min a b
  let max (a : int) (b : int) = max a b
end

let format_as_unboxed_literal s =
  if String.starts_with ~prefix:"-" s
  then "-#" ^ (String.sub s 1 (String.length s - 1))
  else "#" ^ s

module Le_result = struct
  type t =
    | Equal
    | Less
    | Not_le

  let combine sr1 sr2 =
    match sr1, sr2 with
    | Equal, Equal -> Equal
    | Equal, Less | Less, Equal | Less, Less -> Less
    | Not_le, _ | _, Not_le -> Not_le

  let combine_list ts = List.fold_left combine Equal ts

  let is_le = function
    | Equal -> true
    | Less -> true
    | Not_le -> false

  let is_equal = function
    | Equal -> true
    | Less | Not_le -> false
end

(*********************************************)
(* Fancy types *)

type (_, _) eq = Refl : ('a, 'a) eq

module type T1 = sig
  type 'a t
end
