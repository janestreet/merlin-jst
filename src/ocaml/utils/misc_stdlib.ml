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

let to_string_of_print print =
  let to_string t =
    (* Implemented similarly to [Format.asprintf] *)
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    Format.pp_set_margin ppf Int.max_int;
    print ppf t;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  to_string

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

  let map3 f =
    let rec loop acc as_ bs cs = match as_, bs, cs with
      | [], [], [] -> List.rev acc
      | a :: as_, b :: bs, c :: cs -> loop (f a b c :: acc) as_ bs cs
      | _ -> invalid_arg "map3"
    in
    loop []

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

  let [@inline] merge_fold ~cmp ~left_only ~right_only ~both ~init t1 t2 =
    let rec loop acc t1 t2 =
      match t1, t2 with
      | [], [] -> acc
      | a :: t1', [] -> loop (left_only acc a) t1' []
      | [], b :: t2' -> loop (right_only acc b) [] t2'
      | a :: t1', b :: t2' ->
          match cmp a b with
          | 0 -> loop (both acc a b) t1' t2'
          | c when c < 0 -> loop (left_only acc a) t1' t2
          | _ -> loop (right_only acc b) t1 t2'
    in
    loop init t1 t2

  let [@inline] merge_iter ~cmp ~left_only ~right_only ~both t1 t2 =
      merge_fold t1 t2 ~cmp
        ~init:()
        ~left_only:(fun () a -> left_only a)
        ~right_only:(fun () b -> right_only b)
        ~both:(fun () a b -> both a b)
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

module Monad = struct
  module type Basic2 = sig
    (** Multi parameter monad. The second parameter gets unified across all the computation.
        This is used to encode monads working on a multi parameter data structure like
        ([('a,'b) result]). *)

    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val return : 'a -> ('a, _) t
  end

  module type S2 = sig
    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val return : 'a -> ('a, _) t
    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
    val ignore_m : (_, 'e) t -> (unit, 'e) t
    val all : ('a, 'e) t list -> ('a list, 'e) t
    val all_unit : (unit, 'e) t list -> (unit, 'e) t
  end

  module Make2 (X : Basic2) = struct
    include X

    let map f m =
      bind m (fun a -> return (f a))

    let join m = bind m Fun.id

    let ignore_m m = bind m (fun _ -> return ())

    let all ms =
      let rec loop acc = function
        | [] -> return (List.rev acc)
        | m :: ms -> bind m (fun a -> loop (a :: acc) ms)
      in
      loop [] ms

    let rec all_unit = function
      | [] -> return ()
      | m :: ms -> bind m (fun _ -> all_unit ms)
  end

  module Result = Make2(struct
      include Result
      let return = ok
    end)
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
