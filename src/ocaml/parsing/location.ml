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

open Lexing

type t = Warnings.loc =
  { loc_start: position; loc_end: position; loc_ghost: bool }

let compare_position : position -> position -> int =
  fun
    { pos_fname = pos_fname_1
    ; pos_lnum = pos_lnum_1
    ; pos_bol = pos_bol_1
    ; pos_cnum = pos_cnum_1
    }
    { pos_fname = pos_fname_2
    ; pos_lnum = pos_lnum_2
    ; pos_bol = pos_bol_2
    ; pos_cnum = pos_cnum_2
    }
  ->
    match String.compare pos_fname_1 pos_fname_2 with
    | 0 -> begin match Int.compare pos_lnum_1 pos_lnum_2 with
      | 0 -> begin match Int.compare pos_bol_1 pos_bol_2 with
        | 0 -> Int.compare pos_cnum_1 pos_cnum_2
        | i -> i
      end
      | i -> i
    end
    | i -> i
;;

let compare
      { loc_start = loc_start_1
      ; loc_end = loc_end_1
      ; loc_ghost = loc_ghost_1 }
      { loc_start = loc_start_2
      ; loc_end = loc_end_2
      ; loc_ghost = loc_ghost_2 }
  =
  match compare_position loc_start_1 loc_start_2 with
  | 0 -> begin match compare_position loc_end_1 loc_end_2 with
    | 0 -> Bool.compare loc_ghost_1 loc_ghost_2
    | i -> i
  end
  | i -> i
;;


let in_file = Warnings.ghost_loc_in_file

let none = in_file "_none_"
let is_none l = (l = none)

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }


let ghostify l =
  if l.loc_ghost
  then l
  else { l with loc_ghost = true }

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
}

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
}

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

let rhs_interval m n = {
  loc_start = Parsing.rhs_start_pos m;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)


let merge ?(ghost = true) locs =
  let hd, tl =
    match locs with
    | hd :: tl -> hd, tl
    | [] -> failwith "Compiler bug: Called [Location.merge] with an empty list"
  in
  List.fold_left
    (fun acc x ->
      let loc_start =
        if compare_position x.loc_start acc.loc_start < 0
        then x.loc_start else acc.loc_start
      in
      let loc_end =
        if compare_position x.loc_end acc.loc_end > 0
        then x.loc_end else acc.loc_end
      in
      let loc_ghost = x.loc_ghost || acc.loc_ghost in
      { loc_start; loc_end; loc_ghost })
    { hd with loc_ghost = hd.loc_ghost || ghost }
    tl

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none
let get_txt { txt } = txt
let get_loc { loc } = loc
let map f { txt; loc} = {txt = f txt; loc}
let compare_txt f { txt=t1 } { txt=t2 } = f t1 t2

(******************************************************************************)
(* Input info *)

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)
let input_phrase_buffer = ref (None : Buffer.t option)

(******************************************************************************)
(* Terminal info *)

(*
let status = ref Terminfo.Uninitialised

let setup_terminal () =
  if !status = Terminfo.Uninitialised then
    status := Terminfo.setup stdout
*)

(* The number of lines already printed after input.

   This is used by [highlight_terminfo] to identify the current position of the
   input in the terminal. This would not be possible without this information,
   since printing several warnings/errors adds text between the user input and
   the bottom of the terminal.

   We also use for {!is_first_report}, see below.
*)
let num_loc_lines = ref 0

(* We use [num_loc_lines] to determine if the report about to be
   printed is the first or a follow-up report of the current
   "batch" -- contiguous reports without user input in between, for
   example for the current toplevel phrase. We use this to print
   a blank line between messages of the same batch.
*)
let is_first_message () =
  !num_loc_lines = 0

(* This is used by the toplevel to reset [num_loc_lines] before each phrase *)
let reset () =
  num_loc_lines := 0

(* This is used by the toplevel *)
let echo_eof () =
  print_newline ();
  incr num_loc_lines

(* This is used by the toplevel and the report printers below. *)
let separate_new_message ppf =
  if not (is_first_message ()) then begin
    Format.pp_print_newline ppf ();
    incr num_loc_lines
  end

(* Code printing errors and warnings must be wrapped using this function, in
   order to update [num_loc_lines].

   [print_updating_num_loc_lines ppf f arg] is equivalent to calling [f ppf
   arg], and additionally updates [num_loc_lines]. *)
let print_updating_num_loc_lines ppf f arg =
  let open Format in
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_string str start len =
    let rec count i c =
      if i = start + len then c
      else if String.get str i = '\n' then count (succ i) (succ c)
      else count (succ i) c in
    num_loc_lines := !num_loc_lines + count start 0 ;
    out_functions.out_string str start len in
  pp_set_formatter_out_functions ppf
    { out_functions with out_string } ;
  f ppf arg ;
  pp_print_flush ppf ();
  pp_set_formatter_out_functions ppf out_functions

(*
let setup_tags () =
  Misc.Style.setup !Clflags.color
*)

(******************************************************************************)
(* Printing locations, e.g. 'File "foo.ml", line 3, characters 10-12' *)

let rewrite_absolute_path path =
  (*
  match Misc.get_build_path_prefix_map () with
  | None -> path
  | Some map -> Build_path_prefix_map.rewrite map path
  *)
  path

(*
let rewrite_find_first_existing path =
  match Misc.get_build_path_prefix_map () with
  | None ->
      if Sys.file_exists path then Some path
      else None
  | Some prefix_map ->
    match Build_path_prefix_map.rewrite_all prefix_map path with
    | [] ->
      if Sys.file_exists path then Some path
      else None
    | matches ->
      Some (List.find Sys.file_exists matches)

let rewrite_find_all_existing_dirs path =
  let ok path = Sys.file_exists path && Sys.is_directory path in
  match Misc.get_build_path_prefix_map () with
  | None ->
      if ok path then [path]
      else []
  | Some prefix_map ->
    match Build_path_prefix_map.rewrite_all prefix_map path with
    | [] ->
        if ok path then [path]
        else []
    | matches ->
      match (List.filter ok matches) with
      | [] -> raise Not_found
      | results -> results *)

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if (is_relative s) then (concat (Sys.getcwd ()) s) else s in
  let s = rewrite_absolute_path s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  (* if !Clflags.absname then absolute_path file else *) file

let print_filename ppf file =
  Format.pp_print_string ppf (show_filename file)

(* Best-effort printing of the text describing a location, of the form
   'File "foo.ml", line 3, characters 10-12'.

   Some of the information (filename, line number or characters numbers) in the
   location might be invalid; in which case we do not print it.
 *)
let print_loc ~capitalize_first ppf loc =
  (* setup_tags (); *)
  let file_valid = function
    | "_none_" ->
        (* This is a dummy placeholder, but we print it anyway to please editors
           that parse locations in error messages (e.g. Emacs). *)
        true
    | "" | "//toplevel//" -> false
    | _ -> true
  in
  let line_valid line = line > 0 in
  let chars_valid ~startchar ~endchar = startchar <> -1 && endchar <> -1 in

  let file =
    (* According to the comment in location.mli, if [pos_fname] is "", we must
       use [!input_name]. *)
    if loc.loc_start.pos_fname = "" then !input_name
    else loc.loc_start.pos_fname
  in
  let line = loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in

  let first = ref true in
  let capitalize s =
    if !first then (first := false;
                    if capitalize_first then String.capitalize_ascii s else s)
    else s in
  let comma () =
    if !first then () else Format.fprintf ppf ", " in

  Format.fprintf ppf "@{<loc>";

  if file_valid file then
    Format.fprintf ppf "%s \"%a\"" (capitalize "file") print_filename file;

  (* Print "line 1" in the case of a dummy line number. This is to please the
     existing setup of editors that parse locations in error messages (e.g.
     Emacs). *)
  comma ();
  Format.fprintf ppf "%s %i" (capitalize "line")
    (if line_valid line then line else 1);

  if chars_valid ~startchar ~endchar then (
    comma ();
    Format.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  );

  Format.fprintf ppf "@}"

let print_loc_in_lowercase = print_loc ~capitalize_first:false
let print_loc = print_loc ~capitalize_first:true

(* Print a comma-separated list of locations *)
let print_locs ppf locs =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
    print_loc ppf locs

(******************************************************************************)
(* An interval set structure; additionally, it stores user-provided information
   at interval boundaries.

   The implementation provided here is naive and assumes the number of intervals
   to be small, but the interface would allow for a more efficient
   implementation if needed.

   Note: the structure only stores maximal intervals (that therefore do not
   overlap).
*)

(*
module ISet : sig
  type 'a bound = 'a * int
  type 'a t
  (* bounds are included *)
  val of_intervals : ('a bound * 'a bound) list -> 'a t

  val mem : 'a t -> pos:int -> bool
  val find_bound_in : 'a t -> range:(int * int) -> 'a bound option

  val is_start : 'a t -> pos:int -> 'a option
  val is_end : 'a t -> pos:int -> 'a option

  val extrema : 'a t -> ('a bound * 'a bound) option
end
=
struct
  type 'a bound = 'a * int

  (* non overlapping intervals *)
  type 'a t = ('a bound * 'a bound) list

  let compare (fst1, snd1) (fst2, snd2) =
    match Int.compare fst1 fst2 with
    | 0 -> Int.compare snd1 snd2
    | i -> i

  let of_intervals intervals =
    let pos =
      List.map (fun ((a, x), (b, y)) ->
        if x > y then [] else [((a, x), `S); ((b, y), `E)]
      ) intervals
      |> List.flatten
      |> List.sort (fun ((_, x), k) ((_, y), k') ->
        (* Make `S come before `E so that consecutive intervals get merged
           together in the fold below *)
        let kn = function `S -> 0 | `E -> 1 in
        compare (x, kn k) (y, kn k'))
    in
    let nesting, acc =
      List.fold_left (fun (nesting, acc) (a, kind) ->
        match kind, nesting with
        | `S, `Outside -> `Inside (a, 0), acc
        | `S, `Inside (s, n) -> `Inside (s, n+1), acc
        | `E, `Outside -> assert false
        | `E, `Inside (s, 0) -> `Outside, ((s, a) :: acc)
        | `E, `Inside (s, n) -> `Inside (s, n-1), acc
      ) (`Outside, []) pos in
    assert (nesting = `Outside);
    List.rev acc

  let mem iset ~pos =
    List.exists (fun ((_, s), (_, e)) -> s <= pos && pos <= e) iset

  let find_bound_in iset ~range:(start, end_)  =
    try Some (
    List.find_map ~f:(fun ((a, x), (b, y)) ->
      if start <= x && x <= end_ then Some (a, x)
      else if start <= y && y <= end_ then Some (b, y)
      else None
    ) iset
    ) with Not_found -> None

  let is_start iset ~pos =
    try Some (
    List.find_map ~f:(fun ((a, x), _) ->
      if pos = x then Some a else None
    ) iset
    ) with Not_found -> None

  let is_end iset ~pos =
    try Some (
    List.find_map ~f:(fun (_, (b, y)) ->
      if pos = y then Some b else None
    ) iset
    ) with Not_found -> None

  let extrema iset =
    if iset = [] then None
    else Some (fst (List.hd iset), snd (List.hd (List.rev iset)))
end
*)


(* Highlight the location by printing it again.

   There are two different styles for highlighting errors in "dumb" mode,
   depending if the error fits on a single line or spans across several lines.

   For single-line errors,

     foo the_error bar

   gets displayed as follows, where X is the line number:

     X | foo the_error bar
             ^^^^^^^^^


   For multi-line errors,

     foo the_
     error bar

   gets displayed as:

     X1 | ....the_
     X2 | error....

   An ellipsis hides the middle lines of the multi-line error if it has more
   than [max_lines] lines.

   If [locs] is empty then this function is a no-op.
*)

(*
type input_line = {
  text : string;
  start_pos : int;
}
*)

(* Takes a list of lines with possibly missing line numbers.

   If the line numbers that are present are consistent with the number of lines
   between them, then infer the intermediate line numbers.

   This is not always the case, typically if lexer line directives are
   involved... *)
(*
let infer_line_numbers
    (lines: (int option * input_line) list):
  (int option * input_line) list
  =
  let (_, offset, consistent) =
    List.fold_left (fun (i, offset, consistent) (lnum, _) ->
      match lnum, offset with
      | None, _ -> (i+1, offset, consistent)
      | Some n, None -> (i+1, Some (n - i), consistent)
      | Some n, Some m -> (i+1, offset, consistent && n = m + i)
    ) (0, None, true) lines
  in
  match offset, consistent with
  | Some m, true ->
      List.mapi (fun i (_, line) -> (Some (m + i), line)) lines
  | _, _ ->
      lines
*)
(* [get_lines] must return the lines to highlight, given starting and ending
   positions.

   See [lines_around_from_current_input] below for an instantiation of
   [get_lines] that reads from the current input.
*)
(*
let highlight_quote ppf
    ~(get_lines: start_pos:position -> end_pos:position -> input_line list)
    ?(max_lines = 10)
    highlight_tag
    locs
  =
  let iset = ISet.of_intervals @@ List.filter_map (fun loc ->
    let s, e = loc.loc_start, loc.loc_end in
    if s.pos_cnum = -1 || e.pos_cnum = -1 then None
    else Some ((s, s.pos_cnum), (e, e.pos_cnum - 1))
  ) locs in
  match ISet.extrema iset with
  | None -> ()
  | Some ((leftmost, _), (rightmost, _)) ->
      let lines =
        get_lines ~start_pos:leftmost ~end_pos:rightmost
        |> List.map (fun ({ text; start_pos } as line) ->
          let end_pos = start_pos + String.length text - 1 in
          let line_nb =
            match ISet.find_bound_in iset ~range:(start_pos, end_pos) with
            | None -> None
            | Some (p, _) -> Some p.pos_lnum
          in
          (line_nb, line))
        |> infer_line_numbers
        |> List.map (fun (lnum, { text; start_pos }) ->
          (text,
           Option.fold ~some:Int.to_string ~none:"" lnum,
           start_pos))
      in
    Format.fprintf ppf "@[<v>";
    begin match lines with
    | [] | [("", _, _)] -> ()
    | [(line, line_nb, line_start_cnum)] ->
        (* Single-line error *)
        Format.fprintf ppf "%s | %s@," line_nb line;
        Format.fprintf ppf "%*s   " (String.length line_nb) "";
        (* Iterate up to [rightmost], which can be larger than the length of
           the line because we may point to a location after the end of the
           last token on the line, for instance:
           {[
             token
                       ^
             Did you forget ...
           ]} *)
        for i = 0 to rightmost.pos_cnum - line_start_cnum - 1 do
          let pos = line_start_cnum + i in
          if ISet.is_start iset ~pos <> None then
            Format.fprintf ppf "@{<%s>" highlight_tag;
          if ISet.mem iset ~pos then Format.pp_print_char ppf '^'
          else if i < String.length line then begin
            (* For alignment purposes, align using a tab for each tab in the
               source code *)
            if line.[i] = '\t' then Format.pp_print_char ppf '\t'
            else Format.pp_print_char ppf ' '
          end;
          if ISet.is_end iset ~pos <> None then
            Format.fprintf ppf "@}"
        done;
        Format.fprintf ppf "@}@,"
    | _ ->
        (* Multi-line error *)
        Misc.pp_two_columns ~sep:"|" ~max_lines ppf
        @@ List.map (fun (line, line_nb, line_start_cnum) ->
          let line = String.mapi (fun i car ->
            if ISet.mem iset ~pos:(line_start_cnum + i) then car else '.'
          ) line in
          (line_nb, line)
        ) lines
    end;
    Format.fprintf ppf "@]"
*)



(*
let lines_around
    ~(start_pos: position) ~(end_pos: position)
    ~(seek: int -> unit)
    ~(read_char: unit -> char option):
  input_line list
  =
  seek start_pos.pos_bol;
  let lines = ref [] in
  let bol = ref start_pos.pos_bol in
  let cur = ref start_pos.pos_bol in
  let b = Buffer.create 80 in
  let add_line () =
    if !bol < !cur then begin
      let text = Buffer.contents b in
      Buffer.clear b;
      lines := { text; start_pos = !bol } :: !lines;
      bol := !cur
    end
  in
  let rec loop () =
    if !bol >= end_pos.pos_cnum then ()
    else begin
      match read_char () with
      | None ->
          (* end of input *)
          add_line ()
      | Some c ->
          incr cur;
          match c with
          | '\r' -> loop ()
          | '\n' -> add_line (); loop ()
          | _ -> Buffer.add_char b c; loop ()
    end
  in
  loop ();
  List.rev !lines
*)

(*
(* Get lines from a file *)
let lines_around_from_file
    ~(start_pos: position) ~(end_pos: position)
    (filename: string):
  input_line list
  =
  try
    let cin = open_in_bin filename in
    let read_char () =
      try Some (input_char cin) with End_of_file -> None
    in
    let lines =
      lines_around ~start_pos ~end_pos ~seek:(seek_in cin) ~read_char
    in
    close_in cin;
    lines
  with Sys_error _ -> []

(* Attempt to get lines from the lexing buffer. *)
let lines_around_from_lexbuf
    ~(start_pos: position) ~(end_pos: position)
    (lb: lexbuf):
  input_line list
  =
  (* Converts a global position to one that is relative to the lexing buffer *)
  let rel n = n - lb.lex_abs_pos in
  if rel start_pos.pos_bol < 0 then begin
    (* Do nothing if the buffer does not contain the input (because it has been
       refilled while lexing it) *)
    []
  end else begin
    let pos = ref 0 in (* relative position *)
    let seek n = pos := rel n in
    let read_char () =
      if !pos >= lb.lex_buffer_len then (* end of buffer *) None
      else
        let c = Bytes.get lb.lex_buffer !pos in
        incr pos; Some c
    in
    lines_around ~start_pos ~end_pos ~seek ~read_char
  end
*)

(*
(* Attempt to get lines from the phrase buffer *)
let lines_around_from_phrasebuf
    ~(start_pos: position) ~(end_pos: position)
    (pb: Buffer.t):
  input_line list
  =
  let pos = ref 0 in
  let seek n = pos := n in
  let read_char () =
    if !pos >= Buffer.length pb then None
    else begin
      let c = Buffer.nth pb !pos in
      incr pos; Some c
    end
  in
  lines_around ~start_pos ~end_pos ~seek ~read_char
*)

(*
(* A [get_lines] function for [highlight_quote] that reads from the current
   input. *)
let lines_around_from_current_input ~start_pos ~end_pos =
  match !input_lexbuf, !input_phrase_buffer, !input_name with
  | _, Some pb, "//toplevel//" ->
      lines_around_from_phrasebuf pb ~start_pos ~end_pos
  | Some lb, _, _ ->
      lines_around_from_lexbuf lb ~start_pos ~end_pos
  | None, _, filename ->
      (* A situation where we have no input buffer and no phrase buffer
         is when the compiler is getting the binary AST directly as input. *)
      (* Be a bit defensive, and do not try to open one of the possible
         [!input_name] values that we know do not denote valid filenames. *)
      let file_valid = match filename with
        | "//toplevel//" | "_none_" | "" -> false
        | _ -> true
      in
      if file_valid
      then lines_around_from_file filename ~start_pos ~end_pos
      else []
*)

(******************************************************************************)
(* Reporting errors and warnings *)

type msg = (Format.formatter -> unit) loc

let msg ?(loc = none) fmt =
  Format.kdprintf (fun txt -> { loc; txt }) fmt

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type error_source = Lexer | Parser | Typer | Warning | Unknown | Env | Config

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
  source : error_source;
}

let loc_of_report { main; _ } = main.loc
let print_msg fmt msg = msg.txt fmt
let print_main fmt { main; _ } = print_msg fmt main
let print_sub_msg = print_msg


type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;

  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
  pp_submsgs : report_printer -> report ->
    Format.formatter -> msg list -> unit;
  pp_submsg : report_printer -> report ->
    Format.formatter -> msg -> unit;
  pp_submsg_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_submsg_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
}

(*
let is_dummy_loc loc =
  (* Fixme: this should be just [loc.loc_ghost] and the function should be
     inlined below. However, currently, the compiler emits in some places ghost
     locations with valid ranges that should still be printed. These locations
     should be made non-ghost -- in the meantime we just check if the ranges are
     valid. *)
  loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1
*)

(* It only makes sense to highlight (i.e. quote or underline the corresponding
   source code) locations that originate from the current input.

   As of now, this should only happen in the following cases:

   - if dummy locs or ghost locs leak out of the compiler or a buggy ppx;

   - more generally, if some code uses the compiler-libs API and feeds it
   locations that do not match the current values of [!Location.input_name],
   [!Location.input_lexbuf];

   - when calling the compiler on a .ml file that contains lexer line directives
   indicating an other file. This should happen relatively rarely in practice --
   in particular this is not what happens when using -pp or -ppx or a ppx
   driver.
*)
                                 (*
let is_quotable_loc loc =
  not (is_dummy_loc loc)
  && loc.loc_start.pos_fname = !input_name
  && loc.loc_end.pos_fname = !input_name

let error_style () =
  let open Misc.Error_style in
  match !Clflags.error_style with
  | Some Contextual | None -> Contextual
  | Some Short -> Short
                                 *)

let batch_mode_printer : report_printer =
  let pp_loc _self _report _ppf _loc =
    (*
    let tag = match report.kind with
      | Report_warning_as_error _
      | Report_alert_as_error _
      | Report_error -> "error"
      | Report_warning _
      | Report_alert _ -> "warning"
    in
    let highlight ppf loc =
      match error_style () with
      | Misc.Error_style.Contextual ->
          if is_quotable_loc loc then
            highlight_quote ppf
              ~get_lines:lines_around_from_current_input
              tag [loc]
      | Misc.Error_style.Short ->
          ()
    in
    Format.fprintf ppf "@[<v>%a:@ %a@]" print_loc loc highlight loc
    *)
    ()
  in
  let pp_txt ppf txt = Format.fprintf ppf "@[%t@]" txt in
  let pp self ppf report =
    (* setup_tags (); *)
    separate_new_message ppf;
    (* Make sure we keep [num_loc_lines] updated.
        The tabulation box is here to give submessage the option
        to be aligned with the main message box
    *)
    print_updating_num_loc_lines ppf (fun ppf () ->
      Format.fprintf ppf "@[<v>%a%a%a: %a%a%a%a@]@."
      Format.pp_open_tbox ()
      (self.pp_main_loc self report) report.main.loc
      (self.pp_report_kind self report) report.kind
      Format.pp_set_tab ()
      (self.pp_main_txt self report) report.main.txt
      (self.pp_submsgs self report) report.sub
      Format.pp_close_tbox ()
    ) ()
  in
  let pp_report_kind _self _ ppf = function
    | Report_error -> Format.fprintf ppf "@{<error>Error@}"
    | Report_warning w -> Format.fprintf ppf "@{<warning>Warning@} %s" w
    | Report_warning_as_error w ->
        Format.fprintf ppf "@{<error>Error@} (warning %s)" w
    | Report_alert w -> Format.fprintf ppf "@{<warning>Alert@} %s" w
    | Report_alert_as_error w ->
        Format.fprintf ppf "@{<error>Error@} (alert %s)" w
  in
  let pp_main_loc self report ppf loc =
    pp_loc self report ppf loc
  in
  let pp_main_txt _self _ ppf txt =
    pp_txt ppf txt
  in
  let pp_submsgs self report ppf msgs =
    List.iter (fun msg ->
      Format.fprintf ppf "@,%a" (self.pp_submsg self report) msg
    ) msgs
  in
  let pp_submsg self report ppf { loc; txt } =
    Format.fprintf ppf "@[%a  %a@]"
      (self.pp_submsg_loc self report) loc
      (self.pp_submsg_txt self report) txt
  in
  let pp_submsg_loc self report ppf loc =
    if not loc.loc_ghost then
      pp_loc self report ppf loc
  in
  let pp_submsg_txt _self _ ppf loc =
    pp_txt ppf loc
  in
  { pp; pp_report_kind; pp_main_loc; pp_main_txt;
    pp_submsgs; pp_submsg; pp_submsg_loc; pp_submsg_txt }

(*
let terminfo_toplevel_printer (lb: lexbuf): report_printer =
  let pp self ppf err =
    setup_tags ();
    (* Highlight all toplevel locations of the report, instead of displaying
       the main location. Do it now instead of in [pp_main_loc], to avoid
       messing with Format boxes. *)
    let sub_locs = List.map (fun { loc; _ } -> loc) err.sub in
    let all_locs = err.main.loc :: sub_locs in
    let locs_highlighted = List.filter is_quotable_loc all_locs in
    highlight_terminfo lb ppf locs_highlighted;
    batch_mode_printer.pp self ppf err
  in
  let pp_main_loc _ _ _ _ = () in
  let pp_submsg_loc _ _ ppf loc =
    if not loc.loc_ghost then
      Format.fprintf ppf "%a:@ " print_loc loc in
  { batch_mode_printer with pp; pp_main_loc; pp_submsg_loc }

let best_toplevel_printer () =
  setup_terminal ();
  match !status, !input_lexbuf with
  | Terminfo.Good_term, Some lb ->
      terminfo_toplevel_printer lb
  | _, _ ->
      batch_mode_printer
*)

(* Creates a printer for the current input *)
let default_report_printer () : report_printer =
  batch_mode_printer

let report_printer = ref default_report_printer

let print_report ppf report =
  let printer = !report_printer () in
  printer.pp printer ppf report

(******************************************************************************)
(* Reporting errors *)

type error = report

let report_error ppf err =
  print_report ppf err

let mkerror loc sub txt source =
  { kind = Report_error; main = { loc; txt }; sub; source }

let errorf ?(loc = none) ?(sub = []) ?(source=Typer) =
  Format.kdprintf (fun msg -> mkerror loc sub msg source)

let error ?(loc = none) ?(sub = []) ?(source=Typer) msg_str =
  mkerror loc sub (fun ppf -> Format.pp_print_string ppf msg_str) source

let error_of_printer ?(loc = none) ?(sub = []) ?(source=Typer) pp x =
  mkerror loc sub (fun ppf -> pp ppf x) source

let error_of_printer_file ?source print x =
  error_of_printer ?source ~loc:(in_file !input_name) print x

(******************************************************************************)
(* Reporting warnings: generating a report from a warning number using the
   information in [Warnings] + convenience functions. *)

let default_warning_alert_reporter ?(source = Typer) report mk (loc: t) w : report option =
  match report w with
  | `Inactive -> None
  | `Active { Warnings.id; message; is_error; sub_locs } ->
      let msg_of_str str = fun ppf -> Format.pp_print_string ppf str in
      let kind = mk is_error id in
      let main = { loc; txt = msg_of_str message } in
      let sub = List.map (fun (loc, sub_message) ->
        { loc; txt = msg_of_str sub_message }
      ) sub_locs in
      Some { kind; main; sub; source }


let default_warning_reporter =
  default_warning_alert_reporter
    Warnings.report
    (fun is_error id ->
       if is_error then Report_warning_as_error id
       else Report_warning id
    )

let warning_reporter = ref default_warning_reporter
let report_warning loc w = !warning_reporter loc w

let formatter_for_warnings = ref Format.err_formatter

let print_warning loc ppf w =
  match report_warning loc w with
  | None -> ()
  | Some report -> print_report ppf report

let prerr_warning_ref =
  ref (fun loc w -> print_warning loc !formatter_for_warnings w)
let prerr_warning loc w = !prerr_warning_ref loc w

let default_alert_reporter =
  default_warning_alert_reporter
    Warnings.report_alert
    (fun is_error id ->
       if is_error then Report_alert_as_error id
       else Report_alert id
    )

let alert_reporter = ref default_alert_reporter
let report_alert loc w = !alert_reporter loc w

let print_alert loc ppf w =
  match report_alert loc w with
  | None -> ()
  | Some report -> print_report ppf report

let prerr_alert_ref =
  ref (fun loc w -> print_alert loc !formatter_for_warnings w)

let prerr_alert loc w = !prerr_alert_ref loc w

let alert ?(def = none) ?(use = none) ~kind loc message =
  prerr_alert loc {Warnings.kind; message; def; use}

let deprecated ?def ?use loc message =
  alert ?def ?use ~kind:"deprecated" loc message


module Style = Misc.Style

let auto_include_alert lib =
  let message = Format.asprintf "\
    OCaml's lib directory layout changed in 5.0. The %a subdirectory has been \
    automatically added to the search path, but you should add %a to the \
    command-line to silence this alert (e.g. by adding %a to the list of \
    libraries in your dune file, or adding %a to your %a file for \
    ocamlbuild, or using %a for ocamlfind)."
      Style.inline_code lib
      Style.inline_code ("-I +" ^lib)
      Style.inline_code lib
      Style.inline_code ("use_"^lib)
      Style.inline_code "_tags"
      Style.inline_code ("-package " ^ lib) in
  let alert =
    {Warnings.kind="ocaml_deprecated_auto_include"; use=none; def=none;
     message = Format.asprintf "@[@\n%a@]" Format.pp_print_text message}
  in
  prerr_alert none alert

let deprecated_script_alert program =
  let message = Format.asprintf "\
    Running %a where the first argument is an implicit basename with no \
    extension (e.g. %a) is deprecated. Either rename the script \
    (%a) or qualify the basename (%a)"
      Style.inline_code program
      Style.inline_code (program ^ " script-file")
      Style.inline_code (program ^ " script-file.ml")
      Style.inline_code (program ^ " ./script-file")
  in
  let alert =
    {Warnings.kind="ocaml_deprecated_cli"; use=none; def=none;
     message = Format.asprintf "@[@\n%a@]" Format.pp_print_text message}
  in
  prerr_alert none alert

(******************************************************************************)
(* Reporting errors on exceptions *)

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

exception Already_displayed_error = Warnings.Errors

let error_of_exn exn =
  match exn with
  | Already_displayed_error -> Some `Already_displayed
  | _ ->
     let rec loop = function
       | [] -> None
       | f :: rest ->
          match f exn with
          | Some error -> Some (`Ok error)
          | None -> loop rest
     in
     loop !error_of_exn

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name) "I/O error: %s" msg)
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let report_exception ppf exn =
  let rec loop n exn =
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> report_error ppf err
    | exception exn when n > 0 -> loop (n-1) exn
  in
  loop 5 exn

exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(source = Typer)=
  Format.kdprintf (fun txt -> raise (Error (mkerror loc sub txt source)))

let todo_overwrite_not_implemented ?(kind = "") t =
  alert ~kind t "Overwrite not implemented.";
  assert false
