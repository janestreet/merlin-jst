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

module CamlList = List
module CamlString = String

open Std

(* Errors *)

exception Fatal_error of string * Printexc.raw_backtrace

let () = Printexc.register_printer (function
    | Fatal_error (msg, bt) ->
      Some (Printf.sprintf "Fatal error: %s\n%s"
              msg (Printexc.raw_backtrace_to_string bt))
    | _ -> None
  )

let fatal_error msg =
  raise (Fatal_error (msg, Printexc.get_callstack 50))

let fatal_errorf fmt =
  (*Format.kasprintf is not available in 4.02.3 *)
  (*Format.kasprintf fatal_error fmt*)
  ignore (Format.flush_str_formatter ());
  Format.kfprintf
    (fun _ppf -> fatal_error (Format.flush_str_formatter ()))
    Format.str_formatter fmt

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          (* raise_with_backtrace is not available before OCaml 4.05 *)
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end
    | exception work_exn ->
      (*let work_bt = Printexc.get_raw_backtrace () in*)
      begin match always () with
        | () ->
          exceptionally ();
          (*Printexc.raise_with_backtrace work_exn work_bt*)
          raise work_exn
        | exception always_exn ->
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end

let reraise_preserving_backtrace e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter ~f:(fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map ~f:(fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    match f () with
    | x           -> set_refs backup; x
    | exception e -> set_refs backup; raise e

(* List functions *)

let map_end f l1 l2 = List.map_end ~f l1 l2

let rev_map_end f l1 l2 =
  let rec rmap_f accu = function
    | [] -> accu
    | hd::tl -> rmap_f (f hd :: accu) tl
  in
  rmap_f l2 l1

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let for_all2 pred l1 l2 = List.for_all2 ~f:pred l1 l2

let replicate_list = List.replicate

let list_remove x = List.remove ~phys:false x

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

(* Options *)

let may f x = Option.iter ~f x
let may_map f x = Option.map ~f x

(* File functions *)

let remove_file filename =
  try
    (* merge5: Partial revert of upstream PR 11412, to allow building
       with OCaml 4.14 (which does not have is_regular_file *)
    if Sys.file_exists filename
    then Sys.remove filename
  with Sys_error _msg ->
    ()

let rec split_path_and_prepend path acc =
  match Filename.dirname path with
  | dir when dir = path ->
    let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let dir =
      if not Sys.unix && String.length dir > 2 && is_letter dir.[0] && dir.[1] = ':'
      then
        (* We do two things here:
            - We use an uppercase letter to match Dune's behavior
            - We also add the separator ousrselves because [Filename.concat]
            does not if its first argument is of the form ["C:"] *)
        Printf.sprintf "%c:%s"
          (Char.uppercase_ascii dir.[0])
          Filename.dir_sep
      else dir
    in
    dir :: acc
  | dir -> split_path_and_prepend dir (Filename.basename path :: acc)

let split_path path = split_path_and_prepend path []

(* Deal with case insensitive FS *)

external fs_exact_case : string -> string = "ml_merlin_fs_exact_case"
external fs_exact_case_basename: string -> string option = "ml_merlin_fs_exact_case_basename"

(* A replacement for sys_file_exists that makes use of stat_cache *)
module Exists_in_directory = File_cache.Make(struct
    let cache_name = "Exists_in_directory"
    type t = string -> bool
    let read dir =
      if Sys.file_exists dir &&
         Sys.is_directory dir
      then
        let cache = Hashtbl.create 4 in
        (fun filename ->
           match Hashtbl.find cache filename with
           | x -> x
           | exception Not_found ->
             let exists = Sys.file_exists (Filename.concat dir filename) in
             Hashtbl.add cache filename exists;
             exists)
      else (fun _ -> false)
  end)

let exact_file_exists ~dirname ~basename =
  Exists_in_directory.read dirname basename &&
  let path = Filename.concat dirname basename in
  match fs_exact_case_basename path with
  | None ->
    let path' = fs_exact_case path in
    path == path' || (* only on macos *) basename = Filename.basename path'
  | Some bn ->
    (* only on windows *)
    basename = bn

let canonicalize_filename ?cwd path =
  let parts =
    match split_path path with
    | dot :: rest when dot = Filename.current_dir_name ->
      split_path_and_prepend (match cwd with None -> Sys.getcwd () | Some c -> c) rest
    | parts -> parts
  in
  let goup path = function
    | dir when dir = Filename.parent_dir_name ->
      (match path with _ :: t -> t | [] -> [])
    | dir when dir = Filename.current_dir_name ->
      path
    | dir -> dir :: path
  in
  let parts = List.rev (List.fold_left ~f:goup ~init:[] parts) in
  let filename_concats = function
    | [] -> ""
    | root :: subs -> List.fold_left ~f:Filename.concat ~init:root subs
  in
  fs_exact_case (filename_concats parts)

let rec expand_glob ~filter acc root = function
  | [] -> root :: acc
  | Glob.Wildwild :: _tl -> (* FIXME: why is tl not used? *)
    let rec append acc root =
      let items = try Sys.readdir root with Sys_error _ -> [||] in
      let process acc dir =
        let filename = Filename.concat root dir in
        if filter filename
        then append (filename :: acc) filename
        else acc
      in
      Array.fold_left process (root :: acc) items
    in
    append acc root
  | Glob.Exact component :: tl ->
    let filename = Filename.concat root component in
    expand_glob ~filter acc filename tl
  | pattern :: tl ->
    let items = try Sys.readdir root with Sys_error _ -> [||] in
    let process acc dir =
      if Glob.match_pattern pattern dir then
        let root' = Filename.concat root dir in
        if filter root' then
          expand_glob ~filter acc root' tl
        else acc
      else acc
    in
    Array.fold_left process acc items

let expand_glob ?(filter=fun _ -> true) path acc =
  match split_path path with
  | [] -> acc
  | root :: subs ->
    let patterns = List.map ~f:Glob.compile_pattern subs in
    expand_glob ~filter acc root patterns

let find_in_path path name =
  canonicalize_filename
  begin
    if not (Filename.is_implicit name) then
      if exact_file_exists
          ~dirname:(Filename.dirname name)
          ~basename:(Filename.basename name)
      then name
      else raise Not_found
    else List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else None
      )
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    | [] -> raise Not_found
    | dir::rem ->
      let dir = simplify dir in
      if Exists_in_directory.read dir name
      then Filename.concat dir name
      else try_dir rem
  in try_dir path

let normalized_unit_filename = String.uncapitalize_ascii

let find_in_path_normalized ?(fallback="") path name =
  let has_fallback = fallback <> "" in
  canonicalize_filename
  begin
    let uname = normalized_unit_filename name in
    let ufallback = normalized_unit_filename fallback in
    List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:uname
        then Some (Filename.concat dirname uname)
        else if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else
          let () = Logger.log
            ~section:"locate"
            ~title:"find_in_path_uncap"
            "Failed to load %s/%s" dirname name
          in
          if has_fallback && exact_file_exists ~dirname ~basename:ufallback
        then Some (Filename.concat dirname ufallback)
        else if has_fallback && exact_file_exists ~dirname ~basename:fallback
        then Some (Filename.concat dirname fallback)
        else None
      )
  end

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s ~pos:1 ~len:(String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter ~f:(fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode (*~perms:0o666*) ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

(* Reading from a channel *)

let input_bytes ic n =
  let result = Bytes.create n in
  really_input ic result 0 n;
  result

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let letter_of_int n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  let num = n / 26 in
  if num = 0 then letter
  else letter ^ Int.to_string num

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* [find_first_mono p] assumes that there exists a natural number
   N such that [p] is false on [0; N[ and true on [N; max_int], and
   returns this N. (See misc.mli for the detailed specification.) *)
let find_first_mono =
  let rec find p ~low ~jump ~high =
    (* Invariants:
       [low, jump, high] are non-negative with [low < high],
       [p low = false],
       [p high = true]. *)
    if low + 1 = high then high
    (* ensure that [low + jump] is in ]low; high[ *)
    else if jump < 1 then find p ~low ~jump:1 ~high
    else if jump >= high - low then find p ~low ~jump:((high - low) / 2) ~high
    else if p (low + jump) then
      (* We jumped too high: continue with a smaller jump and lower limit *)
      find p ~low:low ~jump:(jump / 2) ~high:(low + jump)
    else
      (* we jumped too low:
         continue from [low + jump] with a larger jump *)
      let next_jump = max jump (2 * jump) (* avoid overflows *) in
      find p ~low:(low + jump) ~jump:next_jump ~high
  in
  fun p ->
    if p 0 then 0
    else find p ~low:0 ~jump:1 ~high:max_int

(* String operations *)

(* let split_null_terminated s =
  let[@tail_mod_cons] rec discard_last_sep = function
    | [] | [""] -> []
    | x :: xs -> x :: discard_last_sep xs
  in
  discard_last_sep (String.split_on_char ~sep:' ' s) *)

(* let concat_null_terminated = function
  | [] -> ""
  | l -> String.concat ~sep:" " (l @ [""]) *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename ~pos:0 ~len:pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str ~pos:curr ~len:(next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str ~pos:curr ~len:(String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat ~sep:after (search [] 0)


let rev_split_string cond s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      if cond s.[i] then
        split1 res (i+1)
      else
        split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s ~pos:i ~len:(j-i) :: res else begin
      if cond s.[j] then
        split1 (String.sub s ~pos:i ~len:(j-i) :: res) (j+1)
      else
        split2 res i (j+1)
    end
  in split1 [] 0

let rev_split_words s =
  let helper = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  rev_split_string helper s

let rev_string_split ~on s =
  rev_split_string ((=) on) s

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x

let cut_at s c =
  let pos = String.index s c in
  String.sub s ~pos:0 ~len:pos,
  String.sub s ~pos:(pos+1) ~len:(String.length s - pos - 1)

let ordinal_suffix n =
  let teen = (n mod 100)/10 = 1 in
  match n mod 10 with
  | 1 when not teen -> "st"
  | 2 when not teen -> "nd"
  | 3 when not teen -> "rd"
  | _ -> "th"

(* Color support handling *)
module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ~sep:";" (List.map ~f:code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found

  let as_inline_code printer ppf x =
    Format.pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    Format.pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format.pp_print_string ppf s

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter ~f:set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    Int.min (Int.max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = Int.max 1 (i - cutoff - 1) to Int.min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          Int.min (1 + Int.min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else Int.min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
      | None -> acc
      | Some dist ->
         let (best_choice, best_dist) = acc in
         if dist < best_dist then ([head], dist)
         else if dist = best_dist then (head :: best_choice, dist)
         else acc
  in
  let env = List.sort_uniq ~cmp:(fun s1 s2 -> String.compare s2 s1) env in
  fst (List.fold_left ~f:(compare name) ~init:([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
    let comma ppf () = Format.fprintf ppf ", " in
     Format.fprintf ppf "@\n@{<hint>Hint@}: Did you mean %a%s%a?@?"
       (Format.pp_print_list ~pp_sep:comma Style.inline_code) rest
       (if rest = [] then "" else " or ")
       Style.inline_code last

let print_see_manual ppf manual_section =
  let open Format in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section

let output_of_print print =
  let output out_channel t =
    let ppf = Format.formatter_of_out_channel out_channel in
    print ppf t;
    (* Must flush the formatter immediately because it has a buffer separate
        from the output channel's buffer *)
    Format.pp_print_flush ppf ()
  in
  output

let is_print_longer_than size p =
  let exception Limit_exceeded in
  let limit = ref size in
  let count_down len =
    limit := !limit - len;
    if !limit < 0 then raise Limit_exceeded
  in
  let out_string _ _ len = count_down len in
  let out_newline () = count_down 1 in
  let out_spaces n = count_down n in
  let out_flush _ = () in
  let out_indent _ = () in
  let out_functions : Format.formatter_out_functions = {
    out_string;
    out_flush;
    out_newline;
    out_spaces;
    out_indent}
  in
  let ppf = Format.formatter_of_out_functions out_functions in
  try p ppf; false
  with Limit_exceeded -> true

let time_spent () =
  let open Unix in
  let t = times () in
  ((t.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime) *. 1000.0)

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let unitname filename =
  let unitname =
    try String.sub filename ~pos:0 ~len:(String.index filename '.')
    with Not_found -> filename
  in
  String.capitalize unitname

(* [modules_in_path ~ext path] lists ocaml modules corresponding to
                    * filenames with extension [ext] in given [path]es.
                    * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
                    * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
                    * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
let modules_in_path ~ext path =
  let seen = Hashtbl.create 7 in
  List.fold_left ~init:[] path
    ~f:begin fun results dir ->
      try
        Array.fold_left
          begin fun results file ->
            if Filename.check_suffix file ext
            then let name = Filename.chop_extension file in
              (if Hashtbl.mem seen name
               then results
               else
                 (Hashtbl.add seen name (); String.capitalize name :: results))
            else results
          end results (Sys.readdir dir)
      with Sys_error _ -> results
    end

module List = struct
  include CamlList

  (* merlin-jst: From the compiler's `Misc.Stdlib.List` *)
  let rec is_prefix ~equal t ~of_ =
    match t, of_ with
    | [], [] -> true
    | _::_, [] -> false
    | [], _::_ -> true
    | x1::t, x2::of_ -> equal x1 x2 && is_prefix ~equal t ~of_

  let rec iteri2 i f l1 l2 =
    match (l1, l2) with
      ([], []) -> ()
    | (a1::l1, a2::l2) -> f i a1 a2; iteri2 (i + 1) f l1 l2
    | (_, _) -> raise (Invalid_argument "iteri2")

  let iteri2 f l1 l2 = iteri2 0 f l1 l2
end

module String = struct
  include CamlString
  module Ord = struct
    type t = string
    let compare = String.compare
  end
  module Set = Set.Make (Ord)
  module Map = Map.Make (Ord)
  module Tbl = Hashtbl.Make (struct
      type t = string
      let equal (x : string) (y : string) : bool = (x = y)
      let hash = Hashtbl.hash
    end)
end


type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string String.Map.t
