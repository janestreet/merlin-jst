(* Backport functionality from 4.13 to 4.12 *)

module String = struct
  let starts_with ~prefix str =
    (* Easy implementation, rather than copying the stdlib one *)
    let prefix_len = String.length prefix in
    prefix_len <= String.length str &&
    String.equal (String.sub str 0 prefix_len) prefix
end
