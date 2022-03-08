(* Backport functionality from 4.13 to 4.12 *)

module String : sig
  val starts_with : prefix:string -> string -> bool
end
