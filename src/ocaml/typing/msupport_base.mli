(** This file exists because Env need to be able to report errors, but Msupport has an
    (indirect) dependency on Env. *)

(** Raise an error that can be caught: normal flow is resumed if a
    [Msupport.catch_errors] handler was installed. *)
val raise_error : ?handler:(exn -> bool) -> exn -> unit

(** Implementation details needed by Msupport *)
module Internal : sig
    module RawTypeHash : Hashtbl.S with type key = Types.TransientTypeOps.t
    val errors : (exn list ref * unit RawTypeHash.t) option ref
    val monitor_errors' : bool ref ref
end
