(* [escape] forces a value to be global. *)
include struct
  let escape _ = ()
end : sig
  val escape : _ -> unit
end

type t0 = string -> string
type t1 = local_ string -> string
type t2 = string -> local_ string
type t3 = local_ string -> local_ string
type t4 = local_ string -> string -> string
type t5 = local_ string -> local_ (string -> string)
type t6 = local_ string -> (string -> string)

let simple0 (x : string) = x
let simple1 (local_ _ : string) = "a"
let simple2 (x : string) = local_ x
let simple3_1 (local_ x : string) = local_ x
let simple3_2 (local_ x : string) = x

(* [value]: The function creates a value.
   What varies: Is the value used only locally?
*)

let value1 (x : int) =
  let y = Some x in
  match y with
  | Some z -> x + z
  | None -> assert false

let value2 (x : int) =
  let y = local_ Some x in
  match y with
  | Some z -> x + z
  | None -> assert false

let value3 (x : int) =
  let y = Some x in
  escape y;
  x

(* [param]: The function uses its param.
   What varies: Is the param used only locally?
*)

let param1 (x : string) =
  match x with
  | "" -> true
  | _ -> false

let param2 (x : string) =
  escape x

let param3 (local_ x : string) =
  match x with
  | "" -> true
  | _ -> false

let param4_noncompiling (local_ x : string) =
  escape x

(* [expr]: There is a larger expression that is local. *)

let expr1 (x : string) = local_
  Some (Some (Some (Some x)))
