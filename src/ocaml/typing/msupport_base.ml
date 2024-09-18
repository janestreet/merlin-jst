open Std

module RawTypeHash = Hashtbl.Make(Types.TransientTypeOps)

let errors : (exn list ref * unit RawTypeHash.t) option ref = ref None

let monitor_errors' = ref (ref false)

let raise_error ?handler exn =
  !monitor_errors' := true;
  match !errors with
  | Some (l,_) ->
    let handler = Option.value ~default:(fun _ -> false) handler in
    let handled = handler exn in
    if not handled
    then l := exn :: !l
  | None -> raise exn

module Internal = struct
  module RawTypeHash = RawTypeHash
  let errors = errors
  let monitor_errors' = monitor_errors'
end
