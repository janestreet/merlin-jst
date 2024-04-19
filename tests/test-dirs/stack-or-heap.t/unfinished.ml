(* Snippets that don't fully compile *)

type t = { x : int ref }

let f : int ref -> local_ int ref = fun x -> x

let g x =
  let t = { x = f x } in
               (* ^ *)
  let y =
;;

let f : int ref -> int ref = fun x -> x

let g x =
  let t = { x = f x } in
               (* ^ *)
  let y =
;;

let f x =
  let z = Some x in
            (* ^ *)
;;

let f (local_ x) =
  let z = Some x in
            (* ^ *)
;;

let f x =
  let z = Some x in
            (* ^ *)
  5
;;

let f x =
  let z = Some y in
            (* ^ *)
  z
;;
