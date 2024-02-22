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
