(* Snippets that don't fully compile *)

let f g x y =
  let z = Some (g x) in
             (* ^ *)
;;

let f : int ref -> local_ int ref = fun x -> x

type t = { x : int ref }

let g x =
  let t = { x = f x } in
               (* ^ *)
  let y =
;;
