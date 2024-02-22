(* Function expressions, including those that don't actually close over anything *)

let f g =
  fun x -> g x
        (* ^ *)
;;

let f g =
  exclave_ fun x -> g x
                 (* ^ *)
;;

(* It is unfortunate that this allocates no closure, but still has allocation mode
   [global] *)
let f g =
  fun x -> x
        (* ^ *)
;;
