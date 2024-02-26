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

(* Sometimes, a function expression is in a non-obvious way the nearest enclosing
   allocating expression. It's possible we'll decide to ignore functions unless the cursor
   is on something that syntactically belongs just to the function *)

let f g x y =
  let z = x + y in
         (* ^ *)
  exclave_ Some (g z)
;;


let f g x y =
       (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
;;
