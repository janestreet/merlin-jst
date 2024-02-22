(* Various cases where it doesn't make much sense to report a stack-or-heap, even though
   there is an allocating expression in sight. *)

let f g x y =
  let z = x + y in
         (* ^ *)
  exclave_ Some (g z)
;;
