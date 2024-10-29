(* Function expressions, including those that don't actually close over anything *)

let f g =
  fun x -> g x
      (* ^ *)
;;

let f g =
  exclave_ fun x -> g x
               (* ^ *)
;;

let f g = (fun x -> g x)
                    (* ^ *)

(* Doesn't close over anything, but merlin does not know this *)
let f g =
  fun x -> x
      (* ^ *)
;;

let f =
  function | x -> x
      (* ^ *)
;;

let f x =
  exclave_ function | y -> y
               (* ^ *)
;;

let f = (function | x -> x)
                       (* ^ *)

(* Doesn't close over anything, but merlin does not know this *)
let f g =
  function | x -> g x
      (* ^ *)
;;

let f g x y =
       (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
;;

(* If we are inside the body of a function, we don't count that function as enclosing *)

let f g x y =
  let z = x + y in
         (* ^ *)
  exclave_ Some (g z)
;;

let f = function
  | None -> 0
  | Some _ -> 1
         (* ^ *)

(* Pattern of a [let]-bound function (we treat this case specially to improve LSP
   compatibility) *)

let f g x y =
 (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
and h g x y =
 (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
;;

let ignore (local_ _) = ()

let () =
  let f g x y =
   (* ^ *)
    let z = x + y in
    exclave_ Some (g z)
  and h g x y =
   (* ^ *)
    let z = x + y in
    exclave_ Some (g z)
  in
  ignore f;
  ignore h

(* Ensure other [let]-bound patterns aren't treated this way *)

let x = Some 5
 (* ^ *)
