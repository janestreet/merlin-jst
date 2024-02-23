(* Cases where the cursor is on a field access, which allocates only in the float case *)

type t = { a : int; b : float }

let f (t : t) = t.a
              (* ^ *)

let f (t : t) = t.a
              (* ^ *)

type floats_t = { a : float; b : float }

let f (t : floats_t) = t.a
                     (* ^ *)

let f (t : floats_t) = exclave_ t.a
                              (* ^ *)
