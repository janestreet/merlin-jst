(* Various cases where it doesn't make much sense to report a stack-or-heap, even though
   there might be an allocating expression in sight. *)

module M = struct
    (* ^ *)
  fun x = x


   (* ^ *)
end
