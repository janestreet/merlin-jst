Test locating a value whose usage inside a locked context triggers a mode error

escape lock
  $ $MERLIN single locate -position 5:15 <<EOF
  > let () =
  >   let x = local_ "" in
  >   let module M = struct
  >     class foo = object
  >       val y = x
  >     end
  >   end in
  >   ()
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'x'",
    "notifications": []
  }

share lock
  $ $MERLIN single locate -position 4:4 <<EOF
  > let () =
  >   let f : unit -> unit @@ once = fun () -> () in
  >   for _ = 1 to 10 do
  >     f ()
  >   done
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'f'",
    "notifications": []
  }

closure lock
  $ $MERLIN single locate -position 8:37 <<EOF
  > module List : sig
  >   val iter : f:('a -> unit) -> 'a list -> unit
  > end = struct
  >   let iter = failwith ""
  > end
  > let length xs =
  >   let local_ count = ref 0 in
  >   List.iter xs ~f:(fun () -> incr count);
  >   !count
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'count'",
    "notifications": []
  }

closure lock
  $ $MERLIN single locate -position 6:23 <<EOF
  > let y = ref ()
  > let g x =
  >   let _ = y in
  >   x
  > ;;
  > let (f @ portable) x = g x
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'g'",
    "notifications": []
  }

exclave lock
  $ $MERLIN single locate -position 3:11 <<EOF
  > let f () =
  >   let x = local_ Some 10 in
  >   exclave_ x
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'x'",
    "notifications": []
  }

unboxed lock
  $ $MERLIN single locate -position 5:18 <<EOF
  > let f : unit -> ('a : bits32) = assert false
  > let _ =
  >   let x = f () in
  >   object
  >     method bar = x
  >   end
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'x'",
    "notifications": []
  }
