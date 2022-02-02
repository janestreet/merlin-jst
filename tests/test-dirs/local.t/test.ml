let f () =
  let foo = local_ ref 1 in
  let fn () =
    incr foo
  in
  fn ();
  fn ();
  foo
