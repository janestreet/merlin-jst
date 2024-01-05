  $ mkdir lib
  $ cat >lib/gadt.ml <<EOF
  > type _ u =
  >   | A : A.t u
  >   | B : B.t u
  > type packed = Packed : _ u -> packed
  > type 'a t = 'a u
  > EOF

  $ cat >lib/a.ml <<EOF
  > type t = int
  > EOF

  $ cat >lib/b.ml <<EOF
  > type t = string
  > EOF

  $ $OCAMLC -I lib lib/a.ml lib/b.ml lib/gadt.ml

Currently, there is a bug, and merlin-destruct doesn't do a good job. Pass [-log-file -] to get
the below to log some additional diagnostics to stderr. In brief, it looks like
merlin-destruct is finding that all constructors of [Gadt.u] are ill-typed in
this position, but not for any good reason -- the fact that they're in a
separate module seem to have something to do with it.

  $ $MERLIN single case-analysis -I lib -start 3:5 -end 3:5 -filename client.ml <<EOF
  > let f (Packed t : Gadt.packed) =
  >   match (t : _ Gadt.t) with
  >   | _ -> assert false
  > EOF
  {
    "class": "error",
    "value": "Destruct not allowed on type that merlin doesn't know enough about",
    "notifications": []
  }
