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

  $ $MERLIN single case-analysis -I lib -start 3:5 -end 3:5 -filename client.ml <<EOF
  > let f (Packed t : Gadt.packed) =
  >   match (t : _ Gadt.t) with
  >   | _ -> assert false
  > EOF
  {
    "class": "exception",
    "value": "File \"src/analysis/destruct.ml\", line 663, characters 14-20: Assertion failed
  Raised at Merlin_analysis__Destruct.node in file \"src/analysis/destruct.ml\", line 663, characters 14-26
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 46, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 63, characters 10-24
  Called from Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\", line 83, characters 10-14
  Re-raised at Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\", line 85, characters 38-45
  Called from Ocaml_typing__Persistent_env.without_cmis in file \"src/ocaml/typing/persistent_env.ml\", line 194, characters 10-109
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 684, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 686, characters 30-39
  Called from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\", line 65, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 684, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 686, characters 30-39
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 46, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 63, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 33, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 38, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 20, characters 42-53
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", line 101, characters 14-110
  ",
    "notifications": []
  }
