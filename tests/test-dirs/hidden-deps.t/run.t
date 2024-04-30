Test that Merlin correctly handles BH and SH directives.
These are for hidden dependencies and correspond to the
-H compiler flag.

Start by building some dependencies. C depends on B
which depends on A. A will be a hidden dependency of C,
while B will be direct.

B exposes a variable from A, allowing C to directly use
it via B.

  $ mkdir -p _build/a
  $ cp a/*.ml* _build/a
  $ pushd _build/a > /dev/null
  $ $OCAMLC -c -bin-annot a.mli -o a.cmi
  $ $OCAMLC -c -bin-annot a.ml -o a.cmo
  $ popd > /dev/null

  $ mkdir -p _build/b
  $ cp b/*.ml* _build/b
  $ pushd _build/b > /dev/null
  $ $OCAMLC -c -bin-annot b.mli -o b.cmi
  $ $OCAMLC -c -bin-annot -I ../a b.ml -o b.cmo
  $ popd > /dev/null

Merlin does not report errors when there are none

  $ $MERLIN single errors -filename c/correct.ml < c/correct.ml | jq ".value"
  []

Merlin can locate a value in an interface from a direct dependency

  $ $MERLIN single locate -position 1:17 -look-for interface -filename c/correct.ml < c/correct.ml | jq ".value"
  {
    "file": "$TESTCASE_ROOT/b/b.mli",
    "pos": {
      "line": 2,
      "col": 0
    }
  }

Merlin can locate a value in an interface from a hidden dependency

  $ $MERLIN single locate -position 1:11 -look-for interface -filename c/correct.ml < c/correct.ml | jq ".value"
  {
    "file": "$TESTCASE_ROOT/b/b.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Merlin can locate a value in an implementation from a direct dependency

  $ $MERLIN single locate -position 1:17 -look-for implementation -filename c/correct.ml < c/correct.ml | jq ".value"
  {
    "file": "$TESTCASE_ROOT/b/b.ml",
    "pos": {
      "line": 2,
      "col": 4
    }
  }

Merlin can locate a value in an implementation from a hidden dependency

  $ $MERLIN single locate -position 1:11 -look-for implementation -filename c/correct.ml < c/correct.ml | jq ".value"

Merlin reports an error when a hidden dependency is directly used

  $ $MERLIN single errors -filename c/error.ml < c/error.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 8
      },
      "end": {
        "line": 1,
        "col": 11
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound module A"
    }
  ]

Merlin can locate a value in an interface from a direct dependency when there is an error

  $ $MERLIN single locate -position 1:17 -look-for interface -filename c/error.ml < c/error.ml | jq ".value"
  {
    "file": "$TESTCASE_ROOT/b/b.mli",
    "pos": {
      "line": 2,
      "col": 0
    }
  }

Merlin fails locate a value in an interface from a hidden dependency that is illegally used

  $ $MERLIN single locate -position 1:11 -look-for interface -filename c/error.ml < c/error.ml | jq ".value"
  "Not in environment 'A.a'"

Merlin can locate a value in an implementation from a direct dependency when there is an error

  $ $MERLIN single locate -position 1:17 -look-for implementation -filename c/error.ml < c/error.ml | jq ".value"
  {
    "file": "$TESTCASE_ROOT/b/b.ml",
    "pos": {
      "line": 2,
      "col": 4
    }
  }

Merlin fails locate a value in an implementation from a hidden dependency that is illegally used

  $ $MERLIN single locate -position 1:11 -look-for implementation -filename c/error.ml < c/error.ml | jq ".value"
  "Not in environment 'A.a'"
