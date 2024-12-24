  $ cat >lib.ml <<'EOF'
  > (* blah *)
  > let foo = "bar"
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml main.ml

  $ ocaml-index aggregate main.cmt lib.cmt
  $ ocaml-index dump-file-stats project.ocaml-index
  File stats for index "project.ocaml-index":
    "lib.ml": { mtime=1735062283.193617; size=27; source_digest="o\183+\155\030\018\214\030\137\200\198\231\024z\158\240" }
    "main.ml": { mtime=1735062283.196618; size=30; source_digest="e)\028\0281\244\1875EN\151 z@%\217" }

Foo was defined on line 2 when the index was built, but is now defined on line 1
  $ cat >lib.ml <<'EOF'
  > let foo = "bar"
  > EOF

TODO: Report the stale occurrence too
  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -filename main.ml < main.ml | jq .value
  [
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 22
      },
      "end": {
        "line": 1,
        "col": 29
      }
    }
  ]
