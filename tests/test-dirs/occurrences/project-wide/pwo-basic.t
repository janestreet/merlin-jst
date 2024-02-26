 $ cat >lib.ml <<'EOF'
  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml main.ml
  File "lib.ml", line 1:
  Error: I/O error: lib.ml: No such file or directory
  [2]
  $ ocaml-index aggregate main.cmt lib.cmt
  ocaml-index: command not found
  [127]
  $ ocaml-index dump project.ocaml-index
  ocaml-index: command not found
  [127]
  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
