Check that ocaml-jst-specific flags are correctly ignored:

  $ cat > .merlin << EOF
  > FLG -flambda2-inline-max-depth 42 -flambda2-join-points -no-ocamlcfg
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
