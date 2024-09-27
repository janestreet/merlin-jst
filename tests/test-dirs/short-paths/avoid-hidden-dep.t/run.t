Test that short paths will prefer a visible dependency over a hidden one

  $ $OCAMLC -c hidden/hidden.ml

  $ $OCAMLC -c visible/visible.ml -I hidden

  $ $MERLIN single type-enclosing -position 1:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Visible.Foo.Bar.t
