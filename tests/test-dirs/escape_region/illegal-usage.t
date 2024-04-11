  $ cat >local.ml <<EOF
  > let () =
  >   let local_ local_value = "foo" in
  >   let x = ref "bar" in
  >   x := local_value
  > ;;
  > EOF

  $ $MERLIN single locate -position 4:13 \
  > -filename local.ml < local.ml | jq '.value.pos'
  {
    "line": 2,
    "col": 13
  }
