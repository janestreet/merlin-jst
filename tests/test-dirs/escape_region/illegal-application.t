  $ cat >local.ml <<EOF
  > let foo _ = local_ failwith "unimplemented"
  > let bar x = foo x
  > EOF

  $ $MERLIN single locate -position 2:15 \
  > -filename local.ml < local.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 4
  }
