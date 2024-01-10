Test case-analysis in the middle of a [fun].

  $ cat >fun.ml <<EOF
  > let f x (bb : bool) y = something
  > EOF

  $ $MERLIN single case-analysis -start 1:10 -end 1:11 -filename fun.ml <fun.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "error",
    "value": "Destruct not allowed on pattern in function argument",
    "notifications": []
  }
