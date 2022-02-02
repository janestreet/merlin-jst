
Without `-extension local`, `local_` is not considered a keyword:

  $ $MERLIN single errors -filename test.ml < test.ml | jq .value[].message
  "Unbound value local_"

But enabling the extension enables local allocation typing:

  $ $MERLIN single errors -extension comprehensions -extension local -filename test.ml < test.ml | jq .value[].message
  "This local value escapes its region"
