Test that Merlin can recover from a function taking local arguments being under-applied
when performing a locate query.

  $ ocamlmerlin single locate -position 4:15 -filename test.ml << EOF | jq .value
  > module List = struct
  >   let map : f:('a -> 'b) -> 'a list -> 'b list = failwith ""
  > end
  > let () = List.map ~invalid_arg:2
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 2,
      "col": 6
    }
  }

  $ ocamlmerlin single locate -position 4:15 -filename test.ml << EOF | jq .value
  > module List = struct
  >   let map : f:local_ ('a -> 'b) -> 'a list -> 'b list = failwith ""
  > end
  > let () = List.map ~invalid_arg:2
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 2,
      "col": 6
    }
  }
