
  $ $MERLIN single occurrences -identifier-at 1:6 -filename type.ml <<EOF | \
  > jq '.value'
  > type t
  > type b = t
  > EOF
  [
    {
      "start": {
        "line": 1,
        "col": 5
      },
      "end": {
        "line": 1,
        "col": 6
      }
    },
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 10
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 1:19 -filename type.ml <<EOF | \
  > jq '.value'
  > let f = fun (type t) (foo : t list) -> let (_ : t) = () in ()
  > EOF
  parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 29, column 1
  [4]

  $ $MERLIN single occurrences -identifier-at 1:29 -filename type.ml <<EOF | \
  > jq '.value'
  > let f = fun (x : int) (type t) (foo : t list) -> let (_ : t) = () in ()
  > EOF
  []
