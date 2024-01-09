FIXME

First result was incorrect when in the body of a function with an optional argument.

Jane Street internal: the above text is incorrect -- this bug was fixed -- but
we should make sure it stays fixed when we merge with upstream's implementation
of n-ary functions.

  $ $MERLIN single occurrences -identifier-at 3:3 -filename opt.ml <<EOF | \
  > jq '.value'
  > (* test case *)
  > let f ?(x=1) () = 2 ;;
  > None
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 3,
        "col": 4
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 3:3 -filename opt.ml <<EOF | \
  > jq '.value'
  > (* test case *)
  > let f () = 2 ;;
  > None
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 3,
        "col": 4
      }
    }
  ]
