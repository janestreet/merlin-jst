  $ cat >test.ml <<"EOF"
  > module Outer = struct
  >   let x = 3
  >   module Inner = struct
  >     let y = 4
  >     type t = { field : int }
  >     let default = { field = 3 }
  >   end
  > end
  > 
  > let create (_ : Outer.Inner.t) = ()
  > 
  > let () = create {
  > EOF

`run` positions the cursor at the very end of the last line in the above
program, right after the curly brace. We treat the first argument to `run`
as what the user has typed at that location.

  $ run () {
  >   $MERLIN single complete-prefix -position 12:18 -prefix "$1" \
  >     -filename test.ml < test.ml
  > }

The best autocompletion option is the field that is known to belong
to the record type under consideration.

  $ run "" | jq '.value.entries[0]'
  {
    "name": "field",
    "kind": "Label",
    "desc": "Outer.Inner.t -> int",
    "info": "",
    "deprecated": false
  }

If the user types `Outer`, it is important to provide auto-completion
options other than just `field`. The user is probably trying to type
`Outer.Inner.field` here. (Just typing `Outer.field` would be a type
error.)

  $ run Outer. | jq '.value.entries'
  [
    {
      "name": "field",
      "kind": "Label",
      "desc": "Outer.Inner.t -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "x",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Inner",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

  $ run Outer.I | jq '.value.entries'
  [
    {
      "name": "Inner",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

At this point, the user likely intends to type
`Outer.Inner.field`. This is no guarantee, though.
They might be starting to type `Outer.Inner.default with y = 5`.
So we should continue to provide other auto-completion options.

  $ run Outer.Inner. | jq '.value.entries'
  [
    {
      "name": "field",
      "kind": "Label",
      "desc": "Outer.Inner.t -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "default",
      "kind": "Value",
      "desc": "Outer.Inner.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "y",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "t",
      "kind": "Type",
      "desc": "type t = { field : int; }",
      "info": "",
      "deprecated": false
    }
  ]
