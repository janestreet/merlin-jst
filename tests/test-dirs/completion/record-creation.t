  $ cat >test.ml <<"EOF"
  > module Outer = struct
  >   let x = 3
  >   module Inner = struct
  >     let y = 4
  >     type t = { field : int }
  >   end
  > end
  > 
  > let create (_ : Outer.Inner.t) = ()
  > 
  > let () = create { $1
  > EOF

  $ run () {
  >   $MERLIN single complete-prefix -position 11:18 -prefix "$1" \
  >     -filename test.ml < test.ml | jq '.value.entries'
  > }

  $ run ""
  [
    {
      "name": "field",
      "kind": "Label",
      "desc": "Outer.Inner.t -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Topdirs",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Stdlib",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Std_exit",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Opttopdirs",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Gc_timings",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalMod",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalFormatBasics",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Compiler_owee",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalFormat",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalComprehension",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalOO",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalAtomic",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "CamlinternalLazy",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

  $ run Outer.
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

  $ run Outer.I
  [
    {
      "name": "Inner",
      "kind": "Module",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

  $ run Outer.Inner.
  [
    {
      "name": "field",
      "kind": "Label",
      "desc": "Outer.Inner.t -> int",
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
