  $ cat > test.ml << EOF
  > module Foo = struct
  >   module Bar = struct
  >     module M = struct end
  >     type t
  >   end
  > end
  > 
  > module Import = struct
  >   module M = Foo.Bar.M
  >   type t = Foo.Bar.t
  > end
  > 
  > open! Import
  > EOF

  $ cat > .merlin << EOF
  > FLG -short-paths
  > EOF

Type-enclosing on a module should avoid writing identifiers in terms of the module
  $ $MERLIN single type-enclosing -position 13:7 -verbosity 1 -filename test.ml < test.ml | jq .value[].type -r
  sig module M = Import.M type t = Import.t end
