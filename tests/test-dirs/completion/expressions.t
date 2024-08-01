Create some values that we'll put at the beginning of each test so that they're in the
environment. Everything is prefixed with [Atest] to because nothing in the stdlib shares
this prefix. This avoids polluting test results with stdlib items.

  $ cat >env.ml <<EOF
  > module Atest_module = struct
  >   module Atest_module = struct
  >     type atest_record_type = { atest_field : unit }
  >     type atest_variant_type = Atest_variant of unit
  >     let atest_value = ()
  >     let atest_function ~atest_label:_ = ()
  >   end
  >   include Atest_module
  > end
  > open Atest_module
  > EOF

Verify that the above compiles. The compilation output is never actually used

  $ $OCAMLC env.ml

Create a function that takes in a file with a (*cursor*) comment, and asks Merlin to
complete at the cursor with the above environment.

  $ function runtest() {
  >   # write the stdin to file.ml, adding env.ml to the start of it
  >   cat env.ml > file.ml
  >   cat >> file.ml
  > 
  >   # this gets the position of the cursor
  >   pattern="\(\*cursor\*\)"
  >   line=$(grep -n -E "$pattern" file.ml | awk -F: '{print $1}')
  >   col=$(sed -n "$line p" file.ml | grep -b -o -E "$pattern" | awk -F: '{print $1}')
  > 
  >   # Extract the prefix to pass into merlin
  >   prefix=$(grep -oP "((?<=[^a-zA-Z0-9_\.])|^)[a-zA-Z0-9_\.]*(?=$pattern)" file.ml)
  > 
  >   # query merlin
  >   $MERLIN single complete-prefix -position "$line:$col" -prefix "$prefix" \
  >     -filename file.ml < file.ml | revert-newlines | jq -r .value.entries[].name
  > }

  $ runtest <<EOF
  > let x = atest(*cursor*)
  > EOF
  atest_function
  atest_value
  atest_record_type
  atest_variant_type

  $ runtest <<EOF
  > let x = Atest(*cursor*)
  > EOF
  Atest_variant
  Atest_module

  $ runtest <<EOF
  > Atest(*cursor*)
  > EOF
  Atest_module

  $ runtest <<EOF
  > atest(*cursor*)
  > EOF

  $ runtest <<EOF
  > let atest(*cursor*)
  > EOF
  atest_field
  atest_function
  atest_value
  atest_record_type
  atest_variant_type

  $ runtest <<EOF
  > let _ = Atest_module.(*cursor*)
  > EOF
  atest_function
  atest_value
  Atest_variant
  atest_record_type
  atest_variant_type

  $ runtest <<EOF
  > let _ = { atest_field = atest(*cursor*) }
  > EOF
  atest_value
  atest_function
  atest_record_type
  atest_variant_type

  $ runtest <<EOF
  > let _ = (value : atest(*cursor*))
  > EOF
  atest_record_type
  atest_variant_type
  atest_function
  atest_value
