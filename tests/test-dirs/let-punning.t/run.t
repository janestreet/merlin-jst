Test Merlin's behavior in the presense of let-punning

  $ type_enclosing () {
  >   $MERLIN single type-enclosing -position "$1" -filename test.ml < test.ml | jq '.value[0].type' -r
  > }

  $ locate () {
  >   res=$($MERLIN single locate -position "$1" -filename test.ml < test.ml)
  >   if (echo "$res" | jq -e '.value | type == "string"' > /dev/null); then
  >     # an error occurred, so print the error
  >     echo "$res" | jq -r .value
  >   else
  >     line=$(echo "$res" | jq .value.pos.line)
  >     col=$(echo "$res" | jq .value.pos.col)
  >     echo "$line:$col"
  >   fi
  > }

  $ occurrences () {
  >   res=$($MERLIN single occurrences -identifier-at "$1" -filename test.ml < test.ml)
  >   echo "$res" | jq -c '.value[]' | while read -r occurrence; do
  >     line=$(echo "$occurrence" | jq .start.line)
  >     start=$(echo "$occurrence" | jq .start.col)
  >     end=$(echo "$occurrence" | jq .end.col)
  >     echo "$line:$start-$end"
  >   done
  > }

Test that locating a variable in a punned let goes to the original definition of the
variable rather than the new definition. i.e., in:
 1. let x = ... in
 2. let* x in
locating the x on line 2 goes to the definition on line 1.

let*
  $ locate 13:9
  12:8

parallel let*
  $ locate 20:9
  18:8
  $ locate 20:16
  19:8

sequential let*
  $ locate 27:9
  25:8
  $ locate 28:9
  26:8

Test that locating a variable bound in a punned let goes to the definition. i.e in:
 1. let x = ... in
 2. let* x in
 3. x
locating the x on line 3 goes to the definition on line 2.

let*
  $ locate 14:10
  13:9

parallel let*
  $ locate 21:11
  20:9
  $ locate 21:14
  20:16

sequential let*
  $ locate 29:11
  27:9
  $ locate 29:14
  28:9

Test that getting the type of a variable in a punned let returns the type of the newly
bound variable. i.e. in:
 1. let x : int option = ... in
 2. let* x in
the type-enclosing of x on line 2 is [int], not [int option].

The type for all the below tests should be [int], not [int option].

let*
  $ type_enclosing 13:9
  int

parallel let*
  $ type_enclosing 20:9
  int
  $ type_enclosing 20:16
  int

sequential let*
  $ type_enclosing 27:9
  int
  $ type_enclosing 28:9
  int

Test that finding occurrences of a variable in a punned let finds the occurrences of the
newly defined variable, rather than the original one. i.e., in:
 1. let x = ... in
 2. let* x in
 3. x
finding occurrences of x on line 2 returns the definition on line 2 and the usage on
line 3.

let*
  $ occurrences 13:9
  13:9-10
  14:9-10

parallel let*
  $ occurrences 20:9
  20:9-10
  21:10-11
  $ occurrences 20:16
  20:16-17
  21:13-14

sequential let*
  $ occurrences 27:9
  27:9-10
  29:10-11
  $ occurrences 28:9
  28:9-10
  29:13-14
