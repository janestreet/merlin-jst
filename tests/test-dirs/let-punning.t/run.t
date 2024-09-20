Test Merlin's behavior in the presense of let-punning

  $ file="test.ml"

  $ highlight_char_range_from_file () {
  >   line=$1
  >   start=$2
  >   end=$3
  >   sed -n "${line}p" "$file" 
  >   printf ' %.0s' $(seq 1 $start)
  >   printf '^%.0s' $(seq 1 $(($end - $start)))
  >   printf "\n"
  > }

  $ highlight_line_colon_col_from_file () {
  >   line_col=$1
  >   IFS=: read line col <<(echo "$line_col")
  >   highlight_char_range_from_file "$line" "$col" "$((col + 1))"
  > }

  $ type_enclosing () {
  >   $MERLIN single type-enclosing -position "$1" -filename "$file" < "$file" | jq '.value[0].type' -r
  >   highlight_line_colon_col_from_file "$1"
  > }

  $ locate () {
  >   echo "Locating:"
  >   highlight_line_colon_col_from_file "$1"
  >   res=$($MERLIN single locate -position "$1" -filename "$file" < "$file")
  >   if (echo "$res" | jq -e '.value | type == "string"' > /dev/null); then
  >     # an error occurred, so print the error
  >     echo "$res" | jq -r
  >   else
  >     line=$(echo "$res" | jq .value.pos.line)
  >     col=$(echo "$res" | jq .value.pos.col)
  >     echo "Found definition at $line:$col:"
  >     highlight_char_range_from_file $line $col $((col + 1))
  >   fi
  > }

  $ occurrences () {
  >   echo "Occurrences of:"
  >   highlight_line_colon_col_from_file "$1"
  >   res=$($MERLIN single occurrences -identifier-at "$1" -filename "$file" < "$file")
  >   echo "$res" | jq -c .value[] | while read -r occurrence; do
  >     line=$(echo "$occurrence" | jq .start.line)
  >     start=$(echo "$occurrence" | jq .start.col)
  >     end=$(echo "$occurrence" | jq .end.col)
  >     echo "Occurrence at $line:$start-$end:"
  >     highlight_char_range_from_file "$line" "$start" "$end"
  >   done
  > }

Test that locating a variable in a punned let goes to the original definition of the
variable rather than the new definition. i.e., in:
 1. let x = ... in
 2. let* x in
locating the x on line 2 goes to the definition on line 1.

let*
  $ locate 13:9
  Locating:
      let* a in
           ^
  Found definition at 12:8:
      let a = return 1 in
          ^

parallel let*
  $ locate 20:9
  Locating:
      let* a and* b in
           ^
  Found definition at 18:8:
      let a = return 1 in
          ^
  $ locate 20:16
  Locating:
      let* a and* b in
                  ^
  Found definition at 19:8:
      let b = return 1 in
          ^

sequential let*
  $ locate 27:9
  Locating:
      let* a in
           ^
  Found definition at 25:8:
      let a = return 1 in
          ^
  $ locate 28:9
  Locating:
      let* b in
           ^
  Found definition at 26:8:
      let b = return 1 in
          ^

Test that locating a variable bound in a punned let goes to the definition. i.e in:
 1. let x = ... in
 2. let* x in
 3. x
locating the x on line 3 goes to the definition on line 2.

let*
  $ locate 14:10
  Locating:
      Some a
            ^
  Found definition at 13:9:
      let* a in
           ^

parallel let*
  $ locate 21:11
  Locating:
      Some (a, b)
             ^
  Found definition at 20:9:
      let* a and* b in
           ^
  $ locate 21:14
  Locating:
      Some (a, b)
                ^
  Found definition at 20:16:
      let* a and* b in
                  ^

sequential let*
  $ locate 29:11
  Locating:
      Some (a, b)
             ^
  Found definition at 27:9:
      let* a in
           ^
  $ locate 29:14
  Locating:
      Some (a, b)
                ^
  Found definition at 28:9:
      let* b in
           ^

Test that getting the type of a variable in a punned let returns the type of the newly
bound variable. i.e. in:
 1. let x : int option = ... in
 2. let* x in
the type-enclosing of x on line 2 is [int], not [int option].

The type for all the below tests should be [int], not [int option].

let*
  $ type_enclosing 13:9
  int
      let* a in
           ^

parallel let*
  $ type_enclosing 20:9
  int
      let* a and* b in
           ^
  $ type_enclosing 20:16
  int
      let* a and* b in
                  ^

sequential let*
  $ type_enclosing 27:9
  int
      let* a in
           ^
  $ type_enclosing 28:9
  int
      let* b in
           ^

Test that finding occurrences of a variable in a punned let finds the occurrences of the
newly defined variable, rather than the original one. i.e., in:
 1. let x = ... in
 2. let* x in
 3. x
finding occurrences of x on line 2 returns the definition on line 2 and the usage on
line 3.

let*
  $ occurrences 13:9
  Occurrences of:
      let* a in
           ^
  Occurrence at 13:9-10:
      let* a in
           ^
  Occurrence at 14:9-10:
      Some a
           ^

parallel let*
  $ occurrences 20:9
  Occurrences of:
      let* a and* b in
           ^
  Occurrence at 20:9-10:
      let* a and* b in
           ^
  Occurrence at 21:10-11:
      Some (a, b)
            ^
  $ occurrences 20:16
  Occurrences of:
      let* a and* b in
                  ^
  Occurrence at 20:16-17:
      let* a and* b in
                  ^
  Occurrence at 21:13-14:
      Some (a, b)
               ^

sequential let*
  $ occurrences 27:9
  Occurrences of:
      let* a in
           ^
  Occurrence at 27:9-10:
      let* a in
           ^
  Occurrence at 29:10-11:
      Some (a, b)
            ^
  $ occurrences 28:9
  Occurrences of:
      let* b in
           ^
  Occurrence at 28:9-10:
      let* b in
           ^
  Occurrence at 29:13-14:
      Some (a, b)
               ^

Test that finding occurrences of a variable includes usages in a punned let. i.e., in:
 1. let x = ... in
 2. let%bind x in
finding occurrences of x on line 1 returns the definition on line 1 and the usage on
line 2.

TODO: fix these tests

let*
  $ occurrences 12:8
  Occurrences of:
      let a = return 1 in
          ^
  Occurrence at 12:8-9:
      let a = return 1 in
          ^

parallel let*
  $ occurrences 18:8
  Occurrences of:
      let a = return 1 in
          ^
  Occurrence at 18:8-9:
      let a = return 1 in
          ^
  $ occurrences 19:8
  Occurrences of:
      let b = return 1 in
          ^
  Occurrence at 19:8-9:
      let b = return 1 in
          ^

sequential let*
  $ occurrences 25:8
  Occurrences of:
      let a = return 1 in
          ^
  Occurrence at 25:8-9:
      let a = return 1 in
          ^
  $ occurrences 26:8
  Occurrences of:
      let b = return 1 in
          ^
  Occurrence at 26:8-9:
      let b = return 1 in
          ^
