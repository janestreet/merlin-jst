[run <file> <pos>] queries <file> at position <pos>. The multiline
json that merlin produces can't be parsed by jq, as they include
escape characters in string literals, so we do a tremendous hack to
rewrite \n in string literals to \\n. Really we should teach merlin
how to produce valid json.

  $ rep () {
  >   printf '%*s' "$1" '' | tr ' ' "$2"
  > }

  $ highlight_range () {
  >   file=$1
  >   l1=$2
  >   c1=$3
  >   l2=$4
  >   c2=$5
  >   for l in $(seq $l1 $l2)
  >   do
  >     txt=$(sed -n "${l}p" "$file")
  >     if [ $l -eq $l1 ]; then u1=$c1; else u1=1; fi
  >     if [ $l -eq $l2 ]; then u2=$c2; else u2=${#txt}; fi
  >     printf '|%s\n|%s%s\n' \
  >       "$txt" "$(rep $(expr $u1 - 1) ' ')" "$(rep $(expr $u2 - $u1 + 1) '^')"
  >   done
  > }

  $ process_cursors () {
  >   orig_file=$1
  >   l=1
  >   echo '' > "$orig_file.tmp.ml"
  >   while IFS= read -r line
  >   do
  >     cursor=$(echo "$line" | sed -En 's/^( *\(\* *\^) *\*\)$/\1/p' | tr -d '\n')
  >     if [ -n "$cursor" ]
  >     then
  >       printf "%s:%s\n" $l $(echo -n "$cursor" | wc -c)
  >     else
  >       echo "$line" >> "$orig_file.tmp.ml"
  >       l=$(expr $l + 1)
  >     fi
  >   done < "$orig_file"
  > }

  $ run_with_verbosity () {
  >   file=$1
  >   position=$2
  >   verbosity=$3
  >   merlin=$(
  >     $MERLIN single stack-or-heap-enclosing -position "$position" -verbosity "$verbosity" \
  >       -filename "$file" < "$file" |
  >       tr '\n' '\a'  |
  >       sed ':a;s/\(^[^\"]*\"\([^\"]*\"[^\"]*\"\)*[^\"]*\)\a/\1\\n/;ta' |
  >       sed 's/\a/\n/g'
  >   )
  >   if [ $verbosity -eq 0 ]; then
  >     echo
  >     highlight_range "$file" $(
  >       echo "$merlin" |
  >         jq '.value[0] | "\(.start.line) \(.start.col) \(.end.line) \(.end.col)"' |
  >         tr -d '"'
  >     )
  >     echo
  >   fi
  >   printf "With verbosity $verbosity: "
  >   echo "$merlin" | jq '.value[0].type' | sed 's/\\n/\n/g'
  > }

  $ run () {
  >   file=$1
  >   position=$2
  >   line=$(echo "$position" | cut -d ':' -f 1)
  >   col=$(echo "$position" | cut -d ':' -f 2)
  >   highlight_range "$file" $line $col $line $col
  >   run_with_verbosity "$file" "$position" 0
  >   run_with_verbosity "$file" "$position" 1
  >   echo
  > }

  $ run_annotated_file () {
  >   orig_file=$1
  >   process_cursors "$orig_file" | while read -r lc
  >   do
  >     run "$orig_file.tmp.ml" $lc
  >   done
  >   rm "$orig_file.tmp.ml"
  > }

(I) Variants

  $ run_annotated_file variants.ml
  |  Some (g z)
  |        ^
  
  |  Some (g z)
  | ^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, linearity:?"
  With verbosity 1: "Global, uniqueness:?21[> ?33], linearity:^?20"
  
  |  exclave_ Some (g z)
  |                 ^
  
  |  exclave_ Some (g z)
  |          ^^^^^^^^^^^
  
  With verbosity 0: "Local, uniqueness:?, linearity:?"
  With verbosity 1: "Local, uniqueness:?57[> ?69], linearity:^?56"
  
  |  let z = Some (g x) in
  |                ^
  
  |  let z = Some (g x) in
  |         ^^^^^^^^^^^
  
  With verbosity 0: "locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "locality:?58[> ?62], uniqueness:?94[> ?101], linearity:^?95"
  
  |  None
  |    ^
  
  |  None
  | ^^^^^
  
  With verbosity 0: "does not allocate"
  With verbosity 1: "does not allocate"
  
  |  exclave_ None
  |             ^
  
  |  exclave_ None
  |          ^^^^^
  
  With verbosity 0: "does not allocate"
  With verbosity 1: "does not allocate"
  

(II) Records

  $ run_annotated_file records.ml
  |  { z }
  |    ^
  
  |  { z }
  | ^^^^^^
  
  With verbosity 0: "Global, Shared, linearity:?"
  With verbosity 1: "Global, Shared, linearity:^?20"
  
  |  exclave_ { z }
  |             ^
  
  |  exclave_ { z }
  |          ^^^^^^
  
  With verbosity 0: "Local, Shared, linearity:?"
  With verbosity 1: "Local, Shared, linearity:^?51"
  
  |  let y = { z = x } in
  |                ^
  
  |  let y = { z = x } in
  |         ^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, linearity:?"
  With verbosity 1: "Global, uniqueness:?77[> ?74], linearity:^?78[> ?75]"
  
  |  { z }
  |    ^
  
  |  { z }
  | ^^^^^^
  
  With verbosity 0: "does not allocate"
  With verbosity 1: "does not allocate"
  
  |  exclave_ { z }
  |             ^
  
  |  exclave_ { z }
  |          ^^^^^^
  
  With verbosity 0: "does not allocate"
  With verbosity 1: "does not allocate"
  

(III) Closures

  $ run_annotated_file closures.ml
  |  fun x -> g x
  |           ^
  
  |  fun x -> g x
  | ^^^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, linearity:?"
  With verbosity 1: "Global, uniqueness:?8, linearity:^?9[> ?6]"
  
  |  exclave_ fun x -> g x
  |                    ^
  
  |  exclave_ fun x -> g x
  |          ^^^^^^^^^^^^^
  
  With verbosity 0: "Local, uniqueness:?, linearity:?"
  With verbosity 1: "Local, uniqueness:?31, linearity:^?32[> ?29]"
  
  |  fun x -> x
  |           ^
  
  |  fun x -> x
  | ^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, linearity:?"
  With verbosity 1: "Global, uniqueness:?54, linearity:^?55[> ?52]"
  
(IV) Nonsense

  $ run_annotated_file nonsensical.ml
  |  let z = x + y in
  |            ^
  
  |let f g x y =
  |     ^^^^^^^^
  |  let z = x + y in
  |^^^^^^^^^^^^^^^^^^
  |  exclave_ Some (g z)
  |^^^^^^^^^^^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, Many"
  With verbosity 1: "Global, uniqueness:?2, Many"
  
(V) Unfinished

  $ run_annotated_file unfinished.ml
  |  let z = Some (g x) in
  |                ^
  
  |  let z = Some (g x) in
  |         ^^^^^^^^^^^
  
  With verbosity 0: "locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "locality:?12[> ?16], uniqueness:?22[> ?29], linearity:^?23"
  
  |  let t = { x = f x } in
  |                  ^
  
  |  let t = { x = f x } in
  |         ^^^^^^^^^^^^
  
  With verbosity 0: "Local, Shared, linearity:?"
  With verbosity 1: "Local, Shared, linearity:^?45"
  
