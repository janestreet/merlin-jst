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

(I) Tests

  $ run inputs.ml 3:9
  > run inputs.ml 7:13
  > run inputs.ml 13:18
  > run inputs.ml 17:17
  > run inputs.ml 22:17
  |  Some (g z)
  |        ^
  
  |  Some (g z)
  | ^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, linearity:?"
  With verbosity 1: "Global, uniqueness:?21[> ?33], linearity:^?20"
  
  |  let z = x + y in
  |            ^
  
  |let f g x y =
  |     ^^^^^^^^
  |  let z = x + y in
  |^^^^^^^^^^^^^^^^^^
  |  exclave_ Some (g z)
  |^^^^^^^^^^^^^^^^^^^^^
  
  With verbosity 0: "Global, uniqueness:?, Many"
  With verbosity 1: "Global, uniqueness:?38, Many"
  
  |  exclave_ Some (g z)
  |                 ^
  
  |  exclave_ Some (g z)
  |          ^^^^^^^^^^^
  
  With verbosity 0: "Local, uniqueness:?, linearity:?"
  With verbosity 1: "Local, uniqueness:?93[> ?105], linearity:^?92"
  
  |  let z = Some (g x) in
  |                ^
  
  |  let z = Some (g x) in
  |         ^^^^^^^^^^^
  
  With verbosity 0: "locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "locality:?81[> ?85], uniqueness:?130[> ?137], linearity:^?131"
  
  |  let z = Some (g x) in
  |                ^
  
  |  let z = Some (g x) in
  |         ^^^^^^^^^^^
  
  With verbosity 0: "locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "locality:?99[> ?103], uniqueness:?162[> ?169], linearity:^?163"
  

