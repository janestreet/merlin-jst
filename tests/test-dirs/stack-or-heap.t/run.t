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
  >     if [ $l -eq $l1 ]; then u1=$c1; else u1=0; fi
  >     if [ $l -eq $l2 ]; then u2=$c2; else u2=${#txt}; fi
  >     printf '|%s\n|%s%s\n' \
  >       "$txt" "$(rep $u1 ' ')" "$(rep $(expr $u2 - $u1) '^')"
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

  $ run () {
  >   file=$1
  >   position=$2
  >   index=$3
  >   line=$(echo "$position" | cut -d ':' -f 1)
  >   col=$(echo "$position" | cut -d ':' -f 2)
  >   highlight_range "$file" $line $(expr $col - 1) $line $col
  >   merlin=$(
  >     $MERLIN single stack-or-heap-enclosing -position "$position" -verbosity "$verbosity" \
  >       -filename "$file" < "$file" |
  >       tr '\n' '\a'  |
  >       sed ':a;s/\(^[^\"]*\"\([^\"]*\"[^\"]*\"\)*[^\"]*\)\a/\1\\n/;ta' |
  >       sed 's/\a/\n/g'
  >   )
  >   echo
  >   if [ "$(echo "$merlin" | jq ".value[$index]")" != null ]
  >   then
  >     highlight_range "$file" $(
  >       echo "$merlin" |
  >         jq ".value[$index] | \"\(.start.line) \(.start.col) \(.end.line) \(.end.col)\"" |
  >         tr -d '"'
  >     )
  >     echo
  >   fi
  >   echo "$merlin" | jq ".value[$index].stack_or_heap" | sed 's/\\n/\n/g'
  >   echo
  > }

  $ run_annotated_file () {
  >   orig_file=$1
  >   if [ $# -lt 2 ]; then until=0; else until=$(expr "$2" - 1); fi
  >   process_cursors "$orig_file" | while read -r lc
  >   do
  >     for i in $(seq 0 $until)
  >     do
  >       run "$orig_file.tmp.ml" $lc $i
  >     done
  >   done
  >   rm "$orig_file.tmp.ml"
  > }

(I) Variant constructors

  $ run_annotated_file constructors.ml
  |  Some (g z)
  |        ^
  
  |  Some (g z)
  |  ^^^^^^^^^^
  
  "heap"
  
  |  exclave_ Some (g z)
  |                 ^
  
  |  exclave_ Some (g z)
  |           ^^^^^^^^^^
  
  "stack"
  
  |  let z = Some (g x) in
  |                ^
  
  |  let z = Some (g x) in
  |          ^^^^^^^^^^
  
  "couldn't tell whether stack or heap"
  
  |  None
  |    ^
  
  |  None
  |  ^^^^
  
  "does not allocate (constructors without arguments don't allocate)"
  
  |  exclave_ None
  |             ^
  
  |  exclave_ None
  |           ^^^^
  
  "does not allocate (constructors without arguments don't allocate)"
  
  |  Box (g z)
  |       ^
  
  |  Box (g z)
  |  ^^^^^^^^^
  
  "does not allocate (unboxed constructors don't allocate)"
  
(II) Variants

  $ run_annotated_file variants.ml
  |  `Some (g z)
  |         ^
  
  |  `Some (g z)
  |  ^^^^^^^^^^^
  
  "heap"
  
  |  exclave_ `Some (g z)
  |                  ^
  
  |  exclave_ `Some (g z)
  |           ^^^^^^^^^^^
  
  "stack"
  
  |  let z = `Some (g x) in
  |                 ^
  
  |  let z = `Some (g x) in
  |          ^^^^^^^^^^^
  
  "couldn't tell whether stack or heap"
  
  |  `None
  |     ^
  
  |  `None
  |  ^^^^^
  
  "does not allocate (variants without arguments don't allocate)"
  
  |  exclave_ `None
  |              ^
  
  |  exclave_ `None
  |           ^^^^^
  
  "does not allocate (variants without arguments don't allocate)"
  

(III) Records

  $ run_annotated_file records.ml
  |  { z }
  |    ^
  
  |  { z }
  |  ^^^^^
  
  "heap"
  
  |  exclave_ { z }
  |             ^
  
  |  exclave_ { z }
  |           ^^^^^
  
  "stack"
  
  |  let y = { z = x } in
  |                ^
  
  |  let y = { z = x } in
  |          ^^^^^^^^^
  
  "heap"
  
  |  { z }
  |    ^
  
  |  { z }
  |  ^^^^^
  
  "does not allocate (unboxed records don't allocate)"
  
  |  exclave_ { z }
  |             ^
  
  |  exclave_ { z }
  |           ^^^^^
  
  "does not allocate (unboxed records don't allocate)"
  

(IV) Closures

  $ run_annotated_file closures.ml
  |  fun x -> g x
  |         ^
  
  |  fun x -> g x
  |  ^^^^^^^^^^^^
  
  "heap"
  
  |  exclave_ fun x -> g x
  |                  ^
  
  |  exclave_ fun x -> g x
  |           ^^^^^^^^^^^^
  
  "stack"
  
  |  fun x -> x
  |         ^
  
  |  fun x -> x
  |  ^^^^^^^^^^
  
  "heap"
  
  |  let z = x + y in
  |            ^
  
  null
  
  |let f g x y =
  |          ^
  
  |let f g x y =
  |      ^^^^^^^
  |  let z = x + y in
  |^^^^^^^^^^^^^^^^^^
  |  exclave_ Some (g z)
  |^^^^^^^^^^^^^^^^^^^^^
  
  "heap"
  

(V) Records

  $ run_annotated_file field_access.ml
  |let f (t : t) = t.a
  |                 ^
  
  |let f (t : t) = t.a
  |                ^^^
  
  "does not allocate (field access only allocates when retrieving [float]s from an unboxed float record)"
  
  |let f (t : t) = t.a
  |                 ^
  
  |let f (t : t) = t.a
  |                ^^^
  
  "does not allocate (field access only allocates when retrieving [float]s from an unboxed float record)"
  
  |let f (t : floats_t) = t.a
  |                        ^
  
  |let f (t : floats_t) = t.a
  |                       ^^^
  
  "heap"
  
  |let f (t : floats_t) = exclave_ t.a
  |                                 ^
  
  |let f (t : floats_t) = exclave_ t.a
  |                                ^^^
  
  "stack"
  

(VI) Arrays

  $ run_annotated_file arrays.ml
  |let x () = [| 1; 2; 3 |]
  |                 ^
  
  |let x () = [| 1; 2; 3 |]
  |           ^^^^^^^^^^^^^
  
  "heap"
  
  |let x () = exclave_ [| 1; 2; 3 |]
  |                          ^
  
  |let x () = exclave_ [| 1; 2; 3 |]
  |                    ^^^^^^^^^^^^^
  
  "stack"
  

(VII) Nested

  $ run_annotated_file nested.ml 5
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                       ^
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                 ^^^^^^^^
  
  "heap"
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                       ^
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                           ^^^^^^^^^^^^^^
  
  "heap"
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                       ^
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                     ^^^^^^^^^^^^^^^^^^^^^^
  
  "stack"
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                       ^
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                   ^^^^^^^^^^^^^^^^^^^^^^^^
  
  "stack"
  
  |let f x = exclave_ T { t = Not_t (Some x) }
  |                                       ^
  
  null
  
(VIII) Nonsense

  $ run_annotated_file nonsensical.ml
  |module M = struct
  |       ^
  
  null
  
  |
  |      ^
  
  null
  
(IX) Unfinished

  $ run_annotated_file unfinished.ml
  |  let t = { x = f x } in
  |                  ^
  
  |  let t = { x = f x } in
  |          ^^^^^^^^^^^
  
  "stack"
  
  |  let t = { x = f x } in
  |                  ^
  
  |  let t = { x = f x } in
  |          ^^^^^^^^^^^
  
  "couldn't tell whether stack or heap"
  
