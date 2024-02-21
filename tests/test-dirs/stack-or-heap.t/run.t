[run <file> <pos>] queries <file> at position <pos>. The multiline
json that merlin produces can't be parsed by jq, as they include
escape characters in string literals, so we do a tremendous hack to
rewrite \n in string literals to \\n. Really we should teach merlin
how to produce valid json.

  $ run_with_verbosity () {
  >   file=$1
  >   position=$2
  >   verbosity=$3
  >   echo -n "With verbosity $verbosity: "
  >   $MERLIN single stack-or-heap-enclosing -position "$position" -verbosity "$verbosity" \
  >     -filename "$file" < "$file" |
  >     tr '\n' '\a'  |
  >     sed ':a;s/\(^[^\"]*\"\([^\"]*\"[^\"]*\"\)*[^\"]*\)\a/\1\\n/;ta' |
  >     sed 's/\a/\n/g' |
  >     jq '.value[0] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col) \(.type)"' |
  >     sed 's/\\n/\n/g'
  > }

  $ run () {
  >   file=$1
  >   position=$2
  >   line=$(echo "$position" | cut -d ':' -f 1)
  >   col=$(echo "$position" | cut -d ':' -f 2)
  >   sed -n "${line}p" "$file"
  >   printf "%*s^\n" "$(expr $col - 1)" ''
  >   run_with_verbosity "$file" "$position" 0
  >   run_with_verbosity "$file" "$position" 1
  >   echo
  > }

(I) Type declarations

  $ run inputs.ml 3:9
  > run inputs.ml 7:13
  > run inputs.ml 13:18
  > run inputs.ml 17:17
  > run inputs.ml 22:17
    Some (g z)
          ^
  With verbosity 0: "3:2-3:12 Global, uniqueness:?, linearity:?"
  With verbosity 1: "3:2-3:12 Global, uniqueness:?21[> ?33], linearity:^?20"
  
    let z = x + y in
              ^
  With verbosity 0: "6:6-8:21 Global, uniqueness:?, Many"
  With verbosity 1: "6:6-8:21 Global, uniqueness:?38, Many"
  
    exclave_ Some (g z)
                   ^
  With verbosity 0: "13:11-13:21 Local, uniqueness:?, linearity:?"
  With verbosity 1: "13:11-13:21 Local, uniqueness:?93[> ?105], linearity:^?92"
  
    let z = Some (g x) in
                  ^
  With verbosity 0: "17:10-17:20 locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "17:10-17:20 locality:?81[> ?85], uniqueness:?130[> ?137], linearity:^?131"
  
    let z = Some (g x) in
                  ^
  With verbosity 0: "22:10-22:20 locality:?, uniqueness:?, linearity:?"
  With verbosity 1: "22:10-22:20 locality:?99[> ?103], uniqueness:?162[> ?169], linearity:^?163"
  

