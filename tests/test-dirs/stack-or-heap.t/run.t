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
  >     jq '.value[0].type' |
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
    Some (g z)
          ^
  With verbosity 0: null
  With verbosity 1: null
  
