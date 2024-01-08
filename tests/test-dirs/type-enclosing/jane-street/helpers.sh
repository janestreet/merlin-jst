# [run <file> <pos>] queries <file> at position <pos>. The multiline
# json that merlin produces can't be parsed by jq, so we use [paste -sd ' ']
# to print everything on one line.

run_raw_with_verbosity () {
  file=$1
  position=$2
  verbosity=$3
  $MERLIN single type-enclosing -position "$position" -verbosity "$verbosity" \
    -filename "$file" < "$file"
}

# The multiline json that merlin produces can't be parsed by jq, as they include
# escape characters in string literals, so we do a tremendous hack to rewrite \n
# in string literals to \\n. Really we should teach merlin how to produce valid
# json.

run_with_verbosity () {
  verbosity=$3
  echo -n "With verbosity $verbosity: "
  run_raw_with_verbosity "$@" |
  tr '\n' '\a'  |
  sed ':a;s/\(^[^\"]*\"\([^\"]*\"[^\"]*\"\)*[^\"]*\)\a/\1\\n/;ta' |
  sed 's/\a/\n/g' |
  jq '.value[0].type' |
  sed 's/\\n/\n/g'
}

run () {
  file=$1
  position=$2
  line=$(echo "$position" | cut -d ':' -f 1)
  col=$(echo "$position" | cut -d ':' -f 2)
  sed -n "${line}p" "$file"
  printf "%*s^\n" "$(expr $col - 1)" ''
  run_with_verbosity "$file" "$position" 0
  run_with_verbosity "$file" "$position" 1
  echo
}
