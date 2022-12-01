#!/bin/bash

# Script arguments with their default values
repository=https://github.com/ocaml-flambda/flambda-backend
commitish=main
subdirectory=ocaml

function usage () {
  cat <<USAGE
Usage: $0 [COMMITISH [REPO [SUBDIRECTORY]]]

Fetch the new compiler sources and patch Merlin to keep Merlin's local copies of
things in sync.  By default, this will pull the "$commitish" branch from
<$repository> and look in "$subdirectory/" for the compiler source, but the
branch can be overridden by any commitish (branch, tag, full (not abbreviated!)
commit hash, etc.), the repository can be overridden by any URL, and the
subdirectory can be overriden by any path (including ".").
USAGE
}

function repository-commit () {
  if [[ $repository =~ ^https://github.com/(.*) ]]; then
    echo "${BASH_REMATCH[1]}@$1"
  else
    echo "$repository @ $1"
  fi
}

case "$1" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
esac

if [[ $# -le 3 ]]; then
  commitish="${1-$commitish}"
  repository="${2-$repository}"
  subdirectory="${3-$subdirectory}"
else
  usage >&2
  exit 1
fi

if ! git diff --quiet; then
  echo "Working directory must be clean before using this script,"
  echo "but currently has the following changes:"
  git diff --stat
  exit 1
fi

# Used for patch output
old_base_rev="$(cat upstream/ocaml_flambda/base-rev.txt)"
current_head="$(git symbolic-ref --short HEAD)"

# First, fetch the new flambda-backend sources (which include ocaml-jst) and
# copy into upstream/ocaml_flambda
git fetch "$repository" "$commitish"
rev=$(git rev-parse FETCH_HEAD)
cd upstream/ocaml_flambda
echo $rev > base-rev.txt
for file in $(git ls-tree --name-only -r HEAD | grep -v base-rev.txt); do
  git show "FETCH_HEAD:$subdirectory/$file" > "$file";
done
git add -u .
cd ../..
git commit -m "Import ocaml sources for $(repository-commit "$(git describe --always $rev)")"

# Annotations for diff3 regions; "@" would be more natural than ":" but confuses
# smerge-mode's highlighting
short_ocaml_repo="${repository#https://github.com/}"
old_marker="janestreet/merlin-jst:$current_head"
parent_marker="$short_ocaml_repo:$old_base_rev"
new_marker="$short_ocaml_repo:$commitish"

# Then patch src/ocaml using the changes you just imported
for file in $(git diff --no-ext-diff --name-only HEAD^ HEAD); do
  base=${file#upstream/ocaml_flambda/}
  case $base in
    # If you add new files here, you need to apply the full diff manually once,
    # otherwise the merge won't pick up on old changes!
    parsing/lexer.mll) tgt=preprocess/lexer_raw.mll;;
    parsing/parser.mly) tgt=preprocess/parser_raw.mly;;
    utils/clflags.ml*|utils/config.ml*)
      printf '\e[7mIgnoring changes to %s, inspect it manually.\e[0m\n' "$base"
      continue;;
    *) tgt=$base;;
  esac
  tgt=src/ocaml/$tgt

  # Not all files are necessary
  if [ ! -e $tgt ]; then continue; fi

  err=$(patch --merge=diff3 $tgt <(git diff --no-ext-diff HEAD^ HEAD -- $file))
  # ignore patch output if it worked
  if [ $? = 0 ]; then
    git add -u $tgt
  else
    sed -i \
        -e 's!^<<<<<<<$!& '"$old_marker"'!'    \
        -e 's!^|||||||$!& '"$parent_marker"'!' \
        -e 's!^>>>>>>>$!& '"$new_marker"'!'    \
        $tgt
    echo "$err"
  fi
  rm -f $tgt.orig
done
