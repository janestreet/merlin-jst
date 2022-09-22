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
<$repository> and look in "$subdirectory/" for the compiler
source, but the branch can be overridden by any commitish (branch, tag, commit,
etc.), the repository can be overridden by any URL, and the subdirectory can be
overriden by any path (including ".").
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

# First, fetch the new ocaml-jst sources and copy into upstream/ocaml_jst
git fetch "$repository" "$commitish"
rev=$(git rev-parse FETCH_HEAD)
cd upstream/ocaml_jst
echo $rev > base-rev.txt
for file in $(git ls-tree --name-only -r HEAD | grep -v base-rev.txt); do
  git show "FETCH_HEAD:$subdirectory/$file" > "$file";
done
git add -u .
cd ../..
git commit -m "Import ocaml sources for $(repository-commit "$(git describe --always $rev)")"

# Then patch src/ocaml using the changes you just imported
for file in $(git diff --no-ext-diff --name-only HEAD^ HEAD); do
  base=${file#upstream/ocaml_jst/}
  case $base in
    # If you add new files here, you need to apply the full diff manually once,
    # otherwise the merge won't pick up on old changes!
    parsing/lexer.mll) tgt=preprocess/lexer_raw.mll;;
    parsing/parser.mly) tgt=preprocess/parser_raw.mly;;
    utils/clflags.ml*) echo "Ignoring changes to $base"; continue;;
    *) tgt=$base;;
  esac
  tgt=src/ocaml/$tgt

  # Not all files are necessary
  if [ ! -e $tgt ]; then continue; fi

  err=$(patch --merge $tgt <(git diff --no-ext-diff HEAD^ HEAD -- $file))
  # ignore patch output if it worked
  if [ $? = 0 ]; then
    git add -u $tgt
  else
    echo "$err"
  fi
  rm -f $tgt.orig
done

