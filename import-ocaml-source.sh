#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

# Script arguments with their default values
commitish=main
repository=https://github.com/ocaml-flambda/flambda-backend
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

This attempts to import new files from the compiler by running the
"import_added_ocaml_source_files.sh" script. If that doesn't work, you can also
try making matched pairs of files in this repository with the right names: one
in "upstream/ocaml_flambda/", and one in "src/ocaml".  Then running the script
will pull in the named file(s).
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

# First, add any files that have been added since the last import.
./import-added-ocaml-source-files.sh "$commitish" "$repository" "$subdirectory"

# Then, fetch the new flambda-backend sources (which include ocaml-jst) and
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
