#!/bin/bash

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

# Script arguments with their default values
commitish=main
repository=https://github.com/ocaml-flambda/flambda-backend
subdirectory=ocaml

function usage () {
  cat <<USAGE
Usage: $0 [COMMITISH [REPO [SUBDIRECTORY]]]

Fetches any new files that previously hadn't been imported. This ignores
files outside of *directories* that were previously imported,
so if a whole new directory is added, you may need to manually
add the new file.
USAGE
}

if [[ $# -le 3 ]]; then
  commitish="${1-$commitish}"
  repository="${2-$repository}"
  subdirectory="${3-$subdirectory}"
else
  usage >&2
  exit 1
fi

case "${1-unused}" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
esac

# First, fetch the new flambda-backend sources (which include ocaml-jst).

function sorted_files_at_committish() {
  git ls-tree -r --name-only "$1" | sort
}

git fetch "$repository" "$(cat upstream/ocaml_flambda/base-rev.txt)"
git fetch "$repository" "$commitish"
rev=$(git rev-parse FETCH_HEAD)

function files_new_at_fetch_head() {
  comm -13 \
    <(sorted_files_at_committish "$(cat upstream/ocaml_flambda/base-rev.txt)") \
    <(sorted_files_at_committish FETCH_HEAD)
}

function directories_from_previous_import() {
  comm -12 \
    <(cd src/ocaml; ls -d */) \
    <(cd upstream/ocaml_flambda; ls -d */) \
  | xargs -n 1 printf "^$subdirectory/%s\n"
}

files=$(files_new_at_fetch_head | grep -f <(directories_from_previous_import))

echo "The script will attempt to import these files added to directories that had previously been imported:"
echo "$files"

for file in $files; do
  read -p "Import new file $file? [Y/n] " answer
  case ${answer} in
    y|Y|"" )
      echo "Importing $file"
      ocaml_flambda_file=upstream/ocaml_flambda/"${file#$subdirectory/}"
      git show "FETCH_HEAD:$file" > "$ocaml_flambda_file"
      cp "$ocaml_flambda_file" src/$file
      ;;
    * )
      echo "Skipping $file; run '$0' again in order to make a different decision"
      ;;
  esac
done
