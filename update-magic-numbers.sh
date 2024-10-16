#!/bin/bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

# This script updates the magic numbers in src/ocaml/utils/config.ml and
# src/ocaml/typing/magic_numbers.ml based on the magic numbers in
# upstream/ocaml_flambda/configure

source_file=upstream/ocaml_flambda/configure
target_magic_file=src/ocaml/typing/magic_numbers.ml
target_config_file=src/ocaml/utils/config.ml

function usage () {
  cat <<USAGE
Usage: $0 VERSION

Update the magic numbers in $target_config_file and $target_magic_file
based on the magic numbers in $source_file.

VERSION is the OCaml version that this version of Merlin corresponds to.
USAGE
}

if [[ $# = 1 ]]; then
  case "$1" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
  *)
    version="$1"
    ;;
  esac
else
  usage >&2
  exit 1
fi

# Check that git repo is clean
if [ -n "$(git status --porcelain)" ]; then
  echo "Git workspace is unclean.\nCommit changes before continuing to ensure changes don't get overwritten" >&2
  exit 1
fi

# Collect magic numbers by greping for them in upstream/ocaml_flambda/configure
cmi_magic_number=$(grep -oP 'CMI_MAGIC_NUMBER=\K[a-zA-Z0-9]*' "$source_file")
ast_impl_magic_number=$(grep -oP 'AST_IMPL_MAGIC_NUMBER=\K[a-zA-Z0-9]*' "$source_file")
ast_intf_magic_number=$(grep -oP 'AST_INTF_MAGIC_NUMBER=\K[a-zA-Z0-9]*' "$source_file")
cmt_magic_number=$(grep -oP 'CMT_MAGIC_NUMBER=\K[a-zA-Z0-9]*' "$source_file")
cms_magic_number=$(grep -oP 'CMS_MAGIC_NUMBER=\K[a-zA-Z0-9]*' "$source_file")

# Create an index magic number based on the cmi magic number.
# The cmi magic number is expected to be start with Caml1999I and end in some number.
# The index magic number is made to be Merl2023I, suffixed with that number.
# ex: Caml1999I551 -> Merl2023I551
index_magic_number=$(echo "$cmi_magic_number" | sed -E 's/Caml1999I([0-9]+)/Merl2023I\1/')

# Update src/ocaml/typing/magic_numbers.ml
if grep -q "$version" "$target_magic_file"; then
  echo "$target_magic_file already contains magic number for $version; skipping"
else
  echo "Updating magic numbers in $target_magic_file"
  line_to_insert="| \"$cmi_magic_number\" -> Some \"$version\""
  # The below sed looks for a line like:
  #    | "Caml1999I551" -> Some "5.2.0minus-1"
  # followed by a line like:
  #    | _ -> None
  # and inserts $line_to_insert between them.
  # The :a;N;$!ba; is some sed magic that makes it read in the entire file before
  # substituting, which is necessary to match mutli-line patterns.
  sed -i -E ':a;N;$!ba;s/(\| "[a-zA-Z0-9]+" -> Some "\S*")\n(\s*)(\| _ -> None)/\1\n\2'"$line_to_insert"'\n\2\3/' "$target_magic_file"
fi

# Update the magic numbers in src/ocaml/utils/config.ml
echo "Updating magic numbers in $target_config_file"
function replace_magic_number () {
  name="$1"
  value="$2"
  sed -i 's/let '"$name"' = "[^"]*"/let '"$name"' = "'"$value"'"/' "$target_config_file"
}
replace_magic_number cmi_magic_number "$cmi_magic_number"
replace_magic_number ast_impl_magic_number "$ast_impl_magic_number"
replace_magic_number ast_intf_magic_number "$ast_intf_magic_number"
replace_magic_number cmt_magic_number "$cmt_magic_number"
replace_magic_number cms_magic_number "$cms_magic_number"
replace_magic_number index_magic_number "$index_magic_number"

# After updating magic numbers, check that all strings in $target_config_file are one of
# the above magic numbers (or some other known string). This helps ensure that there were
# not any new magic numbers added that this script does not know about.
okay_strings=".mli|$cmi_magic_number|$ast_impl_magic_number|$ast_intf_magic_number|$cmt_magic_number|$cms_magic_number|$index_magic_number"
regex='"(?!'"$okay_strings"')[^"]*"'
if grep -q -P "$regex" "$target_config_file"; then
  echo "Warning: There may be new magic numbers in $target_config_file that were not updated:" >&2
  grep -P "$regex" "$target_config_file" >&2
fi
