  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I558",
    "ast_intf_magic_number": "Caml1999N558",
    "ast_impl_magic_number": "Caml1999M558",
    "cmt_magic_number": "Caml1999T558",
    "cms_magic_number": "Caml1999S558",
    "index_magic_number": "Merl2023I558"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I558",
    "ast_intf_magic_number": "Caml1999N558",
    "ast_impl_magic_number": "Caml1999M558",
    "cmt_magic_number": "Caml1999T558",
    "cms_magic_number": "Caml1999S558",
    "index_magic_number": "Merl2023I558"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
