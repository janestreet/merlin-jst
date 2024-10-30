  $ $MERLIN single version | revert-newlines | jq .value
  {
    "version": "The Merlin toolkit version %VERSION%, for Ocaml 5.1.1\n",
    "magicNumbers": {
      "cmi_magic_number": "Caml1999I552",
      "ast_intf_magic_number": "Caml1999N552",
      "ast_impl_magic_number": "Caml1999M552",
      "cmt_magic_number": "Caml1999T552",
      "cms_magic_number": "Caml1999S552",
      "index_magic_number": "Merl2023I552"
    }
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I552",
    "ast_intf_magic_number": "Caml1999N552",
    "ast_impl_magic_number": "Caml1999M552",
    "cmt_magic_number": "Caml1999T552",
    "cms_magic_number": "Caml1999S552",
    "index_magic_number": "Merl2023I552"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
