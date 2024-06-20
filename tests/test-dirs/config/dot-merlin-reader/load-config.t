  $ cat > .merlin <<EOF
  > B build/dir
  > S source/dir
  > BH build-hidden/dir
  > SH source-hidden/dir
  > EOF

  $ FILE=$(pwd)/test.ml; dot-merlin-reader <<EOF | sed 's#[0-9]*:#?:#g'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:B?:$TESTCASE_ROOT/build/dir)(?:S?:$TESTCASE_ROOT/source/dir)(?:BH?:$TESTCASE_ROOT/build-hidden/dir)(?:SH?:$TESTCASE_ROOT/source-hidden/dir))

  $ echo | $MERLIN single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin'
  {
    "build_path": [
      "$TESTCASE_ROOT/build/dir"
    ],
    "source_path": [
      "$TESTCASE_ROOT/source/dir"
    ],
    "hidden_build_path": [
      "$TESTCASE_ROOT/build-hidden/dir"
    ],
    "hidden_source_path": [
      "$TESTCASE_ROOT/source-hidden/dir"
    ],
    "cmi_path": [],
    "cmt_path": [],
    "index_files": [],
    "flags_applied": [],
    "extensions": [],
    "suffixes": [
      {
        "impl": ".ml",
        "intf": ".mli"
      },
      {
        "impl": ".re",
        "intf": ".rei"
      }
    ],
    "stdlib": "lib/ocaml",
    "unit_name": null,
    "wrapping_prefix": null,
    "reader": [],
    "protocol": "json",
    "log_file": null,
    "log_sections": [],
    "flags_to_apply": [],
    "failures": [],
    "assoc_suffixes": [
      {
        "extension": ".re",
        "reader": "reason"
      },
      {
        "extension": ".rei",
        "reader": "reason"
      }
    ]
  }

  $ rm .merlin
