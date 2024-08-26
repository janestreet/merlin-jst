  $ cat > .merlin <<EOF
  > B build/dir
  > S source/dir
  > BH build-hidden/dir
  > SH source-hidden/dir
  > STDLIB /stdlib
  > WRAPPING_PREFIX Prefix__
  > UNIT_NAME_FOR a Prefix__a
  > UNIT_NAME_FOR b b
  > EOF

  $ FILE=$(pwd)/test.ml; dot-merlin-reader <<EOF | sed 's#[0-9]*:#?:#g'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:B?:$TESTCASE_ROOT/build/dir)(?:S?:$TESTCASE_ROOT/source/dir)(?:BH?:$TESTCASE_ROOT/build-hidden/dir)(?:SH?:$TESTCASE_ROOT/source-hidden/dir)(?:WRAPPING_PREFIX?:Prefix__)(?:UNIT_NAME_FOR(?:a?:Prefix__a))(?:UNIT_NAME_FOR(?:b?:b))(?:STDLIB?:/stdlib))

Use ocamlmerlin instead of $MERLIN for this test because $MERLIN configures the stdlib,
but we want to observe the stdlib being configured via the STDLIB directive.
  $ echo | ocamlmerlin single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin'
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
    "stdlib": "/stdlib",
    "unit_name": null,
    "unit_name_for": {
      "b": "b",
      "a": "Prefix__a"
    },
    "wrapping_prefix": null,
    "source_root": null,
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
