Setup the test context:

  $ $OCAMLC -c -bin-annot -keep-locs a.ml

Make sure that we do not use locations coming from the cmi when the cmt is
available:

  $ $MERLIN single locate -look-for ml -log-section locate -log-file log \
  > -position 1:12 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ grep -A1 from_uid log | grep -v from_uid | sed '/^--$/d'

  $ rm log

  $ $MERLIN single locate -look-for ml -log-section locate -log-file log \
  > -position 5:12 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ grep -A1 from_uid log | grep -v from_uid | sed '/^--$/d'

  $ rm log

In the absence of cmt though, fallbacking to the cmi loc makes sense:

  $ rm a.cmt

  $ $MERLIN single locate -look-for ml -log-section locate -log-file log \
  > -position 1:12 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ grep -A1 from_uid log | grep -v from_uid
  [1]

  $ rm log
