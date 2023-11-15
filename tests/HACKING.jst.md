
# Testing

To run the tests, first set the `MERLIN_TEST_OCAML_PATH` to point to the install
directory for your flambda-backend build.  Then, `dune test` should work.  E.g.,:

```
MERLIN_TEST_OCAML_PATH=/path/to/installed/flambda-backend/compiler/ dune test
```

Note that we are currently only running a subset of the tests.  Tests are
disabled either because we haven't yet had time to look at why they are failing,
or because they invoke the compiler through dune and it will take some effort
to get that working.  See the FIXMEs in various dune files under `tests/`.
