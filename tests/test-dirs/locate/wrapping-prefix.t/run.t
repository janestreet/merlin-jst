Merlin should be able to resolve a compilation unit whose name is the same as the current
file.

Compile a library named "foo"
  $ $OCAMLC -c foo/foo.ml

Jump to the definition of "Foo.hello".
  $ $MERLIN single locate -position 1:17 -filename bar/foo.ml < bar/foo.ml
  {
    "class": "exception",
    "value": "File \"src/analysis/locate.ml\", line 338, characters 44-50: Assertion failed
  Raised at Merlin_analysis__Locate.Artifact.read in file \"src/analysis/locate.ml\", line 338, characters 44-56
  Called from Merlin_analysis__Locate.load_cmt in file \"src/analysis/locate.ml\", line 525, characters 19-37
  Called from Merlin_analysis__Locate.find_loc_of_uid in file \"src/analysis/locate.ml\", line 797, characters 10-36
  Called from Merlin_analysis__Locate.from_path in file \"src/analysis/locate.ml\", line 914, characters 4-20
  Called from Query_commands.dispatch in file \"src/frontend/query_commands.ml\", lines 648-649, characters 10-34
  Called from Merlin_commands__New_commands.run in file \"src/commands/new_commands.ml\", line 98, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 735, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 741, characters 4-13
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 46, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 63, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 39, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 24, characters 4-15
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", lines 118-119, characters 16-52
  ",
    "notifications": []
  }
