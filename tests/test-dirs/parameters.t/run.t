Check that merlin understands the `-parameter` flag.

  $ query_file() {
  >   local action=$1
  >   local file=$2
  >   shift 2
  >   $MERLIN server "$action" "$@" -verbosity 0 -filename "$file" \
  >     < "$file"
  > }

  $ query() {
  >   local action=$1
  >   local file=$2
  >   local pos=$3
  >   shift 3
  >   query_file "$action" "$file" -position "$pos" "$@"
  > }

  $ j() {
  >   revert-newlines | jq "$@" | sed 's/\\n/\n/g'
  > }

  $ query_errors() {
  >   query_file errors "$@" | j '.value'
  > }

  $ query_type() {
  >   query type-enclosing "$@" | j '.value[0]?.type? // .value'
  > }

  $ multi_query() {
  >   query_type "$@"
  >   query document "$@" | j '.value'
  >   query locate "$@" | j '.value'
  > }

  $ multi_query_impl() {
  >   query_type "$@"
  >   query document "$@" | j '.value'
  >   query locate "$@" -look-for interface | j '.value'
  >   query locate "$@" -look-for implementation | j '.value'
  > }

Start by compiling P and a module that uses it:

  $ $OCAMLC -bin-annot-cms -c p.mli -as-parameter

  $ $OCAMLC -bin-annot-cms -c basic.mli basic.ml -parameter P

  $ query_errors ./basic.mli -parameter P
  []

  $ query_errors ./basic.ml -parameter P
  []

The following test is broken. It asks about P itself:

  $ multi_query ./basic.mli 10:11 -parameter P
  "sig
    type t
    val create : unit -> t
    val frob : t -> t
    val to_string : t -> string
  end"
  "\"P\" is a builtin, no documentation is available"
  "\"<internal>\" is a builtin, and it is therefore impossible to jump to its definition"

But this one works. It asks for P.t:

  $ multi_query ./basic.mli 10:13 -parameter P
  "type t"
  "A thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 4,
      "col": 5
    }
  }

Now check the implementation. This is P.create:

  $ multi_query ./basic.ml 3:16 -parameter P
  "unit -> P.t"
  "Make a thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }

And P.to_string:

  $ multi_query ./basic.ml 6:32 -parameter P
  "P.t -> string"
  "Show the thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 13,
      "col": 4
    }
  }

Now let's try with a file that uses P indirectly via Basic:

  $ $OCAMLC -bin-annot-cms -c fancy.mli fancy.ml -parameter P

  $ query_errors ./fancy.mli -parameter P
  []

  $ query_errors ./fancy.ml -parameter P
  []

This is Basic itself:

  $ multi_query fancy.mli 4:13 -parameter P
  "sig
    type t
    val create : unit -> t
    val wrap : P.t -> t
    val p : t -> P.t
    val to_string : t -> string
  end"
  "Basic functionality implemented over the [P] parameter."
  {
    "file": "$TESTCASE_ROOT/basic.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

And Basic.t:

  $ multi_query fancy.mli 4:19 -parameter P
  "type t"
  "A [P.t] with minor enhancements."
  {
    "file": "$TESTCASE_ROOT/basic.mli",
    "pos": {
      "line": 4,
      "col": 5
    }
  }

Also check the implementation. This is Basic.to_string:

  $ multi_query_impl fancy.ml 5:37 -parameter P
  "Basic.t -> string"
  "Convert [t] to string."
  {
    "file": "$TESTCASE_ROOT/basic.mli",
    "pos": {
      "line": 15,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/basic.ml",
    "pos": {
      "line": 6,
      "col": 4
    }
  }

And Basic.wrap, whose type involves the parameter P:

  $ multi_query_impl fancy.ml 4:21 -parameter P
  "P.t -> Basic.t"
  "Make a [t] from a [P.t]."
  {
    "file": "$TESTCASE_ROOT/basic.mli",
    "pos": {
      "line": 10,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/basic.ml",
    "pos": {
      "line": 4,
      "col": 4
    }
  }

Now let's see what happens with aliases and includes:

  $ $OCAMLC -bin-annot-cms -c reexport.mli reexport.ml -parameter P

First check this file without -parameter P to make sure it breaks. This means
we're correctly tracking parameters separately per file, even in server mode.

  $ query_errors reexport.mli
  [
    {
      "start": {
        "line": 6,
        "col": 12
      },
      "end": {
        "line": 6,
        "col": 13
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "The file $TESTCASE_ROOT/p.cmi
  contains the interface of a parameter. P
  is not declared as a parameter for the current unit.
  Hint: Compile the current unit with -parameter P."
    }
  ]

Now check that it works with the flag.

  $ query_errors reexport.mli -parameter P
  []

  $ query_errors reexport.ml -parameter P
  []

Do we understand the type of [As_alias]?

  $ query_type reexport.ml 4:10 -parameter P
  "sig
    type t = P.t
    val create : unit -> t
    val frob : t -> t
    val to_string : t -> string
  end"

This only compiles if the various ways of getting at [P.t] are equivalent:

  $ $OCAMLC -bin-annot-cms -c use_reexported.mli use_reexported.ml -parameter P

This is the function [check_that_types_are_the_same]:

  $ query_type use_reexported.ml 1:5 -parameter P
  "Reexport.As_alias.t -> Reexport.Included.t -> P.t"

This is a reference to [Reexport.As_alias.create]:

  $ multi_query use_reexported.ml 6:30 -parameter P
  "unit -> Reexport.As_alias.t"
  "Make a thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }

And this goes to [Reexport.Included.create]:

  $ multi_query use_reexported.ml 7:30 -parameter P
  "unit -> Reexport.Included.t"
  "Make a thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }

Now let's try instantiating things.

  $ $OCAMLC -bin-annot-cms -c p_int.mli p_int.ml -as-argument-for P

Suppress some warnings that are only there due to compiler bugs:

  $ instance_warnings="-w -53"

Compile a file that uses things from an instance of [Basic]:

  $ $OCAMLC -bin-annot-cms -c use_basic_int.ml $instance_warnings

  $ query_errors use_basic_int.ml $instance_warnings
  []
 
Check that we understand everything about [Basic[P:P_int].create]:

  $ multi_query_impl use_basic_int.ml 3:28
  "unit -> Basic_int.t"
  "Make a [t] from scratch."
  {
    "file": "$TESTCASE_ROOT/basic.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/basic.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Do the same with [Fancy]:

  $ $OCAMLC -bin-annot-cms -c use_fancy_int.ml $instance_warnings

  $ query_errors use_fancy_int.ml $instance_warnings
  []

The type of [Fancy_p_int.create] is something that isn't currently valid OCaml
syntax, so this output isn't ideal:
 
  $ multi_query_impl use_fancy_int.ml 5:28
  "Basic[P:P_int].t -> Fancy_int.t"
  "Make something fancy out of something basic."
  {
    "file": "$TESTCASE_ROOT/fancy.mli",
    "pos": {
      "line": 4,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/fancy.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Now let's see how instances work with aliases and includes:

  $ $OCAMLC -bin-annot-cms -c use_reexported_int.ml $instance_warnings

  $ query_errors use_reexported_int.ml $instance_warnings
  []

Do we see where [Reexport_int.As_alias.create] comes from? (Note: It would be
nice to delegate to [p.mli] here when there's no documentation for the
implementing .mli.)

  $ multi_query_impl use_reexported_int.ml 3:37 $instance_warnings
  "unit -> Reexport_int.As_alias.t"
  "No documentation available"
  {
    "file": "$TESTCASE_ROOT/p_int.mli",
    "pos": {
      "line": 3,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/p_int.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Can we follow an include? (FIXME: Currently no. It should go to [p_int.ml]
rather than [p.mli].)

  $ multi_query_impl use_reexported_int.ml 4:37 $instance_warnings
  "unit -> Reexport_int.Included.t"
  "Make a thing."
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }
  {
    "file": "$TESTCASE_ROOT/p.mli",
    "pos": {
      "line": 7,
      "col": 4
    }
  }
