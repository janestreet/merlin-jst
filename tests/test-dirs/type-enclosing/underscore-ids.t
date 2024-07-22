These tests ensure the stability of identifier reconstruction
in the presence of underscores.

1.1
  $ $MERLIN single type-enclosing -position 3:2 -filename under.ml <<EOF | \
  > jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

1.2
  $ $MERLIN single type-enclosing -position 3:3 -filename under.ml <<EOF | \
  >  jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

We try several places in the identifier to check the result stability
2.1
  $ $MERLIN single type-enclosing -position 3:5 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.2
  $ $MERLIN single type-enclosing -position 3:6 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.3
  $ $MERLIN single type-enclosing -position 3:7 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.4
  $ $MERLIN single type-enclosing -position 3:8 -filename under.ml <<EOF
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

3.1
  $ $MERLIN single type-enclosing -position 5:10 -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int option",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "int option -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "'a -> 'b",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": "[
    structure_item (under.ml[1,0+0]..under.ml[1,0+12])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[1,0+4]..under.ml[1,0+6])
            Tpat_var \"aa/275\"
            value_mode meet(local,once,nonportable)(modevar#0[global,many,portable .. global,many,nonportable]);join(shared,contended)(modevar#1[shared,contended .. unique,uncontended])
          expression (under.ml[1,0+9]..under.ml[1,0+12])
            Texp_constant Const_float 4.2
      ]
    structure_item (under.ml[2,13+0]..under.ml[5,70+17])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[2,13+4]..under.ml[2,13+5])
            Tpat_var \"f/276\"
            value_mode meet(local,once,nonportable)(modevar#2[global,many,portable .. global,many,nonportable]);join(shared,contended)(modevar#3[shared,contended .. unique,uncontended])
          expression (under.ml[2,13+6]..under.ml[5,70+17]) ghost
            Texp_function
            region true
            alloc_mode map_comonadic(regional_to_global)(modevar#4[global,many,portable .. global,many,nonportable]);id(modevar#5[shared,contended .. unique,uncontended])
            [
              Nolabel
              Param_pat
                pattern (under.ml[2,13+6]..under.ml[2,13+9])
                  Tpat_var \"x/278\"
                  value_mode map_comonadic(local_to_regional)(modevar#6[global,many,portable .. local,once,nonportable]);join(shared,contended)(modevar#7[shared,contended .. unique,uncontended])
            ]
            Tfunction_body
              expression (under.ml[2,13+18]..under.ml[5,70+17])
                attribute \"merlin.incorrect\"
                  []
                attribute \"merlin.saved-parts\"
                  [
                    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                      Pstr_eval
                      expression (_none_[0,0+-1]..[0,0+-1]) ghost
                        Pexp_constant PConst_int (1,None)
                  ]
                Texp_ident \"*type-error*/281\"
      ]
  ]
  
  
  ",
    "notifications": []
  }
