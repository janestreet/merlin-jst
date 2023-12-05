  $ . ./helpers.sh

(I) Type declarations

  $ run modes.ml 8:6
  > run modes.ml 9:6
  > run modes.ml 10:6
  > run modes.ml 11:6
  > run modes.ml 12:6
  > run modes.ml 13:6
  > run modes.ml 14:6
  type t0 = string -> string
       ^
  With verbosity 0: "type t0 = string -> string"
  With verbosity 1: "type t0 = string -> string"
  
  type t1 = local_ string -> string
       ^
  With verbosity 0: "type t1 = local_ string -> string"
  With verbosity 1: "type t1 = local_ string -> string"
  
  type t2 = string -> local_ string
       ^
  With verbosity 0: "type t2 = string -> local_ string"
  With verbosity 1: "type t2 = string -> local_ string"
  
  type t3 = local_ string -> local_ string
       ^
  With verbosity 0: "type t3 = local_ string -> local_ string"
  With verbosity 1: "type t3 = local_ string -> local_ string"
  
  type t4 = local_ string -> string -> string
       ^
  With verbosity 0: "type t4 = local_ string -> string -> string"
  With verbosity 1: "type t4 = local_ string -> string -> string"
  
  type t5 = local_ string -> local_ (string -> string)
       ^
  With verbosity 0: "type t5 = local_ string -> string -> string"
  With verbosity 1: "type t5 = local_ string -> string -> string"
  
  type t6 = local_ string -> (string -> string)
       ^
  With verbosity 0: "type t6 = local_ string -> (string -> string)"
  With verbosity 1: "type t6 = local_ string -> (string -> string)"
  

(II) Functions

- declarations
  $ run modes.ml 16:5
  > run modes.ml 17:5
  > run modes.ml 18:5
  > run modes.ml 19:5
  > run modes.ml 20:5
  let simple0 (x : string) = x
      ^
  With verbosity 0: "string -> string"
  With verbosity 1: "string -> string"
  
  let simple1 (local_ _ : string) = "a"
      ^
  With verbosity 0: "local_ string -> string"
  With verbosity 1: "local_ string -> string"
  
  let simple2 (x : string) = local_ x
      ^
  With verbosity 0: "string -> local_ string"
  With verbosity 1: "string -> local_ string"
  
  let simple3_1 (local_ x : string) = local_ x
      ^
  With verbosity 0: "local_ string -> local_ string"
  With verbosity 1: "local_ string -> local_ string"
  
  let simple3_2 (local_ x : string) = x
      ^
  With verbosity 0: "local_ string -> local_ string"
  With verbosity 1: "local_ string -> local_ string"
  
- params
  $ run modes.ml 16:14
  > run modes.ml 17:21
  > run modes.ml 18:14
  > run modes.ml 19:23
  > run modes.ml 20:23
  let simple0 (x : string) = x
               ^
  With verbosity 0: "string"
  With verbosity 1: "string"
  
  let simple1 (local_ _ : string) = "a"
                      ^
  With verbosity 0: "string"
  With verbosity 1: "string"
  
  let simple2 (x : string) = local_ x
               ^
  With verbosity 0: "string"
  With verbosity 1: "string"
  
  let simple3_1 (local_ x : string) = local_ x
                        ^
  With verbosity 0: "string"
  With verbosity 1: "string @ local"
  
  let simple3_2 (local_ x : string) = x
                        ^
  With verbosity 0: "string"
  With verbosity 1: "string @ local"
  
(III) Constructing values

  $ run modes.ml 27:7
  > run modes.ml 33:7
  > run modes.ml 39:7
    let y = Some x in
        ^
  With verbosity 0: "int option"
  With verbosity 1: "int option
  
  type 'a option = None | Some of 'a"
  
    let y = local_ Some x in
        ^
  With verbosity 0: "int option"
  With verbosity 1: "int option @ local
  
  type 'a option = None | Some of 'a"
  
    let y = Some x in
        ^
  With verbosity 0: "int option"
  With verbosity 1: "int option
  
  type 'a option = None | Some of 'a"
  
(IV) Local parameters

  $ run modes.ml 48:9
  > run modes.ml 53:10
  > run modes.ml 56:9
    match x with
          ^
  With verbosity 0: "string"
  With verbosity 1: "string"
  
    escape x
           ^
  With verbosity 0: "string"
  With verbosity 1: "string"
  
    match x with
          ^
  With verbosity 0: "string"
  With verbosity 1: "string @ local"
  
(V) Larger expression that is local

In this test, we use [run_raw_with_verbosity] so we see all
enclosing types, and not just the one immediately at the cursor.

  $ verbosity0=$(mktemp)
  $ verbosity1=$(mktemp)
  $ run_raw_with_verbosity modes.ml 66:26 0 > "$verbosity0"
  $ run_raw_with_verbosity modes.ml 66:26 1 > "$verbosity1"

Don't print with additional context -- from the [expr1] function
in [modes.ml] it's clear enough what each of the types correspond
to.

  $ diff "$verbosity0" "$verbosity1"
  25c25,27
  <       "type": "string option",
  ---
  >       "type": "string option @ local
  > 
  > type 'a option = None | Some of 'a",
  37c39,41
  <       "type": "string option option",
  ---
  >       "type": "string option option @ local
  > 
  > type 'a option = None | Some of 'a",
  49c53,55
  <       "type": "string option option option",
  ---
  >       "type": "string option option option @ local
  > 
  > type 'a option = None | Some of 'a",
  61c67,69
  <       "type": "string option option option option",
  ---
  >       "type": "string option option option option @ local
  > 
  > type 'a option = None | Some of 'a",
  [1]
