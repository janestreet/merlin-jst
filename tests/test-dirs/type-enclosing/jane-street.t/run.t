  $ function run () {
  >   file=$1
  >   position=$2
  >   line=$(cut -d ':' -f 1 <<< "$position")
  >   col=$(cut -d ':' -f 2 <<< "$position")
  >   sed -n ${line}p $file
  >   printf "%*s^\n" $((col-1)) ''
  >   function with_verbosity () {
  >     verbosity=$1
  >     echo -n "With verbosity $verbosity: "
  >     $MERLIN single type-enclosing -position $position -verbosity $verbosity \
  >       -filename $file < $file |
  >       jq '.value[0].type'
  >   }
  >   with_verbosity 0
  >   with_verbosity 1
  >   echo
  > }

(I) Type declarations

  $ run layouts.ml 1:5
  > run layouts.ml 2:5
  > run layouts.ml 3:5
  > run layouts.ml 4:5
  > run layouts.ml 5:5
  type t0 = int
      ^
  With verbosity 0: "type t0 = int"
  With verbosity 1: "type t0 = int"
  
  type t1 : immediate = int
      ^
  With verbosity 0: "type t1 = int"
  With verbosity 1: "type t1 = int"
  
  type t2 : immediate = A | B | C
      ^
  With verbosity 0: "type t2 = A | B | C"
  With verbosity 1: "type t2 = A | B | C"
  
  type t3 : value = D | E of int
      ^
  With verbosity 0: "type t3 = D | E of int"
  With verbosity 1: "type t3 = D | E of int"
  
  type t4 : immediate64 = A
      ^
  With verbosity 0: "type t4 = A"
  With verbosity 1: "type t4 = A"
  

(II) Functions

- definition
  $ run layouts.ml 7:5
  > run layouts.ml 8:5
  > run layouts.ml 9:5
  > run layouts.ml 10:5
  > run layouts.ml 11:5
  let f0 (x : t0) = x
      ^
  With verbosity 0: "t0 -> t0"
  With verbosity 1: "int -> int"
  
  let f1 (x : t1) = x
      ^
  With verbosity 0: "t1 -> t1"
  With verbosity 1: "int -> int"
  
  let f2 (x : t2) = x
      ^
  With verbosity 0: "t2 -> t2"
  With verbosity 1: "t2 -> t2"
  
  let f3 (x : t3) = x
      ^
  With verbosity 0: "t3 -> t3"
  With verbosity 1: "t3 -> t3"
  
  let f4 (x : t4) = x
      ^
  With verbosity 0: "t4 -> t4"
  With verbosity 1: "t4 -> t4"
  

- parameter
  $ run layouts.ml 7:9
  > run layouts.ml 8:9
  > run layouts.ml 9:9
  > run layouts.ml 10:9
  > run layouts.ml 11:9
  let f0 (x : t0) = x
          ^
  With verbosity 0: "t0"
  With verbosity 1: "int"
  
  let f1 (x : t1) = x
          ^
  With verbosity 0: "t1"
  With verbosity 1: "int"
  
  let f2 (x : t2) = x
          ^
  With verbosity 0: "t2"
  With verbosity 1: "type t2 = A | B | C"
  
  let f3 (x : t3) = x
          ^
  With verbosity 0: "t3"
  With verbosity 1: "type t3 = D | E of int"
  
  let f4 (x : t4) = x
          ^
  With verbosity 0: "t4"
  With verbosity 1: "type t4 = A"
  

(III) Polymorphic functions
- definition
  $ run layouts.ml 13:5
  > run layouts.ml 14:5
  > run layouts.ml 15:5
  > run layouts.ml 16:5
  let poly1 (type a)           (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly2 (type a : value)   (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly3 (type a : float64) (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly4 (type (a : immediate) (b : value)) (f : a -> b -> _) = f
      ^
  With verbosity 0: "('a -> ('b -> 'c)) -> 'a -> ('b -> 'c)"
  With verbosity 1: "('a -> ('b -> 'c)) -> 'a -> ('b -> 'c)"
  

-parameter
  $ run layouts.ml 13:30
  > run layouts.ml 14:30
  > run layouts.ml 15:30
  > run layouts.ml 16:46
  let poly1 (type a)           (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly2 (type a : value)   (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly3 (type a : float64) (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly4 (type (a : immediate) (b : value)) (f : a -> b -> _) = f
                                               ^
  With verbosity 0: "a -> b -> 'a"
  With verbosity 1: "a -> b -> 'a"
  

(V) Polymorphic function client
- definition
  $ run layouts.ml 18:5
  > run layouts.ml 19:5
  > run layouts.ml 20:5
  > run layouts.ml 21:5
  let poly_client1 x = poly1 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly_client2 x = poly2 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly_client3 x = poly3 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly_client4 x = poly4 x
      ^
  With verbosity 0: "('a -> ('b -> 'c)) -> 'a -> ('b -> 'c)"
  With verbosity 1: "('a -> ('b -> 'c)) -> 'a -> ('b -> 'c)"
  

-parameter
  $ run layouts.ml 18:18
  > run layouts.ml 19:18
  > run layouts.ml 20:18
  > run layouts.ml 21:18
  let poly_client1 x = poly1 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  let poly_client2 x = poly2 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  let poly_client3 x = poly3 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  let poly_client4 x = poly4 x
                   ^
  With verbosity 0: "'a -> ('b -> 'c)"
  With verbosity 1: "'a -> ('b -> 'c)"
  
