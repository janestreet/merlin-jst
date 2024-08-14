<<<<<<< HEAD
(* The lambda representation is of no interest for Merlin, but some types are
   used by [value_rec_check]. *)

type immediate_or_pointer =
  | Immediate
  | Pointer

type boxed_float = Primitive.boxed_float =
  | Pfloat64
  | Pfloat32

type boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

type unboxed_float = boxed_float

type unboxed_integer = boxed_integer

type array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray of unboxed_float
  | Punboxedintarray of unboxed_integer
||||||| 7b73c6aa3f
=======
(* The lambda representation is of no interest for Merlin, but some types are
   used by [value_rec_check]. *)

type immediate_or_pointer =
  | Immediate
  | Pointer

type array_kind =
  Pgenarray | Paddrarray | Pintarray | Pfloatarray
>>>>>>> upstream/main
