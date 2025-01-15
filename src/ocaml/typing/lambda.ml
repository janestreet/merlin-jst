(* The lambda representation is of no interest for Merlin, but some types are
   used by [value_rec_check]. *)

type immediate_or_pointer =
  | Immediate
  | Pointer

type boxed_float = Primitive.boxed_float =
  | Boxed_float64
  | Boxed_float32

type boxed_integer = Primitive.boxed_integer =
  | Boxed_int64
  | Boxed_nativeint
  | Boxed_int32

type boxed_vector = Primitive.boxed_vector =
  | Boxed_vec128

type unboxed_float = Primitive.unboxed_float =
  | Unboxed_float64
  | Unboxed_float32

type unboxed_integer = Primitive.unboxed_integer =
  | Unboxed_int64
  | Unboxed_nativeint
  | Unboxed_int32

type unboxed_vector = Primitive.unboxed_vector =
  | Unboxed_vec128

type array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray of unboxed_float
  | Punboxedintarray of unboxed_integer
  | Punboxedvectorarray of unboxed_vector
  | Pgcscannableproductarray of unit
  | Pgcignorableproductarray of unit
