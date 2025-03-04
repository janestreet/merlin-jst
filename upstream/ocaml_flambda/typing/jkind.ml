(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mode
open Jkind_types

[@@@warning "+9"]

(* A *sort* is the information the middle/back ends need to be able to
   compile a manipulation (storing, passing, etc) of a runtime value. *)
module Sort = struct
  include Jkind_types.Sort

  module Flat = struct
    type t =
      | Var of Var.id
      | Base of base
  end
end

type sort = Sort.t

type type_expr = Types.type_expr

(* A *layout* of a type describes the way values of that type are stored at
   runtime, including details like width, register convention, calling
   convention, etc. A layout may be *representable* or *unrepresentable*.  The
   middle/back ends are unable to cope with values of types with an
   unrepresentable layout. The only unrepresentable layout is `any`, which is
   the top of the layout lattice. *)
module Layout = struct
  open Jkind_types.Layout

  type nonrec 'sort t = 'sort t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  module Const = struct
    type t = Const.t =
      | Any
      | Base of Sort.base
      | Product of t list

    let max = Any

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> Sort.equal_base b1 b2
      | Any, Any -> true
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | (Base _ | Any | Product _), _ -> false

    module Static = struct
      let value = Base Sort.Value

      let void = Base Sort.Void

      let float64 = Base Sort.Float64

      let float32 = Base Sort.Float32

      let word = Base Sort.Word

      let bits32 = Base Sort.Bits32

      let bits64 = Base Sort.Bits64

      let vec128 = Base Sort.Vec128

      let of_base : Sort.base -> t = function
        | Value -> value
        | Void -> void
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
    end

    include Static

    let rec get_sort : t -> Sort.Const.t option = function
      | Any -> None
      | Base b -> Some (Base b)
      | Product ts ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)

    let of_sort s =
      let rec of_sort : Sort.t -> _ = function
        | Var _ -> None
        | Base b -> Some (Static.of_base b)
        | Product sorts ->
          Option.map
            (fun x -> Product x)
            (* [Sort.get] is deep, so no need to repeat it here *)
            (Misc.Stdlib.List.map_option of_sort sorts)
      in
      of_sort (Sort.get s)

    let of_flat_sort : Sort.Flat.t -> _ = function
      | Var _ -> None
      | Base b -> Some (Static.of_base b)

    let rec of_sort_const : Sort.Const.t -> t = function
      | Base b -> Base b
      | Product consts -> Product (List.map of_sort_const consts)

    let to_string t =
      let rec to_string nested (t : t) =
        match t with
        | Any -> "any"
        | Base b -> Sort.to_string_base b
        | Product ts ->
          String.concat ""
            [ (if nested then "(" else "");
              String.concat " & " (List.map (to_string true) ts);
              (if nested then ")" else "") ]
      in
      to_string false t

    module Debug_printers = struct
      open Format

      let t ppf t = fprintf ppf "%s" (to_string t)
    end
  end

  module Debug_printers = struct
    open Format

    let rec t format_sort ppf = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" format_sort s
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             (t format_sort))
          ts
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any -> Any
    | Base b -> Sort (Sort.of_base b)
    | Product cs -> Product (List.map of_const cs)

  let rec to_sort = function
    | Any -> None
    | Sort s -> Some s
    | Product ts -> to_product_sort ts

  and to_product_sort ts =
    Option.map
      (fun x -> Sort.Product x)
      (Misc.Stdlib.List.map_option to_sort ts)

  let rec get : Sort.t t -> Sort.Flat.t t =
    let rec flatten_sort : Sort.t -> Sort.Flat.t t = function
      | Var v -> Sort (Var (Sort.Var.get_id v))
      | Base b ->
        Sort (Base b)
        (* No need to call [Sort.get] here, because one [get] is deep. *)
      | Product sorts -> Product (List.map flatten_sort sorts)
    in
    function
    | Any -> Any
    | Sort s -> flatten_sort (Sort.get s)
    | Product ts -> Product (List.map get ts)

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any -> Some Any
    | Sort s -> of_sort s
    | Product layouts ->
      Option.map
        (fun x -> Layout.Const.Product x)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let sort_equal_result ~allow_mutation result =
    match (result : Sort.equate_result) with
    | (Equal_mutated_first | Equal_mutated_second | Equal_mutated_both)
      when not allow_mutation ->
      Misc.fatal_errorf "Jkind.equal: Performed unexpected mutation"
    | Unequal -> false
    | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second
    | Equal_mutated_both ->
      true

  let rec equate_or_equal ~allow_mutation t1 t2 =
    match t1, t2 with
    | Sort s1, Sort s2 ->
      sort_equal_result ~allow_mutation (Sort.equate_tracking_mutation s1 s2)
    | Product ts, Sort sort | Sort sort, Product ts -> (
      (* If [ts] can't be turned into a product sort -- because it has [any]
         -- then equality will surely fail. No need to create new sort
         variables here. *)
      match to_product_sort ts with
      | None -> false
      | Some sort' ->
        sort_equal_result ~allow_mutation
          (Sort.equate_tracking_mutation sort sort'))
    | Product ts1, Product ts2 ->
      List.equal (equate_or_equal ~allow_mutation) ts1 ts2
    | Any, Any -> true
    | (Any | Sort _ | Product _), _ -> false

  let rec sub t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | Any, Any -> Equal
    | _, Any -> Less
    | Any, _ -> Not_le
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Equal else Not_le
    | Product ts1, Product ts2 ->
      if List.compare_lengths ts1 ts2 = 0
      then Misc.Le_result.combine_list (List.map2 sub ts1 ts2)
      else Not_le
    | Product ts1, Sort s2 -> (
      (* This case could use [to_product_sort] because every component will need
         to end up less than a sort (so, no [any]), but it seems easier to keep
         this case lined up with the inverse case, which definitely cannot use
         [to_product_sort]. *)
      match Sort.decompose_into_product s2 (List.length ts1) with
      | None -> Not_le
      | Some ss2 ->
        Misc.Le_result.combine_list
          (List.map2 (fun t1 s2 -> sub t1 (Sort s2)) ts1 ss2))
    | Sort s1, Product ts2 -> (
      match Sort.decompose_into_product s1 (List.length ts2) with
      | None -> Not_le
      | Some ss1 ->
        Misc.Le_result.combine_list
          (List.map2 (fun s1 t2 -> sub (Sort s1) t2) ss1 ts2))

  let rec intersection t1 t2 =
    (* pre-condition to [products]: [ts1] and [ts2] have the same length *)
    let products ts1 ts2 =
      let components = List.map2 intersection ts1 ts2 in
      Option.map
        (fun x -> Product x)
        (Misc.Stdlib.List.some_if_all_elements_are_some components)
    in
    match t1, t2 with
    | _, Any -> Some t1
    | Any, _ -> Some t2
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Some t1 else None
    | Product ts1, Product ts2 ->
      if List.compare_lengths ts1 ts2 = 0 then products ts1 ts2 else None
    | Product ts, Sort sort | Sort sort, Product ts -> (
      match Sort.decompose_into_product sort (List.length ts) with
      | None -> None
      | Some sorts -> products ts (List.map (fun x -> Sort x) sorts))

  let of_new_sort_var () =
    let sort = Sort.new_var () in
    Sort sort, sort

  let rec default_to_value_and_get : _ Layout.t -> Const.t = function
    | Any -> Any
    | Sort s -> Const.of_sort_const (Sort.default_to_value_and_get s)
    | Product p -> Product (List.map default_to_value_and_get p)

  let format ppf layout =
    let open Format in
    let rec pp_element ~nested ppf : _ Layout.t -> unit = function
      | Any -> fprintf ppf "any"
      | Sort s -> Sort.format ppf s
      | Product ts ->
        let pp_sep ppf () = Format.fprintf ppf " & " in
        Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
    in
    pp_element ~nested:false ppf layout
end

module Externality = Jkind_axis.Externality
module Nullability = Jkind_axis.Nullability
module Modes = Jkind_axis.Of_lattice (Alloc.Const)

module History = struct
  include Jkind_intf.History

  let is_imported t =
    match t.history with Creation Imported -> true | _ -> false

  (* CR layouts: Anything that returns false here could probably just be removed,
     but let's keep the info around at least during development. *)
  let is_informative t =
    match t.history with Creation Imported -> false | _ -> true

  let update_reason t reason = { t with history = Creation reason }

  let with_warning t = { t with has_warned = true }

  let has_warned t = t.has_warned
end

(*********************************)
(* Main type declarations *)

type +'d const = (type_expr, 'd) Jkind_types.Const.t

type 'd t = (type_expr, 'd) Jkind_types.t

type jkind_l = (allowed * disallowed) t

type packed = Pack : 'd t -> packed [@@unboxed]

include Allowance.Magic_allow_disallow (struct
  type (_, _, 'd) sided = 'd t

  let disallow_right ({ jkind = { layout = _; _ }; _ } as t) = t

  let disallow_left ({ jkind = { layout = _; _ }; _ } as t) = t

  let allow_right ({ jkind = { layout = _; _ }; _ } as t) = t

  let allow_left ({ jkind = { layout = _; _ }; _ } as t) = t
end)

let terrible_relax_l ({ jkind = { layout = _; _ }; _ } as t) = t

let fresh_jkind jkind ~annotation ~why =
  { jkind; annotation; history = Creation why; has_warned = false }

(******************************)
(*** user errors ***)

module Error = struct
  type t =
    | Insufficient_level :
        { jkind : Parsetree.jkind_annotation;
          required_layouts_level : Language_extension.maturity
        }
        -> t
    | Unknown_jkind of Parsetree.jkind_annotation
    | Multiple_jkinds of
        { from_annotation : Parsetree.jkind_annotation;
          from_attribute : Builtin_attributes.jkind_attribute Location.loc
        }
    | Unimplemented_syntax

  exception User_error of Location.t * t
end

let raise ~loc err = raise (Error.User_error (loc, err))

module Const = struct
  open Jkind_types.Layout_and_axes

  type +'d t = 'd const

  include Allowance.Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd t

    let disallow_left ({ layout = _; _ } as t) = t

    let disallow_right ({ layout = _; _ } as t) = t

    let allow_left ({ layout = _; _ } as t) = t

    let allow_right ({ layout = _; _ } as t) = t
  end)

  let max =
    { layout = Layout.Const.max;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Nullability.max
    }

  let equal_after_all_inference_is_done t1 t2 =
    Layout_and_axes.equal_after_all_inference_is_done Layout.Const.equal t1 t2

  module Builtin = struct
    type nonrec +'d t =
      { jkind : 'd t;
        name : string
      }

    let mk_jkind ~mode_crossing ~nullability (layout : Layout.Const.t) =
      let modes_upper_bounds, externality_upper_bound =
        match mode_crossing with
        | true -> Modes.min, Externality.min
        | false -> Modes.max, Externality.max
      in
      { layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound = nullability
      }

    let any =
      { jkind = mk_jkind Any ~mode_crossing:false ~nullability:Maybe_null;
        name = "any"
      }

    let any_non_null =
      { jkind = mk_jkind Any ~mode_crossing:false ~nullability:Non_null;
        name = "any_non_null"
      }

    let value_or_null =
      { jkind =
          mk_jkind (Base Value) ~mode_crossing:false ~nullability:Maybe_null;
        name = "value_or_null"
      }

    let value =
      { jkind = mk_jkind (Base Value) ~mode_crossing:false ~nullability:Non_null;
        name = "value"
      }

    let immutable_data =
      { jkind =
          { layout = Base Value;
            modes_upper_bounds =
              { linearity = Linearity.Const.min;
                contention = Contention.Const.min;
                portability = Portability.Const.min;
                uniqueness = Uniqueness.Const.max;
                areality = Locality.Const.max;
                yielding = Yielding.Const.min
              };
            externality_upper_bound = Externality.max;
            nullability_upper_bound = Nullability.Non_null
          };
        name = "immutable_data"
      }

    let mutable_data =
      { jkind =
          { layout = Base Value;
            modes_upper_bounds =
              { linearity = Linearity.Const.min;
                contention = Contention.Const.max;
                portability = Portability.Const.min;
                uniqueness = Uniqueness.Const.max;
                areality = Locality.Const.max;
                yielding = Yielding.Const.min
              };
            externality_upper_bound = Externality.max;
            nullability_upper_bound = Nullability.Non_null
          };
        name = "mutable_data"
      }

    (* CR layouts v3: change to [or_null] when separability is implemented. *)
    let void =
      { jkind = mk_jkind (Base Void) ~mode_crossing:false ~nullability:Non_null;
        name = "void"
      }

    let immediate =
      { jkind = mk_jkind (Base Value) ~mode_crossing:true ~nullability:Non_null;
        name = "immediate"
      }

    (* [immediate64] describes types that are stored directly (no indirection)
       on 64-bit platforms but indirectly on 32-bit platforms. The key question:
       along which modes should a [immediate64] cross? As of today, all of them,
       but the reasoning for each is independent and somewhat subtle:

       * Locality: This is fine, because we do not have stack-allocation on
       32-bit platforms. Thus mode-crossing is sound at any type on 32-bit,
       including immediate64 types.

       * Linearity: This is fine, because linearity matters only for function
       types, and an immediate64 cannot be a function type and cannot store
       one either.

       * Uniqueness: This is fine, because uniqueness matters only for
       in-place update, and no record supporting in-place update is an
       immediate64. ([@@unboxed] records do not support in-place update.)

       * Syncness: This is fine, because syncness matters only for function
       types, and an immediate64 cannot be a function type and cannot store
       one either.

       * Contention: This is fine, because contention matters only for
       types with mutable fields, and an immediate64 does not have immutable
       fields.

       In practice, the functor that creates immediate64s,
       [Stdlib.Sys.Immediate64.Make], will require these conditions on its
       argument. But the arguments that we expect here will have no trouble
       meeting the conditions.
    *)
    let immediate64 =
      { jkind = { immediate.jkind with externality_upper_bound = External64 };
        name = "immediate64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float64 =
      { jkind =
          mk_jkind (Base Float64) ~mode_crossing:true ~nullability:Non_null;
        name = "float64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float32 =
      { jkind =
          mk_jkind (Base Float32) ~mode_crossing:true ~nullability:Non_null;
        name = "float32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let word =
      { jkind = mk_jkind (Base Word) ~mode_crossing:true ~nullability:Non_null;
        name = "word"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits32 =
      { jkind = mk_jkind (Base Bits32) ~mode_crossing:true ~nullability:Non_null;
        name = "bits32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits64 =
      { jkind = mk_jkind (Base Bits64) ~mode_crossing:true ~nullability:Non_null;
        name = "bits64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let vec128 =
      { jkind = mk_jkind (Base Vec128) ~mode_crossing:true ~nullability:Non_null;
        name = "vec128"
      }

    let all =
      [ any;
        any_non_null;
        value_or_null;
        value;
        immutable_data;
        mutable_data;
        void;
        immediate;
        immediate64;
        float64;
        float32;
        word;
        bits32;
        bits64;
        vec128 ]

    let of_attribute : Builtin_attributes.jkind_attribute -> _ t = function
      | Immediate -> immediate
      | Immediate64 -> immediate64
  end

  module To_out_jkind_const : sig
    (** Convert a [t] into a [Outcometree.out_jkind_const].
        The jkind is written in terms of the built-in jkind that requires the least amount
        of modes after the mod. For example,
        [value mod global many unique portable uncontended external_ non_null] could be
        written in terms of [value] (as it appears above), or in terms of [immediate]
        (which would just be [immediate]). Since the latter requires less modes to be
        printed, it is chosen. *)
    val convert : 'd t -> Outcometree.out_jkind_const
  end = struct
    type printable_jkind =
      { base : string;
        modal_bounds : string list
      }

    module Bounds = struct
      type t =
        { alloc_bounds : Alloc.Const.t;
          externality_bound : Externality.t;
          nullability_bound : Nullability.t
        }

      let of_jkind jkind =
        { alloc_bounds = jkind.modes_upper_bounds;
          externality_bound = jkind.externality_upper_bound;
          nullability_bound = jkind.nullability_upper_bound
        }
    end

    let get_modal_bound ~le ~print ~base actual =
      match le actual base with
      | true -> (
        match le base actual with
        | true -> `Valid None
        | false -> `Valid (Some (Format.asprintf "%a" print actual)))
      | false -> `Invalid

    let get_modal_bounds ~(base : Bounds.t) (actual : Bounds.t) =
      [ get_modal_bound ~le:Locality.Const.le ~print:Locality.Const.print
          ~base:base.alloc_bounds.areality actual.alloc_bounds.areality;
        get_modal_bound ~le:Uniqueness.Const.le ~print:Uniqueness.Const.print
          ~base:base.alloc_bounds.uniqueness actual.alloc_bounds.uniqueness;
        get_modal_bound ~le:Linearity.Const.le ~print:Linearity.Const.print
          ~base:base.alloc_bounds.linearity actual.alloc_bounds.linearity;
        get_modal_bound ~le:Contention.Const.le ~print:Contention.Const.print
          ~base:base.alloc_bounds.contention actual.alloc_bounds.contention;
        get_modal_bound ~le:Portability.Const.le ~print:Portability.Const.print
          ~base:base.alloc_bounds.portability actual.alloc_bounds.portability;
        get_modal_bound ~le:Externality.le ~print:Externality.print
          ~base:base.externality_bound actual.externality_bound;
        get_modal_bound ~le:Nullability.le ~print:Nullability.print
          ~base:base.nullability_bound actual.nullability_bound ]
      |> List.rev
      |> List.fold_left
           (fun acc mode ->
             match acc, mode with
             | _, `Invalid | None, _ -> None
             | acc, `Valid None -> acc
             | Some acc, `Valid (Some mode) -> Some (mode :: acc))
           (Some [])

    (** Write [actual] in terms of [base] *)
    let convert_with_base ~(base : _ Builtin.t) actual =
      let matching_layouts =
        Layout.Const.equal base.jkind.layout actual.layout
      in
      let modal_bounds =
        get_modal_bounds
          ~base:(Bounds.of_jkind base.jkind)
          (Bounds.of_jkind actual)
      in
      match matching_layouts, modal_bounds with
      | true, Some modal_bounds -> Some { base = base.name; modal_bounds }
      | false, _ | _, None -> None

    (** Select the out_jkind_const with the least number of modal bounds to print *)
    let rec select_simplest = function
      | a :: b :: tl ->
        let simpler =
          if List.length a.modal_bounds < List.length b.modal_bounds
          then a
          else b
        in
        select_simplest (simpler :: tl)
      | [out] -> Some out
      | [] -> None

    let convert jkind =
      (* For each primitive jkind, we try to print the jkind in terms of it (this is
         possible if the primitive is a subjkind of it). We then choose the "simplest". The
           "simplest" is taken to mean the one with the least number of modes that need to
         follow the [mod]. *)
      let simplest =
        Builtin.all
        |> List.filter_map (fun base -> convert_with_base ~base jkind)
        |> select_simplest
      in
      let printable_jkind =
        match simplest with
        | Some simplest -> simplest
        | None -> (
          (* CR layouts v2.8: sometimes there is no valid way to build a jkind from a
             built-in abbreviation. For now, we just pretend that the layout name is a valid
             jkind abbreviation whose modal bounds are all max, even though this is a
             lie. *)
          let out_jkind_verbose =
            convert_with_base
              ~base:
                { jkind =
                    { layout = jkind.layout;
                      modes_upper_bounds = Modes.max;
                      externality_upper_bound = Externality.max;
                      nullability_upper_bound = Nullability.Non_null
                    };
                  name = Layout.Const.to_string jkind.layout
                }
              jkind
          in
          match out_jkind_verbose with
          | Some out_jkind -> out_jkind
          | None ->
            (* If we fail, try again with nullable jkinds. *)
            let out_jkind_verbose =
              convert_with_base
                ~base:
                  { jkind =
                      { layout = jkind.layout;
                        modes_upper_bounds = Modes.max;
                        externality_upper_bound = Externality.max;
                        nullability_upper_bound = Nullability.max
                      };
                    name = Layout.Const.to_string jkind.layout
                  }
                jkind
            in
            (* convert_with_base is guaranteed to succeed since the layout matches and the
                 modal bounds are all max *)
            Option.get out_jkind_verbose)
      in
      match printable_jkind with
      | { base; modal_bounds = _ :: _ as modal_bounds } ->
        Outcometree.Ojkind_const_mod
          (Ojkind_const_abbreviation base, modal_bounds)
      | { base; modal_bounds = [] } ->
        Outcometree.Ojkind_const_abbreviation base
  end

  let to_out_jkind_const jkind = To_out_jkind_const.convert jkind

  let format ppf jkind = to_out_jkind_const jkind |> !Oprint.out_jkind_const ppf

  let jkind_of_product_annotations jkinds =
    let folder (layouts, mode_ub, ext_ub, null_ub)
        { layout;
          modes_upper_bounds;
          externality_upper_bound;
          nullability_upper_bound
        } =
      ( layout :: layouts,
        Modes.join mode_ub modes_upper_bounds,
        Externality.join ext_ub externality_upper_bound,
        Nullability.join null_ub nullability_upper_bound )
    in
    let layouts, mode_ub, ext_ub, null_ub =
      List.fold_left folder
        ([], Modes.min, Externality.min, Nullability.min)
        jkinds
    in
    { layout = Layout.Const.Product (List.rev layouts);
      modes_upper_bounds = mode_ub;
      externality_upper_bound = ext_ub;
      nullability_upper_bound = null_ub
    }

  let rec of_user_written_annotation_unchecked_level :
      type l r.
      (l * r) History.annotation_context ->
      Parsetree.jkind_annotation ->
      (l * r) t =
   fun context jkind ->
    match jkind.pjkind_desc with
    | Abbreviation name ->
      (* CR layouts v2.8: move this to predef *)
      (match name with
      | "any" -> Builtin.any.jkind
      | "any_non_null" -> Builtin.any_non_null.jkind
      | "value_or_null" -> Builtin.value_or_null.jkind
      | "value" -> Builtin.value.jkind
      | "void" -> Builtin.void.jkind
      | "immediate64" -> Builtin.immediate64.jkind
      | "immediate" -> Builtin.immediate.jkind
      | "float64" -> Builtin.float64.jkind
      | "float32" -> Builtin.float32.jkind
      | "word" -> Builtin.word.jkind
      | "bits32" -> Builtin.bits32.jkind
      | "bits64" -> Builtin.bits64.jkind
      | "vec128" -> Builtin.vec128.jkind
      | _ -> raise ~loc:jkind.pjkind_loc (Unknown_jkind jkind))
      |> allow_left |> allow_right
    | Mod (jkind, modifiers) ->
      let base = of_user_written_annotation_unchecked_level context jkind in
      (* for each mode, lower the corresponding modal bound to be that mode *)
      let parsed_modifiers = Typemode.transl_modifier_annots modifiers in
      let parsed_modes : Alloc.Const.Option.t =
        { areality = parsed_modifiers.locality;
          linearity = parsed_modifiers.linearity;
          uniqueness = parsed_modifiers.uniqueness;
          portability = parsed_modifiers.portability;
          contention = parsed_modifiers.contention;
          yielding = parsed_modifiers.yielding
        }
      in
      { layout = base.layout;
        modes_upper_bounds =
          Alloc.Const.meet base.modes_upper_bounds
            (Alloc.Const.Option.value ~default:Alloc.Const.max parsed_modes);
        nullability_upper_bound =
          Nullability.meet base.nullability_upper_bound
            (Option.value ~default:Nullability.max parsed_modifiers.nullability);
        externality_upper_bound =
          Externality.meet base.externality_upper_bound
            (Option.value ~default:Externality.max parsed_modifiers.externality)
      }
    | Product ts ->
      let jkinds =
        List.map (of_user_written_annotation_unchecked_level context) ts
      in
      jkind_of_product_annotations jkinds
    | Default | With _ | Kind_of _ ->
      raise ~loc:jkind.pjkind_loc Unimplemented_syntax

  (* The [annotation_context] parameter can be used to allow annotations / kinds
     in different contexts to be enabled with different extension settings.
     At some points in time, we will not care about the context, and so this
     parameter might effectively be unused.
  *)
  (* CR layouts: When everything is stable, remove this function. *)
  let get_required_layouts_level (_context : 'd History.annotation_context)
      (jkind : 'd t) =
    let rec scan_layout (l : Layout.Const.t) : Language_extension.maturity =
      match l, jkind.nullability_upper_bound with
      | (Base (Float64 | Float32 | Word | Bits32 | Bits64 | Vec128) | Any), _
      | Base Value, Non_null
      | Base Value, Maybe_null ->
        Stable
      | Product layouts, _ ->
        List.fold_left
          (fun m l -> Language_extension.Maturity.max m (scan_layout l))
          Language_extension.Stable layouts
      | Base Void, _ -> Alpha
    in
    scan_layout jkind.layout

  let of_user_written_annotation ~context (annot : Parsetree.jkind_annotation) =
    let const = of_user_written_annotation_unchecked_level context annot in
    let required_layouts_level = get_required_layouts_level context const in
    if not (Language_extension.is_at_least Layouts required_layouts_level)
    then
      raise ~loc:annot.pjkind_loc
        (Insufficient_level { jkind = annot; required_layouts_level });
    const
end

module Desc = struct
  type 'd t = (Sort.Flat.t Layout.t, 'd) Layout_and_axes.t

  let get_const t = Layout_and_axes.map_option Layout.get_flat_const t

  (* CR layouts v2.8: This will probably need to be overhauled with
     [with]-types. See also [Printtyp.out_jkind_of_desc], which uses the same
     algorithm. *)
  let format ppf t =
    let open Format in
    let rec format_desc ~nested ppf
        (desc : (Sort.Flat.t Layout.t, _) Layout_and_axes.t) =
      match desc.layout with
      | Sort (Var n) -> fprintf ppf "'s%d" (Sort.Var.get_print_number n)
      (* Analyze a product before calling [get_const]: the machinery in
         [Const.format] works better for atomic layouts, not products. *)
      | Product lays ->
        let pp_sep ppf () = fprintf ppf "@ & " in
        Misc.pp_nested_list ~nested ~pp_element:format_desc ~pp_sep ppf
          (List.map (fun layout -> { desc with layout }) lays)
      | _ -> (
        match get_const desc with
        | Some c -> Const.format ppf c
        | None -> assert false (* handled above *))
    in
    format_desc ~nested:false ppf t
end

module Jkind_desc = struct
  open Jkind_types.Layout_and_axes

  let of_const t = Layout_and_axes.map Layout.of_const t

  let add_nullability_crossing t =
    { t with nullability_upper_bound = Nullability.min }

  let unsafely_set_upper_bounds t ~from =
    { t with modes_upper_bounds = from.modes_upper_bounds }

  let max = of_const Const.max

  let equate_or_equal ~allow_mutation
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Layout.equate_or_equal ~allow_mutation lay1 lay2
    && Modes.equal modes1 modes2
    && Externality.equal ext1 ext2
    && Nullability.equal null1 null2

  let sub ?allow_any_crossing t1 t2 =
    Layout_and_axes.sub ?allow_any_crossing Layout.sub t1 t2

  let intersection
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Option.bind (Layout.intersection lay1 lay2) (fun layout ->
        Some
          { layout;
            modes_upper_bounds = Modes.meet modes1 modes2;
            externality_upper_bound = Externality.meet ext1 ext2;
            nullability_upper_bound = Nullability.meet null1 null2
          })

  let of_new_sort_var nullability_upper_bound =
    let layout, sort = Layout.of_new_sort_var () in
    ( { layout;
        modes_upper_bounds = Modes.max;
        externality_upper_bound = Externality.max;
        nullability_upper_bound
      },
      sort )

  module Builtin = struct
    let any = max

    let value_or_null = of_const Const.Builtin.value_or_null.jkind

    let value = of_const Const.Builtin.value.jkind

    let void = of_const Const.Builtin.void.jkind

    let immediate = of_const Const.Builtin.immediate.jkind
  end

  let product jkinds =
    (* CR layouts v7.1: Here we throw away the history of the component
       jkinds. This is not great. We should, as part of a broader pass on error
       messages around product kinds, zip them up into some kind of product
       history. *)
    let folder (layouts, annotations, mode_ub, ext_ub, null_ub)
        { jkind =
            { layout;
              modes_upper_bounds;
              externality_upper_bound;
              nullability_upper_bound
            };
          annotation;
          history = _;
          has_warned = _
        } =
      ( layout :: layouts,
        annotation :: annotations,
        Modes.join mode_ub modes_upper_bounds,
        Externality.join ext_ub externality_upper_bound,
        Nullability.join null_ub nullability_upper_bound )
    in
    let layouts, annotations, mode_ub, ext_ub, null_ub =
      List.fold_left folder
        ([], [], Modes.min, Externality.min, Nullability.min)
        jkinds
    in
    let layouts = List.rev layouts in
    let annotations = List.rev annotations in
    let annotations = Misc.Stdlib.Monad.Option.all annotations in
    let annotation =
      Option.map
        (fun annotations ->
          Parsetree.
            { pjkind_loc = Location.none; pjkind_desc = Product annotations })
        annotations
    in
    ( { layout : _ Layout.t = Product layouts;
        modes_upper_bounds = mode_ub;
        externality_upper_bound = ext_ub;
        nullability_upper_bound = null_ub
      },
      annotation )

  let get t = Layout_and_axes.map Layout.get t

  let get_const t = Layout_and_axes.map_option Layout.get_const t

  module Debug_printers = struct
    let t ppf t =
      Layout_and_axes.format
        (Layout.Debug_printers.t Sort.Debug_printers.t)
        ppf t
  end
end

(******************************)
(* constants *)

(* every context where this is used actually wants an [option] *)
let mk_annot name =
  Some Parsetree.{ pjkind_loc = Location.none; pjkind_desc = Abbreviation name }

module Builtin = struct
  let any_dummy_jkind =
    { jkind = Jkind_desc.max;
      annotation = None;
      (* this should never get printed: it's a dummy *)
      history = Creation (Any_creation Dummy_jkind);
      has_warned = false
    }

  (* CR layouts: Should we be doing more memoization here? *)
  let any ~(why : History.any_creation_reason) =
    match why with
    | Dummy_jkind -> any_dummy_jkind (* share this one common case *)
    | _ ->
      fresh_jkind Jkind_desc.Builtin.any ~annotation:(mk_annot "any")
        ~why:(Any_creation why)

  let value_v1_safety_check =
    { jkind = Jkind_desc.Builtin.value_or_null;
      annotation = mk_annot "value";
      history = Creation (Value_or_null_creation V1_safety_check);
      has_warned = false
    }

  let void ~why =
    fresh_jkind Jkind_desc.Builtin.void ~annotation:(mk_annot "void")
      ~why:(Void_creation why)

  let value_or_null ~why =
    match (why : History.value_or_null_creation_reason) with
    | V1_safety_check -> value_v1_safety_check
    | _ ->
      fresh_jkind Jkind_desc.Builtin.value_or_null
        ~annotation:(mk_annot "value_or_null") ~why:(Value_or_null_creation why)

  let value ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Builtin.value ~annotation:(mk_annot "value")
      ~why:(Value_creation why)

  let immediate ~why =
    fresh_jkind Jkind_desc.Builtin.immediate ~annotation:(mk_annot "immediate")
      ~why:(Immediate_creation why)

  let product ~why ts =
    match ts with
    | [] -> Misc.fatal_error "Jkind.Builtin.product: empty product"
    | [t] -> t
    | ts ->
      let desc, annotation = Jkind_desc.product ts in
      fresh_jkind desc ~annotation ~why:(Product_creation why)
end

let add_nullability_crossing t =
  { t with jkind = Jkind_desc.add_nullability_crossing t.jkind }

let unsafely_set_upper_bounds ~from t =
  { t with
    jkind = Jkind_desc.unsafely_set_upper_bounds t.jkind ~from:from.jkind
  }

(******************************)
(* construction *)

let of_new_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Maybe_null in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_creation why), sort

let of_new_sort ~why = fst (of_new_sort_var ~why)

let of_new_legacy_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Non_null in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_legacy_creation why), sort

let of_new_legacy_sort ~why = fst (of_new_legacy_sort_var ~why)

let of_const ~annotation ~why
    ({ layout;
       modes_upper_bounds;
       externality_upper_bound;
       nullability_upper_bound
     } :
      'd Const.t) =
  { jkind =
      { layout = Layout.of_const layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      };
    annotation;
    history = Creation why;
    has_warned = false
  }

let of_builtin ~why Const.Builtin.{ jkind; name } =
  of_const ~annotation:(mk_annot name) ~why jkind

let of_annotated_const ~context ~annotation ~const ~const_loc =
  of_const ~annotation ~why:(Annotated (context, const_loc)) const

let of_annotation ~context (annot : Parsetree.jkind_annotation) =
  let const = Const.of_user_written_annotation ~context annot in
  of_annotated_const ~annotation:(Some annot) ~const ~const_loc:annot.pjkind_loc
    ~context

let of_annotation_option_default ~default ~context = function
  | None -> default
  | Some annot -> of_annotation ~context annot

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let ({ jkind = const; name } : _ Const.Builtin.t) =
    Const.Builtin.of_attribute attribute.txt
  in
  of_annotated_const ~context ~annotation:(mk_annot name) ~const
    ~const_loc:attribute.loc

let of_type_decl ~context (decl : Parsetree.type_declaration) =
  let jkind_of_annotation =
    decl.ptype_jkind_annotation
    |> Option.map (fun annot -> of_annotation ~context annot, annot)
  in
  let jkind_of_attribute =
    Builtin_attributes.jkind decl.ptype_attributes
    |> Option.map (fun attr -> (of_attribute ~context attr, None), attr)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | Some (jkind, annot), None -> Some (jkind, Some annot)
  | None, Some (jkind_with_annot, _) -> Some jkind_with_annot
  | Some (_, from_annotation), Some (_, from_attribute) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let of_type_decl_default ~context ~default (decl : Parsetree.type_declaration) =
  match of_type_decl ~context decl with Some (t, _) -> t | None -> default

let for_boxed_record ~all_void =
  if all_void
  then Builtin.immediate ~why:Empty_record
  else Builtin.value ~why:Boxed_record

let for_boxed_variant ~all_voids =
  if all_voids
  then Builtin.immediate ~why:Enumeration
  else Builtin.value ~why:Boxed_variant

let for_arrow =
  fresh_jkind
    { layout = Sort (Base Value);
      modes_upper_bounds =
        { linearity = Linearity.Const.max;
          areality = Locality.Const.max;
          uniqueness = Uniqueness.Const.min;
          portability = Portability.Const.max;
          contention = Contention.Const.min;
          yielding = Yielding.Const.max
        };
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Non_null
    }
    ~annotation:None ~why:(Value_creation Arrow)

let for_object =
  fresh_jkind
    { layout = Sort (Base Value);
      modes_upper_bounds =
        (* The crossing of objects are based on the fact that they are
           produced/defined/allocated at legacy, which applies to only the
           comonadic axes. *)
        Alloc.Const.merge
          { comonadic = Alloc.Comonadic.Const.legacy;
            monadic = Alloc.Monadic.Const.max
          };
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Non_null
    }
    ~annotation:None ~why:(Value_creation Object)

(******************************)
(* elimination and defaulting *)

let get_layout_defaulting_to_value { jkind = { layout; _ }; _ } =
  Layout.default_to_value_and_get layout

let default_to_value t = ignore (get_layout_defaulting_to_value t)

let get t = Jkind_desc.get t.jkind

let get_const t = Jkind_desc.get_const t.jkind

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let sort_of_jkind (t : jkind_l) : sort =
  let rec sort_of_layout (t : _ Layout.t) =
    match t with
    | Any -> Misc.fatal_error "Jkind.sort_of_jkind"
    | Sort s -> s
    | Product ls -> Sort.Product (List.map sort_of_layout ls)
  in
  sort_of_layout t.jkind.layout

let get_layout jk : Layout.Const.t option = Layout.get_const jk.jkind.layout

let get_modal_upper_bounds jk = jk.jkind.modes_upper_bounds

let get_externality_upper_bound jk = jk.jkind.externality_upper_bound

let set_externality_upper_bound jk externality_upper_bound =
  { jk with jkind = { jk.jkind with externality_upper_bound } }

let get_annotation jk = jk.annotation

let decompose_product ({ jkind; _ } as jk) =
  let mk_jkind layout = { jk with jkind = { jkind with layout } } in
  let deal_with_sort : Sort.t -> _ = function
    | Var _ -> None (* we've called [get] and there's *still* a variable *)
    | Base _ -> None
    | Product sorts -> Some (List.map (fun sort -> mk_jkind (Sort sort)) sorts)
  in
  match jkind.layout with
  | Any -> None
  | Product layouts ->
    (* CR layouts v7.1: The histories here are wrong (we are giving each
       component the history of the whole product).  They don't show up in
       errors, so it's fine for now, but we'll probably need to fix this as
       part of improving errors around products. A couple options: re-work the
       relevant bits of [Ctype.type_jkind_sub] to just work on layouts, or
       introduce product histories. *)
    Some (List.map mk_jkind layouts)
  | Sort s -> deal_with_sort (Sort.get s)

(*********************************)
(* pretty printing *)

(* CR layouts v2.8: This is the spot where we could print the annotation in
   the jkind, if there is one. But actually the output seems better without
   doing so, because it teaches the user that e.g. [value mod local] is better
   off spelled [value]. Possibly remove [jkind.annotation], but only after
   we have a proper printing story. *)
let format ppf jkind = Desc.format ppf (Jkind_desc.get jkind.jkind)

let printtyp_path = ref (fun _ _ -> assert false)

let set_printtyp_path f = printtyp_path := f

module Report_missing_cmi : sig
  (* used both in format_history and in Violation.report_general *)
  val report_missing_cmi : Format.formatter -> Path.t option -> unit
end = struct
  open Format

  (* CR layouts: Remove this horrible (but useful) heuristic once we have
     transitive dependencies in jenga. *)
  let missing_cmi_hint ppf type_path =
    let root_module_name p = p |> Path.head |> Ident.name in
    let delete_trailing_double_underscore s =
      if Misc.Stdlib.String.ends_with ~suffix:"__" s
      then String.sub s 0 (String.length s - 2)
      else s
    in
    (* A heuristic for guessing at a plausible library name for an identifier
       with a missing .cmi file; definitely less likely to be right outside of
       Jane Street. *)
    let guess_library_name : Path.t -> string option = function
      | Pdot _ as p ->
        Some
          (match root_module_name p with
          | "Location" | "Longident" -> "ocamlcommon"
          | mn ->
            mn |> String.lowercase_ascii |> delete_trailing_double_underscore)
      | Pident _ | Papply _ | Pextra_ty _ -> None
    in
    Option.iter
      (fprintf ppf "@,Hint: Adding \"%s\" to your dependencies might help.")
      (guess_library_name type_path)

  let report_missing_cmi ppf = function
    | Some p ->
      fprintf ppf "@,@[No .cmi file found containing %a.%a@]" !printtyp_path p
        missing_cmi_hint p
    | None -> ()
end

include Report_missing_cmi

(* CR layouts: should this be configurable? In the meantime, you
   may want to change these to experiment / debug. *)

(* should we print histories at all? *)
let display_histories = true

(* should we print histories in a way users can understand?
   The alternative is to print out all the data, which may be useful
   during debugging. *)
let flattened_histories = true

(* This module is just to keep all the helper functions more locally
   scoped. *)
module Format_history = struct
  (* CR layouts: all the output in this section is subject to change;
     actually look closely at error messages once this is activated *)

  open Format

  let format_with_notify_js ppf str =
    fprintf ppf
      "@[%s.@ Please notify the Jane Street compilers group if you see this \
       output@]"
      str

  let format_position ~arity position =
    let to_ordinal num = Int.to_string num ^ Misc.ordinal_suffix num in
    match arity with 1 -> "" | _ -> to_ordinal position ^ " "

  let format_concrete_creation_reason ppf :
      History.concrete_creation_reason -> unit = function
    | Match -> fprintf ppf "a value of this type is matched against a pattern"
    | Constructor_declaration _ ->
      fprintf ppf "it's the type of a constructor field"
    | Label_declaration lbl ->
      fprintf ppf "it is the type of record field %s" (Ident.name lbl)
    | Record_projection ->
      fprintf ppf "it's the record type used in a projection"
    | Record_assignment ->
      fprintf ppf "it's the record type used in an assignment"
    | Record_functional_update ->
      fprintf ppf "it's the record type used in a functional update"
    | Let_binding -> fprintf ppf "it's the type of a variable bound by a `let`"
    | Function_argument ->
      fprintf ppf "we must know concretely how to pass a function argument"
    | Function_result ->
      fprintf ppf "we must know concretely how to return a function result"
    | Structure_item_expression ->
      fprintf ppf "it's the type of an expression in a structure"
    | External_argument ->
      fprintf ppf "it's the type of an argument in an external declaration"
    | External_result ->
      fprintf ppf "it's the type of the result of an external declaration"
    | Statement -> fprintf ppf "it's the type of a statement"
    | Optional_arg_default ->
      fprintf ppf "it's the type of an optional argument default"
    | Unboxed_tuple_element ->
      fprintf ppf "it's the type of unboxed tuple element"
    | Layout_poly_in_external ->
      fprintf ppf
        "it's the layout polymorphic type in an external declaration@ \
         ([@@layout_poly] forces all variables of layout 'any' to be@ \
         representable at call sites)"
    | Peek_or_poke ->
      fprintf ppf "it's the type being used for a peek or poke primitive"

  let format_concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "it instantiates an unannotated type parameter of %a"
        !printtyp_path path
    | Wildcard -> fprintf ppf "it's a _ in the type"
    | Unification_var -> fprintf ppf "it's a fresh unification variable"
    | Array_element -> fprintf ppf "it's the type of an array element"
    | Old_style_unboxed_type -> fprintf ppf "it's an [@@@@unboxed] type"

  let rec format_annotation_context :
      type l r. _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p ->
      fprintf ppf "the declaration of the type %a" !printtyp_path p
    | Type_parameter (path, var) ->
      let var_string = match var with None -> "_" | Some v -> "'" ^ v in
      fprintf ppf "@[%s@ in the declaration of the type@ %a@]" var_string
        !printtyp_path path
    | Newtype_declaration name ->
      fprintf ppf "the abstract type declaration for %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "@[%s@ in the declaration of constructor@ %a@]" name
        !printtyp_path cstr
    | Univar name -> fprintf ppf "the universal variable %s" name
    | Type_variable name -> fprintf ppf "the type variable %s" name
    | Type_wildcard loc ->
      fprintf ppf "the wildcard _ at %a" Location.print_loc_in_lowercase loc
    | With_error_message (_message, context) ->
      (* message gets printed in [format_flattened_history] so we ignore it here *)
      format_annotation_context ppf context

  let format_any_creation_reason ppf : History.any_creation_reason -> unit =
    function
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Initial_typedecl_env ->
      format_with_notify_js ppf
        "a dummy kind of any is used to check mutually recursive datatypes"
    | Wildcard -> format_with_notify_js ppf "there's a _ in the type"
    | Unification_var ->
      format_with_notify_js ppf "it's a fresh unification variable"
    | Dummy_jkind ->
      format_with_notify_js ppf
        "it's assigned a dummy kind that should have been overwritten"
    (* CR layouts: Improve output or remove this constructor ^^ *)
    | Type_expression_call ->
      format_with_notify_js ppf
        "there's a call to [type_expression] via the ocaml API"
    | Inside_of_Tarrow -> fprintf ppf "argument or result of a function type"
    | Array_type_argument ->
      fprintf ppf "it's the type argument to the array type"

  let format_immediate_creation_reason ppf :
      History.immediate_creation_reason -> _ = function
    | Empty_record ->
      fprintf ppf "it's a record type containing all void elements"
    | Enumeration ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"
    | Primitive id ->
      fprintf ppf "it is the primitive immediate type %s" (Ident.name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"

  let format_value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive value_or_null type %s" (Ident.name id)
    | Tuple_element -> fprintf ppf "it's the type of a tuple element"
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"
    | Polymorphic_variant_field ->
      fprintf ppf "it's the type of the field of a polymorphic variant"
    | Structure_element ->
      fprintf ppf "it's the type of something stored in a module structure"
    | V1_safety_check ->
      fprintf ppf "it has to be value for the V1 safety check"
    | Probe -> format_with_notify_js ppf "it's a probe"
    | Captured_in_object ->
      fprintf ppf "it's the type of a variable captured in an object"
    | Let_rec_variable v ->
      fprintf ppf "it's the type of the recursive variable %s" (Ident.name v)

  let format_value_creation_reason ppf ~layout_or_kind :
      History.value_creation_reason -> _ = function
    | Class_let_binding ->
      fprintf ppf "it's the type of a let-bound variable in a class expression"
    | Object -> fprintf ppf "it's the type of an object"
    | Instance_variable -> fprintf ppf "it's the type of an instance variable"
    | Object_field -> fprintf ppf "it's the type of an object field"
    | Class_field -> fprintf ppf "it's the type of a class field"
    | Boxed_record -> fprintf ppf "it's a boxed record type"
    | Boxed_variant -> fprintf ppf "it's a boxed variant type"
    | Extensible_variant -> fprintf ppf "it's an extensible variant type"
    | Primitive id ->
      fprintf ppf "it is the primitive value type %s" (Ident.name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s value"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Tuple -> fprintf ppf "it's a tuple type"
    | Row_variable -> format_with_notify_js ppf "it's a row variable"
    | Polymorphic_variant -> fprintf ppf "it's a polymorphic variant type"
    | Arrow -> fprintf ppf "it's a function type"
    | Tfield ->
      format_with_notify_js ppf
        "it's an internal Tfield type (you shouldn't see this)"
    | Tnil ->
      format_with_notify_js ppf
        "it's an internal Tnil type (you shouldn't see this)"
    | First_class_module -> fprintf ppf "it's a first-class module type"
    | Univar ->
      fprintf ppf "it is or unifies with an unannotated universal variable"
    | Default_type_jkind ->
      fprintf ppf "an abstract type has the value %s by default" layout_or_kind
    | Existential_type_variable ->
      fprintf ppf "it's an unannotated existential type variable"
    | Array_comprehension_element ->
      fprintf ppf "it's the element type of array comprehension"
    | Lazy_expression -> fprintf ppf "it's the type of a lazy expression"
    | Class_type_argument ->
      fprintf ppf "it's a type argument to a class constructor"
    | Class_term_argument ->
      fprintf ppf
        "it's the type of a term-level argument to a class constructor"
    | Debug_printer_argument ->
      format_with_notify_js ppf
        "it's the type of an argument to a debugger printer function"
    | Recmod_fun_arg ->
      fprintf ppf
        "it's the type of the first argument to a function in a recursive \
         module"
    | Unknown s ->
      fprintf ppf
        "unknown @[(please alert the Jane Street@;\
         compilers team with this message: %s)@]" s

  let format_product_creation_reason ppf : History.product_creation_reason -> _
      = function
    | Unboxed_tuple -> fprintf ppf "it is an unboxed tuple"
    | Unboxed_record -> fprintf ppf "it is an unboxed record"

  let format_creation_reason ppf ~layout_or_kind :
      History.creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Any_creation any -> format_any_creation_reason ppf any
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Void_creation _ -> .
    | Value_or_null_creation value ->
      format_value_or_null_creation_reason ppf value
    | Value_creation value ->
      format_value_creation_reason ppf ~layout_or_kind value
    | Product_creation product -> format_product_creation_reason ppf product
    | Concrete_creation concrete -> format_concrete_creation_reason ppf concrete
    | Concrete_legacy_creation concrete ->
      format_concrete_legacy_creation_reason ppf concrete
    | Primitive id -> fprintf ppf "it is the primitive type %s" (Ident.name id)
    | Imported ->
      fprintf ppf "of %s requirements from an imported definition"
        layout_or_kind
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has this %s"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Generalized (id, loc) ->
      let format_id ppf = function
        | Some id -> fprintf ppf " of %s" (Ident.name id)
        | None -> ()
      in
      fprintf ppf "of the definition%a at %a" format_id id
        Location.print_loc_in_lowercase loc

  let format_interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation name ->
      fprintf ppf "a GADT match refining the type %a" !printtyp_path name
    | Tyvar_refinement_intersection -> fprintf ppf "updating a type variable"
    | Subjkind -> fprintf ppf "subkind check"

  (* CR layouts: An older implementation of format_flattened_history existed
      which displays more information not limited to one layout and one creation_reason
      around commit 66a832d70bf61d9af3b0ec6f781dcf0a188b324d in main.

      Consider revisiting that if the current implementation becomes insufficient. *)

  let format_flattened_history ~intro ~layout_or_kind ppf t =
    let jkind_desc = Jkind_desc.get t.jkind in
    fprintf ppf "@[<v 2>%t" intro;
    (match t.history with
    | Creation reason ->
      if History.is_informative t
      then (
        fprintf ppf "@ because %a"
          (format_creation_reason ~layout_or_kind)
          reason;
        match reason, Desc.get_const jkind_desc with
        | Concrete_legacy_creation _, Some _ ->
          fprintf ppf ",@ chosen to have %s %a" layout_or_kind format t
        | _ -> ())
    | Interact _ ->
      Misc.fatal_error "Non-flat history in format_flattened_history");
    fprintf ppf ".";
    (match t.history with
    | Creation (Annotated (With_error_message (message, _), _)) ->
      fprintf ppf "@ @[%s@]" message
    | _ -> ());
    fprintf ppf "@]"

  (* this isn't really formatted for user consumption *)
  let format_history_tree ~intro ~layout_or_kind ppf t =
    let rec in_order ppf = function
      | Interact { reason; history1; history2; jkind1 = _; jkind2 = _ } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]" in_order history1
          format_interact_reason reason in_order history2
      | Creation c -> format_creation_reason ppf ~layout_or_kind c
    in
    fprintf ppf "@;%t has this %s history:@;@[<v 2>  %a@]" intro layout_or_kind
      in_order t.history

  let format_history ~intro ~layout_or_kind ppf t =
    if display_histories
    then
      if flattened_histories
      then format_flattened_history ~intro ~layout_or_kind ppf t
      else format_history_tree ~intro ~layout_or_kind ppf t
end

let format_history ~intro ppf t =
  Format_history.format_history ~intro ~layout_or_kind:"kind" ppf t

(******************************)
(* errors *)

module Violation = struct
  open Format

  type violation =
    | Not_a_subjkind : (allowed * 'r) t * ('l * allowed) t -> violation
    | No_intersection : 'd t * ('l * allowed) t -> violation

  type nonrec t =
    { violation : violation;
      missing_cmi : Path.t option
    }
  (* [missing_cmi]: is this error a result of a missing cmi file?
     This is stored separately from the [violation] because it's
     used to change the behavior of [value_kind], and we don't
     want that function to inspect something that is purely about
     the choice of error message. (Though the [Path.t] payload *is*
     indeed just about the payload.) *)

  let of_ ?missing_cmi violation = { violation; missing_cmi }

  let is_missing_cmi viol = Option.is_some viol.missing_cmi

  type locale =
    | Mode
    | Layout

  let report_general preamble pp_former former ppf t =
    let mismatch_type =
      match t.violation with
      | Not_a_subjkind (k1, k2) ->
        if Misc.Le_result.is_le (Layout.sub k1.jkind.layout k2.jkind.layout)
        then Mode
        else Layout
      | No_intersection _ -> Layout
    in
    let layout_or_kind =
      match mismatch_type with Mode -> "kind" | Layout -> "layout"
    in
    let rec has_sort_var : Sort.Flat.t Layout.t -> bool = function
      | Sort (Var _) -> true
      | Product layouts -> List.exists has_sort_var layouts
      | Sort (Base _) | Any -> false
    in
    let format_layout_or_kind ppf jkind =
      match mismatch_type with
      | Mode -> Format.fprintf ppf "@,%a" format jkind
      | Layout -> Layout.format ppf jkind.jkind.layout
    in
    let subjkind_format verb k2 =
      if has_sort_var (get k2).layout
      then dprintf "%s representable" verb
      else
        dprintf "%s a sub%s of %a" verb layout_or_kind format_layout_or_kind k2
    in
    let Pack k1, Pack k2, fmt_k1, fmt_k2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (k1, k2); missing_cmi } -> (
        let missing_cmi =
          match missing_cmi with
          | None -> (
            match k1.history with
            | Creation (Missing_cmi p) -> Some p
            | Creation (Any_creation (Missing_cmi p)) -> Some p
            | _ -> None)
          | Some _ -> missing_cmi
        in
        match missing_cmi with
        | None ->
          ( Pack k1,
            Pack k2,
            dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
            subjkind_format "is not" k2,
            None )
        | Some p ->
          ( Pack k1,
            Pack k2,
            dprintf "an unknown %s" layout_or_kind,
            subjkind_format "might not be" k2,
            Some p ))
      | { violation = No_intersection (k1, k2); missing_cmi } ->
        assert (Option.is_none missing_cmi);
        ( Pack k1,
          Pack k2,
          dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
          dprintf "does not overlap with %a" format_layout_or_kind k2,
          None )
    in
    if display_histories
    then
      let connective =
        match t.violation, has_sort_var (get k2).layout with
        | Not_a_subjkind _, false ->
          dprintf "be a sub%s of %a" layout_or_kind format_layout_or_kind k2
        | No_intersection _, false ->
          dprintf "overlap with %a" format_layout_or_kind k2
        | _, true -> dprintf "be representable"
      in
      fprintf ppf "@[<v>%a@;%a@]"
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>The %s of %a is %a@]" layout_or_kind pp_former
                former format_layout_or_kind k1)
           ~layout_or_kind)
        k1
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>But the %s of %a must %t@]" layout_or_kind
                pp_former former connective)
           ~layout_or_kind)
        k2
    else
      fprintf ppf "@[<hov 2>%s%a has %t,@ which %t.@]" preamble pp_former former
        fmt_k1 fmt_k2;
    report_missing_cmi ppf missing_cmi_option

  let pp_t ppf x = fprintf ppf "%t" x

  let report_with_offender ~offender = report_general "" pp_t offender

  let report_with_offender_sort ~offender =
    report_general "A representable layout was expected, but " pp_t offender

  let report_with_name ~name = report_general "" pp_print_string name
end

(******************************)
(* relations *)

let equate_or_equal ~allow_mutation
    { jkind = jkind1; annotation = _; history = _; has_warned = _ }
    { jkind = jkind2; annotation = _; history = _; has_warned = _ } =
  Jkind_desc.equate_or_equal ~allow_mutation jkind1 jkind2

(* CR layouts v2.8: Switch this back to ~allow_mutation:false *)
let equal t1 t2 = equate_or_equal ~allow_mutation:true t1 t2

let () = Types.set_jkind_equal equal

let equate t1 t2 = equate_or_equal ~allow_mutation:true t1 t2

(* Not all jkind history reasons are created equal. Some are more helpful than others.
    This function encodes that information.

    The reason with higher score should get preserved when combined with one of lower
    score. *)
let score_reason = function
  (* error_message annotated by the user should always take priority *)
  | Creation (Annotated (With_error_message _, _)) -> 1
  (* Concrete creation is quite vague, prefer more specific reasons *)
  | Creation (Concrete_creation _ | Concrete_legacy_creation _) -> -1
  | _ -> 0

let combine_histories reason (Pack k1) (Pack k2) =
  if flattened_histories
  then
    let choose_higher_scored_history history_a history_b =
      if score_reason history_a >= score_reason history_b
      then history_a
      else history_b
    in
    let choose_subjkind_history k_a history_a k_b history_b =
      match Jkind_desc.sub k_a k_b with
      | Less -> history_a
      | Not_le ->
        (* CR layouts: this will be wrong if we ever have a non-trivial meet in
           the kind lattice -- which is now! So this is actually wrong. *)
        history_b
      | Equal -> choose_higher_scored_history history_a history_b
    in
    match Layout_and_axes.(try_allow_l k1.jkind, try_allow_r k2.jkind) with
    | Some k1_l, Some k2_r ->
      choose_subjkind_history k1_l k1.history k2_r k2.history
    | _ -> (
      match Layout_and_axes.(try_allow_r k1.jkind, try_allow_l k2.jkind) with
      | Some k1_r, Some k2_l ->
        choose_subjkind_history k2_l k2.history k1_r k1.history
      | _ -> choose_higher_scored_history k1.history k2.history)
  else
    Interact
      { reason;
        jkind1 = Pack k1.jkind;
        history1 = k1.history;
        jkind2 = Pack k2.jkind;
        history2 = k2.history
      }

let has_intersection t1 t2 =
  Option.is_some (Jkind_desc.intersection t1.jkind t2.jkind)

let intersection_or_error ~reason t1 t2 =
  match Jkind_desc.intersection t1.jkind t2.jkind with
  | None -> Error (Violation.of_ (No_intersection (t1, t2)))
  | Some jkind ->
    Ok
      { jkind;
        annotation = None;
        history = combine_histories reason (Pack t1) (Pack t2);
        has_warned = t1.has_warned || t2.has_warned
      }

let intersect_l_l ~reason t1 t2 =
  (* CR layouts v2.8: Do something cleverer here once we have more
     expressive l-kinds. *)
  intersection_or_error ~reason t1 (terrible_relax_l t2)

let has_intersection_l_l t1 t2 =
  (* CR layouts v2.8: Do something cleverer here once we have more
     expressive l-kinds. *)
  has_intersection (terrible_relax_l t1) (terrible_relax_l t2)

(* this is hammered on; it must be fast! *)
let check_sub ?allow_any_crossing sub super =
  Jkind_desc.sub ?allow_any_crossing sub.jkind super.jkind

let sub sub super = Misc.Le_result.is_le (check_sub sub super)

type sub_or_intersect =
  | Sub
  | Disjoint
  | Has_intersection

let sub_or_intersect t1 t2 =
  if sub t1 t2
  then Sub
  else if has_intersection t1 t2
  then Has_intersection
  else Disjoint

let sub_or_error t1 t2 =
  match sub_or_intersect t1 t2 with
  | Sub -> Ok ()
  | _ -> Error (Violation.of_ (Not_a_subjkind (t1, t2)))

(* CR layouts v2.8: Rewrite this to do the hard subjkind check from the
   kind polymorphism design. *)
let sub_jkind_l ?allow_any_crossing sub super =
  let super = terrible_relax_l super in
  match check_sub ?allow_any_crossing sub super with
  | Less | Equal ->
    Ok { sub with history = combine_histories Subjkind (Pack sub) (Pack super) }
  | Not_le -> Error (Violation.of_ (Not_a_subjkind (sub, super)))

let is_void_defaulting = function
  | { jkind = { layout = Sort s; _ }; _ } -> Sort.is_void_defaulting s
  | _ -> false

(* This doesn't do any mutation because mutating a sort variable can't make it
   any, and modal upper bounds are constant. *)
let is_max jkind = sub Builtin.any_dummy_jkind jkind

let has_layout_any jkind =
  match jkind.jkind.layout with Any -> true | _ -> false

let is_value_for_printing ~ignore_null
    { jkind =
        { layout;
          modes_upper_bounds;
          externality_upper_bound;
          nullability_upper_bound
        };
      _
    } =
  match Layout.get_const layout with
  | Some const ->
    let value = Const.Builtin.value.jkind in
    Layout.Const.equal const value.layout
    && Modes.equal modes_upper_bounds value.modes_upper_bounds
    && Externality.equal externality_upper_bound value.externality_upper_bound
    && (ignore_null
       || Nullability.equal nullability_upper_bound
            value.nullability_upper_bound)
  | None -> false

(*********************************)
(* debugging *)

module Debug_printers = struct
  open Format

  let concrete_creation_reason ppf : History.concrete_creation_reason -> unit =
    function
    | Match -> fprintf ppf "Match"
    | Constructor_declaration idx ->
      fprintf ppf "Constructor_declaration %d" idx
    | Label_declaration lbl ->
      fprintf ppf "Label_declaration %a" Ident.print lbl
    | Record_projection -> fprintf ppf "Record_projection"
    | Record_assignment -> fprintf ppf "Record_assignment"
    | Record_functional_update -> fprintf ppf "Record_functional_update"
    | Let_binding -> fprintf ppf "Let_binding"
    | Function_argument -> fprintf ppf "Function_argument"
    | Function_result -> fprintf ppf "Function_result"
    | Structure_item_expression -> fprintf ppf "Structure_item_expression"
    | External_argument -> fprintf ppf "External_argument"
    | External_result -> fprintf ppf "External_result"
    | Statement -> fprintf ppf "Statement"
    | Optional_arg_default -> fprintf ppf "Optional_arg_default"
    | Layout_poly_in_external -> fprintf ppf "Layout_poly_in_external"
    | Unboxed_tuple_element -> fprintf ppf "Unboxed_tuple_element"
    | Peek_or_poke -> fprintf ppf "Peek_or_poke"

  let concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "Unannotated_type_parameter %a" !printtyp_path path
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Array_element -> fprintf ppf "Array_element"
    | Old_style_unboxed_type -> fprintf ppf "Old_style_unboxed_type"

  let rec annotation_context :
      type l r. _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p -> fprintf ppf "Type_declaration %a" Path.print p
    | Type_parameter (p, var) ->
      fprintf ppf "Type_parameter (%a, %a)" Path.print p
        (Misc.Stdlib.Option.print Misc.Stdlib.String.print)
        var
    | Newtype_declaration name -> fprintf ppf "Newtype_declaration %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "Constructor_type_parameter (%a, %S)" Path.print cstr name
    | Univar name -> fprintf ppf "Univar %S" name
    | Type_variable name -> fprintf ppf "Type_variable %S" name
    | Type_wildcard loc ->
      fprintf ppf "Type_wildcard (%a)" Location.print_loc loc
    | With_error_message (message, context) ->
      fprintf ppf "With_error_message (%s, %a)" message annotation_context
        context

  let any_creation_reason ppf : History.any_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Type_expression_call -> fprintf ppf "Type_expression_call"
    | Inside_of_Tarrow -> fprintf ppf "Inside_of_Tarrow"
    | Array_type_argument -> fprintf ppf "Array_type_argument"

  let immediate_creation_reason ppf : History.immediate_creation_reason -> _ =
    function
    | Empty_record -> fprintf ppf "Empty_record"
    | Enumeration -> fprintf ppf "Enumeration"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf "Immediate_polymorphic_variant"

  let value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Tuple_element -> fprintf ppf "Tuple_element"
    | Separability_check -> fprintf ppf "Separability_check"
    | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
    | Structure_element -> fprintf ppf "Structure_element"
    | V1_safety_check -> fprintf ppf "V1_safety_check"
    | Probe -> fprintf ppf "Probe"
    | Captured_in_object -> fprintf ppf "Captured_in_object"
    | Let_rec_variable v -> fprintf ppf "Let_rec_variable %a" Ident.print v

  let value_creation_reason ppf : History.value_creation_reason -> _ = function
    | Class_let_binding -> fprintf ppf "Class_let_binding"
    | Object -> fprintf ppf "Object"
    | Instance_variable -> fprintf ppf "Instance_variable"
    | Object_field -> fprintf ppf "Object_field"
    | Class_field -> fprintf ppf "Class_field"
    | Boxed_record -> fprintf ppf "Boxed_record"
    | Boxed_variant -> fprintf ppf "Boxed_variant"
    | Extensible_variant -> fprintf ppf "Extensible_variant"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path
    | Tuple -> fprintf ppf "Tuple"
    | Row_variable -> fprintf ppf "Row_variable"
    | Polymorphic_variant -> fprintf ppf "Polymorphic_variant"
    | Arrow -> fprintf ppf "Arrow"
    | Tfield -> fprintf ppf "Tfield"
    | Tnil -> fprintf ppf "Tnil"
    | First_class_module -> fprintf ppf "First_class_module"
    | Univar -> fprintf ppf "Univar"
    | Default_type_jkind -> fprintf ppf "Default_type_jkind"
    | Existential_type_variable -> fprintf ppf "Existential_type_variable"
    | Array_comprehension_element -> fprintf ppf "Array_comprehension_element"
    | Lazy_expression -> fprintf ppf "Lazy_expression"
    | Class_type_argument -> fprintf ppf "Class_type_argument"
    | Class_term_argument -> fprintf ppf "Class_term_argument"
    | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
    | Recmod_fun_arg -> fprintf ppf "Recmod_fun_arg"
    | Unknown s -> fprintf ppf "Unknown %s" s

  let product_creation_reason ppf : History.product_creation_reason -> _ =
    function
    | Unboxed_tuple -> fprintf ppf "Unboxed_tuple"
    | Unboxed_record -> fprintf ppf "Unboxed_record"

  let creation_reason ppf : History.creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Value_or_null_creation value ->
      fprintf ppf "Value_or_null_creation %a" value_or_null_creation_reason
        value
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Product_creation product ->
      fprintf ppf "Product_creation %a" product_creation_reason product
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_creation_reason concrete
    | Concrete_legacy_creation concrete ->
      fprintf ppf "Concrete_legacy_creation %a" concrete_legacy_creation_reason
        concrete
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.name id)
    | Imported -> fprintf ppf "Imported"
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "Imported_type_argument (pos %d, arity %d) of %a" position
        arity !printtyp_path parent_path
    | Generalized (id, loc) ->
      fprintf ppf "Generalized (%s, %a)"
        (match id with Some id -> Ident.unique_name id | None -> "")
        Location.print_loc loc

  let interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation p -> fprintf ppf "Gadt_equation %a" Path.print p
    | Tyvar_refinement_intersection ->
      fprintf ppf "Tyvar_refinement_intersection"
    | Subjkind -> fprintf ppf "Subjkind"

  let rec history ppf = function
    | Interact
        { reason;
          jkind1 = Pack jkind1;
          history1;
          jkind2 = Pack jkind2;
          history2
        } ->
      fprintf ppf
        "Interact {@[reason = %a;@ jkind1 = %a;@ history1 = %a;@ jkind2 = %a;@ \
         history2 = %a}@]"
        interact_reason reason Jkind_desc.Debug_printers.t jkind1 history
        history1 Jkind_desc.Debug_printers.t jkind2 history history2
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  let t ppf ({ jkind; annotation = a; history = h; has_warned = _ } : 'd t) :
      unit =
    fprintf ppf "@[<v 2>{ jkind = %a@,; annotation = %a@,; history = %a }@]"
      Jkind_desc.Debug_printers.t jkind
      (pp_print_option Pprintast.jkind_annotation)
      a history h

  module Const = struct
    let t ppf (jkind : _ Const.t) =
      fprintf ppf
        "@[<v 2>{ layout = %a@,\
         ; modes_upper_bounds = %a@,\
         ; externality_upper_bound = %a@,\
         ; nullability_upper_bound = %a@,\
         }@]"
        Layout.Const.Debug_printers.t jkind.layout Modes.print
        jkind.modes_upper_bounds Externality.print jkind.externality_upper_bound
        Nullability.print jkind.nullability_upper_bound
  end
end

(*** formatting user errors ***)
let report_error ~loc : Error.t -> _ = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]" Pprintast.jkind_annotation jkind
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%a) and a layout attribute \
       ([@@@@%s]).@]"
      Pprintast.jkind_annotation from_annotation
      (Builtin_attributes.jkind_attribute_to_string from_attribute.txt)
  | Insufficient_level { jkind; required_layouts_level } -> (
    let hint ppf =
      Format.fprintf ppf "You must enable -extension %s to use this feature."
        (Language_extension.to_command_line_string Layouts
           required_layouts_level)
    in
    match Language_extension.is_enabled Layouts with
    | false ->
      Location.errorf ~loc
        "@[<v>The appropriate layouts extension is not enabled.@;%t@]" hint
    | true ->
      Location.errorf ~loc
        (* CR layouts errors: use the context to produce a better error message.
           When RAE tried this, some types got printed like [t/2], but the
           [/2] shouldn't be there. Investigate and fix. *)
        "@[<v>Layout %a is more experimental than allowed by the enabled \
         layouts extension.@;\
         %t@]"
        Pprintast.jkind_annotation jkind hint)
  | Unimplemented_syntax ->
    Location.errorf ~loc "@[<v>Unimplemented kind syntax@]"

let () =
  Location.register_error_of_exn (function
    | Error.User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)
