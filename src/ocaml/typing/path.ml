(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
    Pident of Ident.t
  | Pdot of t * string
  | Papply of t * t
  | Pextra_ty of t * extra_ty
and extra_ty =
  | Pcstr_ty of string
  | Pext_ty

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | (Pdot(p1, s1), Pdot(p2, s2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec compare p1 p2 =
=======
  | (Pdot(p1, s1), Pdot(p2, s2)) ->
      s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      same fun1 fun2 && same arg1 arg2
  | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
      let same_extra = match t1, t2 with
        | (Pcstr_ty s1, Pcstr_ty s2) -> s1 = s2
        | (Pext_ty, Pext_ty) -> true
        | ((Pcstr_ty _ | Pext_ty), _) -> false
      in same_extra && same p1 p2
  | (_, _) -> false

let rec compare p1 p2 =
>>>>>>> ocaml-flambda/flambda-backend:main
let rec same p1 p2 =
  p1 == p2
  || match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) ->
      s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      same fun1 fun2 && same arg1 arg2
  | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
      let same_extra = match t1, t2 with
        | (Pcstr_ty s1, Pcstr_ty s2) -> s1 = s2
        | (Pext_ty, Pext_ty) -> true
        | ((Pcstr_ty _ | Pext_ty), _) -> false
      in same_extra && same p1 p2
  | (_, _) -> false

let rec compare p1 p2 =
  if p1 == p2 then 0
  else match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.compare id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) ->
      let h = compare p1 p2 in
      if h <> 0 then h else String.compare s1 s2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
      let h = compare_extra t1 t2 in
      if h <> 0 then h else compare p1 p2
  | (Pident _, (Pdot _ | Papply _ | Pextra_ty _))
  | (Pdot _, (Papply _ | Pextra_ty _))
  | (Papply _, Pextra_ty _)
    -> -1
  | ((Pextra_ty _ | Papply _ | Pdot _), Pident _)
  | ((Pextra_ty _ | Papply _) , Pdot _)
  | (Pextra_ty _, Papply _)
    -> 1
and compare_extra t1 t2 =
  match (t1, t2) with
    Pcstr_ty s1, Pcstr_ty s2 -> String.compare s1 s2
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
  | (Pext_ty, Pext_ty)
    -> 0
  | (Pcstr_ty _, Pext_ty)
    -> -1
  | (Pext_ty, Pcstr_ty _)
    -> 1
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | ((Pident _ | Pdot _), (Pdot _ | Papply _)) -> -1
  | ((Pdot _ | Papply _), (Pident _ | Pdot _)) -> 1
=======
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | (Pextra_ty (p1, t1), Pextra_ty (p2, t2)) ->
      let h = compare_extra t1 t2 in
      if h <> 0 then h else compare p1 p2
  | (Pident _, (Pdot _ | Papply _ | Pextra_ty _))
  | (Pdot _, (Papply _ | Pextra_ty _))
  | (Papply _, Pextra_ty _)
    -> -1
  | ((Pextra_ty _ | Papply _ | Pdot _), Pident _)
  | ((Pextra_ty _ | Papply _) , Pdot _)
  | (Pextra_ty _, Papply _)
    -> 1
and compare_extra t1 t2 =
  match (t1, t2) with
    Pcstr_ty s1, Pcstr_ty s2 -> String.compare s1 s2
  | (Pext_ty, Pext_ty)
    -> 0
  | (Pcstr_ty _, Pext_ty)
    -> -1
  | (Pext_ty, Pcstr_ty _)
    -> 1
>>>>>>> ocaml-flambda/flambda-backend:main

let rec find_free_opt ids = function
    Pident id -> List.find_opt (Ident.same id) ids
  | Pdot(p, _) | Pextra_ty (p, _) -> find_free_opt ids p
  | Papply(p1, p2) -> begin
      match find_free_opt ids p1 with
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
      | None -> find_free_opt ids p2
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
=======
    end
>>>>>>> ocaml-flambda/flambda-backend:main
      | Some _ as res -> res
    end

let exists_free ids p =
  match find_free_opt ids p with
  | None -> false
  | _ -> true

let rec scope = function
    Pident id -> Ident.scope id
  | Pdot(p, _) | Pextra_ty (p, _) -> scope p
  | Papply(p1, p2) -> Int.max (scope p1) (scope p2)

let kfalse _ = false

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
let rec name ?(paren=kfalse) = function
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | Pdot(p, s) ->
=======
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
>>>>>>> ocaml-flambda/flambda-backend:main
    Pident id -> Ident.name id
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
=======
  | Pextra_ty (p, Pext_ty) -> name ~paren p
>>>>>>> ocaml-flambda/flambda-backend:main
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"
  | Pextra_ty (p, Pext_ty) -> name ~paren p

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
let rec print ppf = function
  | Pident id -> Ident.print_with_scope ppf id
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | Pdot(p, s) -> Format.fprintf ppf "%a.%s" print p s
=======
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
      Format.fprintf ppf "%a.%s" print p s
>>>>>>> ocaml-flambda/flambda-backend:main
  | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
      Format.fprintf ppf "%a.%s" print p s
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
=======
  | Pextra_ty (p, Pext_ty) -> print ppf p
>>>>>>> ocaml-flambda/flambda-backend:main
  | Papply(p1, p2) -> Format.fprintf ppf "%a(%a)" print p1 print p2
  | Pextra_ty (p, Pext_ty) -> print ppf p

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
let rec head = function
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | Pdot(p, _s) -> head p
=======
  | Pdot(p, _) | Pextra_ty (p, _) -> head p
>>>>>>> ocaml-flambda/flambda-backend:main
    Pident id -> id
  | Pdot(p, _) | Pextra_ty (p, _) -> head p
  | Papply _ -> assert false

let flatten =
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
  let rec flatten acc = function
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
    | Pdot (p, s) -> flatten (s :: acc) p
=======
    | Pdot (p, s) | Pextra_ty (p, Pcstr_ty s) -> flatten (s :: acc) p
>>>>>>> ocaml-flambda/flambda-backend:main
    | Pident id -> `Ok (id, acc)
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
    | Pdot (p, s) | Pextra_ty (p, Pcstr_ty s) -> flatten (s :: acc) p
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
=======
    | Pextra_ty (p, Pext_ty) -> flatten acc p
>>>>>>> ocaml-flambda/flambda-backend:main
    | Papply _ -> `Contains_apply
    | Pextra_ty (p, Pext_ty) -> flatten acc p
  in
  fun t -> flatten [] t

let heads p =
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
  let rec heads p acc = match p with
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
    | Pdot (p, _s) -> heads p acc
=======
    | Pdot (p, _) | Pextra_ty (p, _) -> heads p acc
>>>>>>> ocaml-flambda/flambda-backend:main
    | Pident id -> id :: acc
    | Pdot (p, _) | Pextra_ty (p, _) -> heads p acc
    | Papply(p1, p2) ->
        heads p1 (heads p2 acc)
  in heads p []

<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501
let rec last = function
  | Pident id -> Ident.name id
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  | Pdot(_, s) -> s
  | Papply(_, p) -> last p

let is_uident s =
  assert (s <> "");
  match s.[0] with
  | 'A'..'Z' -> true
  | _ -> false

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

let constructor_typath = function
  | Pident id when is_uident (Ident.name id) -> LocalExt id
  | Pdot(ty_path, s) when is_uident s ->
      if is_uident (last ty_path) then Ext (ty_path, s)
      else Cstr (ty_path, s)
  | p -> Regular p
=======
  | Pdot(_, s) | Pextra_ty (_, Pcstr_ty s) -> s
  | Papply(_, p) | Pextra_ty (p, Pext_ty) -> last p
>>>>>>> ocaml-flambda/flambda-backend:main
  | Pdot(_, s) | Pextra_ty (_, Pcstr_ty s) -> s
  | Papply(_, p) | Pextra_ty (p, Pext_ty) -> last p
<<<<<<< janestreet/merlin-jst:merge-flambda-backend-501

let is_constructor_typath p =
  match p with
||||||| ocaml-flambda/flambda-backend:0c8a400e403b8f888315d92b4a01883a3f971435
  match constructor_typath p with
  | Regular _ -> false
  | _ -> true
=======
  match p with
  | Pident _ | Pdot _ | Papply _ -> false
  | Pextra_ty _ -> true
>>>>>>> ocaml-flambda/flambda-backend:main
  | Pident _ | Pdot _ | Papply _ -> false
  | Pextra_ty _ -> true

module T = struct
  type nonrec t = t
  let compare = compare
end
module Set = Set.Make(T)
module Map = Map.Make(T)
