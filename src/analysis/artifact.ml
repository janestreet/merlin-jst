open Std

let {Logger. log} = Logger.for_section "locate"

type t = Cmt of Cmt_format.cmt_infos
       | Cms of Cms_format.cms_infos

let builddir = function
  | Cmt cmt_infos -> cmt_infos.cmt_builddir
  | Cms cms_infos -> cms_infos.cms_builddir
let sourcefile = function
  | Cmt cmt_infos -> cmt_infos.cmt_sourcefile
  | Cms cms_infos -> cms_infos.cms_sourcefile
let source_digest = function
  | Cmt cmt_infos -> cmt_infos.cmt_source_digest
  | Cms cms_infos -> cms_infos.cms_source_digest
let comments = function
  | Cmt cmt_infos -> cmt_infos.cmt_comments
  | Cms cms_infos -> cms_infos.cms_comments
let impl_shape = function
  | Cmt cmt_infos -> cmt_infos.cmt_impl_shape
  | Cms cms_infos -> cms_infos.cms_impl_shape

let uid_to_loc uid = function
  | Cmt cmt_infos ->
    Shape.Uid.Tbl.find_opt cmt_infos.cmt_uid_to_decl uid
    |> Option.bind ~f:(Misc_utils.loc_of_decl ~uid)
    |> Option.map ~f:(fun { Location.loc; _ } -> loc)
  | Cms cms_infos -> Shape.Uid.Tbl.find_opt cms_infos.cms_uid_to_loc uid

let uid_to_attributes uid = function
  | Cms cms_infos -> Shape.Uid.Tbl.find_opt cms_infos.cms_uid_to_attributes uid
  | Cmt cmt_infos ->
  let exception Found of Typedtree.attributes in
  let test elt_uid attributes =
    if Shape.Uid.equal uid elt_uid then raise (Found attributes)
  in
  let iterator =
    let first_item = ref true in
    let uid_is_comp_unit = match uid with
      | Shape.Uid.Compilation_unit _ -> true
      | _ -> false
    in
    fun env -> { Tast_iterator.default_iterator with

      (* Needed to return top-level module doc (when the uid is a compunit).
        The module docstring must be the first signature or structure item *)
      signature_item = (fun sub ({ sig_desc; _} as si) ->
        begin match sig_desc, !first_item, uid_is_comp_unit with
        | Tsig_attribute attr, true, true -> raise (Found [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.signature_item sub si);

      structure_item = (fun sub ({ str_desc; _} as sti) ->
        begin match str_desc, !first_item, uid_is_comp_unit with
        | Tstr_attribute attr, true, true -> raise (Found [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.structure_item sub sti);

      value_description = (fun sub ({ val_val; val_attributes; _ } as vd) ->
        test val_val.val_uid val_attributes;
        Tast_iterator.default_iterator.value_description sub vd);

      type_declaration = (fun sub ({ typ_type; typ_attributes; _ } as td) ->
        test typ_type.type_uid typ_attributes;
        Tast_iterator.default_iterator.type_declaration sub td);

      value_binding = (fun sub ({ vb_pat; vb_attributes; _ } as vb) ->
        begin match vb_pat.pat_desc with
        | Tpat_var (id, _, _, _) ->
            begin try
              let vd = Env.find_value (Pident id) env in
              test vd.val_uid vb_attributes
            with Not_found -> () end
        | _ -> () end;
        Tast_iterator.default_iterator.value_binding sub vb)
    }
  in
  let typedtree =
    match cmt_infos.Cmt_format.cmt_annots with
    | Interface s -> Some (`Interface { s with
        sig_final_env = Envaux.env_of_only_summary s.sig_final_env})
    | Implementation str -> Some (`Implementation { str with
        str_final_env = Envaux.env_of_only_summary str.str_final_env})
    | _ -> None
  in
  try begin match typedtree with
    | Some (`Interface s) ->
        let iterator = iterator s.sig_final_env in
        iterator.signature iterator s;
        log ~title:"doc_from_uid" "uid not found in the signature"
    | Some (`Implementation str) ->
        let iterator = iterator str.str_final_env in
        iterator.structure iterator str;
        log ~title:"doc_from_uid" "uid not found in the implementation"
    | _ -> () end;
    None
  with
    | Found attrs -> Some attrs
    | Not_found -> None

let read file =
  match File.of_filename file with
  | Some (CMT _ | CMTI _) -> Cmt (Cmt_cache.read file).cmt_infos
  | Some (CMS _ | CMSI _) -> Cms (Cms_cache.read file).cms_infos
  | Some (ML _ | MLL _ | MLI _) | None -> assert false

