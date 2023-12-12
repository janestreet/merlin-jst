open Mconfig

let {Logger. log} = Logger.for_section "Mppx"

(* CR -H: do we need to do something for hidden includes here? *)
let with_include_dir path f =
  let saved = !Clflags.include_dirs in
  let restore () = Clflags.include_dirs := saved in
  Clflags.include_dirs := path;
  let result =
    begin
      try
        f ()
      with
      | e ->
         restore ();
         raise e
    end
  in
  restore ();
  result

(* CR -H: do we need to do something for hidden includes here? *)
let rewrite parsetree cfg =
  let ppx = cfg.ocaml.ppx in
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg).visible @@ fun () ->
  match
    Pparse.apply_rewriters ~restore:false ~ppx ~tool_name:"merlin" parsetree
  with
  | parsetree -> parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %a" Logger.fmt (fun fmt ->
      match Location.error_of_exn exn with
      | None | Some `Already_displayed ->
        Format.fprintf fmt "%s" (Printexc.to_string exn)
      | Some (`Ok err) ->
        Location.print_main fmt err
    );
    Msupport.raise_error exn;
    parsetree
