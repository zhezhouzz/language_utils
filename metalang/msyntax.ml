open Ocaml5_parser
open Parsetree
open Sugar
open Convention
open Mutils

(* let mk_vb args = *)
(*   { *)
(*     pvb_pat = To_pat.typed_ids_to_pattern lhs; *)
(*     pvb_expr = expr_to_ocamlexpr rhs; *)
(*     pvb_attributes = []; *)
(*     pvb_loc = Location.none; *)
(*   } *)

let mk_first_match target_e
    (f : string -> (string * core_type) list -> string list * expression)
    (tyd : type_declaration) : expression =
  let cs =
    match tyd.ptype_kind with
    | Ptype_variant cs -> cs
    | _ -> _failatwith __FILE__ __LINE__ "unimp"
  in
  let cs =
    List.map
      (fun { pcd_name; pcd_args; _ } ->
        let name = pcd_name.Location.txt in
        (* let vars = List.map (fun arg -> arg.Location.txt) pcd_vars in *)
        let args =
          match pcd_args with
          | Pcstr_tuple cts ->
              List.mapi
                (fun idx ct ->
                  (mk_tmp_name idx (layout_ct (clear_ct_attr ct)), ct))
                cts
          | _ -> _failatwith __FILE__ __LINE__ "unimp"
        in
        (name, args))
      cs
  in
  mk_match target_e
  @@ List.map (fun (dtname, args) -> (dtname, f dtname args)) cs

let mk_fv (tyd : type_declaration) : structure =
  let selfname = "fv" in
  let ty = get_ty_from_type_dedeclaration tyd in
  let mk_t_typed ty =
    mk_ty_constr "typed" (List.map fst tyd.ptype_params @ [ ty ])
  in
  let type_name = tyd.ptype_name.Location.txt in
  let func_name = mk_func_name selfname type_name in
  let e_name = mk_var_name "e" type_name in
  let f_one (arg, ct) =
    (* let () = Printf.printf "ty: %s; ct: %s\n" (layout_ct ty) (layout_ct ct) in *)
    if ct_eq ty ct then mk_op_apply (func_name, [ mkvar arg ])
    else
      match ct.ptyp_desc with
      | Ptyp_constr (dt, ct') -> (
          (* let () = *)
          (*   Printf.printf "%s\n" *)
          (*     (Zzdatatype.Datatype.StrList.to_string *)
          (*     @@ Longident.flatten dt.Location.txt) *)
          (* in *)
          (* let () = *)
          (*   Printf.printf "%s\n" *)
          (*     (Zzdatatype.Datatype.List.split_by_comma layout_ct ct') *)
          (* in *)
          match (Longident.flatten dt.Location.txt, ct') with
          | [ "typed" ], [ ct_x; _ ] ->
              (* let att = *)
              (*   List.map *)
              (*     (fun a -> a.attr_name.Location.txt) *)
              (*     ct_x.ptyp_attributes *)
              (* in *)
              (* let _ = *)
              (*   Printf.printf "%s\n" (Zzdatatype.Datatype.StrList.to_string att) *)
              (* in *)
              if exists_attributes ct_x.ptyp_attributes "free" then
                mk_op_apply ("::", [ mkvar arg; mk_empty_list ])
              else mk_empty_list
          | [ "list" ], [ ct' ] when ct_eq (mk_t_typed ty) ct' ->
              mk_op_apply
                ( "List.concat",
                  [ mk_op_apply ("List.map", [ mkvar func_name; mkvar arg ]) ]
                )
          | [ "list" ], [ ct' ] when ct_eq ty ct' ->
              mk_op_apply
                ( "List.concat",
                  [ mk_op_apply ("List.map", [ mkvar func_name; mkvar arg ]) ]
                )
          | _ -> mk_empty_list (* _failatwith __FILE__ __LINE__ "unimp" *))
      | _ -> mk_empty_list
  in
  let f _ args =
    let args_name =
      List.map
        (fun (name, ct) ->
          if ct_eq ct ty then name
          else
            match ct.ptyp_desc with
            | Ptyp_constr (dt, ct') -> (
                match (Longident.flatten dt.Location.txt, ct') with
                | [ "typed" ], [ ct_x; _ ] ->
                    if exists_attributes ct_x.ptyp_attributes "free" then name
                    else "_"
                | [ "list" ], [ ct' ] when ct_eq (mk_t_typed ty) ct' -> name
                | [ "list" ], [ ct' ] when ct_eq ty ct' -> name
                | _ -> "_")
            | _ -> "_")
        args
    in
    ( args_name,
      List.fold_left
        (fun res e -> mk_op_apply ("@", [ res; e ]))
        mk_empty_list (List.map f_one args) )
  in
  let body = mk_first_match (mkvar e_name) f tyd in
  let fv_def = mk_rec_funcdef func_name (e_name, ty) body in
  let typed_fv_def =
    mk_funcdef
      (mk_typed_func_name selfname type_name)
      (e_name, mk_t_typed ty)
      (mk_op_apply (func_name, [ mkvar (e_name ^ ".x") ]))
  in
  [ fv_def; typed_fv_def ]

let handle_type_declaration (sourcefile : string) : string =
  let structure = Frontend.parse ~sourcefile in
  (* let _ = Printf.printf "%s\n" @@ Pprintast.string_of_structure structure in *)
  (* let item = *)
  (*   match structure_get_valuedefs structure with *)
  (*   | [ (flag, vbs) ] -> desc_to_structure_item @@ Pstr_value (flag, vbs) *)
  (*   | _ -> _failatwith __FILE__ __LINE__ "die" *)
  (* in *)
  (* let _ = Printf.printf "%s\n" @@ Pprintast.string_of_structure [ item ] in *)
  (* let _ = failwith "end" in *)
  let tyd =
    match structure_get_typedefs structure with
    | [ res ] -> res
    | [] -> _failatwith __FILE__ __LINE__ "die"
    | _ -> _failatwith __FILE__ __LINE__ "multiple typedef"
  in
  (* let tyd = match tyds with *)
  (*   | [tyd] -> tyd *)
  (*   | _ -> _failatwith __FILE__ __LINE__ "unimp" *)
  (* in *)
  let subst_code = mk_fv tyd in
  Pprintast.string_of_structure subst_code

let%test "test1" =
  let res =
    handle_type_declaration
      "/Users/zhezhou/workspace/research/language_utils/metalang/test2.ml"
  in
  Printf.printf "%s\n" res;
  true
