open Ocaml5_parser
open Parsetree
open Convention
open Mutils
open Sugar

let selfname = "subst"

(* type 'a ct_cases = { *)
(*   ct_eq : (core_type * 'a) list; *)
(*   ct_typed : core_type * core_type -> 'a; *)
(*   ct_list : core_type -> 'a; *)
(*   ct_string : 'a; *)
(*   ct_default : 'a; *)
(* } *)

(* let match_ct (ct : core_type) (handler : 'a ct_cases) : 'a = *)
(*   let res = *)
(*     List.fold_left *)
(*       (fun res (ct', res') -> *)
(*         match res with *)
(*         | Some _ -> res *)
(*         | None -> if ct_eq ct ct' then Some res' else None) *)
(*       None handler.ct_eq *)
(*   in *)
(*   match res with *)
(*   | Some res -> res *)
(*   | None -> ( *)
(*       match ct.ptyp_desc with *)
(*       | Ptyp_constr (dt, ct') -> ( *)
(*           match (Longident.flatten dt.Location.txt, ct') with *)
(*           | [ "typed" ], [ ct_ty; ct_x ] -> handler.ct_typed (ct_ty, ct_x) *)
(*           | [ "list" ], [ ct ] -> handler.ct_list ct *)
(*           | [ "string" ], [] -> handler.ct_string *)
(*           | _, _ -> handler.ct_default) *)
(*       | _ -> handler.ct_default) *)

let mk_subst (tyd : type_declaration) : structure =
  let mk_t_typed ty =
    mk_ty_constr "typed" (List.map fst tyd.ptype_params @ [ ty ])
  in
  let ty = get_ty_from_type_dedeclaration tyd in
  let x_ty = ty_var in
  (* let f_ty = mk_arrty (mk_t_typed x_ty) (mk_t_typed ty) in *)
  let f_ty = mk_arrty (mk_t_typed x_ty) ty in
  let type_name = tyd.ptype_name.Location.txt in
  let func_name = mk_func_name selfname type_name in
  let typed_func_name = mk_typed_func_name selfname type_name in
  let x_name = mk_var_name "x" "string" in
  let f_name = "f" in
  let e_name = mk_var_name "e" type_name in
  let x_term, f_term, e_term = map3 mkvar (x_name, f_name, e_name) in
  let f { cargs = args; cmkpat; cmke } =
    let rec aux (is_closed, args', args) =
      match args with
      | [] -> cmke (List.map (fun x -> Some x) args')
      | (arg, ct) :: args ->
          let arg_term = mkvar arg in
          let attr, ct = split_ct_attr ct in
          let ret b x = aux (b, args' @ [ x ], args) in
          let default = ret false arg_term in
          if is_closed then ret true arg_term
          else
            let typed_selfrec =
              mk_op_apply (typed_func_name, [ x_term; f_term; arg_term ])
            in
            let cases =
              [
                (mk_t_typed ty, ret false typed_selfrec);
                ( mk_t_typed x_ty,
                  let cond =
                    mk_op_apply ("String.equal", [ mkvar (arg ^ ".x"); x_term ])
                  in
                  match attr with
                  | Some "free" ->
                      mk_ite
                        ( cond,
                          mk_op_apply (f_name, [ arg_term ]),
                          ret false arg_term )
                  | Some "bound" -> mk_ite (cond, ret true arg_term, default)
                  | _ -> default );
                ( mk_ty_list (mk_t_typed ty),
                  ret false
                    (mk_list_map_apply
                       (mk_op_apply (typed_func_name, [ x_term; f_term ]))
                       arg_term) );
                ( mk_ty_list (mk_t_typed ty_var),
                  let cond =
                    mk_op_apply
                      ( "List.exists",
                        [
                          mklam (string_to_pattern "x")
                            (mk_op_apply
                               ("String.equal", [ x_term; mkvar "x.x" ]));
                          arg_term;
                        ] )
                  in
                  match attr with
                  | Some "bound" -> mk_ite (cond, ret true arg_term, default)
                  | _ ->
                      (* Printf.printf "%s\n" (layout_ct ct); *)
                      default );
              ]
            in
            match_ct_switch ct (default, cases)
    in
    let body = aux (false, [], args) in
    let lhs =
      cmkpat (List.map (fun (x, _) -> Some (string_to_pattern x)) args)
    in
    (lhs, body)
  in
  let body = mk_first_match e_term f tyd in
  let subst_def =
    (func_name, [ (x_name, x_ty); (f_name, f_ty); (e_name, ty) ], body)
  in
  let typed_body =
    mk_typed_map e_term @@ mk_op_apply (func_name, [ x_term; f_term ])
  in
  let typed_subst_def =
    ( typed_func_name,
      [ (x_name, x_ty); (f_name, f_ty); (e_name, mk_t_typed ty) ],
      typed_body )
  in
  [ mk_multi_rec_funcdef_args [ subst_def; typed_subst_def ] ]
