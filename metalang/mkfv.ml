open Ocaml5_parser
open Parsetree
open Convention
open Mutils
(* open Sugar *)

type varset =
  | Pos of (pattern * expression)
  | Neg of (pattern * expression)
  | Unused

let mk_fv (tyd : type_declaration) : structure =
  let selfname = "fv" in
  let ty = get_ty_from_type_dedeclaration tyd in
  let mk_t_typed ty =
    mk_ty_constr "typed" (List.map fst tyd.ptype_params @ [ ty ])
  in
  let type_name = tyd.ptype_name.Location.txt in
  let func_name = mk_func_name selfname type_name in
  let typed_func_name = mk_typed_func_name selfname type_name in
  let e_name = mk_var_name "e" type_name in
  let f_one (arg, ct) =
    let default = Unused in
    let arg_term = mkvar arg in
    let attr, ct = split_ct_attr ct in
    let arg_pat = string_to_pattern arg in
    match_ct_switch ct
      ( default,
        [
          ( mk_t_typed ty_var,
            match attr with
            | Some "free" -> Pos (arg_pat, mk_list_literal [ arg_term ])
            | Some "bound" -> Neg (arg_pat, mk_list_literal [ arg_term ])
            | _ -> default );
          ( mk_ty_list (mk_t_typed ty_var),
            match attr with
            | Some "free" -> Pos (arg_pat, arg_term)
            | Some "bound" -> Neg (arg_pat, arg_term)
            | _ -> default );
          ( mk_t_typed ty,
            Pos (arg_pat, mk_op_apply (typed_func_name, [ mkvar arg ])) );
          ( mk_ty_list (mk_t_typed ty),
            Pos
              ( arg_pat,
                mk_list_concat_apply
                @@ mk_list_map_apply (mkvar typed_func_name) arg_term ) );
        ] )
  in
  let f { cargs = args; cmkpat; _ } =
    let res = List.map f_one args in
    let args =
      List.fold_left
        (fun args res ->
          match res with
          | Unused -> args @ [ None ]
          | Pos (name, _) -> args @ [ Some name ]
          | Neg (name, _) -> args @ [ Some name ])
        [] res
    in
    let body =
      List.fold_right
        (fun res body ->
          match res with
          | Unused -> body
          | Pos (_, e) -> mk_list_union_apply body e
          | Neg (_, e) -> mk_list_subtract_apply body e)
        res mk_empty_list
    in
    let lhs = cmkpat args in
    (lhs, body)
  in
  let body = mk_first_match (mkvar e_name) f tyd in
  let fv_def = (func_name, [ (e_name, ty) ], body) in
  let typed_fv_def =
    ( typed_func_name,
      [ (e_name, mk_t_typed ty) ],
      mk_op_apply (func_name, [ mkvar (e_name ^ ".x") ]) )
  in
  [ mk_multi_rec_funcdef_args [ fv_def; typed_fv_def ] ]
