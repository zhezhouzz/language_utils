open Ocaml5_parser
open Parsetree
(* open Sugar *)

let exists_attributes l name =
  List.exists (fun a -> String.equal a.attr_name.Location.txt name) l

let rec clear_ct_attr ct =
  let aux = function
    | Ptyp_tuple cts -> Ptyp_tuple (List.map clear_ct_attr cts)
    | Ptyp_constr (name, cts) -> Ptyp_constr (name, List.map clear_ct_attr cts)
    | _ as t -> t
  in
  { ct with ptyp_attributes = []; ptyp_desc = aux ct.ptyp_desc }

let layout_ t =
  let _ = Format.flush_str_formatter () in
  Pprintast.pattern Format.str_formatter t;
  Format.flush_str_formatter ()

let dest_to_pat pat =
  {
    ppat_desc = pat;
    ppat_loc = Location.none;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let desc_to_ct t =
  {
    ptyp_desc = t;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

let get_if_rec flag =
  match flag with Asttypes.Recursive -> true | Asttypes.Nonrecursive -> false

let mk_idloc names =
  match Longident.unflatten names with
  | None -> failwith "die"
  | Some id -> Location.mknoloc id

let desc_to_ocamlexpr desc =
  {
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let desc_to_structure_item desc = { pstr_desc = desc; pstr_loc = Location.none }
let mk_ty_constr name args = desc_to_ct @@ Ptyp_constr (mk_idloc [ name ], args)

let get_ty_from_type_dedeclaration { ptype_name; ptype_params; _ } =
  let type_name = ptype_name.Location.txt in
  let args = List.map fst ptype_params in
  mk_ty_constr type_name args

let string_to_pattern name = dest_to_pat @@ Ppat_var (Location.mknoloc name)
let tuple_to_pattern pats = dest_to_pat @@ Ppat_tuple pats

let string_dataconstr_to_pattern (name, args) =
  let c = Location.mknoloc (Longident.Lident name) in
  let res =
    match args with
    | [] -> Ppat_construct (c, None)
    | [ arg ] -> Ppat_construct (c, Some ([], arg))
    | _ -> Ppat_construct (c, Some ([], tuple_to_pattern args))
  in
  dest_to_pat res

let string_to_ct name =
  desc_to_ct @@ Ptyp_constr (Location.mknoloc @@ Longident.Lident name, [])

let typed_to_pattern (pat, ct) = dest_to_pat @@ Ppat_constraint (pat, ct)
let typed_to_expr (expr, ct) = desc_to_ocamlexpr @@ Pexp_constraint (expr, ct)

let structure_item_get_typedefs (structure : structure_item) =
  match structure.pstr_desc with Pstr_type (_, tyds) -> Some tyds | _ -> None

let structure_get_typedefs (structure : structure) =
  List.concat @@ List.filter_map structure_item_get_typedefs structure

let structure_item_get_valuedefs (structure : structure_item) =
  match structure.pstr_desc with
  | Pstr_value (flag, vbs) -> Some (flag, vbs)
  | _ -> None

let structure_get_valuedefs (structure : structure) =
  List.filter_map structure_item_get_valuedefs structure

let mklam argpat body =
  desc_to_ocamlexpr @@ Pexp_fun (Asttypes.Nolabel, None, argpat, body)

let mkvar name = desc_to_ocamlexpr @@ Pexp_ident (mk_idloc [ name ])

let mk_vb (lhs, rhs) =
  {
    pvb_pat = string_to_pattern lhs;
    pvb_expr = rhs;
    pvb_attributes = [];
    pvb_loc = Location.none;
  }

let mk_valuedef vbs =
  desc_to_structure_item @@ Pstr_value (Asttypes.Nonrecursive, vbs)

let mk_rec_valuedef vbs =
  desc_to_structure_item @@ Pstr_value (Asttypes.Recursive, vbs)

let mk_rec_funcdef fname (argname, ct) body =
  let argpat = typed_to_pattern (string_to_pattern argname, ct) in
  mk_rec_valuedef [ mk_vb (fname, mklam argpat body) ]

let mk_funcdef fname (argname, ct) body =
  let argpat = typed_to_pattern (string_to_pattern argname, ct) in
  mk_valuedef [ mk_vb (fname, mklam argpat body) ]

let mk_match_case (constructor, (args, exp)) =
  let lhs =
    string_dataconstr_to_pattern (constructor, List.map string_to_pattern args)
  in
  { pc_lhs = lhs; pc_guard = None; pc_rhs = exp }

let mk_match case_target cases =
  desc_to_ocamlexpr @@ Pexp_match (case_target, List.map mk_match_case cases)

let layout_ct t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter t;
  Format.flush_str_formatter ()

let ct_eq ct ct' = String.equal (layout_ct ct) (layout_ct ct')

let mk_op_apply (op, args) =
  desc_to_ocamlexpr
  @@ Pexp_apply (mkvar op, List.map (fun x -> (Asttypes.Nolabel, x)) args)

let mk_construct (name, args) =
  desc_to_ocamlexpr
  @@ Pexp_construct (Location.mknoloc (Longident.Lident name), args)

let mk_empty_list = mk_construct ("[]", None)
