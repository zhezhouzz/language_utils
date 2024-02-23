open Ocaml5_parser

(* open Parsetree *)
open Sugar
open Mutils

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
  let fv_code = Mkfv.mk_fv tyd in
  let subst_code = Mksubst.mk_subst tyd in
  Pprintast.string_of_structure (structure @ fv_code @ subst_code)

let%test "test1" =
  let res =
    handle_type_declaration
      "/Users/zhezhou/workspace/research/language_utils/metalang/test2.ml"
  in
  Printf.printf "%s\n" res;
  true
