open Mtyped
open Sexplib.Std

type 't term =
  | Var of (('t, string) typed[@free])
  | Const of int
  | Lam of {
      lamarg : (('t, string) typed[@bound]);
      lambody : ('t, 't term) typed;
    }
  | Err
  | Let of {
      if_rec : bool;
      rhs : ('t, 't term) typed;
      lhs : (('t, string) typed list[@bound]);
      letbody : ('t, 't term) typed;
    }
  | App of ('t, 't term) typed * ('t, 't term) typed list
  | AppOp of ('t, string) typed * ('t, 't term) typed list
  | Ite of ('t, 't term) typed * ('t, 't term) typed * ('t, 't term) typed
  | Tu of ('t, 't term) typed list
[@@deriving sexp]

let rec fv_term (term_e : 't term) =
  match term_e with
  | Var _t_stringtyped0 -> [] @ [ _t_stringtyped0 ]
  | Const _ -> []
  | Lam { lamarg; lambody } ->
      typed_subtract ([] @ typed_fv_term lambody) [ lamarg ]
  | Err -> []
  | Let { rhs; lhs; letbody; _ } ->
      typed_subtract ([] @ typed_fv_term letbody) lhs @ typed_fv_term rhs
  | App (_t__ttermtyped0, _t__ttermtypedlist1) ->
      ([] @ List.concat (List.map typed_fv_term _t__ttermtypedlist1))
      @ typed_fv_term _t__ttermtyped0
  | AppOp (_, _t__ttermtypedlist1) ->
      [] @ List.concat (List.map typed_fv_term _t__ttermtypedlist1)
  | Ite (_t__ttermtyped0, _t__ttermtyped1, _t__ttermtyped2) ->
      (([] @ typed_fv_term _t__ttermtyped2) @ typed_fv_term _t__ttermtyped1)
      @ typed_fv_term _t__ttermtyped0
  | Tu _t__ttermtypedlist0 ->
      [] @ List.concat (List.map typed_fv_term _t__ttermtypedlist0)

and typed_fv_term (term_e : ('t, 't term) typed) = fv_term term_e.x

let rec subst_term (string_x : string) (f : ('t, string) typed -> 't term)
    (term_e : 't term) =
  match term_e with
  | Var _t_stringtyped0 ->
      if String.equal _t_stringtyped0.x string_x then f _t_stringtyped0
      else Var _t_stringtyped0
  | Const int0 -> Const int0
  | Lam { lamarg; lambody } ->
      if String.equal lamarg.x string_x then Lam { lamarg; lambody }
      else Lam { lamarg; lambody = typed_subst_term string_x f lambody }
  | Err -> Err
  | Let { if_rec; rhs; lhs; letbody } ->
      if List.exists (fun x -> String.equal string_x x.x) lhs then
        Let { if_rec; rhs = typed_subst_term string_x f rhs; lhs; letbody }
      else
        Let
          {
            if_rec;
            rhs = typed_subst_term string_x f rhs;
            lhs;
            letbody = typed_subst_term string_x f letbody;
          }
  | App (_t__ttermtyped0, _t__ttermtypedlist1) ->
      App
        ( typed_subst_term string_x f _t__ttermtyped0,
          List.map (typed_subst_term string_x f) _t__ttermtypedlist1 )
  | AppOp (_t_stringtyped0, _t__ttermtypedlist1) ->
      AppOp
        ( _t_stringtyped0,
          List.map (typed_subst_term string_x f) _t__ttermtypedlist1 )
  | Ite (_t__ttermtyped0, _t__ttermtyped1, _t__ttermtyped2) ->
      Ite
        ( typed_subst_term string_x f _t__ttermtyped0,
          typed_subst_term string_x f _t__ttermtyped1,
          typed_subst_term string_x f _t__ttermtyped2 )
  | Tu _t__ttermtypedlist0 ->
      Tu (List.map (typed_subst_term string_x f) _t__ttermtypedlist0)

and typed_subst_term (string_x : string) (f : ('t, string) typed -> 't term)
    (term_e : ('t, 't term) typed) =
  term_e #-> (subst_term string_x f)
(* open Mtyped *)
(* open Sexplib.Std *)

(* type 't lit = *)
(*   | AC of int *)
(*   | AVar of ((string[@free]), 't) typed *)
(*   | ATu of ('t, 't lit) typed list *)
(*   | AProj of ('t, 't lit) typed * int *)
(*   | AAppOp of (string, 't) typed * ('t, 't lit) typed list *)
(* [@@deriving sexp] *)

(* let rec fv_lit (lit_e : 't lit) = *)
(*   match lit_e with *)
(*   | AC _ -> [] @ [] *)
(*   | AVar string__ttyped0 -> [] @ (string__ttyped0 :: []) *)
(*   | ATu _t__tlittypedlist0 -> *)
(*       [] @ List.concat (List.map typed_fv_lit _t__tlittypedlist0) *)
(*   | AProj (_, _) -> ([] @ []) @ [] *)
(*   | AAppOp (_, _t__tlittypedlist1) -> *)
(*       ([] @ []) @ List.concat (List.map typed_fv_lit _t__tlittypedlist1) *)

(* and typed_fv_lit (lit_e : ('t, 't lit) typed) = fv_lit lit_e.x *)

(* let rec subst_lit (string_x : string) (f : ('t, string) typed -> 't lit) *)
(*     (lit_e : 't lit) = *)
(*   match lit_e with *)
(*   | AC int0 -> AC int0 *)
(*   | AVar string__ttyped0 -> *)
(*       if String.equal string__ttyped0.x string_x then f string__ttyped0 *)
(*       else AVar string__ttyped0 *)
(*   | ATu _t__tlittypedlist0 -> *)
(*       ATu (List.map (typed_subst_lit string_x f) _t__tlittypedlist0) *)
(*   | AProj (_t__tlittyped0, int1) -> AProj (_t__tlittyped0, int1) *)
(*   | AAppOp (string__ttyped0, _t__tlittypedlist1) -> *)
(*       AAppOp *)
(*         ( string__ttyped0, *)
(*           List.map (typed_subst_lit string_x f) _t__tlittypedlist1 ) *)

(* and typed_subst_lit (string_x : string) (f : ('t, string) typed -> 't lit) *)
(*     (lit_e : ('t, 't lit) typed) = *)
(*   lit_e #-> (subst_lit string_x f) *)

(* let subst_lit_lit (string_x : string) (lit_y : 't lit) lit_e = *)
(*   subst_lit string_x (fun _ -> lit_y) lit_e *)

(* let subst_lit_id lit_x lit_y lit_e = *)
(*   subst_lit lit_x (fun x -> AVar lit_y #: x.ty) lit_e *)
