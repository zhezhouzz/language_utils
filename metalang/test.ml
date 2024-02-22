type ('a, 't) typed = { x : 'a; ty : 't }

(* type 't lit = *)
(*   | AC of int *)
(*   | AVar of ((string[@free]), 't) typed *)
(*   | ATu of 't lit list *)
(*   | AProj of 't lit * int *)
(*   | AAppOp of (string, 't) typed * 't, 't lit list *)

(* let rec fv (lit_e : 't lit) = *)
(*   match lit_e with *)
(*   | AC _ -> [] @ [] *)
(*   | AVar string__ttyped_0 -> [] @ (string__ttyped_0 :: []) *)
(*   | ATu _tlitlist_0 -> [] @ List.concat (List.map fv _tlitlist_0) *)
(*   | AProj (_tlit_0, _) -> ([] @ fv _tlit_0) @ [] *)
(*   | AAppOp (_, _tlitlist_1) -> ([] @ []) @ List.concat (List.map fv _tlitlist_1) *)

(* let typed_fv (lit_e: 't lit typed) *)
