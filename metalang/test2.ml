(* type ('a, 't) typed = { x : 'a; ty : 't } *)

type 't lit =
  | AC of Constant.t
  | AVar of ((string[@free]), 't) typed
  | ATu of ('t, 't lit) typed list
  | AProj of ('t, 't lit) typed * int
  | AAppOp of (Op.t, 't) typed * ('t, 't lit) typed list
[@@deriving sexp]

(* type t = U | B of bool | I of int | Tu of t list | Dt of string * t list *)
(* [@@deriving sexp] *)

let typed_fv_lit e = fv_lit e.x
