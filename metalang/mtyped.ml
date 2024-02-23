type ('t, 'a) typed = { x : 'a; ty : 't } [@@deriving sexp]

let ( #: ) x ty = { x; ty }
let ( #-> ) { x; ty } f = (f x) #: ty
let typed_eq eq a b = eq a.x b.x

let typed_subtract a b =
  Zzdatatype.Datatype.List.substract (typed_eq String.equal) a b
