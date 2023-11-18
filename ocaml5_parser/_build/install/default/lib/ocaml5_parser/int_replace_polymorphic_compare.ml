# 1 "utils/int_replace_polymorphic_compare.ml"
let ( = )   : int -> int -> bool = Stdlib.( = )
let ( <> )  : int -> int -> bool = Stdlib.( <> )
let ( < )   : int -> int -> bool = Stdlib.( < )
let ( > )   : int -> int -> bool = Stdlib.( > )
let ( <= )  : int -> int -> bool = Stdlib.( <= )
let ( >= )  : int -> int -> bool = Stdlib.( >= )

let compare : int -> int -> int  = Stdlib.compare
