open Sugar

let escape_to_underscore str =
  String.of_seq
  @@ Seq.filter_map (fun x ->
         match x with
         | '.' -> Some '_'
         | ',' -> Some '_'
         | ' ' -> None
         | '\'' -> Some '_'
         | '(' -> None
         | ')' -> None
         | c -> Some (Char.lowercase_ascii c))
  @@ String.to_seq str

let mk_func_name (auxname : string) (tyname : string) =
  spf "%s_%s" auxname tyname

let mk_typed_func_name (auxname : string) (tyname : string) =
  spf "typed_%s" (mk_func_name auxname tyname)

let mk_var_name (name : string) (tyname : string) =
  spf "%s_%s" (escape_to_underscore tyname) (escape_to_underscore name)

let mk_tmp_name (idx : int) (tyname : string) =
  spf "%s%i" (escape_to_underscore tyname) idx
