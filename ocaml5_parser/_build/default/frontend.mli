# 1 "driver/frontend.mli"
val parse: sourcefile:string -> Parsetree.structure
val parse_string: string -> Parsetree.structure
