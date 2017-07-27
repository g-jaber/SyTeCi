open Pmap
open Syntax


type gsubst = (id,exprML) pmap

let string_of_gsubst = string_of_pmap "=" string_of_exprML
