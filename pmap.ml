type ('a,'b) pmap = ('a*'b) list

let empty_pmap = []

let rec lookup_pmap x = function
  | [] -> None
  | (y,v)::_ when x = y -> Some v
  | _::pmap -> lookup_pmap x pmap

let rec modadd_pmap x v = function
   | [] -> [(x,v)]
   | (y,_)::pmap when x = y -> (y,v)::pmap
   | hd::pmap -> hd::(modadd_pmap x v pmap)

let rec string_of_pmap sep f = function
  | [] -> ""
  | [(x,v)] -> x ^ sep ^ (f v)
  | (x,v)::pmap -> x ^ sep ^ (f v) ^ "," ^ (string_of_pmap sep f pmap)

let rec codom_of_pmap = function
  | [] -> []
  | (x,v)::l -> v::(codom_of_pmap l)
