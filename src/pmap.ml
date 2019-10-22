type ('a,'b) pmap = ('a*'b) list

let empty = []

let singleton a = [a]

let union p1 p2 = p1@p2

let rec lookup_pmap x = function
  | [] -> None
  | (y,v)::_ when x = y -> Some v
  | _::pmap -> lookup_pmap x pmap

let add u p  = u::p

let rec modadd_pmap (x,v) = function
   | [] -> [(x,v)]
   | (y,_)::pmap when x = y -> (y,v)::pmap
   | hd::pmap -> hd::(modadd_pmap (x,v) pmap)

let rec modadd_pmap2 (x1,v1) (x2,v2) = function
   | [] -> [(x1,v1);(x2,v2)]
   | (y,_)::pmap when x1 = y -> (y,v1)::(modadd_pmap (x2,v2) pmap)
   | (y,_)::pmap when x2 = y -> (y,v2)::(modadd_pmap (x1,v1) pmap)   
   | hd::pmap -> hd::(modadd_pmap2 (x1,v1) (x2,v2) pmap)

let rec string_of_pmap sep f empty = function
  | [] -> empty
  | [(x,v)] -> x ^ sep ^ (f v)
  | (x,v)::pmap -> x ^ sep ^ (f v) ^ "," ^ (string_of_pmap sep f empty pmap)

let dom_of_pmap pmap = List.map fst pmap

let codom_of_pmap pmap = List.map snd pmap

let map f = List.map (fun (x,v) -> (x, f v))

let fold = List.fold_left