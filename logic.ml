open Syntax
open Pmap

let rec iter n f x = match n with
  | 0 ->  x
  | _ -> let y = f x in iter (n-1) f y

type symbheap =  Syntax.exprML pmap

let count_locvar = ref 0
let fresh_locvar () = 
  let l = !count_locvar in
  count_locvar := !count_locvar + 1; ("l" ^ (string_of_int l))

let string_of_symb_heap = function
  | [] -> "emp"
  | heap -> "[" ^ (string_of_pmap "->" string_of_exprML heap) ^ "]"  
  
let count_lvar = ref 0
let fresh_lvar () = 
  let x = !count_lvar in
  count_lvar := !count_lvar + 1;("x" ^ (string_of_int x))

type arith_pred =
  | ATrue
  | AFalse
  | AExpr of Syntax.exprML
  | AAnd of arith_pred list
  | AOr of arith_pred list  
  | AEqual of arith_pred * arith_pred
  | ANEqual of arith_pred * arith_pred
  | ALess of arith_pred * arith_pred
  | ALessEq of arith_pred * arith_pred
  | AGreat of arith_pred * arith_pred
  | AGreatEq of arith_pred * arith_pred

let rec negate_arith_pred = function
  | ATrue -> AFalse
  | AFalse -> ATrue
  | AAnd preds -> AOr (List.map negate_arith_pred preds)
  | AOr preds -> AAnd (List.map negate_arith_pred preds)  
  | AEqual (p1,p2) -> ANEqual (p1,p2)
  | ANEqual (p1,p2) -> AEqual (p1,p2)
  | ALess (p1,p2) -> AGreatEq (p1,p2)
  | ALessEq (p1,p2) -> AGreat (p1,p2)
  | AGreat (p1,p2) -> ALessEq (p1,p2)
  | AGreatEq (p1,p2) -> ALess (p1,p2)
  | AExpr e -> failwith ("Error: trying to negate the expression " ^ (string_of_exprML e) ^".")

let rec simplify_arith_pred = function
  | AAnd [] -> ATrue
  | AAnd [pred] -> pred
  | AAnd preds ->
      let preds' = List.filter (fun x-> x<>ATrue) preds in 
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem AFalse preds'' then AFalse else AAnd preds''
  | AOr preds ->
      let preds' = List.filter (fun x-> x<>AFalse) preds in 
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem ATrue preds'' then ATrue else AAnd preds''
  | AEqual (expr1,expr2) when (expr1 = expr2) -> ATrue
  | ANEqual (expr1,expr2) when (expr1 = expr2) -> AFalse
  | ALess (expr1,expr2) when (expr1 = expr2) -> AFalse
  | ALessEq (expr1,expr2) when (expr1 = expr2) -> ATrue
  | AGreat (expr1,expr2) when (expr1 = expr2) -> AFalse
  | AGreatEq (expr1,expr2) when (expr1 = expr2) -> ATrue
  | pred -> pred

let rec trivially_false preds =
(*  let preds = List.map (fun pred -> iter 3  simplify_arith_pred pred) preds in*)
  (List.mem AFalse preds)
  || (List.fold_left (fun b -> fun pred -> (b || (List.mem (negate_arith_pred pred) preds))) false preds)
  
let expr_to_arith_pred = function
  | Bool true -> ATrue
  | Bool false -> AFalse
  | Equal (expr1,expr2) -> AEqual (AExpr expr1, AExpr expr2)
  | NEqual (expr1,expr2) -> ANEqual (AExpr expr1, AExpr expr2)
  | Less (expr1,expr2) -> ALess (AExpr expr1, AExpr expr2)
  | LessEq (expr1,expr2) -> ALessEq (AExpr expr1, AExpr expr2)
  | Great (expr1,expr2) -> AGreat (AExpr expr1, AExpr expr2)
  | GreatEq (expr1,expr2) -> AGreatEq (AExpr expr1, AExpr expr2)
  | expr -> failwith ("Error: trying to transform the expression " ^ (string_of_exprML expr) ^ " into a predicate.")  

let rec string_of_conj sep g = function 
  | [] -> ""
  | [p] -> g p
  | p::preds' -> let str1 = g p in 
                 let str2 = (string_of_conj sep g preds') in
                 str1 ^ sep ^ str2     
  
let rec string_of_arith_pred = function
  | AExpr expr -> Syntax.string_of_exprML expr
  | ATrue -> "True"
  | AFalse -> "False"
  | AAnd preds -> string_of_conj " /\\ " string_of_arith_pred preds
  | AOr preds -> string_of_conj " \\/ " string_of_arith_pred preds  
  | AEqual (pred1,pred2) -> (string_of_arith_pred pred1) ^ " = " ^ (string_of_arith_pred pred2)
  | ANEqual (pred1,pred2) -> (string_of_arith_pred pred1) ^ " <> " ^ (string_of_arith_pred pred2)
  | ALess (pred1,pred2) -> (string_of_arith_pred pred1) ^ " < " ^ (string_of_arith_pred pred2)
  | ALessEq (pred1,pred2) -> (string_of_arith_pred pred1) ^ " <= " ^ (string_of_arith_pred pred2)
  | AGreat (pred1,pred2) -> (string_of_arith_pred pred1) ^ " > " ^ (string_of_arith_pred pred2)
  | AGreatEq (pred1,pred2) -> (string_of_arith_pred pred1) ^ " >= " ^ (string_of_arith_pred pred2)


  

  
  
 
(*let rec string_of_heap = function 
  | [] -> ""
  | [(l,v)] -> l ^ " -> " ^ (Syntax.string_of_exprML v)
  | (l,v)::heap -> l ^ " -> " ^ (Syntax.string_of_exprML v) ^ "," ^ (string_of_heap heap)*)
         
      
let rec string_of_vars = string_of_pmap ":" string_of_typeML

      
