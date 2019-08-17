open Syntax

let rec iter n f x = match n with
  | 0 ->  x
  | _ -> let y = f x in iter (n-1) f y

let count_locvar = ref 0
let fresh_locvar () =
  let l = !count_locvar in
  count_locvar := !count_locvar + 1; ("ℓ" ^ (string_of_int l))



let count_lvar = ref 0
let fresh_lvar () =
  let x = !count_lvar in
  count_lvar := !count_lvar + 1;("x" ^ (string_of_int x))

let count_bvar = ref 0
let fresh_bvar () =
  let x = !count_bvar in
  count_bvar := !count_bvar + 1;("b" ^ (string_of_int x))

type arith_pred =
  | ATrue
  | AFalse
  | AAnd of arith_pred list
  | AOr of arith_pred list
  | AEqual of Syntax.exprML * Syntax.exprML
  | ANEqual of Syntax.exprML * Syntax.exprML
  | ALess of Syntax.exprML * Syntax.exprML
  | ALessEq of Syntax.exprML * Syntax.exprML
  | AGreat of Syntax.exprML * Syntax.exprML
  | AGreatEq of Syntax.exprML * Syntax.exprML
  | ARel of Syntax.id * (Syntax.exprML list)

let get_consfun_from_binpred = function
  | AEqual _ -> fun (x,y) -> AEqual (x,y)
  | ANEqual _ -> fun (x,y) -> ANEqual (x,y)
  | ALess _ -> fun (x,y) -> ALess (x,y)
  | ALessEq _ -> fun (x,y) -> ALessEq (x,y)
  | AGreat _ -> fun (x,y) -> AGreat (x,y)
  | AGreatEq _ -> fun (x,y) -> AGreatEq (x,y)
  | _ -> failwith ("No binary constructor function can be extracted. Please report.")

let get_consfun_from_polyadpred = function
  | AAnd _ -> fun preds -> AAnd preds
  | AOr _ -> fun preds -> AOr preds
  | _ -> failwith ("No polyadic constructor function can be extracted. Please report.")

let rec negate_arith_pred = function
  | ATrue -> AFalse
  | AFalse -> ATrue
  | AAnd preds -> AOr (List.map negate_arith_pred preds)
  | AOr preds -> AAnd (List.map negate_arith_pred preds)
  | AEqual (e1,e2) -> ANEqual (e1,e2)
  | ANEqual (e1,e2) -> AEqual (e1,e2)
  | ALess (e1,e2) -> AGreatEq (e1,e2)
  | ALessEq (e1,e2) -> AGreat (e1,e2)
  | AGreat (e1,e2) -> ALessEq (e1,e2)
  | AGreatEq (e1,e2) -> ALess (e1,e2)
  | ARel _ -> failwith ("Cannot negate a relation. Please report.")

let rec simplify_arith_pred = function
  | AAnd [] -> ATrue
  | AAnd [pred] -> pred
  | AAnd preds ->
      let preds' = List.filter (fun x-> x<>ATrue) preds in
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem AFalse preds'' then AFalse else AAnd preds''
  | AOr [] -> AFalse
  | AOr [pred] -> pred
  | AOr preds ->
      let preds' = List.filter (fun x-> x<>AFalse) preds in
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem ATrue preds'' then ATrue else AOr preds''
  | AEqual (expr1,expr2) when (expr1 = expr2) -> ATrue
  | ANEqual (expr1,expr2) when (expr1 = expr2) -> AFalse
  | ALess (expr1,expr2) when (expr1 = expr2) -> AFalse
  | ALessEq (expr1,expr2) when (expr1 = expr2) -> ATrue
  | AGreat (expr1,expr2) when (expr1 = expr2) -> AFalse
  | AGreatEq (expr1,expr2) when (expr1 = expr2) -> ATrue
  | pred -> pred


let trivially_false preds =
(*  let preds = List.map (fun pred -> iter 3  simplify_arith_pred pred) preds in*)
  (List.mem AFalse preds)
  || (List.fold_left (fun b -> fun pred -> (b || (List.mem (negate_arith_pred pred) preds))) false preds)

let expr_to_arith_pred = function
  | Bool true -> ATrue
  | Bool false -> AFalse
  | Equal (expr1,expr2) -> AEqual (expr1, expr2)
  | NEqual (expr1,expr2) -> ANEqual (expr1, expr2)
  | Less (expr1,expr2) -> ALess (expr1, expr2)
  | LessEq (expr1,expr2) -> ALessEq (expr1, expr2)
  | Great (expr1,expr2) -> AGreat (expr1, expr2)
  | GreatEq (expr1,expr2) -> AGreatEq (expr1, expr2)
  | expr -> failwith ("Error: trying to transform the expression " ^ (string_of_exprML expr) ^ " into a predicate.")

let rec string_of_conj sep g = function
  | [] -> ""
  | [p] -> g p
  | p::preds' -> let str1 = g p in
                 let str2 = (string_of_conj sep g preds') in
                 str1 ^ sep ^ str2

let rec string_of_arith_pred = function
  | ATrue -> "True"
  | AFalse -> "False"
  | AAnd preds -> "(" ^ string_of_conj " ∧ " string_of_arith_pred preds ^ ")"
  | AOr preds ->  "(" ^ string_of_conj " ∨ " string_of_arith_pred preds ^ ")"
  | AEqual (e1,e2) -> (string_of_exprML e1) ^ " = " ^ (string_of_exprML e2)
  | ANEqual (e1,e2) -> (string_of_exprML e1) ^ " ≠ " ^ (string_of_exprML e2)
  | ALess (e1,e2) -> (string_of_exprML e1) ^ " < " ^ (string_of_exprML e2)
  | ALessEq (e1,e2) -> (string_of_exprML e1) ^ " ≤ " ^ (string_of_exprML e2)
  | AGreat (e1,e2) -> (string_of_exprML e1) ^ " > " ^ (string_of_exprML e2)
  | AGreatEq (e1,e2) -> (string_of_exprML e1) ^ " ≥ " ^ (string_of_exprML e2)
  | ARel (f,lexpr) -> f ^ "(" ^ (String.concat "," (List.map string_of_exprML lexpr)) ^ ")"


let rec full_arith_simplification_aux = function
  | [] -> []
  | (AEqual (e1,e2) as apred)::preds ->
    if (List.mem (ANEqual (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | (ANEqual (e1,e2) as apred)::preds ->
    if (List.mem (AEqual (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | (ALess (e1,e2) as apred)::preds ->
    if (List.mem (AGreatEq (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | (ALessEq (e1,e2) as apred)::preds ->
    if (List.mem (AGreat (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | (AGreat (e1,e2) as apred)::preds ->
    if (List.mem (ALessEq (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | (AGreatEq (e1,e2) as apred)::preds ->
    if (List.mem (ALess (e1,e2)) preds) then [AFalse]
    else apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))
  | apred::preds -> apred::(List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds))

let full_arith_simplification apred = match apred with
  | AAnd preds -> simplify_arith_pred (AAnd (full_arith_simplification_aux preds))
  | _ -> apred

(* Refreshing functions *)

let rec freshen_expr env e = match e with
  | Var x ->
    begin match Pmap.lookup_pmap x env with
      | Some y -> (Var y,env)  (* We may have to use another sort for Locations *)
      | None -> let y = fresh_lvar () in
                (Var y,(x,y)::env)
    end
  | Int _ | Bool _ -> (e,env)
  | Plus (e1,e2) | Minus (e1,e2) | Mult (e1,e2) | Div (e1,e2) ->
    let f = Syntax.get_consfun_from_binexpr e in
    let (e1',env') = freshen_expr env e1 in
    let (e2',env'') = freshen_expr env' e2 in
    (f (e1',e2'),env'')
  | _ -> failwith ("Error: trying to refresh " ^ (string_of_exprML e) ^ " which is not a ground term. Please report.")


let rec freshen_pred env p = match p with
  | ATrue | AFalse -> (p,env)
  | AAnd preds | AOr preds ->
    let f = get_consfun_from_polyadpred p in
    let (preds',env') = freshen_pred_list env preds in
    (f preds',env')
  | AEqual (e1,e2) | ANEqual (e1,e2) | ALess (e1,e2) | ALessEq (e1,e2) | AGreat (e1,e2) | AGreatEq (e1,e2) ->
    let f = get_consfun_from_binpred p in
    let (e1',env') = freshen_expr env e1 in
    let (e2',env'') = freshen_expr env' e2 in
    (f (e1',e2'),env'')
  | ARel _ -> failwith ("Cannot refresh a relation. Please report")

and freshen_pred_list env = function
  | [] -> ([],env)
  | p::preds -> let (p',env') = freshen_pred env p in
                let (preds',env'') = freshen_pred_list env' preds in
                (p'::preds',env'')

let rec freshen_symb_heap env h = match h with
  | [] -> ([],env)
  | (x,e)::h' ->
    let (e',env') = freshen_expr env e in
    let (h'',env'') = freshen_symb_heap env' h' in
    ((x,e')::h'',env'')

let freshen_inv (h1,h2,preds1,preds2) =
  let (h1',env) = freshen_symb_heap [] h1 in
  let (h2',env') = freshen_symb_heap env h2 in
  let (preds1',env'') = freshen_pred env' preds1 in
  let (preds2',env''') = freshen_pred env'' preds2 in
  let var_ctx = List.map (fun (_,x) -> (x,TInt)) env''' in
  (h1',h2',preds1',preds2',var_ctx)
