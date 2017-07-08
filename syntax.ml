type id = string
type loc = int

(* Types *)
type typeML =
  | TUnit
  | TInt
  | TBool
  | TArrow of typeML * typeML
  | TProd of typeML * typeML  
  | TRef of typeML

(* Expressions *)
type exprML =
  | Var of id
  | Loc of loc  
  | Unit
  | Int of int
  | Bool of bool
  | Plus of exprML * exprML
  | Minus of exprML * exprML
  | Mult of exprML * exprML  
  | Div of exprML * exprML
  | Not of exprML
  | And of exprML * exprML
  | Or of exprML * exprML  
  | Equal of exprML * exprML
  | NEqual of exprML * exprML  
  | Less of exprML * exprML
  | LessEq of exprML * exprML
  | Great of exprML * exprML
  | GreatEq of exprML * exprML  
  | If of exprML * exprML * exprML
  | Fun of id * typeML * exprML
  | Fix of id * typeML * id * typeML * exprML
  | Let of id * exprML * exprML
  | App of exprML * exprML
  | Seq of exprML * exprML  
  | Pair of exprML * exprML  
  | Newref of exprML
  | Deref of exprML
  | Assign of exprML * exprML
  | Hole

type eval_context = exprML

(*
let concretize_eval_context ctx = ctx Hole

let rec abstract_eval_context expr = match expr with
  | Hole -> fun x -> x
  | Var _ | Loc _ | Unit | Int _ | Bool _ -> fun _ -> expr
  | Plus (expr1,expr2) -> fun x -> (Plus (abstract_eval_context expr1 x,abstract_eval_context expr2 x))  
  | Minus (expr1,expr2) -> fun x -> (Minus (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | Mult (expr1,expr2) -> fun x -> (Mult (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | Div (expr1,expr2) -> fun x -> (Div (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | And (expr1,expr2) -> fun x -> (And (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | Or (expr1,expr2) -> fun x -> (Or (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | Equal (expr1,expr2) -> fun x -> (Equal (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | NEqual (expr1,expr2) -> fun x -> (NEqual (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | Less (expr1,expr2) -> fun x -> (Less (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | LessEq (expr1,expr2) -> fun x -> (LessEq (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | Great (expr1,expr2) -> fun x -> (Great (abstract_eval_context expr1 x,abstract_eval_context expr2 x)) 
  | GreatEq (expr1,expr2) -> fun x -> (GreatEq (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | Pair (expr1,expr2) -> fun x -> (Pair (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | If (expr1,expr2,expr3) -> fun x -> (If (abstract_eval_context expr1 x,abstract_eval_context expr2 x,abstract_eval_context expr3 x))
  | Fun (var,ty,expr) -> fun x -> (Fun (var,ty,abstract_eval_context expr x))
  | Fix (var,ty,var',ty',expr) -> fun x -> (Fix (var,ty,var',ty',abstract_eval_context expr x))  
  | Let (var,expr1,expr2) -> fun x -> (Let (var,abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | App (expr1,expr2) -> fun x -> (App (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | Seq (expr1,expr2) -> fun x -> (Seq (abstract_eval_context expr1 x,abstract_eval_context expr2 x))
  | Pair (expr1,expr2) -> fun x -> (Pair (abstract_eval_context expr1 x,abstract_eval_context expr2 x))  
  | Newref (expr) -> fun x -> (Newref (abstract_eval_context expr x))
  | Deref (expr) -> fun x -> (Deref (abstract_eval_context expr x))    
  | Assign (expr1,expr2) -> fun x -> (Assign (abstract_eval_context expr1 x,abstract_eval_context expr2 x))  *)       
  
let rec string_of_typeML = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1,ty2) -> (string_of_typeML ty1) ^ "->" ^ (string_of_typeML ty2)
  | TProd (ty1,ty2) -> (string_of_typeML ty1) ^ "*" ^ (string_of_typeML ty2)
  | TRef ty -> "ref " ^ (string_of_typeML ty)
  
let rec string_of_exprML = function
  | Var x -> x
  | Loc l -> "l" ^ (string_of_int l) 
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"  
  | Plus (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "+" ^ (string_of_exprML e2) ^ ")"
  | Minus(e1,e2) -> "(" ^ (string_of_exprML e1) ^ "-" ^ (string_of_exprML e2) ^ ")"
  | Mult (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "*" ^ (string_of_exprML e2) ^ ")"
  | Div (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "/" ^ (string_of_exprML e2) ^ ")"
  | Not e -> "not " ^ (string_of_exprML e)
  | And (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "&&" ^ (string_of_exprML e2) ^ ")"
  | Or (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "||" ^ (string_of_exprML e2) ^ ")"
  | Equal (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "=" ^ (string_of_exprML e2) ^ ")"
  | NEqual (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "<>" ^ (string_of_exprML e2) ^ ")"
  | Less (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "<" ^ (string_of_exprML e2) ^ ")"
  | LessEq (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "<=" ^ (string_of_exprML e2) ^ ")"
  | Great (e1,e2) -> "(" ^ (string_of_exprML e1) ^ ">" ^ (string_of_exprML e2) ^ ")"
  | GreatEq (e1,e2) -> "(" ^ (string_of_exprML e1) ^ ">=" ^ (string_of_exprML e2) ^ ")"  
  | If (e1,e2,e3) -> "if " ^ (string_of_exprML e1) ^ " then " ^ (string_of_exprML e2) ^ " else " ^ (string_of_exprML e3)
  | Fun (var,ty,e) -> "fun (" ^ var ^ ":" ^ (string_of_typeML ty) ^ ") -> " ^ (string_of_exprML e)
  | Fix (idfun,tyf,var,tyv,e) -> idfun ^ "(" ^ var ^ ":" ^ (string_of_typeML tyv) ^ "):(" ^ (string_of_typeML tyf) ^") -> " ^ (string_of_exprML e)
  | Let (var,e1,e2) -> "let " ^ var ^ " = " ^ (string_of_exprML e1) ^ " in " ^ (string_of_exprML e2)
  | Seq (e1,e2) -> (string_of_exprML e1) ^ " ; " ^ (string_of_exprML e2)
  | App (Var f,e2) -> f ^" " ^ (string_of_exprML e2)  
  | App (e1,e2) -> "(" ^ (string_of_exprML e1) ^ ")" ^ (string_of_exprML e2)
  | Pair (e1,e2) -> "(" ^ (string_of_exprML e1) ^ "," ^ (string_of_exprML e2) ^ ")"  
  | Newref e -> "ref " ^ (string_of_exprML e)
  | Deref e -> "!" ^ (string_of_exprML e)
  | Assign (e1,e2) -> (string_of_exprML e1) ^ " := " ^ (string_of_exprML e2)
  | Hole -> "•"

let string_of_eval_context ctx = string_of_exprML ctx
(* Auxiliary functions *)


let get_aop_from_expr = function
  | Plus (expr1,expr2) -> (expr1,expr2,(+),fun (x,y) -> Plus (x,y)) 
  | Mult (expr1,expr2) -> (expr1,expr2,( * ),fun (x,y) -> Mult (x,y)) 
  | Minus (expr1,expr2) -> (expr1,expr2,(-),fun (x,y) -> Minus (x,y)) 
  | Div (expr1,expr2) -> (expr1,expr2,(/),fun (x,y) -> Div (x,y)) 
  
let get_abop_from_expr = function
  | Equal (expr1,expr2) -> (expr1,expr2,(=),fun (x,y) -> Equal (x,y)) 
  | NEqual (expr1,expr2) -> (expr1,expr2,(<>),fun (x,y) -> NEqual (x,y)) 
  | Less (expr1,expr2) -> (expr1,expr2,(<),fun (x,y) -> Less (x,y)) 
  | LessEq (expr1,expr2) -> (expr1,expr2,(<=),fun (x,y) -> LessEq (x,y))   
  | Great (expr1,expr2) -> (expr1,expr2,(<),fun (x,y) -> Great (x,y)) 
  | GreatEq (expr1,expr2) -> (expr1,expr2,(<=),fun (x,y) -> GreatEq (x,y))  
  
let get_bop_from_expr = function
  | And (expr1,expr2) -> (expr1,expr2,(&&),fun (x,y) -> And (x,y)) 
  | Or (expr1,expr2) -> (expr1,expr2,(||),fun (x,y) -> Or (x,y))

 
let rec isval = function
  | Var _ -> true
  | Loc _ -> true  
  | Unit -> true
  | Int _ -> true
  | Bool _ -> true
  | Fix _ -> true    
  | Fun _ -> true
  | Pair (e1,e2) -> (isval e1) && (isval e2) 
  | _ -> false

                     
let rec extract_ctx expr = match expr with
  | _ when (isval expr) -> (expr,Hole)
  | Plus _ | Minus _ | Mult _ | Div _ -> 
     let (expr1,expr2,_,cons_op) = get_aop_from_expr expr in extract_ctx_bin cons_op expr1 expr2
  | Not expr -> extract_ctx_un (fun x -> Not x) expr
  | And _ | Or _ -> let (expr1,expr2,_,cons_op) = get_bop_from_expr expr in extract_ctx_bin cons_op expr1 expr2
  | Equal _ | NEqual _ | Less _ | LessEq _ | Great _ | GreatEq _ ->
     let (expr1,expr2,_,cons_op) = get_abop_from_expr expr in extract_ctx_bin cons_op expr1 expr2
  | If (expr1,expr2,expr3) -> extract_ctx_un (fun x -> If (x,expr2,expr3)) expr1
  | Let (var,expr1,expr2) -> extract_ctx_un (fun x -> Let (var,x,expr2)) expr1
  | App (expr1,expr2) -> extract_ctx_bin (fun (x,y) -> App (x,y)) expr1 expr2
  | Seq (expr1,expr2) -> extract_ctx_un (fun x -> Seq (x,expr2)) expr1
  | Pair (expr1,expr2) -> extract_ctx_bin (fun (x,y) -> Pair (x,y)) expr1 expr2
  | Newref expr -> extract_ctx_un (fun x -> Newref x) expr
  | Deref expr -> extract_ctx_un (fun x -> Deref x) expr
  | Assign (expr1,expr2) -> extract_ctx_bin (fun (x,y) -> Assign (x,y)) expr1 expr2

and extract_ctx_bin cons_op expr1 expr2 = match (isval expr1, isval expr2) with
                                  | (false,_) -> let (res,ctx) = extract_ctx expr1 in
                                                     (res,cons_op (ctx,expr2))
                                  | (_,false) -> let (res,ctx) = extract_ctx expr2 in
                                                     (res,cons_op (expr1,ctx))
                                  | (true,true) -> (cons_op (expr1,expr2),Hole)

and extract_ctx_un cons_op expr = if (isval expr) then (expr,Hole) 
                                  else let (result,ctx) = extract_ctx expr in (result, cons_op ctx)                                                      

let rec extract_call expr = 
  let (expr',ctx) = extract_ctx expr in
  match expr' with
  | App (Var f,expr'') -> (f,expr'',ctx)
  | _ -> failwith ("Error : " ^ (string_of_exprML expr') ^ " is not a call to a function")

  
let rec subst expr value value' = match expr with
  | Var _ when (expr = value) -> value'
  | Loc _ when (expr = value) -> value'  
  | Hole when (expr = value) -> value'
  | Plus (expr1,expr2) -> Plus (subst expr1 value value', subst expr2 value value')
  | Minus (expr1,expr2) -> Minus (subst expr1 value value', subst expr2 value value')
  | Mult (expr1,expr2) -> Mult (subst expr1 value value', subst expr2 value value')
  | Div (expr1,expr2) -> Div (subst expr1 value value', subst expr2 value value')
  | Equal (expr1,expr2) -> Equal (subst expr1 value value', subst expr2 value value')
  | NEqual (expr1,expr2) -> NEqual (subst expr1 value value', subst expr2 value value')  
  | Less (expr1,expr2) -> Less (subst expr1 value value', subst expr2 value value')
  | LessEq (expr1,expr2) -> LessEq (subst expr1 value value', subst expr2 value value')
  | Great (expr1,expr2) -> Great (subst expr1 value value', subst expr2 value value')
  | GreatEq (expr1,expr2) -> GreatEq (subst expr1 value value', subst expr2 value value')  
  | If (expr1,expr2,expr3) -> If (subst expr1 value value', subst expr2 value value', subst expr3 value value')  
  | Fun (var',ty,expr') when (Var var' <> value) -> Fun (var', ty, subst expr' value value')
  | Fix (idfun,tyf,var',tyv,expr') when ((Var var' <> value) && (Var idfun <> value)) -> Fix (idfun, tyf, var', tyv, subst expr' value value')
  | Let (var',expr1,expr2) when (Var var' <> value) -> Let (var', subst expr1 value value', subst expr2 value value')
  | Let (var',expr1,expr2) -> Let (var', subst expr1 value value', expr2)  
  | App (expr1,expr2) -> App (subst expr1 value value', subst expr2 value value')
  | Seq (expr1,expr2) -> Seq (subst expr1 value value', subst expr2 value value')  
  | Pair (expr1,expr2) -> Pair (subst expr1 value value', subst expr2 value value')
  | Newref expr' -> Newref (subst expr' value value')
  | Deref expr' -> Deref (subst expr' value value')    
  | Assign (expr1,expr2) -> Assign (subst expr1 value value', subst expr2 value value')
  | _ -> expr
  
let fill_hole ctx expr = subst ctx Hole expr   