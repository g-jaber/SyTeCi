open Pmap

type id = string
type loc = int
type typevar = string

let compare_id = String.compare

(* Types *)
type typeML =
  | TUnit
  | TInt
  | TBool
  | TArrow of typeML * typeML
  | TProd of typeML * typeML
  | TRef of typeML
  | TVar of typevar
  | TUndef

type var_ctx = (id,typeML) pmap
type loc_ctx = (loc,typeML) pmap

let rec string_par_of_typeML = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1,ty2) -> "(" ^ (string_par_of_typeML ty1) ^ "->"
                        ^ (string_of_typeML ty2) ^")"
  | TProd (ty1,ty2) -> "(" ^ (string_par_of_typeML ty1) ^ "*"
                       ^ (string_par_of_typeML ty2) ^ ")"
  | TRef ty -> "(ref " ^ (string_of_typeML ty)^")"
  | TVar typevar -> typevar
  | TUndef -> "undef"

and string_of_typeML = function
  | TUnit -> "Unit"
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (ty1,ty2) ->
    (string_par_of_typeML ty1) ^ "->" ^ (string_of_typeML ty2)
  | TProd (ty1,ty2) ->
    (string_par_of_typeML ty1) ^ "*" ^ (string_par_of_typeML ty2)
  | TRef ty -> "ref " ^ (string_of_typeML ty)
  | TVar typevar -> typevar
  | TUndef -> "undef"



let string_of_vars =
  let aux = function
    | TUndef -> "undef"
    | ty -> "::" ^ (string_of_typeML ty)
  in string_of_pmap "" aux ""

let count_typevar = ref 0
let fresh_typevar () =
  let a = !count_typevar in
  count_typevar := !count_typevar + 1;
  TVar ("'a" ^ (string_of_int a))

type type_subst = (id,typeML) pmap

let rec apply_type_subst ty subst = match ty with
  | TUnit | TInt | TBool | TRef _ -> ty
  | TArrow (ty1,ty2) ->
    TArrow (apply_type_subst ty1 subst, apply_type_subst ty2 subst)
  | TProd (ty1,ty2) ->
    TProd (apply_type_subst ty1 subst, apply_type_subst ty2 subst)
  | TVar tvar ->
    begin match Pmap.lookup_pmap tvar subst with
      | Some ty' -> ty'
      | None -> ty
    end
  | TUndef -> failwith "Error: undefined type, please report."

let rec subst_type tvar sty ty = match ty with
  | TUnit | TInt | TBool | TRef _ -> ty
  | TArrow (ty1,ty2) ->
    TArrow (subst_type tvar sty ty1, subst_type tvar sty ty2)
  | TProd (ty1,ty2) ->
    TProd (subst_type tvar sty ty1, subst_type tvar sty ty2)
  | TVar tvar' when tvar = tvar' -> sty
  | TVar _ -> ty
  | TUndef -> failwith "Error: undefined type, please report."

let subst_vctx tvar sty =
  Pmap.map (fun ty -> subst_type tvar sty ty)

let lsubst_type lsubst ty =
  Pmap.fold (fun ty (tvar,sty) -> subst_type tvar sty ty) ty lsubst

let lsubst_vctx lsubst =
  Pmap.map (fun ty -> lsubst_type lsubst ty)

let rec unify_type tsubst = function
  | (TUnit,TUnit) -> Some (TUnit, tsubst)
  | (TInt,TInt) -> Some (TInt, tsubst)
  | (TBool,TBool) -> Some (TBool, tsubst)
  | (TRef ty1, TRef ty2) ->
    begin match unify_type tsubst (ty1,ty2) with
      | None -> None
      | Some (ty,tsubst') -> Some (TRef ty,tsubst')
    end
  | (TArrow (ty11,ty12), TArrow (ty21,ty22)) ->
    begin match unify_type tsubst (ty11,ty21) with
      | None ->
        Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty11) ^ " and "
                           ^ (string_of_typeML ty21));
        None
      | Some (ty1,tsubst') ->
        begin match unify_type tsubst' (ty12,ty22) with
          | None ->
            Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty12)
                               ^ " and " ^ (string_of_typeML ty22));
            None
          | Some (ty2,tsubst'') -> Some (TArrow (ty1,ty2),tsubst'')
        end
    end
  | (TProd (ty11,ty12), TProd (ty21,ty22)) ->
    begin match unify_type tsubst (ty11,ty21) with
      | None ->
        Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty11) ^ " and "
                           ^ (string_of_typeML ty21));
        None
      | Some (ty1,lsubst') ->
        begin match unify_type lsubst' (ty12,ty22) with
          | None ->
            Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty12)
                               ^ " and " ^ (string_of_typeML ty22));
            None
          | Some (ty2,lsubst'') -> Some (TProd (ty1,ty2),lsubst'')
        end
    end
  | ((TVar tvar1) as ty1,TVar tvar2) when tvar1 = tvar2 -> Some (ty1, tsubst)
  | ((TVar _) as ty1,TVar tvar2) -> Some (ty1, modadd_pmap (tvar2,ty1) tsubst)
  | (TVar tvar,ty) | (ty, TVar tvar) ->
    begin match lookup_pmap tvar tsubst with
      | None -> Some (ty, modadd_pmap (tvar,ty) tsubst)
      | Some ty' ->
        begin match unify_type tsubst (ty,ty') with
          | None ->
            Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty)
                               ^ " and " ^ (string_of_typeML ty'));
            None
          | Some (ty'',lsubst'') -> Some (ty'', modadd_pmap (tvar,ty'') lsubst'')
        end
    end
  | (ty1,ty2) ->
    Debug.print_debug ("Cannot unify " ^ (string_of_typeML ty1) ^ " and "
                       ^ (string_of_typeML ty2));
    None

let rec close_type sty ty = match ty with
  | TUnit | TInt | TBool -> ty
  | TRef ty' -> TRef (close_type sty ty') 
  | TArrow (ty1,ty2) -> TArrow (close_type sty ty1, close_type sty ty2)
  | TProd (ty1,ty2) -> TProd (close_type sty ty1, close_type sty ty2)
  | TVar _ -> sty
  | TUndef -> failwith "Error: undefined type, please report."

(* Expressions *)

let count_evar = ref 0
let fresh_evar () =
  let x = !count_evar in
  count_evar := !count_evar + 1;("_y" ^ (string_of_int x))

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
  | Fun of (id * typeML) * exprML
  | Fix of (id * typeML) * (id * typeML) * exprML
  | Let of id * exprML * exprML
  | LetPair of id * id * exprML * exprML
  | App of exprML * exprML
  | Seq of exprML * exprML
  | Pair of exprML * exprML
  | Newref of exprML
  | Deref of exprML
  | Assign of exprML * exprML
  | Hole

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

let rec subst expr value value' = match expr with
  | Var _ when (expr = value) -> value'
  | Loc _ when (expr = value) -> value'
  | Hole when (expr = value) -> value'
  | Var _ | Loc _ | Hole | Unit | Int _ | Bool _ -> expr
  | Plus (expr1,expr2) ->
    Plus (subst expr1 value value', subst expr2 value value')
  | Minus (expr1,expr2) ->
    Minus (subst expr1 value value', subst expr2 value value')
  | Mult (expr1,expr2) ->
    Mult (subst expr1 value value', subst expr2 value value')
  | Div (expr1,expr2) ->
    Div (subst expr1 value value', subst expr2 value value')
  | And (expr1,expr2) ->
    And (subst expr1 value value', subst expr2 value value')
  | Or (expr1,expr2) ->
    Or (subst expr1 value value', subst expr2 value value')
  | Not expr ->
    Not (subst expr value value')
  | Equal (expr1,expr2) ->
    Equal (subst expr1 value value', subst expr2 value value')
  | NEqual (expr1,expr2) ->
    NEqual (subst expr1 value value', subst expr2 value value')
  | Less (expr1,expr2) ->
    Less (subst expr1 value value', subst expr2 value value')
  | LessEq (expr1,expr2) ->
    LessEq (subst expr1 value value', subst expr2 value value')
  | Great (expr1,expr2) ->
    Great (subst expr1 value value', subst expr2 value value')
  | GreatEq (expr1,expr2) ->
    GreatEq (subst expr1 value value', subst expr2 value value')
  | If (expr1,expr2,expr3) ->
    If (subst expr1 value value', subst expr2 value value',
        subst expr3 value value')
  | Fun ((var',ty),expr') when (Var var' <> value) ->
    Fun ((var', ty), subst expr' value value')
  | Fun _ -> expr
  | Fix ((idfun,tyf),(var',tyv),expr')
    when ((Var var' <> value) && (Var idfun <> value)) ->
    Fix ((idfun, tyf), (var', tyv), subst expr' value value')
  | Fix _ -> expr
  | Let (var',expr1,expr2) when (Var var' <> value) ->
    Let (var', subst expr1 value value', subst expr2 value value')
  | Let (var',expr1,expr2) ->
    Let (var', subst expr1 value value', expr2)
  | LetPair (var1,var2,expr1,expr2)
    when ((Var var1 <> value) && (Var var2 <> value)) ->
    LetPair (var1,var2, subst expr1 value value', subst expr2 value value')
  | LetPair (var1,var2,expr1,expr2) ->
    LetPair (var1,var2, subst expr1 value value', expr2)
  | App (expr1,expr2) ->
    App (subst expr1 value value', subst expr2 value value')
  | Seq (expr1,expr2) ->
    Seq (subst expr1 value value', subst expr2 value value')
  | Pair (expr1,expr2) ->
    Pair (subst expr1 value value', subst expr2 value value')
  | Newref expr' ->
    Newref (subst expr' value value')
  | Deref expr' ->
    Deref (subst expr' value value')
  | Assign (expr1,expr2) ->
    Assign (subst expr1 value value', subst expr2 value value')

let subst_list expr lsubst =
  List.fold_left (fun expr (var,value) -> subst expr (Var var) value)
    expr lsubst

let string_of_typed_var = function
  | (x,TUndef) -> x
  | (x,ty) -> "(" ^ x ^ ":" ^ (string_of_typeML ty) ^ ")"


let rec string_par_of_exprML = function
  | Var x -> x
  | Loc l -> "l" ^ (string_of_int l)
  | Unit -> "()"
  | Int n -> string_of_int n
  | e -> "(" ^ (string_of_exprML e) ^ ")" 

and string_of_exprML = function
  | Var x -> x
  | Loc l -> "l" ^ (string_of_int l)
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Plus (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "+" ^ (string_of_exprML e2) ^ ")"
  | Minus(e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "-" ^ (string_of_exprML e2) ^ ")"
  | Mult (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "*" ^ (string_of_exprML e2) ^ ")"
  | Div (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "/" ^ (string_of_exprML e2) ^ ")"
  | Not e -> "not " ^ (string_of_exprML e)
  | And (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "&&" ^ (string_of_exprML e2) ^ ")"
  | Or (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "||" ^ (string_of_exprML e2) ^ ")"
  | Equal (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "=" ^ (string_of_exprML e2) ^ ")"
  | NEqual (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "<>" ^ (string_of_exprML e2) ^ ")"
  | Less (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "<" ^ (string_of_exprML e2) ^ ")"
  | LessEq (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "<=" ^ (string_of_exprML e2) ^ ")"
  | Great (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ ">" ^ (string_of_exprML e2) ^ ")"
  | GreatEq (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ ">=" ^ (string_of_exprML e2) ^ ")"
  | If (e1,e2,e3) ->
    "if " ^ (string_of_exprML e1) ^ " then " ^ (string_of_exprML e2)
    ^ " else " ^ (string_of_exprML e3)
  | Fun (typedvar,e) ->
    "fun " ^ (string_of_typed_var typedvar) ^ " -> " ^ (string_of_exprML e)
  | Fix (typedvar1,typedvar2,e) ->
    "fix " ^ (string_of_typed_var typedvar1) ^ " "
    ^ (string_of_typed_var typedvar2)
    ^  " -> " ^ (string_of_exprML e)
  | Let (var,e1,e2) ->
    "let " ^ var ^ " = " ^ (string_of_exprML e1) ^ " in "
    ^ (string_of_exprML e2)
  | LetPair (var1,var2,e1,e2) ->
    "let (" ^ var1 ^ "," ^ var2 ^ ") = " ^ (string_of_exprML e1) ^ " in "
    ^ (string_of_exprML e2)
  | Seq (e1,e2) -> (string_of_exprML e1) ^ " ; " ^ (string_of_exprML e2)
  | App (e1,e2) -> (string_par_of_exprML e1) ^ " " ^ (string_par_of_exprML e2)
  | Pair (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "," ^ (string_of_exprML e2) ^ ")"
  | Newref e -> "ref " ^ (string_of_exprML e)
  | Deref e -> "!" ^ (string_of_exprML e)
  | Assign (e1,e2) -> (string_of_exprML e1) ^ " := " ^ (string_of_exprML e2)
  | Hole -> "•"

(* Auxiliary functions *)

let get_consfun_from_binexpr = function
  | Plus _ -> fun (x,y) -> Plus (x,y)
  | Minus _ -> fun (x,y) -> Minus (x,y)
  | Mult _ -> fun (x,y) -> Mult (x,y)
  | Div _ -> fun (x,y) -> Div (x,y)
  | And _ ->  fun (x,y) -> And (x,y)
  | Or _ ->  fun (x,y) -> Plus (x,y)
  | Equal _ ->  fun (x,y) -> Plus (x,y)
  | NEqual _ -> fun (x,y) -> NEqual (x,y)
  | Less _ ->  fun (x,y) -> Less (x,y)
  | LessEq _ ->  fun (x,y) -> LessEq (x,y)
  | Great _ ->  fun (x,y) -> Great (x,y)
  | GreatEq _ ->  fun (x,y) -> GreatEq (x,y)
  | Let (var,_,_) ->  fun (x,y) -> Let (var,x,y)
  | Seq _ ->  fun (x,y) -> Seq (x,y)
  | App _ ->  fun (x,y) -> App (x,y)
  | Pair _ -> fun (x,y) -> Pair (x,y)
  | Assign _ ->  fun (x,y) -> Assign (x,y)
  | expr -> failwith ("No binary constructor function can be extracted from "
                      ^ (string_of_exprML expr))

let get_consfun_from_unexpr = function
  | Not _ -> fun x -> Not x
  | Newref _ -> fun x -> Newref x
  | Deref _ -> fun x -> Deref x
  | expr -> failwith ("No unary constructor function can be extracted from "
                      ^ (string_of_exprML expr))

let get_aop_from_expr = function
  | Plus (expr1,expr2) -> (expr1,expr2,(+),fun (x,y) -> Plus (x,y))
  | Mult (expr1,expr2) -> (expr1,expr2,( * ),fun (x,y) -> Mult (x,y))
  | Minus (expr1,expr2) -> (expr1,expr2,(-),fun (x,y) -> Minus (x,y))
  | Div (expr1,expr2) -> (expr1,expr2,(/),fun (x,y) -> Div (x,y))
  | expr ->
    failwith ("Error: trying to extract a binary arithmetic function from "
              ^ (string_of_exprML expr))

let get_abop_from_expr = function
  | Equal (expr1,expr2) -> (expr1,expr2,(=),fun (x,y) -> Equal (x,y))
  | NEqual (expr1,expr2) -> (expr1,expr2,(<>),fun (x,y) -> NEqual (x,y))
  | Less (expr1,expr2) -> (expr1,expr2,(<),fun (x,y) -> Less (x,y))
  | LessEq (expr1,expr2) -> (expr1,expr2,(<=),fun (x,y) -> LessEq (x,y))
  | Great (expr1,expr2) -> (expr1,expr2,(<),fun (x,y) -> Great (x,y))
  | GreatEq (expr1,expr2) -> (expr1,expr2,(<=),fun (x,y) -> GreatEq (x,y))
  | expr ->
    failwith ("Error: trying to extract a boolean comparison function from "
              ^ (string_of_exprML expr))

let get_bop_from_expr = function
  | And (expr1,expr2) -> (expr1,expr2,(&&),fun (x,y) -> And (x,y))
  | Or (expr1,expr2) -> (expr1,expr2,(||),fun (x,y) -> Or (x,y))
  | expr ->
    failwith ("Error: trying to extract a boolean function from "
              ^ (string_of_exprML expr))


(* Full Expressions *)

type functional_env = (id,exprML) pmap

let string_of_functional_env = string_of_pmap "->" string_of_exprML

type full_expr = exprML*functional_env

let string_of_full_expr (expr,gamma) =
  "(" ^ (string_of_exprML expr) ^ ",[" ^
  (string_of_pmap "->" string_of_exprML "" gamma) ^ "])"

(* Evaluation Contexts *)

type eval_context = exprML

let rec extract_ctx expr = match expr with
  | _ when (isval expr) -> (expr,Hole)
  | Plus _ | Minus _ | Mult _ | Div _ ->
    let (expr1,expr2,_,cons_op) = get_aop_from_expr expr in
    extract_ctx_bin cons_op expr1 expr2
  | Not expr -> extract_ctx_un (fun x -> Not x) expr
  | And _ | Or _ ->
    let (expr1,expr2,_,cons_op) = get_bop_from_expr expr in
    extract_ctx_bin cons_op expr1 expr2
  | Equal _ | NEqual _ | Less _ | LessEq _ | Great _ | GreatEq _ ->
    let (expr1,expr2,_,cons_op) = get_abop_from_expr expr in
    extract_ctx_bin cons_op expr1 expr2
  | If (expr1,expr2,expr3) ->
    extract_ctx_un (fun x -> If (x,expr2,expr3)) expr1
  | Let (var,expr1,expr2) ->
    extract_ctx_un (fun x -> Let (var,x,expr2)) expr1
  | App (expr1,expr2) ->
    extract_ctx_bin (fun (x,y) -> App (x,y)) expr1 expr2
  | Seq (expr1,expr2) ->
    extract_ctx_un (fun x -> Seq (x,expr2)) expr1
  | Pair (expr1,expr2) ->
    extract_ctx_bin (fun (x,y) -> Pair (x,y)) expr1 expr2
  | Newref expr ->
    extract_ctx_un (fun x -> Newref x) expr
  | Deref expr ->
    extract_ctx_un (fun x -> Deref x) expr
  | Assign (expr1,expr2) ->
    extract_ctx_bin (fun (x,y) -> Assign (x,y)) expr1 expr2
  | expr ->
    failwith ("Error: trying to extract an evaluation context from "
              ^ (string_of_exprML expr) ^ ". Please report.")

and extract_ctx_bin cons_op expr1 expr2 =
  match (isval expr1, isval expr2) with
  | (false,_) -> let (res,ctx) = extract_ctx expr1 in
    (res,cons_op (ctx,expr2))
  | (_,false) -> let (res,ctx) = extract_ctx expr2 in
    (res,cons_op (expr1,ctx))
  | (true,true) -> (cons_op (expr1,expr2),Hole)

and extract_ctx_un cons_op expr =
  if (isval expr) then (expr,Hole)
  else let (result,ctx) = extract_ctx expr in (result, cons_op ctx)

let extract_call expr =
  let (expr',ctx) = extract_ctx expr in
  match expr' with
  | App (Var f,expr'') -> (f,expr'',ctx)
  | _ -> failwith ("Error : " ^ (string_of_exprML expr')
                   ^ " is not a call to a function. Please report.")

let extract_body = function
  | Fun ((var,_),expr) -> (var,(expr,Pmap.empty))
  | Fix ((idfun,_),(var,_),expr) as fullexpr -> (var,(expr,Pmap.singleton (idfun,fullexpr)))
  | expr -> failwith ("Error: " ^ (string_of_exprML expr)
                      ^ " is not a function. Please report.")

let fill_hole ctx expr = subst ctx Hole expr

let string_of_eval_context ctx = string_of_exprML ctx


(* Symbolic Heaps *)

type symbheap =  (id,exprML) pmap

let string_of_symb_heap = string_of_pmap "↪" string_of_exprML "ε"
