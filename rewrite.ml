open Pmap
open Syntax
open Unif
open Logic
open Skor
  
  
let rec rewrite_ac vars expr = match expr with
  | Plus (Int 0,expr) -> rewrite_ac vars expr
  | Plus (expr,Int 0) -> rewrite_ac vars expr  
  | Plus (Plus(Hole,expr1),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Plus (Plus (expr1,expr2),Hole)
  | Plus (Plus(expr1,Hole),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Plus (Plus (expr1,expr2),Hole)
  | Mult (Int 1,expr) -> rewrite_ac vars expr
  | Mult (expr,Int 1) -> rewrite_ac vars expr  
  | Mult (Mult(Hole,expr1),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Mult (Mult (expr1,expr2),Hole)
  | Mult (Mult(expr1,Hole),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Mult (Mult (expr1,expr2),Hole)
  | Mult (expr1,Mult(Hole,expr2)) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Mult (Mult (expr1,expr2),Hole)
  | Mult (expr1,Mult(expr2,Hole)) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Mult (Mult (expr1,expr2),Hole)  
  | And (And(Hole,expr1),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> And (And (expr1,expr2),Hole)
  | And (And(expr1,Hole),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> And (And (expr1,expr2),Hole)
  | Or (Or(Hole,expr1),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Or (Or (expr1,expr2),Hole)
  | Or (Or(expr1,Hole),expr2) when ((is_ground_term vars expr1) && (is_ground_term vars expr2)) -> Or (Or (expr1,expr2),Hole)   
  | Plus (expr1,expr2) when (is_ground_term_with_hole vars expr1) && (is_ground_term vars expr2) -> Plus (expr2,rewrite_ac vars expr1)
  | Plus (expr1,expr2) when (is_ground_term vars expr1) && (is_ground_term_with_hole vars expr2) -> Plus (expr1,rewrite_ac vars expr2)  
  | Mult (expr1,expr2) when (is_ground_term_with_hole vars expr1) && (is_ground_term vars expr2) -> Mult (expr2,rewrite_ac vars expr1)
  | Mult (expr1,expr2) when (is_ground_term vars expr1) && (is_ground_term_with_hole vars expr2) -> Mult (expr1,rewrite_ac vars expr2)    
  | And (expr1,expr2) when (is_ground_term_with_hole vars expr1) && (is_ground_term vars expr2) -> And (expr2,rewrite_ac vars expr1)
  | And (expr1,expr2) when (is_ground_term vars expr1) && (is_ground_term_with_hole vars expr2) -> And (expr1,rewrite_ac vars expr2)
  | Or (expr1,expr2) when (is_ground_term_with_hole vars expr1) && (is_ground_term vars expr2) -> Or (expr2,rewrite_ac vars expr1)
  | Or (expr1,expr2) when (is_ground_term vars expr1) && (is_ground_term_with_hole vars expr2) -> Or (expr1,rewrite_ac vars expr2)
  | Var _ | Loc _ | Unit | Int _  | Bool _ | Hole -> expr
  | Plus (expr1,expr2) -> Plus (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Minus (expr1,expr2) -> Minus (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Mult (expr1,expr2) -> Mult (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Div (expr1,expr2) -> Div (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Not expr' -> Not (rewrite_ac vars expr')
  | And (expr1,expr2) -> And (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Or (expr1,expr2) -> Or (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Equal (expr1,expr2) -> Equal (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | NEqual (expr1,expr2) -> NEqual (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Less (expr1,expr2) -> Less (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | LessEq (expr1,expr2) -> LessEq (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Great (expr1,expr2) -> Great (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | GreatEq (expr1,expr2) -> GreatEq (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | If (expr1,expr2,expr3) -> If (rewrite_ac vars expr1,rewrite_ac vars expr2,rewrite_ac vars expr3)
  | Fun (var,ty,expr') -> Fun (var,ty,rewrite_ac (List.filter (fun (x,_) -> x <> var)  vars) expr')
  | Fix (var,ty,idfun,ty',expr') -> Fix (var,ty,idfun,ty',rewrite_ac (List.filter (fun (x,_) -> x <> var && x <> idfun)  vars) expr')
  | Let (var,expr1,expr2) ->  Let (var,rewrite_ac vars expr1,rewrite_ac (List.filter (fun (x,_) -> x <> var) vars) expr2)
  | App (expr1,expr2) -> App (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Seq (expr1,expr2) -> Seq (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Pair (expr1,expr2) -> Pair (rewrite_ac vars expr1,rewrite_ac vars expr2)
  | Newref expr' -> Newref (rewrite_ac vars expr')
  | Deref expr' -> expr
  | Assign (expr1,expr2) -> Assign (expr1,rewrite_ac vars expr2)

  



