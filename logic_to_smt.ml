open Syntax
open Logic
open Z3
open Arithmetic
open Integer
open Boolean
open Solver
open Symbol



let rec exprML_to_z3_term ctx symbenv = function
  | Var x -> 
    begin match Pmap.lookup_pmap x symbenv with
      | Some s -> Integer.mk_const ctx s  (* We may have to use another sort for Locations *)
      | None -> failwith ("The variable " ^ x ^ "has not been found in the symbol environment. Please report.")
    end  
  | Int n -> mk_numeral_i ctx n
  | Bool b -> mk_val ctx b
  | Plus (e1,e2) -> mk_add ctx [exprML_to_z3_term ctx symbenv e1; exprML_to_z3_term ctx symbenv e2]
  | Minus (e1,e2) -> mk_sub ctx [exprML_to_z3_term ctx symbenv e1; exprML_to_z3_term ctx symbenv e2]
  | Mult (e1,e2) -> mk_mul ctx [exprML_to_z3_term ctx symbenv e1; exprML_to_z3_term ctx symbenv e2]
  | Div (e1,e2) -> mk_div ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2)
  | expr -> failwith ("Error: trying to transform " ^ (string_of_exprML expr) ^ " into a ground term. Please report.")  

let rec arith_pred_to_z3_term ctx symbenv = function
  | ATrue -> mk_true ctx
  | AFalse -> mk_false ctx
  | AAnd apreds -> mk_and ctx (List.map (arith_pred_to_z3_term ctx symbenv) apreds) 
  | AOr apreds -> mk_or ctx (List.map (arith_pred_to_z3_term ctx symbenv) apreds) 
  | AEqual (e1,e2) -> mk_eq ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2)
  | ANEqual (e1,e2) -> mk_not ctx (mk_eq ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2))
  | ALess (e1,e2) -> mk_lt ctx (exprML_to_z3_term ctx symbenv e1)  (exprML_to_z3_term ctx symbenv e2)
  | ALessEq (e1,e2) -> mk_le ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2)
  | AGreat (e1,e2) -> mk_gt ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2)
  | AGreatEq (e1,e2) -> mk_ge ctx (exprML_to_z3_term ctx symbenv e1) (exprML_to_z3_term ctx symbenv e2)
  
let check_sat var_ctx arith_ctx =
  let ctx = mk_context [("model", "true"); ("proof", "false")] in
  let symbenv = List.map (fun (x,_) -> (x,mk_string ctx x)) var_ctx in
  let solver = mk_simple_solver ctx in
  let constraints = List.map (arith_pred_to_z3_term ctx symbenv) arith_ctx in
  add solver constraints;
  let result = check solver [] in
<<<<<<< HEAD
(*  Debug.print_debug ("Checking " ^  (Z3.Solver.to_string solver) ^ " : " ^ (string_of_status result));*)
=======
  Debug.print_debug ("Checking " ^  (Z3.Solver.to_string solver) ^ " : " ^ (string_of_status result));
>>>>>>> Forget a file
  match result with
   | SATISFIABLE -> true 
   | UNKNOWN -> true
   | UNSATISFIABLE -> false
