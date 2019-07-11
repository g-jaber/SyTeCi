open Syntax
open Logic
open Smt
open Str
open Z3.Arithmetic
open Z3.Boolean
open Z3.Solver

let dbi = ref 0

let new_dbi () =
  let x = !dbi in
  dbi:= !dbi+1;
  x

let typeML_to_z3sort ctx = function
  | TInt -> Z3.Arithmetic.Integer.mk_sort ctx
  | TBool -> Z3.Boolean.mk_sort ctx
  | TRef _ -> Z3.Arithmetic.Integer.mk_sort ctx
  | ty -> failwith ("Error: Cannot convert the type " ^ (string_of_typeML ty)
                    ^ " to a Z3 sort. Please report.")

type z3_decl =
  | Z3Var of Z3.Expr.expr
  | Z3Rel of Z3.FuncDecl.func_decl

let smt_decl_to_z3_decl ctx = function
  | SMTVar (x,ty) ->
    let dbi = new_dbi () in
    let sort = typeML_to_z3sort ctx ty in
    let expr = Z3.Quantifier.mk_bound ctx dbi sort in
    (x,Z3Var expr)
  | SMTRel (x,lty) ->
    let lsort = List.map (typeML_to_z3sort ctx) lty in
    let sbool = typeML_to_z3sort ctx TBool in
    let func_decl = Z3.FuncDecl.mk_func_decl_s ctx x lsort sbool in
    (x,Z3Rel func_decl)


let smtenv_to_z3env ctx smtenv =
  let smtenv_list = SMTEnv.elements smtenv in
  List.map (fun decl -> smt_decl_to_z3_decl ctx decl) smtenv_list

let get_z3rel_from_z3env z3env f =
 match Pmap.lookup_pmap f z3env with
      | Some (Z3Rel func_decl) -> func_decl
      | _ -> failwith ("Error: Cannot find the relation " ^ f
                       ^ " in the environment. Please report.")


let rec exprML_to_z3_expr ctx z3env = function
  | Var x ->
    begin match Pmap.lookup_pmap x z3env with
      | Some (Z3Var expr) -> expr  (* We may have to use another sort for Locations *)
      | Some (Z3Rel _) -> failwith ("Error: The variable " ^ x ^
                           " corresponds to a relation symbol. Please report.")
      | None -> failwith ("Error: The variable " ^ x ^
                          " has not been found in the symbol environment. Please report.")
    end
  | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
  | Bool b -> Z3.Boolean.mk_val ctx b
  | Plus (e1,e2) ->
    mk_add ctx [exprML_to_z3_expr ctx z3env e1; exprML_to_z3_expr ctx z3env e2]
  | Minus (e1,e2) ->
    mk_sub ctx [exprML_to_z3_expr ctx z3env e1; exprML_to_z3_expr ctx z3env e2]
  | Mult (e1,e2) ->
    mk_mul ctx [exprML_to_z3_expr ctx z3env e1; exprML_to_z3_expr ctx z3env e2]
  | Div (e1,e2) ->
    mk_div ctx (exprML_to_z3_expr ctx z3env e1) (exprML_to_z3_expr ctx z3env e2)
  | expr ->
    failwith ("Error: Trying to transform " ^ (string_of_exprML expr)
              ^ " into a ground term. Please report.")

let rec arith_pred_to_z3_expr ctx z3env = function
  | ATrue -> mk_true ctx
  | AFalse -> mk_false ctx
  | AAnd apreds ->
    mk_and ctx (List.map (arith_pred_to_z3_expr ctx z3env) apreds)
  | AOr apreds ->
    mk_or ctx (List.map (arith_pred_to_z3_expr ctx z3env) apreds)
  | AEqual (e1,e2) ->
    mk_eq ctx (exprML_to_z3_expr ctx z3env e1) (exprML_to_z3_expr ctx z3env e2)
  | ANEqual (e1,e2) ->
    mk_not ctx (mk_eq ctx (exprML_to_z3_expr ctx z3env e1)
                  (exprML_to_z3_expr ctx z3env e2))
  | ALess (e1,e2) ->
    mk_lt ctx (exprML_to_z3_expr ctx z3env e1)  (exprML_to_z3_expr ctx z3env e2)
  | ALessEq (e1,e2) -> mk_le ctx (exprML_to_z3_expr ctx z3env e1)
                         (exprML_to_z3_expr ctx z3env e2)
  | AGreat (e1,e2) ->
    mk_gt ctx (exprML_to_z3_expr ctx z3env e1)
      (exprML_to_z3_expr ctx z3env e2)
  | AGreatEq (e1,e2) ->
    mk_ge ctx (exprML_to_z3_expr ctx z3env e1)
      (exprML_to_z3_expr ctx z3env e2)
  | ARel (f,lexpr) ->
    begin match Pmap.lookup_pmap f z3env with
      | Some (Z3Rel func_decl) ->
        let arguments = List.map (exprML_to_z3_expr ctx z3env) lexpr in
        Z3.FuncDecl.apply func_decl arguments
      | Some (Z3Var _) ->
        failwith ("The symbol " ^ f
                  ^ " is a variable, not a relation. Please report.")

      | None ->
        failwith ("The relation " ^ f ^
                  " has not been found in the symbol environment. Please report.")
    end

let chc_to_z3_expr ctx z3env (_,rel,preds) =
  let rel_z3 = arith_pred_to_z3_expr ctx z3env rel in
  let preds_z3 = arith_pred_to_z3_expr ctx z3env (simplify_arith_pred preds) in
  mk_implies ctx preds_z3 rel_z3

let register_z3_relation fixedpoint = function
  | (_,Z3Var _) -> ()
  | (_,Z3Rel func_decl) -> Z3.Fixedpoint.register_relation fixedpoint func_decl


let check_sat var_ctx arith_ctx =
  let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
  let aux (x,ty) = (x,Z3Var (Z3.Arithmetic.Integer.mk_const_s ctx x)) in
  let z3env = List.map aux var_ctx in
  let solver = mk_simple_solver ctx in
  let constraints = List.map (arith_pred_to_z3_expr ctx z3env) arith_ctx in
  add solver constraints;
  let result = check solver [] in
  Debug.print_debug ("Checking " ^  (Z3.Solver.to_string solver) ^ " : "
                     ^ (string_of_status result));
  match result with
  | SATISFIABLE -> true
  | UNKNOWN -> true
  | UNSATISFIABLE -> false

let check_sat_chc (lchc,init_rel,smtenv) =
  Z3.toggle_warning_messages true;
  let ctx = Z3.mk_context [] in
  let fixedpoint = Z3.Fixedpoint.mk_fixedpoint ctx in
  let z3env = smtenv_to_z3env ctx smtenv in
  List.iter (register_z3_relation fixedpoint) z3env;
  let lchc_z3 = List.map (chc_to_z3_expr ctx z3env) lchc in
  List.iter (fun expr -> Z3.Fixedpoint.add_rule fixedpoint expr None) lchc_z3;
  let init_func_decl = get_z3rel_from_z3env z3env init_rel in
  let query = Z3.FuncDecl.apply init_func_decl [] in
  (* Debug.print_debug "Z3 fixedpoint parameters:";
  Debug.print_debug (Z3.Fixedpoint.get_help fixedpoint); *)
  Debug.print_debug "Z3 fixedpoint internal representation:";
  Debug.print_debug (Z3.Fixedpoint.to_string fixedpoint);
  let result = Z3.Fixedpoint.query fixedpoint query in
  match result with
   | SATISFIABLE -> "The two programs are not contextually equivalent."
   | UNKNOWN -> Debug.print_debug (Z3.Fixedpoint.get_reason_unknown fixedpoint);
     "Z3 failed to check satisfiability of the constrained Horn clauses associated
      to the reachability problem.";
   | UNSATISFIABLE -> "The two programs are contextually equivalent."

let get_chc_z3_str (lchc,init_rel,smtenv) =
     Z3.toggle_warning_messages true;
     let ctx = Z3.mk_context [] in
     let fixedpoint = Z3.Fixedpoint.mk_fixedpoint ctx in
     let z3env = smtenv_to_z3env ctx smtenv in
     List.iter (register_z3_relation fixedpoint) z3env;
     let lchc_z3 = List.map (chc_to_z3_expr ctx z3env) lchc in
     List.iter (fun expr -> Z3.Fixedpoint.add_rule fixedpoint expr None) lchc_z3;
     let str = Z3.Fixedpoint.to_string fixedpoint in
     let str_list = String.split_on_char '\n' str in
     let regexp = Str.regexp_string "(declare-fun" in
     let str_list = List.filter (fun str -> not (Str.string_match regexp str 0)) str_list in
     let str_list = str_list@["(query P:print-certificate true)"] in
     String.concat "\n" str_list

let check_sat_chc_str str =
  Z3.toggle_warning_messages true;
  let ctx = Z3.mk_context [] in
  let fixedpoint = Z3.Fixedpoint.mk_fixedpoint ctx in
let params = Z3.Params.mk_params ctx in
let symbol = Z3.Symbol.mk_string ctx  "spacer.gpdr" in
Z3.Params.add_bool params symbol true;
Z3.Fixedpoint.set_parameters fixedpoint params;
  let query = List.hd (Z3.Fixedpoint.parse_string fixedpoint str) in
  Debug.print_debug "Z3 fixedpoint internal representation:";
  Debug.print_debug (Z3.Fixedpoint.to_string fixedpoint);
  let result = Z3.Fixedpoint.query fixedpoint query in
  match result with
  | SATISFIABLE -> "The two programs are not contextually equivalent."
  | UNKNOWN -> Debug.print_debug (Z3.Fixedpoint.get_reason_unknown fixedpoint);
    "Z3 failed to check satisfiability of the constrained Horn clauses associated
              to the reachability problem.";
  | UNSATISFIABLE -> "The two programs are contextually equivalent."

  
let check_sat_chc_file file =
  Z3.toggle_warning_messages true;
  let ctx = Z3.mk_context [] in
  let fixedpoint = Z3.Fixedpoint.mk_fixedpoint ctx in
let params = Z3.Params.mk_params ctx in
let symbol = Z3.Symbol.mk_string ctx  "spacer.gpdr" in
Z3.Params.add_bool params symbol true;
Z3.Fixedpoint.set_parameters fixedpoint params;
  let [query] = Z3.Fixedpoint.parse_file fixedpoint file  in
   Debug.print_debug "Z3 fixedpoint parameters:";
  Debug.print_debug (Z3.Fixedpoint.get_help fixedpoint);
   Debug.print_debug "Z3 fixedpoint param:";
  Debug.print_debug (Z3.Params.ParamDescrs.to_string (Z3.Fixedpoint.get_param_descrs fixedpoint));  
   Debug.print_debug "Z3 fixedpoint options:";
  Debug.print_debug (Z3.Params.ParamDescrs.to_string (Z3.Fixedpoint.get_param_descrs fixedpoint));  
  Debug.print_debug "Z3 fixedpoint internal representation:";
  Debug.print_debug (Z3.Fixedpoint.to_string fixedpoint);
  let result = Z3.Fixedpoint.query fixedpoint query in
  match result with
  | SATISFIABLE -> "The two programs are not contextually equivalent."
  | UNKNOWN -> Debug.print_debug (Z3.Fixedpoint.get_reason_unknown fixedpoint);
    "Z3 failed to check satisfiability of the constrained Horn clauses associated
         to the reachability problem.";
  | UNSATISFIABLE -> "The two programs are contextually equivalent." 
