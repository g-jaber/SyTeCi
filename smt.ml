open Syntax
open Logic

let rec string_of_exprML_smtlib = function
  | Var x -> x
  | Loc l -> "l" ^ (string_of_int l)
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Plus (e1,e2) ->
    "(+ " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Minus(e1,e2) ->
    "(- " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Mult (e1,e2) ->
    "(* " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Div (e1,e2) ->
    "(/ " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Not e -> "(not " ^ (string_of_exprML e) ^")"
  | And (e1,e2) ->
    "(and " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Or (e1,e2) ->
    "(or " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Equal (e1,e2) ->
    "(= " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | NEqual (e1,e2) ->
    "(not (= " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Less (e1,e2) ->
    "(< " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | LessEq (e1,e2) ->
    "(<= " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | Great (e1,e2) ->
    "(> " ^ (string_of_exprML e1) ^  " " ^ (string_of_exprML e2) ^ ")"
  | GreatEq (e1,e2) ->
    "(>= " ^ (string_of_exprML e1) ^ " " ^ (string_of_exprML e2) ^ ")"
  | _ ->
    failwith "Error: Cannot print a non-ground term in SMT-LIB format. Please Report."

let rec string_of_arith_pred_smtlib = function
  | ATrue -> "true"
  | AFalse -> "false"
  | AAnd preds ->
    "(and " ^ string_of_conj " " string_of_arith_pred_smtlib preds ^ ")"
  | AOr preds -> "(or " ^ string_of_conj " " string_of_arith_pred_smtlib preds
                 ^ ")"
  | AEqual (e1,e2) -> "(= " ^ (string_of_exprML_smtlib e1) ^ " "
                      ^ (string_of_exprML_smtlib e2) ^")"
  | ANEqual (e1,e2) -> "(not (= " ^ (string_of_exprML_smtlib e1) ^ " "
                       ^ (string_of_exprML_smtlib e2) ^ "))"
  | ALess (e1,e2) -> "(< " ^ (string_of_exprML_smtlib e1) ^ " "
                     ^ (string_of_exprML_smtlib e2) ^ ")"
  | ALessEq (e1,e2) -> "(<= " ^ (string_of_exprML_smtlib e1) ^ " "
                       ^ (string_of_exprML_smtlib e2) ^ ")"
  | AGreat (e1,e2) -> "(> " ^ (string_of_exprML_smtlib e1) ^ " "
                      ^ (string_of_exprML_smtlib e2) ^ ")"
  | AGreatEq (e1,e2) -> "(>= " ^ (string_of_exprML_smtlib e1) ^ " "
                        ^ (string_of_exprML_smtlib e2) ^ ")"
  | ARel (f,[]) -> f
  | ARel (f,lexpr) -> "(" ^ f ^ " " ^
                      (String.concat " " (List.map string_of_exprML_smtlib lexpr)) ^ ")"


type smt_decl =
  | SMTVar of (Syntax.id * Syntax.typeML)
  | SMTRel of (Syntax.id * Syntax.typeML list)

let compare_smt_decl d1 d2 = match (d1,d2) with
  | (SMTVar (x,_),SMTVar (y,_)) | (SMTRel (x,_),SMTRel (y,_)) ->
    Syntax.compare_id x y
  | (SMTVar _, SMTRel _) -> 1
  | (SMTRel _, SMTVar _) -> -1

module SMTEnv = Set.Make(
  struct
    let compare = compare_smt_decl
    type t = smt_decl
  end)

let union_list = List.fold_left (SMTEnv.union) SMTEnv.empty

let rec generate_lty = function
  | 0 -> []
  | n -> TInt::(generate_lty (n-1))


let smt_decl_to_string = function
  | SMTVar (x,ty) -> "(declare-var " ^ x ^ " " ^ (string_of_typeML ty) ^ ")"
  | SMTRel (x,lty) ->
    let string_lty = String.concat " " (List.map string_of_typeML lty) in
    "(declare-rel " ^ x ^ " (" ^ string_lty ^ "))"

let print_smt_env file env =
  SMTEnv.iter (fun x -> Printer.print_to_file file (smt_decl_to_string x)) env
