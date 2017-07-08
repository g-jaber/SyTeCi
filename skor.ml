open Syntax
open Pmap
open Logic


(* -------- SKORs ---------- *) 

type expr_env = exprML pmap
type var_ctx = typeML pmap 

type tag =
  | Intern
  | Extern
  | WB
  | Wrong

type formula =
  | RelV of typeML * var_ctx * exprML * exprML * expr_env * expr_env
  | RelE of typeML * var_ctx * exprML * exprML * expr_env * expr_env
  | RelSI of typeML * var_ctx * exprML * exprML * expr_env * expr_env
  | RelSE of typeML * var_ctx * exprML * exprML * expr_env * expr_env    
  | RelK of typeML * typeML * var_ctx * eval_context * eval_context * expr_env * expr_env 

 
let rec string_of_formula = function
  | RelV (ty, cb_context, expr1, expr2, gamma1, gamma2) -> 
      "V[" ^ (string_of_typeML ty) ^ "]((" ^ string_of_exprML expr1 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma1) 
      ^ "),(" ^ string_of_exprML expr2 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma2) ^ "))"
  | RelE (ty, cb_context, expr1, expr2, gamma1, gamma2) -> 
      "E[" ^ (string_of_typeML ty) ^ "]((" ^ string_of_exprML expr1 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma1) 
      ^ "),(" ^ string_of_exprML expr2 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma2) ^ "))"
  | RelSI (ty, cb_context, expr1, expr2, gamma1, gamma2) -> 
      "S^i[" ^ (string_of_typeML ty) ^ "]((" ^ string_of_exprML expr1 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma1) 
      ^ "),(" ^ string_of_exprML expr2 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma2) ^ "))"
  | RelSE (ty, cb_context, expr1, expr2, gamma1, gamma2) -> 
      "S^e[" ^ (string_of_typeML ty) ^ "]((" ^ string_of_exprML expr1 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma1) 
      ^ "),(" ^ string_of_exprML expr2 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma2) ^ "))"            
  | RelK (ty1, ty2, cb_context, ctx1, ctx2, gamma1, gamma2) ->
     "K[" ^ (string_of_typeML ty1) ^ "," ^ (string_of_typeML ty2) ^ "]((" ^ string_of_eval_context ctx1 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma1) 
      ^ "),(" ^ string_of_eval_context ctx2 ^ "," ^ (string_of_pmap "->" string_of_exprML gamma2) ^ "))" 

(* -------- Sequents ---------- *) 

type log_ctx = arith_pred list  

type id_sequent = int

type annotation =
  | AnnotHeap  of tag*symbheap*symbheap*symbheap*symbheap 

let count_id_sequent = ref 0
let fresh_id_sequent () = 
  let x = !count_id_sequent in
  count_id_sequent := !count_id_sequent + 1;x
  
type sequent = { id : id_sequent;
                 logenvc : var_ctx;
                 logenvl : var_ctx;
                 logenvr : var_ctx;
                 logctx : log_ctx;
                 j : int;
                 k : int;                 
                 annot : annotation option;
                 formula : formula
               }
               
let emptyctx_sequent formula j k =
  { id = fresh_id_sequent (); logenvc = []; logenvl = []; logenvr = []; logctx = []; j = j; k = k; annot = None; formula = formula }               

let new_sequent sequent logenv_c logenv_l logenv_r logctx ?(j=sequent.j) ?(k=sequent.k) ?(annot=None) formula = 
  { id = fresh_id_sequent (); logenvc = logenv_c@sequent.logenvc; logenvl = logenv_l@sequent.logenvl; logenvr = logenv_r@sequent.logenvr; logctx = logctx@sequent.logctx; 
    j = j; k = k; annot = annot; formula = formula }
