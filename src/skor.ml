open Syntax
open Pmap
open Logic


(* -------- SKORs ---------- *)

type tag =
  | Intern
  | Extern
  | WB
  | Wrong

type formula =
  | RelV of typeML * var_ctx * full_expr * full_expr
  | RelE of typeML * var_ctx * full_expr * full_expr
  | RelSI of typeML * var_ctx * full_expr * full_expr
  | RelSE of typeML * var_ctx * full_expr * full_expr
  | RelK of typeML * typeML * var_ctx * full_expr * full_expr
  | RelEquiv of typeML * var_ctx * full_expr * full_expr


let string_of_formula = function
  | RelV (ty, _, fexpr1, fexpr2) ->
    "V[" ^ (string_of_typeML ty) ^ "](" ^ (string_of_full_expr fexpr1)
    ^ "," ^ (string_of_full_expr fexpr2) ^ ")"
  | RelE (ty, _, fexpr1, fexpr2) ->
    "E[" ^ (string_of_typeML ty) ^ "](" ^ (string_of_full_expr fexpr1)
    ^ "," ^ (string_of_full_expr fexpr2) ^ ")"
  | RelSI (ty, _, fexpr1, fexpr2) ->
    "S^i[" ^ (string_of_typeML ty) ^ "](" ^ (string_of_full_expr fexpr1)
    ^ "," ^ (string_of_full_expr fexpr2) ^ ")"
  | RelSE (ty, _, fexpr1, fexpr2) ->
    "S^e[" ^ (string_of_typeML ty) ^ "](" ^ (string_of_full_expr fexpr1)
    ^ "," ^ (string_of_full_expr fexpr2) ^ ")"
  | RelK (ty1, ty2, _, fctx1, fctx2) ->
    "K[" ^ (string_of_typeML ty1) ^ "," ^ (string_of_typeML ty2) ^ "]("
    ^ (string_of_full_expr fctx1) ^ "," ^ (string_of_full_expr fctx2) ^ ")"
  | RelEquiv (ty,var_ctx,fexpr1,fexpr2) ->
    (string_of_full_expr fexpr1) ^ "≃^" ^ (string_of_vars var_ctx)^ " "
    ^ (string_of_full_expr fexpr2) ^ ":" ^ (string_of_typeML ty)

(* -------- Sequents ---------- *)

type arith_ctx = arith_pred list

type id_sequent = int

let count_id_sequent = ref 0

let fresh_id_sequent () =
  let x = !count_id_sequent in
  count_id_sequent := !count_id_sequent + 1;x

type sequent = { id : id_sequent;
                 ground_var_ctx : var_ctx;
                 alpha : (var_ctx,bool) pmap;
                 arith_ctx : arith_ctx;
                 j : int;
                 k : int;
                 formula : formula
               }

let create_sequent ?(ground_var_ctx=Pmap.empty) j k formula =
  { id = fresh_id_sequent (); ground_var_ctx = ground_var_ctx; alpha = Pmap.empty; arith_ctx = [];
    j = j; k = k; formula = formula }

let update_sequent sequent ground_var_ctx ?(alpha=Pmap.empty) ?(arith_ctx=[])
    ?(j=sequent.j) ?(k=sequent.k) formula =
  { id = fresh_id_sequent (); ground_var_ctx = Pmap.union ground_var_ctx sequent.ground_var_ctx;
    alpha = Pmap.union alpha sequent.alpha; arith_ctx = arith_ctx@sequent.arith_ctx;
    j = j; k = k; formula = formula }
