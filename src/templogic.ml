open Syntax
open Logic
open Skor
open Tcstruct
open Unif

(* Temporal Logic *)

let get_svar n = "X" ^ (string_of_int n)

type modality =
  | Square
  | SquarePub
  | NextWB of symbheap*symbheap*symbheap*symbheap
  | NextE of symbheap*symbheap*symbheap*symbheap
  | NextI of symbheap*symbheap*symbheap*symbheap


type temp_formula =
  | TPred of arith_pred
  | Mod of modality * temp_formula
  | TAnd of temp_formula list
  | TImpl of (arith_pred list)*temp_formula
  | TForAll of ((id*typeML) list*temp_formula)
  | GFix of id * gsubst * temp_formula
  | SVar of id * gsubst


let modality_from_annot (tag,h1,h2,h3,h4) =
      match tag with
        | Intern -> NextI (h1,h2,h3,h4)
        | Extern -> NextE (h1,h2,h3,h4)
        | WB -> NextWB (h1,h2,h3,h4)
        | Wrong -> failwith "Error: Cannot create a modality from the tag Wrong. Pleae report."

let string_of_modality = function
  | Square -> "▫V"
  | SquarePub -> "▫K"
  | NextE (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○e_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"
  | NextI (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○I_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"
  | NextWB (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○wb_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"

let rec string_of_temp_formula = function
  | TPred pred -> string_of_arith_pred pred
  | Mod (m,formula) -> (string_of_modality m) ^ "(" ^ (string_of_temp_formula formula) ^ ")"
  | TAnd  preds-> string_of_conj  " /\\ " string_of_temp_formula preds
  | TImpl (preds,formula) -> (string_of_conj " /\\ " string_of_arith_pred preds) ^ " => " ^ (string_of_temp_formula formula)
  | TForAll (vars,formula) -> "∀" ^ (string_of_vars vars) ^ ", " ^ (string_of_temp_formula formula)
  | GFix (x,rho, formula) -> "nu " ^ x ^ "(" ^ (string_of_gsubst rho) ^ ")." ^ (string_of_temp_formula formula)
  | SVar (x,rho) -> x ^ "(" ^ (string_of_gsubst rho) ^ ")"


let rec simplify_temp_formula formula = match formula with
  | TPred pred -> TPred (simplify_arith_pred pred)
  | Mod (m,formula) -> Mod (m,simplify_temp_formula formula)
  | TAnd [] -> TPred ATrue
  | TAnd [formula] -> simplify_temp_formula formula
  | TAnd formulas ->
      let formulas' = List.filter (fun x-> x<>TPred ATrue) formulas in
      let formulas'' = List.map simplify_temp_formula formulas' in
      TAnd formulas''
  | TImpl ([],formula) | TImpl ([ATrue],formula) -> simplify_temp_formula formula
  | TImpl ([AFalse],_) -> TPred ATrue
  | TImpl (preds,formula) ->
      let preds' = List.map simplify_arith_pred preds in
      TImpl (preds',simplify_temp_formula formula)
  | TForAll ([],formula) -> simplify_temp_formula formula
  | TForAll (vars,formula) -> TForAll (vars,simplify_temp_formula formula)
  | GFix (x,rho, formula) -> GFix (x,rho, simplify_temp_formula formula)
  | SVar _ -> formula

let rec tformula_of_tc bc = function
  | RuleVG sequent -> TPred (extract_pred_from_vg sequent)
  | Stop _ | RuleSwrong _ -> TPred AFalse (*TPred (negate_arith_pred (AAnd sequent.arith_ctx))*)
  | LOut _ -> if bc then TPred ATrue else TPred AFalse
  | ROut _ -> TPred AFalse
  | RuleSext ((tc1,tc2),_) | RuleVProd ((tc1,tc2),_) ->
    TAnd [(tformula_of_tc bc tc1); (tformula_of_tc bc tc2)]
  | Unfold (tc,_) | LUnfold (tc,_) | RUnfold (tc,_)  | Rewrite (tc,_) ->
    tformula_of_tc bc tc
  | RuleInit (tcs,sequent) ->
    let aux new_tc =
      let (_,vars) = newelem_of_sequents sequent (get_root new_tc) in
      TForAll (vars, tformula_of_tc bc new_tc)
    in TAnd (List.map aux tcs)
  | RuleK (tcs,sequent) ->
    let aux new_tc =
      let (_,vars) = newelem_of_sequents sequent (get_root new_tc) in
      Mod (SquarePub, TForAll (vars, tformula_of_tc bc new_tc))
    in TAnd (List.map aux tcs)
  | RuleV (tcs,sequent) ->
    let aux new_tc =
      let (_,vars) = newelem_of_sequents sequent (get_root new_tc) in
      Mod (Square, TForAll (vars, tformula_of_tc bc new_tc))
    in TAnd (List.map aux tcs)
  | RuleE (tcs,sequent) ->
    let aux (annot,tc') =
      let (preds,vars) = newelem_of_sequents sequent (get_root tc') in
      begin match annot with
        | (Wrong,_,_,_,_) ->
          TForAll (vars, TImpl (preds, tformula_of_tc bc tc'))
        | _ ->
          TForAll (vars, TImpl (preds, Mod (modality_from_annot annot,
                                            tformula_of_tc bc tc')))
      end
    in TAnd (List.map aux tcs)
  | Circ (rho,backsequent,_) ->
    let svar = get_svar backsequent.id in
    SVar (svar, rho)
  | Gen (rho,tc,_) ->
    let svar = get_svar ((get_root tc).id) in
    GFix (svar, rho, tformula_of_tc bc tc)
