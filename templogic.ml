open Syntax
open Logic
open Skor
open Tcstruct

(* Temporal Logic *) 

type modality =
  | Null
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


let modality_from_annot = function
  | Some (AnnotHeap (tag,h1,h2,h3,h4)) ->
      begin match tag with
        | Intern -> NextI (h1,h2,h3,h4)
        | Extern -> NextE (h1,h2,h3,h4)
        | WB -> NextWB (h1,h2,h3,h4)
        | Wrong -> failwith "Error: Cannot create a modality from the tag Wrong."
      end
  | _  -> failwith "Error: Cannot create a modality from an annotation not coming from RuleE."   
    
let rec string_of_modality = function
  | Null -> ""
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
  | Mod (Null,formula) -> string_of_temp_formula formula
  | Mod (m,formula) -> (string_of_modality m) ^ "(" ^ (string_of_temp_formula formula) ^ ")"
  | TAnd  preds-> string_of_conj  " /\\ " string_of_temp_formula preds 
  | TImpl (preds,formula) -> (string_of_conj " /\\ " string_of_arith_pred preds) ^ " => " ^ (string_of_temp_formula formula)
  | TForAll (vars,formula) -> "∀" ^ (string_of_vars vars) ^ ", " ^ (string_of_temp_formula formula)  

 
let rec simplify_temp_formula = function
  | TPred pred -> TPred (simplify_arith_pred pred)
  | Mod (Null,formula) -> simplify_temp_formula formula  
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
  | p -> p

let rec tformula_of_tc = function
  | RuleVG sequent -> TPred (extract_pred_from_vg sequent)
  | Stop sequent -> TPred (negate_arith_pred (AAnd sequent.arith_ctx))
  | LOut _ -> TPred ATrue
  | ROut _ -> TPred AFalse
  | RuleSext ((tc1,tc2),sequent) | RuleVProd ((tc1,tc2),sequent) -> TAnd [(tformula_of_tc tc1); (tformula_of_tc tc2)]
  | Unfold (tc,sequent) | LUnfold (tc,sequent) | RUnfold (tc,sequent)  | Rewrite (tc,sequent) -> tformula_of_tc tc
  | RuleK (tcs,sequent) -> 
      let aux new_tc =
        let (preds,vars) = newelem_of_sequents sequent (get_root new_tc) in
        Mod (SquarePub, TForAll (vars, tformula_of_tc new_tc))
      in TAnd (List.map aux tcs)  
  | RuleV (tcs,sequent) ->
        let aux new_tc =
        let (preds,vars) = newelem_of_sequents sequent (get_root new_tc) in
        Mod (Square, TForAll (vars, tformula_of_tc new_tc))
      in TAnd (List.map aux tcs)  
  | RuleE (tcs,sequent) ->
        let aux new_tc =
        let (preds,vars) = newelem_of_sequents sequent (get_root new_tc) in
        TForAll (vars, TImpl (preds, Mod (modality_from_annot (get_root new_tc).annot,  tformula_of_tc new_tc)))
        in TAnd (List.map aux tcs)   
  
