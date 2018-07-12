open Pmap
open Syntax
open Logic
open Wts

type chc = (state*Syntax.exprML*Logic.arith_pred)

type tree =
  | Empty
  | Node of state * label option * state list * (tree list)

let isNode = function
  | Empty -> false
  | Node _ -> true

let isOET = function
  | OET _ -> true
  | _ -> false

let isPI = function
  | PI -> true
  | _ -> false

let isOQ = function
  | OT (_,OQ) -> true
  | _ -> false

let goodOET sr s =
  let ltrans = get_ltrans sr.trans_fun s in
  List.exists isOQ ltrans

let isIncons sr s = States.mem s sr.incons_states

let rec freshen_heap = function
  | [] -> []
  | (l,_)::heap -> let x = fresh_lvar () in (l,Var x)::freshen_heap heap

let rec equate_symb_heap h = function
  | [] -> []
  | (l,t)::h' ->
    begin match lookup_pmap l h with
      | Some t' -> (AEqual (t,t'))::(equate_symb_heap h h')
      | None -> equate_symb_heap h h'
    end

let predstring_of_state s = "P"^(string_of_state s)

let apply_pred_rel b heapPre1 heapPre2 heapPost1 heapPost2 s =
  let p = predstring_of_state s in
  let l1 = Pmap.codom_of_pmap heapPre1 in
  let l2 = Pmap.codom_of_pmap heapPre2 in
  let l3 = Pmap.codom_of_pmap heapPost1 in
  let l4 = Pmap.codom_of_pmap heapPost2 in
  ARel (p,b::l1@l2@l3@l4)

(* let generate_incons_pred sr s b = AEqual (b,(Bool (isConsis sr s))) *)

let generate_pred_from_trans b isincons h1 h2 h1' h2' lnextstate (heapPre1,heapPre2,heapPost1,heapPost2,preds,_,polarity) =
  let predPre1 = equate_symb_heap h1 heapPre1 in
  let predPre2 = equate_symb_heap h2 heapPre2 in
  let predPost1 = equate_symb_heap h1' heapPost1 in
  let predPost2 = equate_symb_heap h2' heapPost2 in
  let nextpred = List.map (apply_pred_rel b heapPost1 heapPost2 h1' h2') lnextstate in
  let maycons = match (polarity,isincons) with
    | (PQ,_) | (PI,_) -> []
    | (PA,false) -> [AEqual (b,(Bool true))]
    | (PA,true) -> []
  in
  let predPost = AOr ((AAnd (maycons@predPost1@predPost2))::nextpred) in
  (preds::predPre1@predPre2@[predPost])


let rec remove_none = function
  | [] -> []
  | None::l -> remove_none l
  | (Some v)::l -> v::(remove_none l)

let rec visit_sr sr s b hinit1 hinit2 h1 h2 hfin1 hfin2 =
  let ltrans = get_ltrans sr.trans_fun s in
  let (letrans,ltrans) = List.partition isOET ltrans in
  let lestate = List.map get_state_of_trans letrans in
  let lestate = List.filter (goodOET sr) lestate in
  let loqstate = if List.exists isOQ ltrans then [s] else [] in
  let (lchc,ltree) = List.split (List.map (visit_sr_aux sr s b hinit1 hinit2 h1 h2 hfin1 hfin2) ltrans) in
  let lchc = List.flatten lchc in
  let ltree = remove_none ltree in
  (lchc,ltree,loqstate@lestate)

and visit_sr_aux sr s b hinit1 hinit2 h1 h2 hfin1 hfin2 = function
  | PT (s',((_,_,_,_,_,_,PI) as label)) ->
    let h1' = freshen_heap hfin1 in
    let h2' = freshen_heap hfin2 in
    let (lchc,current,lnextstate) = visit_sr sr s' b hinit1 hinit2 h1' h2' hfin1 hfin2 in
    let hpred = generate_pred_from_trans (Bool true) (isIncons sr s') h1 h2 h1' h2' lnextstate label in
    let pred = simplify_arith_pred (AAnd (hpred@[AOr current])) in
    (lchc,Some pred)
  | PT (s',((_,_,_,_,_,_,PQ) as label)) ->
    let h1' = freshen_heap hfin1 in
    let h2' = freshen_heap hfin2 in
    let (lchc,current,lnextstate) = visit_sr sr s' b hinit1 hinit2 h1' h2' hfin1 hfin2 in
    let hpred = generate_pred_from_trans (Bool true) (isIncons sr s') h1 h2 h1' h2' lnextstate label in
    let pred = simplify_arith_pred (AAnd (hpred@[AOr current])) in
    (lchc,Some pred)
  | PT (s',((_,_,_,_,_,_,PA) as label)) ->
    (* let h1' = freshen_heap h1 in
    let h2' = freshen_heap h2 in
    let hfin1' = freshen_heap hfin1 in
       let hfin2' = freshen_heap hfin2 in *)
    let incons_pred = if isIncons sr s' then [AEqual (b,(Bool false))] else [] in
    let (lchc,_,lnextstate) = visit_sr sr s' b hinit1 hinit2 h1 h2 hfin1 hfin2 in
    let hpred = generate_pred_from_trans b (isIncons sr s') h1 h2 hfin1 hfin2 lnextstate label in
    (* let finpred = apply_pred_rel h1 h2 hfin1 hfin2 s' in *)
    let pred = simplify_arith_pred (AAnd (incons_pred@hpred)) in
    (lchc,Some pred)
  (* | PET (s',gsubt) ->
    let (lchc,ltree,lestate) = visit_sr sr s' in
    let tree = Node (s',None,lestate,ltree) in
    (lchc,Some tree) *)
  | OT (s',OA) ->
    let (lchc,current,[]) = visit_sr sr s' b hinit1 hinit2 h1 h2 hfin1 hfin2 in
    (* let tree = Node (s',None,lestate,ltree) in *)
    let pred = simplify_arith_pred (AOr current) in
    (lchc,Some pred)
  | OT (s',OQ) ->
    let hinit1' = freshen_heap hfin1 in
    let hinit2' = freshen_heap hfin2 in
    let hfin1' = freshen_heap hfin1 in
    let hfin2' = freshen_heap hfin2 in
    let (lchc,current,[]) = visit_sr sr s' b hinit1' hinit2' hinit1' hinit2' hfin1' hfin2' in
    let pred = simplify_arith_pred (AOr current) in
    let rel = apply_pred_rel b hinit1' hinit2' hfin1' hfin2' s in
    let chc = (s,rel,pred) in
    (chc::lchc,None)
  | OET _ -> ([],None)

let rec visit_sr_full sr =
  let ltrans = get_ltrans sr.trans_fun sr.init_state in
  match ltrans with
  | [PT (s',([],[],h1,h2,preds,_,PA))] ->
    (* let h1' = freshen_heap h1 in
       let h2' = freshen_heap h2 in *)
    let b = fresh_bvar () in
    let b = Var b in
    let hfin1 = freshen_heap h1 in
    let hfin2 = freshen_heap h2 in
    let (lchc,current,[]) = visit_sr sr sr.init_state b [] [] [] [] hfin1 hfin2 in
    let rel = apply_pred_rel b [] [] hfin1 hfin2 sr.init_state in
    let chc = (sr.init_state,rel,AOr current) in
    chc::lchc
  | [] -> failwith "Error in the generation of the CHC: Empty STS. Please report."
  | _ -> failwith "Error in the generation of the CHC: Too many initial transitions.
       Please report."

let chc_to_string_smtlib (_,rel,preds) =
  "(rule (=> " ^ string_of_arith_pred_smtlib (simplify_arith_pred preds) ^ " " ^ (string_of_arith_pred_smtlib rel) ^ "))"

let print_lchc_smtlib lchc =
  List.iter (fun chc -> print_endline (chc_to_string_smtlib chc)) lchc

let chc_to_string (_,rel,preds) =
  (string_of_arith_pred rel) ^ "<-" ^ string_of_arith_pred (simplify_arith_pred preds)

let print_lchc lchc =
  List.iter (fun chc -> print_endline (chc_to_string chc)) lchc
