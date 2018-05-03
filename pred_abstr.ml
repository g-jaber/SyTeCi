open Pmap
open Syntax
open Logic
open Wts
open Pushdown_system


let rec equal_symb_heap h = function
  | [] -> []
  | (l,t)::h' ->
    begin match lookup_pmap l h with
      | Some t' -> (AEqual (t,t'))::(equal_symb_heap h h')
      | None -> equal_symb_heap h h'
    end  
  
let predicate_from_label (heap1,heap2,preds,inv_preds,env) (heap1',heap2',preds',inv_preds',env') (heapPre1,heapPre2,heapPost1,heapPost2,preds'',var_ctx,_) =
  let eqhpre1 = equal_symb_heap heap1 heapPre1 in
  let eqhpre2 = equal_symb_heap heap2 heapPre2 in
  let eqhpost1 = equal_symb_heap heap1' heapPost1 in
  let eqhpost2 = equal_symb_heap heap2' heapPost2 in
  (inv_preds::inv_preds'::preds::preds'::preds''::(eqhpre1@eqhpre2@eqhpost1@eqhpost2),env@env'@var_ctx)

let polarity_from_label (_,_,_,_,_,_,polarity) = polarity  
  
  
let may_negate_inv ((heap1,heap2,preds,inv_preds,env) as inv) = function
  | true -> inv
  | false -> (heap1,heap2,preds,negate_arith_pred inv_preds,env)
  
let rec select_preds pred_env pred_val pred_val' = match (pred_env,pred_val,pred_val') with
  | ([],[],[]) -> []
  | ((inv,inv')::pred_env,b::pred_val,b'::pred_val') ->
    let inv1 = may_negate_inv inv b in
    let inv2 = may_negate_inv inv' b' in
    (inv1,inv2)::(select_preds pred_env pred_val pred_val')
  | _ -> failwith "Error selecting the predicates to generate transitions. Please report."
  
let generate_transition_aux pred_env label (s,pred_val) (s',pred_val') =
  let pair_preds = select_preds pred_env pred_val pred_val' in
  List.map (fun (pinv,pinv') -> ((s,pred_val),(s',pred_val'),predicate_from_label pinv pinv' label,polarity_from_label label)) pair_preds

let rec generate_pred_vals = function
  | 0 -> []
  | 1 -> [[true];[false]]
  | n -> let l = generate_pred_vals (n-1) in 
         let lt = List.map (fun pred_val -> (true::pred_val)) l in
         let lf = List.map (fun pred_val -> (false::pred_val)) l in
         lt@lf
  
let generate_transition pred_env (s1,s2,label) = 
   let n = List.length pred_env in
   let l = generate_pred_vals n in  
   let ls1 = List.map (fun pred_val -> (s1,pred_val)) l in
   let ls2 = List.map (fun pred_val -> (s2,pred_val)) l in
   List.concat (List.concat (List.map (fun ps1 -> (List.map (fun ps2 -> generate_transition_aux pred_env label ps1 ps2)) ls2) ls1))


   
let get_invariant_from_wts sr =
  let pred_env = List.map (fun (h1,h2,preds1,preds2,env) -> let (h1',h2',preds1',preds2',env') = freshen_inv (h1,h2,preds1,preds2) in ((h1,h2,preds1,preds2,env),(h1',h2',preds1',preds2',env'))) sr.invariants in
  let transitions = List.concat (List.map (generate_transition pred_env) sr.pintern_transitions) in
  let transitions = List.filter (fun (s1,s2,(preds,env),_) -> (Logic_to_smt.check_sat env preds)) transitions in
  List.iter (fun (s1,s2,(preds,env),_) -> Debug.print_debug ((string_of_pstate s1) ^ "," ^ (string_of_pstate s2) ^ ": " ^ (string_of_arith_pred (AAnd preds)))) transitions
  (*List.iter (fun (s1,s2,(preds,env)) -> Debug.print_debug ((string_of_pstate s1) ^ "," ^ (string_of_pstate s2) ^ ": " ^ (string_of_bool (Logic_to_smt.check_sat env preds)))) transitions;*)

let wts_to_ps sr =
(*  let n = List.length sr.invariants in
  let pred_vals = generate_pred_vals n in
  let init_states = List.map (fun pred_val -> (sr.init_state,pred_val)) pred_vals in
  let ps = { init_pstates = init_states; incons_pstates = sr.incons_states; transitions = PStateMap.empty } in*)
  let aux (h1,h2,preds1,preds2,env) = 
    let (h1',h2',preds1',preds2',env') = freshen_inv (h1,h2,preds1,preds2) in 
    ((h1,h2,preds1,preds2,env),(h1',h2',preds1',preds2',env')) in
  let pred_env = List.map aux sr.invariants in
  let transitions = List.concat (List.map (generate_transition pred_env) sr.pintern_transitions) in
  let transitions = List.filter (fun (s1,s2,(preds,env),_) -> (Logic_to_smt.check_sat env preds)) transitions in
  let transitions = List.map (fun (s1,s2,_,polarity) -> (s1,s2,polarity)) transitions in
  let mtrans = List.fold_left add_trans PStateMap.empty transitions in
  { pstates = PStates.empty;
    init_pstate = (sr.init_state,List.map (fun  _ -> true) sr.invariants);
    incons_pstates = sr.incons_states;
    transitions = mtrans }
