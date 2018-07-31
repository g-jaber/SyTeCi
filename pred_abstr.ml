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

let predicate_from_label (heap1,heap2,preds,inv_preds,env)
    (heap1',heap2',preds',inv_preds',env')
    (heapPre1,heapPre2,heapPost1,heapPost2,preds'',var_ctx,_) =
  (*Debug.print_debug ("Looking at invariant "
    ^ (string_of_invariant (heap1,heap2,preds,inv_preds,env)) ^ " and "
    ^  (string_of_invariant (heap1',heap2',preds',inv_preds',env')) ^ ".");*)
  let eqhpre1 = equal_symb_heap heap1 heapPre1 in
  let eqhpre2 = equal_symb_heap heap2 heapPre2 in
  let eqhpost1 = equal_symb_heap heap1' heapPost1 in
  let eqhpost2 = equal_symb_heap heap2' heapPost2 in
  let preds = (inv_preds::inv_preds'::preds''::(eqhpre1@eqhpre2@eqhpost1@eqhpost2)) in
  (*  let preds' = simplify_arith_pred (AAnd preds) in*)
  (*  Debug.print_debug ("Full: "^ (string_of_arith_pred (preds')));*)
  (preds,env@env'@var_ctx)

let polarity_from_label (_,_,_,_,_,_,ppolarity) = ppolarity_to_polarity ppolarity


let may_negate_inv ((heap1,heap2,preds,inv_preds,env) as inv) = function
  | true -> inv
  | false -> (heap1,heap2,preds,negate_arith_pred inv_preds,env)

let rec select_preds pred_env pred_val pred_val' = match (pred_env,pred_val,pred_val') with
  | ([],[],[]) -> []
  | ((inv,inv')::pred_env,b::pred_val,b'::pred_val') ->
    let inv1 = may_negate_inv inv b in
    let inv2 = may_negate_inv inv' b' in
    (inv1,inv2)::(select_preds pred_env pred_val pred_val')
  | ((inv,inv')::pred_env,[],b'::pred_val') ->
    let inv2 = may_negate_inv inv' b' in
    (([],[],ATrue,ATrue,[]),inv2)::(select_preds pred_env pred_val pred_val')
  | _ -> failwith "Error selecting the predicates to generate transitions. Please report."

let generate_ptransition pred_env label (s,pred_val) (s',pred_val') =
  let pair_preds = select_preds pred_env pred_val pred_val' in
  let preds = List.map (fun (pinv,pinv') -> predicate_from_label pinv pinv' label) pair_preds in
  ((s,pred_val),(s',pred_val'),preds,polarity_from_label label)

let rec generate_pred_vals = function
  | 0 -> []
  | 1 -> [[true];[false]]
  | n -> let l = generate_pred_vals (n-1) in
         let lt = List.map (fun pred_val -> (true::pred_val)) l in
         let lf = List.map (fun pred_val -> (false::pred_val)) l in
         lt@lf

let check_trans (ps1,ps2,list_preds,_) =
(*  Debug.print_debug ("Trying to build a transition from " ^ (string_of_pstate ps1) ^ " to " ^ (string_of_pstate ps2) ^ ".");*)
  List.for_all (fun x -> x)
    (List.map (fun (preds,env) -> (Logic_to_z3.check_sat env preds)) list_preds)

let generate_pstate_list s pred_vals =
  List.map (fun pred_val -> (s,pred_val)) pred_vals

let generate_transition pred_env pred_vals ((_,pred_val1) as ps1) = function
  | PT (s2,label) ->
    let ps2_list = generate_pstate_list s2 pred_vals in
    let trans_list = List.map (generate_ptransition pred_env label ps1) ps2_list in
    let trans_list = List.filter check_trans trans_list in
    List.map (fun (ps1,ps2,_,polarity) -> (ps2,polarity)) trans_list
  | OT (s2,polarity) -> [((s2,pred_val1),opolarity_to_polarity polarity)]
  | OET s2 -> [((s2,pred_val1),OE)]
  | PET _ | PBT _ -> failwith "Error: Not supported yet."

let generate_trans_list trans_fun pred_env pred_vals ((s,_) as ps1) =
  let ltrans = Wts.get_ltrans trans_fun s in
  List.flatten (List.map (generate_transition pred_env pred_vals ps1) ltrans)

(*let generate_transition pred_env (s1,s2,label) =
   let n = List.length pred_env in
   let l = generate_pred_vals n in
   let ls1 = List.map (fun pred_val -> (s1,pred_val)) l in
   let ls2 = List.map (fun pred_val -> (s2,pred_val)) l in
   List.concat (List.concat (List.map (fun ps1 -> (List.map (fun ps2 -> generate_transition_aux pred_env label ps1 ps2)) ls2) ls1))*)



(*let get_invariant_from_wts sr =
  let pred_env = List.map (fun (h1,h2,preds1,preds2,env) -> let (h1',h2',preds1',preds2',env') = freshen_inv (h1,h2,preds1,preds2) in ((h1,h2,preds1,preds2,env),(h1',h2',preds1',preds2',env'))) sr.invariants in
  let transitions = List.concat (List.map (generate_transition pred_env) sr.pintern_transitions) in
  let transitions = List.filter (fun (s1,s2,(preds,env),_) -> (Logic_to_smt.check_sat env preds)) transitions in
  List.iter (fun (s1,s2,(preds,env),_) -> Debug.print_debug ((string_of_pstate s1) ^ "," ^ (string_of_pstate s2) ^ ": " ^ (string_of_arith_pred (AAnd preds)))) transitions
  (*List.iter (fun (s1,s2,(preds,env)) -> Debug.print_debug ((string_of_pstate s1) ^ "," ^ (string_of_pstate s2) ^ ": " ^ (string_of_bool (Logic_to_smt.check_sat env preds)))) transitions;*)
*)

let rec visit_sr sr pred_env pred_vals is_visited result = function
  | [] -> (is_visited,result)
  | ps1::ps_list when PStates.mem ps1 is_visited -> visit_sr sr pred_env pred_vals is_visited result ps_list
  | ps1::ps_list ->
    let is_visited = PStates.add ps1 is_visited in
    let trans_list = generate_trans_list sr.trans_fun pred_env pred_vals ps1 in
    let ps_list' = List.map fst trans_list in
    let result = Pushdown_system.add_list_trans result ps1 trans_list in
    visit_sr sr pred_env pred_vals is_visited result (ps_list'@ps_list)

let wts_to_ps sr =
  let n = List.length sr.invariants in
  List.iter (fun inv -> Debug.print_debug ("Invariant: " ^ string_of_invariant inv)) sr.invariants;
  let pred_vals = generate_pred_vals n in
  let aux (h1,h2,preds1,preds2,env) =
    let (h1',h2',preds1',preds2',env') = freshen_inv (h1,h2,preds1,preds2) in
    ((h1,h2,preds1,preds2,env),(h1',h2',preds1',preds2',env'))
  in
  let pred_env = List.map aux sr.invariants in
  let init_pstate = (sr.init_state,[]) in

  let (pstates,ptrans_fun) = visit_sr sr pred_env pred_vals PStates.empty PStateMap.empty [init_pstate] in
  { pstates = pstates; init_pstate = init_pstate; incons_pstates = sr.incons_states; ptrans_fun = ptrans_fun }
