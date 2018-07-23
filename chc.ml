open Pmap
open Syntax
open Logic
open Wts

type chc = (state*Syntax.exprML*Logic.arith_pred)

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
  end )

let union_list = List.fold_left (SMTEnv.union) SMTEnv.empty

let rec generate_lty = function
  | 0 -> []
  | n -> TInt::(generate_lty (n-1))

let smt_decl_to_string = function
  | SMTVar (x,ty) -> "(declare-var " ^ x ^ " " ^ (string_of_typeML ty) ^ ")"
  | SMTRel (x,lty) ->
    let string_lty = String.concat " " (List.map string_of_typeML lty) in
    "(declare-rel " ^ x ^ " (" ^ string_lty ^ "))"

let print_smt_env env =
  SMTEnv.iter (fun x -> print_endline (smt_decl_to_string x)) env

type tree =
  | Empty
  | Node of state * label option * state list * (tree list)

let isOET = function
  | OET _ -> true
  | _ -> false

let isOQ = function
  | OT (_,OQ) -> true
  | _ -> false

let goodOET sr s =
  let ltrans = get_ltrans sr.trans_fun s in
  List.exists isOQ ltrans

let isIncons sr s = States.mem s sr.incons_states

let rec freshen_heap = function
  | [] -> (SMTEnv.empty,[])
  | (l,_)::heap ->
    let x = fresh_lvar () in
    let (env,newheap) = freshen_heap heap in
    (SMTEnv.add (SMTVar (x,TInt)) env,(l,Var x)::newheap)

let rec equate_symb_heap h = function
  | [] -> []
  | (l,t)::h' ->
    begin match lookup_pmap l h with
      | Some t' -> (AEqual (t,t'))::(equate_symb_heap h h')
      | None -> equate_symb_heap h h'
    end

let rec complete_heap hpre hpost env = function
  (* We suppose that hpre and hpost have the same domain , which is smaller
     or equal to the one of h *)
  | [] -> (hpre,hpost,env)
  | (x,_)::h ->
    begin match lookup_pmap x hpre with
      | Some _ -> complete_heap hpre hpost env h
      | None ->
        let y = fresh_lvar () in
        complete_heap ((x,Var y)::hpre) ((x,Var y)::hpost)
          (SMTEnv.add (SMTVar (y,TInt)) env) h
    end

module IndexLoc = Map.Make(
  struct
    let compare = Syntax.compare_id
    type t = Syntax.id
  end )

let generate_index_loc heap =
  let rec aux n result = function
    | [] -> result
    | (x,v)::h ->
      Debug.print_debug ("Adding " ^ x ^ " to the indexing");
      let result' = IndexLoc.add x n result in
      aux (n+1) result' h
  in aux 0 IndexLoc.empty heap

let predstring_of_state s = "P"^(string_of_state s)

let indexing_heap index heap =
  let n = List.length heap in
  let tab = Array.make n (Var "plop") in
  let rec aux = function
    | [] -> ()
    | (x,v)::heap ->
      Debug.print_debug ("Looking for " ^ x ^ " in the indexing");
      let i = IndexLoc.find x index in
      tab.(i) <- v;
      aux heap
in aux heap; Array.to_list tab

(* let apply_pred_rel' index1 index2 b heapPre1 heapPre2 heapPost1 heapPost2 s =
  let p = predstring_of_state s in
  let l1 = Pmap.codom_of_pmap heapPre1 in
  let l2 = Pmap.codom_of_pmap heapPre2 in
  let l3 = Pmap.codom_of_pmap heapPost1 in
  let l4 = Pmap.codom_of_pmap heapPost2 in
  ARel (p,b::l1@l2@l3@l4) *)

let apply_pred_rel index1 index2 b heapPre1 heapPre2 heapPost1 heapPost2 s =
  let p = predstring_of_state s in
  let l1 = indexing_heap index1 heapPre1 in
  let l2 = indexing_heap index2 heapPre2 in
  let l3 = indexing_heap index1 heapPost1 in
  let l4 = indexing_heap index2 heapPost2 in
  ARel (p,b::l1@l2@l3@l4)

(* let generate_incons_pred sr s b = AEqual (b,(Bool (isConsis sr s))) *)

let generate_env_from_ctx ctx =
  let ctx' = List.filter (fun (x,ty) -> ty = TInt) ctx in
  let env_as_list = List.map (fun x -> (SMTVar x)) ctx' in
  SMTEnv.of_list env_as_list

let generate_pred_from_trans index1 index2 b isincons h1 h2 h1' h2' lnextstate (heapPre1,heapPre2,heapPost1,heapPost2,preds,var_ctx,polarity) =
  let (heapPre1,heapPost1,envh1) = complete_heap heapPre1 heapPost1 SMTEnv.empty h1 in
  let (heapPre2,heapPost2,envh2) = complete_heap heapPre2 heapPost2 SMTEnv.empty h2 in
  let predPre1 = equate_symb_heap h1 heapPre1 in
  let predPre2 = equate_symb_heap h2 heapPre2 in
  let predPost1 = equate_symb_heap h1' heapPost1 in
  let predPost2 = equate_symb_heap h2' heapPost2 in
  let nextpred = List.map (apply_pred_rel index1 index2 b heapPost1 heapPost2 h1' h2') lnextstate in
  let maycons = match (polarity,isincons) with
    | (PQ,_) | (PI,_) -> []
    | (PA,false) -> [AEqual (b,(Bool true))]
    | (PA,true) -> []
  in
  let predPost = AOr ((AAnd (maycons@predPost1@predPost2))::nextpred) in
  let envc = generate_env_from_ctx var_ctx in
  let env = union_list [envh1; envh2;envc] in
  (preds::predPre1@predPre2@[predPost],env)


let rec remove_none = function
  | [] -> []
  | None::l -> remove_none l
  | (Some v)::l -> v::(remove_none l)

let rec split3 = function
  | [] -> ([],[],[])
  | (a,b,c)::tl ->
    let (al,bl,cl) = split3 tl in
    (a::al,b::bl,c::cl)

let rec visit_sr sr index1 index2 s b hinit1 hinit2 h1 h2 hfin1 hfin2 =
  let ltrans = get_ltrans sr.trans_fun s in
  let (letrans,ltrans) = List.partition isOET ltrans in
  let lestate = List.map get_state_of_trans letrans in
  let lestate = List.filter (goodOET sr) lestate in
  let loqstate = if List.exists isOQ ltrans then [s] else [] in
  let (lchc,ltree,lenv) = split3 (List.map (visit_sr_aux sr index1 index2 s b hinit1 hinit2 h1 h2 hfin1 hfin2) ltrans) in
  let lchc = List.flatten lchc in
  let ltree = remove_none ltree in
  let env = union_list lenv in
  (lchc,ltree,loqstate@lestate,env)

and visit_sr_aux sr index1 index2 s b hinit1 hinit2 h1 h2 hfin1 hfin2 = function
  | PT (s',((_,_,_,_,_,_,PI) as label)) ->
    let (envh1,h1') = freshen_heap hfin1 in
    let (envh2,h2') = freshen_heap hfin2 in
    let (lchc,current,lnextstate,env) = visit_sr sr index1 index2 s' b hinit1 hinit2 h1' h2' hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 (Bool true) (isIncons sr s') h1 h2 h1' h2' lnextstate label in
    let pred = simplify_arith_pred (AAnd (hpred@[AOr current])) in
    let env' = union_list [envh1;envh2;envp;env] in
    (lchc,Some pred,env')
  | PT (s',((_,_,_,_,_,_,PQ) as label)) ->
    let (envh1,h1') = freshen_heap hfin1 in
    let (envh2,h2') = freshen_heap hfin2 in
    let (lchc,current,lnextstate,env) = visit_sr sr index1 index2 s' b hinit1 hinit2 h1' h2' hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 (Bool true) (isIncons sr s') h1 h2 h1' h2' lnextstate label in
    let pred = simplify_arith_pred (AAnd (hpred@[AOr current])) in
    let env' = union_list [envh1;envh2;envp;env] in
    (lchc,Some pred,env')
  | PT (s',((_,_,_,_,_,_,PA) as label)) ->
    let incons_pred = if isIncons sr s' then [AEqual (b,(Bool false))] else [] in
    let (lchc,_,lnextstate,env) = visit_sr sr index1 index2 s' b hinit1 hinit2 h1 h2 hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 b (isIncons sr s') h1 h2 hfin1 hfin2 lnextstate label in
    let pred = simplify_arith_pred (AAnd (incons_pred@hpred)) in
    let env' = SMTEnv.union envp env in
    (lchc,Some pred,env')
  (* | PET (s',gsubt) ->
    let (envh1,hinit1') = freshen_heap hfin1 in
    let (envh2,hinit2') = freshen_heap hfin2 in
    let (envh3,hfin1') = freshen_heap hfin1 in
    let (envh4,hfin2') = freshen_heap hfin2 in
    let (lchc,current,_,env) = visit_sr sr index1 index2 s' b hinit1' hinit2' hinit1' hinit2' hfin1' hfin2' in
    let pred = simplify_arith_pred (AOr current) in
    let rel = apply_pred_rel index1 index2 b hinit1' hinit2' hfin1' hfin2' s in
    let chc = (s,rel,pred) in *)
  | OT (s',OA) ->
    let (lchc,current,_,env) = visit_sr sr index1 index2 s' b hinit1 hinit2 h1 h2 hfin1 hfin2 in
    let pred = simplify_arith_pred (AOr current) in
    (lchc,Some pred,env)
  | OT (s',OQ) ->
    let (envh1,hinit1') = freshen_heap hfin1 in
    let (envh2,hinit2') = freshen_heap hfin2 in
    let (envh3,hfin1') = freshen_heap hfin1 in
    let (envh4,hfin2') = freshen_heap hfin2 in
    let (lchc,current,_,env) = visit_sr sr index1 index2 s' b hinit1' hinit2' hinit1' hinit2' hfin1' hfin2' in
    let pred = simplify_arith_pred (AOr current) in
    let rel = apply_pred_rel index1 index2 b hinit1' hinit2' hfin1' hfin2' s in
    let chc = (s,rel,pred) in
    let n = 2*(List.length hfin1) + 2*(List.length hfin2) in
    let lty = generate_lty n in
    let rel_decl = SMTRel (predstring_of_state s,TBool::lty) in
    let env' = union_list [envh1;envh2;envh3;envh4;env] in
    let env'' = SMTEnv.add rel_decl env' in
    (chc::lchc,None,env'')
  | OET _ -> ([],None,SMTEnv.empty)
  | _ -> failwith ("Error: Wrong Transition. Please report.")

let rec visit_sr_full sr =
  let ltrans = get_ltrans sr.trans_fun sr.init_state in
  match ltrans with
  | [PT (s',([],[],h1,h2,preds,_,PA))] ->
    let index1 = generate_index_loc h1 in
    let index2 = generate_index_loc h2 in
    let idb = fresh_bvar () in
    let b = Var idb in
    let (env_hfin1,hfin1) = freshen_heap h1 in
    let (env_hfin2,hfin2) = freshen_heap h2 in
    let (lchc,current,_,env) = visit_sr sr index1 index2 sr.init_state b [] [] [] [] hfin1 hfin2 in
    let rel = apply_pred_rel index1 index2 b [] [] hfin1 hfin2 sr.init_state in
    let chc = (sr.init_state,rel,AOr current) in
    let n = (List.length hfin1) + (List.length hfin2) in
    let lty = generate_lty n in
    let rel_decl = SMTRel (predstring_of_state sr.init_state,TBool::lty) in
    let init_rel_decl = SMTRel ("P",lty) in
    let (env_hinit1,hinit1) = freshen_heap h1 in
    let (env_hinit2,hinit2) = freshen_heap h2 in
    let l1 = indexing_heap index1 hinit1 in
    let l2 = indexing_heap index2 hinit2 in
    let init_rel = ARel ("P",l1@l2) in
    let next_rel = apply_pred_rel index1 index2 (Bool false) [] [] hinit1 hinit2 sr.init_state in
    let init_chc = (-1,init_rel,next_rel) in
    let env' = union_list [env_hinit1; env_hinit2; env_hfin1; env_hfin2; env] in
    let env' = SMTEnv.add (SMTVar (idb,TBool)) env' in
    let env' = SMTEnv.add init_rel_decl env' in
    let env' = SMTEnv.add rel_decl env' in
    (init_chc::chc::lchc,env')
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

 let print_full_chc (lchc,env) =
   print_smt_env env;
   print_lchc_smtlib lchc;
   print_endline "(query P :print-certificate true)"
