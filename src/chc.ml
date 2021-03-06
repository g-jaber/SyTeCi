open Pmap
open Syntax
open Logic
open Wts
open Smt

type chc = (state*Syntax.exprML*Logic.arith_pred)

let isPBT = function
  | PBT _ -> true
  | _ -> false

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

(* let rec freshen_heap = function
  | [] -> (SMTEnv.empty,[])
  | (l,_)::heap ->
    let x = fresh_lvar () in
    let (env,newheap) = freshen_heap heap in
    (SMTEnv.add (SMTVar (x,TInt)) env,(l,Var x)::newheap) *)

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
    begin match lookup_pmap x hpost with
      | Some _ -> complete_heap hpre hpost env h
      | None ->
        let y = fresh_lvar () in
        Debug.print_debug ("Completing the heap with " ^ x ^ " -> " ^ y) ;
        complete_heap ((x,Var y)::hpre) ((x,Var y)::hpost)
          (SMTEnv.add (SMTVar (y,TInt)) env) h
    end

module IndexVar = Map.Make(
  struct
    let compare = Syntax.compare_id
    type t = Syntax.id
  end )

let generate_index pmap =
  let rec aux n result = function
    | [] -> result
    | (x,_)::h ->
      Debug.print_debug ("Adding " ^ x ^ " to the indexing");
      let result' = IndexVar.add x n result in
      aux (n+1) result' h
  in aux 0 IndexVar.empty pmap

let indexing_pmap index pmap =
  let n = List.length pmap in
  Debug.print_debug ("Size of index:" ^ (string_of_int (IndexVar.cardinal index))
                     ^ " and size of heap:" ^ (string_of_int n));
  let tab = Array.make n None in
  let rec aux = function
    | [] -> ()
    | (x,v)::pmap ->
      Debug.print_debug ("Looking for " ^ x ^ " in the indexing");
      let i =
        try IndexVar.find x index with
          Not_found ->
          Error.fail_error ("One of the program is allocating a reference"
          ^ " inside the body of a function. The generation of the"
          ^ " Constrained Horn Clause doo not handle this case.")
      in
      tab.(i) <- Some v;
      aux pmap
  in
  let rec aux2 i =
    if i < n then begin
      match tab.(i) with
      | None -> 
        Error.fail_error ("Error: Trying to access the index " ^ (string_of_int i)
        ^ " which does not exists. Please report.")
      | Some v -> v::(aux2 (i+1))
    end else [] in
  aux pmap; aux2 0

let generate_heap_from_index index =
  let domain = List.map fst (IndexVar.bindings index) in
  let rec aux = function
    | [] -> (SMTEnv.empty,[])
    | l::dom ->
      let x = fresh_lvar () in
      let (env,newheap) = aux dom in
      (SMTEnv.add (SMTVar (x,TInt)) env,(l,Var x)::newheap) in
  aux domain


let predstring_of_state s = "P"^(string_of_state s)

let apply_pred_rel index1 index2 b heapPre1 heapPre2 heapPost1 heapPost2 ?gsubst s =
   let p = predstring_of_state s in
   let l1 = indexing_pmap index1 heapPre1 in
   let l2 = indexing_pmap index2 heapPre2 in
   let l3 = indexing_pmap index1 heapPost1 in
   let l4 = indexing_pmap index2 heapPost2 in
   match gsubst with
   | None -> ARel (p,b::l1@l2@l3@l4)
   | Some gsubst -> ARel (p,b::gsubst@l1@l2@l3@l4)

let generate_env_from_ctx ctx =
  let ctx' = List.filter (fun (_,ty) -> ty = TInt) ctx in
  let env_as_list = List.map (fun x -> (SMTVar x)) ctx' in
  SMTEnv.of_list env_as_list

let generate_pred_from_trans index1 index2 b isincons h1 h2 h1' h2' lnextstate
    (heapPre1,heapPre2,heapPost1,heapPost2,preds,var_ctx,polarity) =
  let (heapPre1,heapPost1,envh1) = complete_heap heapPre1 heapPost1
      SMTEnv.empty h1 in
  let (heapPre2,heapPost2,envh2) = complete_heap heapPre2 heapPost2
      SMTEnv.empty h2 in
  let predPre1 = equate_symb_heap h1 heapPre1 in
  let predPre2 = equate_symb_heap h2 heapPre2 in
  let predPost1 = equate_symb_heap h1' heapPost1 in
  let predPost2 = equate_symb_heap h2' heapPost2 in
  let b' = match polarity with
    | PQ -> Bool true
    | _ -> b
  in
  let nextpred = List.map
      (apply_pred_rel index1 index2 b' heapPost1 heapPost2 h1' h2')
      lnextstate in
  let maycons = match (polarity,isincons) with
    | (_,true) -> [AEqual (b,(Bool false))]
    | (PA,false) -> [AEqual (b,(Bool true))]
    | (PQ,false) | (PI,false) -> []
  in
  let predPost = simplify_arith_pred
      (AOr ((AAnd (maycons@predPost1@predPost2))::nextpred)) in
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

let rec visit_sr sr index1 index2 s b gsubst h1 h2 hfin1 hfin2 =
  let ltrans = get_ltrans sr.trans_fun s in
  let (loetrans,ltrans) = List.partition isOET ltrans in
  let loestate = List.map get_state_of_trans loetrans in
  let loestate = List.filter (goodOET sr) loestate in
  (* let (lpbtrans,ltrans) = List.partition isPBT ltrans in *)
  let loqstate = if List.exists isOQ ltrans then [s] else [] in
  let (lchc,lcurrent,lenv) = split3
      (List.map (visit_sr_aux sr index1 index2 s b gsubst
                   h1 h2 hfin1 hfin2) ltrans) in
  let lchc = List.flatten lchc in
  let lcurrent = remove_none lcurrent in
  let current = match lcurrent with
    | [] -> ATrue
    | [pred] -> simplify_arith_pred pred
    | _ -> simplify_arith_pred (AOr lcurrent)
  in
  let env = union_list lenv in
  (lchc,current,loqstate@loestate,[],env)

and visit_sr_aux sr index1 index2 s b gsubst h1 h2 hfin1 hfin2 = function
  | PT (s',((_,_,_,_,_,_,PI) as label)) ->
    Debug.print_debug ("Dealing with PI:" ^ (string_of_state s) ^ " to " ^ (string_of_state s'));
    let (envh1,h1') = generate_heap_from_index index1 in
    let (envh2,h2') = generate_heap_from_index index2 in
    let (lchc,current,_,_,env) = visit_sr sr index1 index2 s' b gsubst
        h1' h2' hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 b
        (isIncons sr s') h1 h2 h1' h2' [] label in
    let pred = simplify_arith_pred (AAnd (current::hpred)) in
    Debug.print_debug ("Predicate:" ^ string_of_arith_pred pred);
    let env' = union_list [envh1;envh2;envp;env] in
    (lchc,Some pred,env')
  | PT (s',((_,_,_,_,_,_,PQ) as label)) ->
    Debug.print_debug ("Dealing with PQ:" ^ (string_of_state s) ^ " to "
                       ^ (string_of_state s'));
    let (envh1,h1') = generate_heap_from_index index1 in
    let (envh2,h2') = generate_heap_from_index index2 in
    let (lchc,current,lnextstate,_,env) = visit_sr sr index1 index2 s' b gsubst
        h1' h2' hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 b
        (isIncons sr s') h1 h2 h1' h2' lnextstate label in
    let pred = simplify_arith_pred (AAnd (current::hpred)) in
    Debug.print_debug ("Predicate:" ^ string_of_arith_pred pred);
    let env' = union_list [envh1;envh2;envp;env] in
    (lchc,Some pred,env')
  | PT (s',((_,_,_,_,_,_,PA) as label)) ->
    Debug.print_debug ("Dealing with PA:" ^ (string_of_state s) ^ " to "
                       ^ (string_of_state s'));
    (* let incons_pred = if isIncons sr s' then [AEqual (b,(Bool false))] else [] in *)
    let (lchc,_,lnextstate,_,env) = visit_sr sr index1 index2 s' b gsubst
        h1 h2 hfin1 hfin2 in
    let (hpred,envp) = generate_pred_from_trans index1 index2 b (* b is true *)
        (isIncons sr s') h1 h2 hfin1 hfin2 lnextstate label in
    let pred = simplify_arith_pred (AAnd hpred) in
    Debug.print_debug ("Predicate:" ^ string_of_arith_pred pred);
    let env' = SMTEnv.union envp env in
    (lchc,Some pred,env')
  | PET (s',gsubst') ->
    Debug.print_debug ("Dealing with PET:" ^ (string_of_state s) ^ " to "
                       ^ (string_of_state s'));
    let (envh1,hinit1) = generate_heap_from_index index1 in
    let (envh2,hinit2) = generate_heap_from_index index2 in
    let (envh3,hfin1') = generate_heap_from_index index1 in
    let (envh4,hfin2') = generate_heap_from_index index2 in
    let (lchc,current,_,_,env) = visit_sr sr index1 index2 s' b gsubst
        hinit1 hinit2 hfin1' hfin2' in
    let gsubst_dom = List.map (fun x -> Var x) (dom_of_pmap gsubst') in
    let rel_in = apply_pred_rel index1 index2 b hinit1 hinit2 hfin1' hfin2'
        ~gsubst:gsubst_dom s' in
    let rel_out = apply_pred_rel index1 index2 b h1 h2 hfin1 hfin2
        ~gsubst:(Pmap.codom_of_pmap gsubst') s' in
    let pred = simplify_arith_pred current in
    let chc = (s',rel_in,pred) in
    let n = (List.length gsubst') + 2*(List.length hfin1) + 2*(List.length hfin2) in
    let lty = generate_lty n in
    let rel_decl = SMTRel (predstring_of_state s',TBool::lty) in
    let env' = union_list [envh1;envh2;envh3;envh4;env] in
    let env'' = SMTEnv.add rel_decl env' in
    (chc::lchc,Some rel_out,env'')
  | PBT (s',gsubst') ->
    Debug.print_debug ("Dealing with PBT:" ^ (string_of_state s) ^ " to " ^ (string_of_state s'));
    let rel = apply_pred_rel index1 index2 b h1 h2 hfin1 hfin2
        ~gsubst:(Pmap.codom_of_pmap gsubst') s' in
    ([],Some rel,SMTEnv.empty)
  | OT (s',OA) ->
    Debug.print_debug ("Dealing with OA:" ^ (string_of_state s) ^ " to " ^ (string_of_state s'));
    let (lchc,current,_,_,env) = visit_sr sr index1 index2 s' b gsubst
        h1 h2 hfin1 hfin2 in
    let pred = simplify_arith_pred current in
    (lchc,Some pred,env)
  | OT (s',OI)  | OT (s',OQ) ->
    Debug.print_debug ("Dealing with OQ:" ^ (string_of_state s) ^ " to " ^ (string_of_state s'));
    let (envh1,hinit1) = generate_heap_from_index index1 in
    let (envh2,hinit2) = generate_heap_from_index index2 in
    let (envh3,hfin1') = generate_heap_from_index index1 in
    let (envh4,hfin2') = generate_heap_from_index index2 in
    let (lchc,current,_,_,env) = visit_sr sr index1 index2 s' b gsubst
        hinit1 hinit2 hfin1' hfin2' in
    let pred =  current in
    let rel = apply_pred_rel index1 index2 b hinit1 hinit2 hfin1' hfin2' s in
    let chc = (s,rel,pred) in
    let n = 2*(List.length hfin1') + 2*(List.length hfin2') in
    let lty = generate_lty n in
    let rel_decl = SMTRel (predstring_of_state s,TBool::lty) in
    let env' = union_list [envh1;envh2;envh3;envh4;env] in
    let env'' = SMTEnv.add rel_decl env' in
    (chc::lchc,None,env'')
  | OET _ -> ([],None,SMTEnv.empty)

let extract_init_trans sr =
  match get_ltrans sr.trans_fun sr.init_state with
  | [(OT (s,OI) as trans)] ->
    begin match get_ltrans sr.trans_fun s with
      | PT (_,([],[],h1,h2,_,_,_))::_ -> (h1,h2,trans)
      | _ ->  Error.fail_error "Error in the generation of the CHC:
                        The OQ initial transition is not followed by a Player transition."
    end
    | [] -> Error.fail_error "Error in the generation of the CHC: Empty STS. Please report."
    | _ -> Error.fail_error "Error in the generation of the CHC:
                     Too many initial transitions. Please report."

let visit_sr_full sr =
  let (h1,h2,trans) = extract_init_trans sr in
  let index1 = generate_index h1 in
  let index2 = generate_index h2 in
  let idb = fresh_bvar () in
  let b = Var idb in
  let (lchc,_,env) = visit_sr_aux sr index1 index2 sr.init_state b
      [] [] [] [] [] trans in
  let init_rel_name = "P" in
  let init_rel_decl = SMTRel (init_rel_name,[]) in
  let (env_hinit1,hinit1) = generate_heap_from_index index1 in
  let (env_hinit2,hinit2) = generate_heap_from_index index2 in
  let (env_hfin1,hfin1) = generate_heap_from_index index1 in
  let (env_hfin2,hfin2) = generate_heap_from_index index2 in
  let init_rel = ARel (init_rel_name,[]) in
  let next_rel = apply_pred_rel index1 index2 (Bool false)
      hinit1 hinit2 hfin1 hfin2 sr.init_state in
  let init_chc = (-1,init_rel,next_rel) in
  let env = union_list [env_hinit1; env_hinit2; env_hfin1; env_hfin2; env] in
  let env = SMTEnv.add (SMTVar (idb,TBool)) env in
  let env = SMTEnv.add init_rel_decl env in
  (init_chc::lchc,init_rel_name,env)



let chc_to_string_smtlib (_,rel,preds) =
  "(rule (=> " ^ string_of_arith_pred_smtlib (simplify_arith_pred preds) ^ " "
  ^ (string_of_arith_pred_smtlib rel) ^ "))"

let print_lchc_smtlib file lchc =
  List.iter (fun chc -> Printer.print_to_file file (chc_to_string_smtlib chc))
    lchc

let chc_to_string (_,rel,preds) =
  (string_of_arith_pred rel) ^ "<-" ^ string_of_arith_pred
    (simplify_arith_pred preds)

let print_lchc file lchc =
  List.iter (fun chc -> Printer.print_to_file file (chc_to_string chc)) lchc

let print_full_chc file (lchc,init_rel,env) =
   (* Printer.print_to_file file ("Constrained Horn Clause:"); *)
   print_smt_env file env;
   print_lchc_smtlib file lchc;
   Printer.print_to_file file ("(query "^ init_rel ^ ")")
