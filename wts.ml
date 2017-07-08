open Tcstruct
open Logic
open Pmap
open Syntax

type label = (symbheap*symbheap*symbheap*symbheap*Logic.arith_pred)

let string_of_label (heapPre1,heapPre2,heapPost1,heapPost2,pred) =
    "[" ^ (string_of_symb_heap heapPre1) ^ "],[" ^(string_of_symb_heap heapPre2) ^ "],[" ^(string_of_symb_heap heapPost1) ^ "],[" 
      ^ (string_of_symb_heap heapPost2) ^ "]," ^ (string_of_arith_pred pred)

(*let simplify_label (heapPre1,heapPre2,heapPost1,heapPost2,pred) = (heapPre1,heapPre2,heapPost1,heapPost2,simplify_arith_pred pred) *)
      
type atom_state = int

module AStates = Set.Make( 
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)
    type t = atom_state
  end )

let count_atom_state = ref 0
let available_states = ref AStates.empty
let fresh_atom_state () = 
  match AStates.is_empty !available_states with
    | true -> let a = !count_atom_state in
              count_atom_state := !count_atom_state + 1;
              a
    | false -> let a = AStates.min_elt !available_states in
               available_states := AStates.remove a !available_states;
               a
 

type state = (atom_state*((typeML pmap)*arith_pred))

let compare_state (s1,(env1,_)) (s2,(env2,_)) =(*
  print_endline ("Comparing "^ (string_of_state (s1,env1,pred1)) ^ " and " ^ (string_of_state (s2,env2,pred2)));*)
  (s1 = s2) && (env1 = env2)

module States = Set.Make( 
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)
    type t = state
  end )  
 
let get_state states a = 
  let state = States.elements (States.filter (fun (a',_) -> a = a') states) in
  match state with
    | [] -> failwith "Element not found !"
    | [s] -> s 
    | _ -> failwith "Duplicated elements !"
 
let fresh_state () =
  let state = fresh_atom_state () in
  (state,(empty_pmap,ATrue))

let free_state (a,_) =
  available_states := AStates.add a !available_states
  
let extend_env_state (vars1,pred1) (s,(vars2,pred2)) = (s,(vars1@vars2,iter 2 simplify_arith_pred (AAnd [pred1;pred2])))  

let string_of_state (s,env) = match env with
  | ([],ATrue) -> string_of_int s
  | (vars,ATrue) -> "(" ^ (string_of_int s) ^ "," ^ (string_of_pmap ":" string_of_typeML vars) ^ ")"
  | (vars,pred) -> "(" ^ (string_of_int s) ^ "," ^ (string_of_pmap ":" string_of_typeML vars) ^ "," ^ (string_of_arith_pred pred) ^ ")"

type sr = { 
    mutable states : States.t;
    mutable init_state : state;
    mutable incons_states : States.t;    
    mutable pintern_transitions : (state*state*label) list;
    mutable extern_transitions : (state*state*label) list;
    mutable wb_transitions : (state*state) list;        
    mutable peps_transitions : (state*state) list;
    mutable o_transitions : (state*state) list;     
    mutable oeps_transitions : (state*state) list;    
  }

let extend_env_list env l = 
  List.map (extend_env_state env) l

let extend_env_states env states = 
  List.fold_right States.add (extend_env_list env (States.elements states))  States.empty   
  
type esr = sr*States.t*States.t*States.t*arith_pred  
  
let extend_env_esr env (sr,isVal,isSquare,isSquareP,pred) =
  sr.states <- extend_env_states env sr.states;
  sr.init_state <- extend_env_state env sr.init_state;
  sr.incons_states <- extend_env_states env sr.incons_states;
  sr.pintern_transitions <- List.map (fun (s,s',label) -> (extend_env_state env s,extend_env_state env s',label)) sr.pintern_transitions;  
  sr.extern_transitions <- List.map (fun (s,s',label) -> (extend_env_state env s,extend_env_state env s',label)) sr.extern_transitions;
  sr.wb_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.wb_transitions;  
  sr.peps_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.peps_transitions;
  sr.o_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.peps_transitions;
  sr.oeps_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.peps_transitions;      
  let isVal' = extend_env_states env isVal in
  let isSquare' = extend_env_states env isSquare in  
  let isSquareP' = extend_env_states env isSquareP in
  (sr,isVal',isSquare',isSquareP',pred)
                 
let string_of_transition (s1,s2,label) =
  (string_of_state s1) ^ "-(" ^ (string_of_label label) ^")->" ^ (string_of_state s2)

let string_of_atomic_transition (s1,s2) =
  (string_of_state s1) ^ "-->" ^ (string_of_state s2)  
  
let print_sr sr =
  print_endline ("States: ");
  States.iter (fun x -> print_string ((string_of_state x) ^ " ")) sr.states;
  print_newline ();
  print_endline ("Initial State: " ^ (string_of_state sr.init_state));    
  print_endline ("Inconsistent States: ");
  States.iter (fun x -> print_string ((string_of_state x) ^ " ")) sr.incons_states;
  print_newline ();      
  print_endline ("P-Internal Transitions: ");
  List.iter (fun x -> print_endline (string_of_transition x)) sr.pintern_transitions;
  print_newline ();
  print_endline ("External Transitions: ");
  List.iter (fun x -> print_endline (string_of_transition x)) sr.extern_transitions;
  print_newline ();
  print_endline ("WB Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.wb_transitions;  
  print_newline ();      
  print_endline ("P-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.peps_transitions;
  print_newline ();
  print_endline ("O Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.o_transitions;
  print_newline ();
  print_endline ("O-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.oeps_transitions;
  print_newline ()         
  
let states_swap old_state new_state states =
  let (states1,states2) = States.partition (fun z -> compare_state z old_state) states in
  if (States.is_empty states1) then states2 else (States.add new_state states2)
  
let list_swap old_state new_state states =
  let (states1,states2) = List.partition (fun z -> compare_state z old_state) states in
  if (states1 = []) then states2 else  new_state::states2
  
let singleton_sr () = 
  let state = fresh_state () in
  print_endline ("Adding new state " ^(string_of_state state));
  { states = States.singleton state; 
    init_state = state;
    incons_states = States.empty;
    pintern_transitions = []; 
    extern_transitions = [];
    wb_transitions = [];
    peps_transitions = [];     
    o_transitions = [];
    oeps_transitions = [] 
 }

let substitute_state_atom_trans old_state new_state (s1,s2) = (*
  print_endline ("Merging in the transition" ^ (string_of_state s1) ^ " -> " ^ (string_of_state s2));  *)
  let s1' = (if compare_state s1 old_state then new_state else s1) in
  let s2' = (if compare_state s2 old_state then new_state else s2) in
  (s1',s2')   
   
let substitute_state_trans old_state new_state (s1,s2,label) = 
  let (s1',s2') = substitute_state_atom_trans old_state new_state (s1,s2) in
  (s1',s2',label)
  
let substitute_state_esr state (sr,isVal,isSquare,isSquareP,pred) =
  let old_init = sr.init_state in
  let isVal' = states_swap old_init state isVal in  
  let isSquare' = states_swap old_init state isSquare in  
  let isSquareP' = states_swap old_init state isSquareP in    
  let sr' = 
       { states = States.add state (States.remove old_init sr.states); 
         init_state = state;
         incons_states = states_swap old_init state sr.incons_states;
         pintern_transitions = List.map (substitute_state_trans old_init state) sr.pintern_transitions;
         extern_transitions = List.map (substitute_state_trans old_init state) sr.extern_transitions;
         wb_transitions = List.map (substitute_state_atom_trans old_init state) sr.wb_transitions;         
         peps_transitions = List.map (substitute_state_atom_trans old_init state) sr.peps_transitions;
         o_transitions = List.map (substitute_state_atom_trans old_init state) sr.o_transitions;          
         oeps_transitions = List.map (substitute_state_atom_trans old_init state) sr.oeps_transitions }
  in free_state old_init;
     (sr',isVal',isSquare',isSquareP',pred)       

let basic_union init_state (sr1,isVal1,isSquare1,isSquareP1,pred1) (sr2,isVal2,isSquare2,isSquareP2,pred2) =
  let sr = 
    { states = States.union sr1.states sr2.states; 
      init_state = init_state;
      incons_states = States.union  sr1.incons_states sr2.incons_states;       
      pintern_transitions = sr1.pintern_transitions@sr2.pintern_transitions;
      extern_transitions = sr1.extern_transitions@sr2.extern_transitions;
      wb_transitions = sr1.wb_transitions@sr2.wb_transitions;
      peps_transitions = sr1.peps_transitions@sr2.peps_transitions;
      o_transitions = sr1.o_transitions@sr2.o_transitions;
      oeps_transitions = sr1.oeps_transitions@sr2.oeps_transitions} in
  (sr,States.union isVal1 isVal2,States.union isSquare1 isSquare2,States.union isSquareP1 isSquareP2,iter 2 simplify_arith_pred (AAnd [pred1;pred2]))      
  
let union ((sr1,isVal1,isSquare1,isSquareP1,pred1) as esr1) ((sr2,isVal2,isSquare2,isSquareP2,pred2) as esr2) =
  let (i,_) = sr1.init_state in
  let (j,_) = sr2.init_state in
  match (i < j,i=j) with
   | (true,_) -> print_endline ("Merging1 " ^ (string_of_state sr2.init_state) ^ " into " ^ (string_of_state sr1.init_state));
                 let (sr2,isVal2,isSquare2,isSquareP2,pred2) = substitute_state_esr sr1.init_state esr2  in
                 basic_union sr1.init_state (sr1,isVal1,isSquare1,isSquareP1,pred1) (sr2,isVal2,isSquare2,isSquareP2,pred2)
   | (false,true) -> failwith "Error trying to build the WTS: We are trying to merge two ESR which already share the same initial state"
   | (false,false) -> print_endline ("Merging2 " ^ (string_of_state sr1.init_state) ^ " into " ^ (string_of_state sr2.init_state));
                      let (sr1,isVal1,isSquare1,isSquareP1,pred1) = substitute_state_esr sr2.init_state esr1  in
                      basic_union sr2.init_state (sr1,isVal1,isSquare1,isSquareP1,pred1) (sr2,isVal2,isSquare2,isSquareP2,pred2)


let build_esr_modal prev_sequent current_sequent init_state (sr,isVal,isSquare,isSquareP,pred) =
(*  let (sr,isVal,isSquare,isSquareP,pred) = build_esr tc in*)
  let (vars,pred') = newgroundelem_of_sequents prev_sequent current_sequent in
  let preds = iter 2 simplify_arith_pred (AAnd (pred::pred')) in
  match current_sequent.modality with
    | NextE (hpre1,hpre2,hpost1,hpost2) ->
        let (sr',isVal',isSquare',isSquareP',_) = extend_env_esr (vars,preds) (sr,isVal,isSquare,isSquareP,ATrue) in     
        sr'.extern_transitions <- (init_state,sr'.init_state, (hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.extern_transitions;
        begin match preds with
           | ATrue -> (sr',isVal',isSquare',isSquareP',ATrue)
           | _ -> let incons_state = extend_env_state (vars,negate_arith_pred preds) (fresh_state ()) in
                  sr'.states <- States.add incons_state sr'.states;  
                  sr'.incons_states <- States.add incons_state sr'.incons_states;
                  sr'.extern_transitions <- (init_state, incons_state,(hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.extern_transitions;
                  (sr',isVal',isSquare',isSquareP',ATrue)
        end
    | NextWB (hpre1,hpre2,hpost1,hpost2) ->
        let (sr',isVal',isSquare',isSquareP',_) = extend_env_esr (vars,preds) (sr,isVal,isSquare,isSquareP,ATrue) in     
        sr'.extern_transitions <- (init_state,sr'.init_state, (hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.extern_transitions;
        begin match preds with
           | ATrue -> (sr',isVal',isSquare',isSquareP',ATrue)
           | _ -> let incons_state = extend_env_state (vars,negate_arith_pred preds) (fresh_state ()) in
                  sr'.states <- States.add incons_state sr'.states;  
                  sr'.incons_states <- States.add incons_state sr'.incons_states;
                  sr'.extern_transitions <- (init_state, incons_state,(hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.extern_transitions;
                  (sr',isVal',isSquare',isSquareP',ATrue)
        end        
    | NextI (hpre1,hpre2,hpost1,hpost2) ->
        let (sr',isVal',isSquare',isSquareP',_) = extend_env_esr (vars,preds) (sr,isVal,isSquare,isSquareP,ATrue) in     
        sr'.pintern_transitions <- (init_state,sr'.init_state, (hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.pintern_transitions;
        begin match preds with
           | ATrue -> (sr',isVal',isSquare',isSquareP',ATrue)
           | _ -> let incons_state = extend_env_state (vars,negate_arith_pred preds) (fresh_state ()) in
                  sr'.states <- States.add incons_state sr'.states;  
                  sr'.incons_states <- States.add incons_state sr'.incons_states;
                  sr'.pintern_transitions <- (init_state, incons_state,(hpre1,hpre2,hpost1,hpost2,ATrue))::sr'.pintern_transitions;
                  (sr',isVal',isSquare',isSquareP',ATrue)
        end        
    | Square _ ->
        let (sr',isVal',isSquare',isSquareP',_) = extend_env_esr (vars,preds) (sr,isVal,isSquare,isSquareP,ATrue) in         
        sr'.peps_transitions <- (init_state,sr'.init_state)::sr'.peps_transitions;
        let wb_transitions = List.map (fun state' -> (init_state, state')) (States.elements isVal') in
        sr'.wb_transitions <- wb_transitions@sr'.wb_transitions;        
        (sr',isVal',isSquare',isSquareP',ATrue)
    | SquarePub _ -> 
        let (sr',isVal',isSquare',isSquareP',_) = extend_env_esr (vars,preds) (sr,isVal,isSquare,isSquareP,ATrue) in        
        sr'.peps_transitions <- (init_state,sr'.init_state)::sr'.peps_transitions;          
        (sr',isVal',isSquare',isSquareP',ATrue)     
    | _ -> failwith ("Cannot deal with the modality " ^ (string_of_modality current_sequent.modality))

let rec build_esr = function
  | RuleVG sequent -> let sr = singleton_sr () in (sr,States.empty,States.empty,States.empty,extract_pred_from_vg sequent)
  | Stop sequent -> let sr = singleton_sr () in
                        (sr,States.empty,States.empty,States.empty,negate_arith_pred (AAnd sequent.logctx))              
  | RuleVProd ((tc1,tc2),sequent) | RuleSext ((tc1,tc2),sequent) ->
      let esr1 = build_esr tc1 in
      let esr2 = build_esr tc2 in      
      union esr1 esr2
  | Unfold (tc,sequent) | LUnfold (tc,sequent) | RUnfold (tc,sequent)  | Rewrite (tc,sequent) -> build_esr tc        
  | RuleV ([],sequent) -> failwith "Error trying to build the WTS: A rule V does not contain any premise"
  | RuleV ([tc],sequent) ->
      let (sr,isVal,isSquare,isSquareP,pred) = build_esr tc in
      let (vars,preds) = newgroundelem_of_sequents sequent (get_root tc) in
      let pred = iter 2 simplify_arith_pred (AAnd preds) in     
      let wb_transitions = List.map (fun state' -> (sr.init_state, state')) (States.elements isVal) in
      sr.wb_transitions <- wb_transitions@sr.wb_transitions;
      extend_env_esr (vars,pred) (sr,States.empty,States.add sr.init_state isSquare,isSquareP,ATrue)
  | RuleK ([],sequent) -> failwith "Error trying to build the WTS: A rule K does not contain any premise"
  | RuleK ([tc],sequent) ->
      let (sr,isVal,isSquare,isSquareP,preds) = build_esr tc in
      let (vars,preds) = newgroundelem_of_sequents sequent (get_root tc) in
      let pred = iter 2 simplify_arith_pred (AAnd preds) in
      extend_env_esr (vars,pred) (sr,isVal,isSquare,States.add sr.init_state isSquareP,ATrue)        
  | RuleV (tcs,sequent) | RuleK (tcs,sequent) | RuleE (tcs,sequent) -> 
      let result = List.map (fun tc -> build_esr tc,get_root tc) tcs in
      let new_init_state = fresh_state () in      
      let esr::esrs = List.map (fun (esr,sequent') -> build_esr_modal sequent sequent' new_init_state esr) result in
      let (sr,isVal,isSquare,isSquareP,pred) = List.fold_left (basic_union new_init_state) esr esrs in
      sr.init_state <- new_init_state;
(*       print_endline ("Adding new state " ^(string_of_state new_init_state)); *)
      sr.states <- States.add new_init_state sr.states;  
      (sr,isVal,isSquare,isSquareP,ATrue)
  | tc -> failwith ("Error trying to build the WTS: we cannot deal with the tc structure: " ^ (string_of_tc tc))     
 
(*        | Next (hpre1,hpre2,hpost1,hpost2) ->
           let (preds,env) = newelem_of_sequents prev_sequent sequent in
           print_endline ("We have the environment " ^ (string_of_pmap ":" string_of_typeML env));
           let new_state = fresh_state () in
           let old_init_state = sr.init_state in
           sr.states <- States.add new_state sr.states;
           sr.init_state <- new_state;           
           sr.extern_transitions <- (new_state, old_init_state,(hpre1,hpre2,hpost1,hpost2,pred))::sr.extern_transitions;
           begin match pred with
             | ATrue -> ()
             | _ -> let incons_state = fresh_state () 
                    in  sr.incons_states <- States.add incons_state sr.incons_states;
                    sr.extern_transitions <- (new_state, incons_state,(hpre1,hpre2,hpost1,hpost2,negate_arith_pred pred))::sr.extern_transitions
           end;
           extend_env_esr (env,preds) (sr,isVal,isSquare,isSquareP,ATrue)
      end      *)
  
let rec build_sr tc =
  let (sr,isVal,isSquare,isSquareP,pred) = build_esr tc in
  print_endline ("IsVal: ");
  States.iter (fun x -> print_endline ((string_of_state x) ^" ")) isVal;
  print_newline ();
  print_endline ("IsSquare: ");
  States.iter (fun x -> print_endline ((string_of_state x) ^" ")) isSquare;
  print_newline ();
  print_endline ("IsSquareP: ");
  States.iter (fun x -> print_endline ((string_of_state x) ^" ")) isSquareP;
  print_newline ();          
  let eps1 = List.flatten (List.map (fun state -> List.flatten (List.map (fun (s,s',_) -> if s = state then [(s',s)] else []) sr.extern_transitions)) (States.elements isSquare)) in
  let eps2 = List.flatten (List.map (fun state -> List.flatten (List.map (fun (s,s') -> if s = state then [(s',s)] else []) sr.wb_transitions)) (States.elements isSquareP)) in
  let pub = List.map (fun state -> (sr.init_state,state)) (States.elements isVal) in
  sr.peps_transitions <- eps1@eps2@sr.peps_transitions;
  sr.wb_transitions <- pub@sr.wb_transitions;
(*  sr.pintern_transitions <- List.map (fun (s,s',label) -> (s,s',simplify_label label)) sr.pintern_transitions;  
  sr.extern_transitions <- List.map (fun (s,s',label) -> (s,s',simplify_label label)) sr.extern_transitions;*)
  sr

  
