open Tcstruct
open Logic
open Pmap
open Syntax
open Skor

type polarity = PI | PQ | PA | OQ | OA

type label = (symbheap*symbheap*symbheap*symbheap*Logic.arith_pred list*polarity)

let polarity_from_tag = function
  | Intern -> PI
  | Extern -> PQ
  | WB -> PA
  | Wrong -> PI

let string_of_label (heapPre1,heapPre2,heapPost1,heapPost2,preds,_) =
    "[" ^ (string_of_symb_heap heapPre1) ^ "],[" ^(string_of_symb_heap heapPre2) ^ "],[" ^(string_of_symb_heap heapPost1) ^ "],[" 
      ^ (string_of_symb_heap heapPost2) ^ "]," ^ (string_of_conj "/\\" string_of_arith_pred preds)

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
 

type state = atom_state*(var_ctx*arith_pred list)

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
  (state,(empty_pmap,[]))
  
let fresh_incons_state (_,_,_,preds) =
  let state = fresh_atom_state () in
  (state,(empty_pmap,preds))  

let free_state (a,_) =
  available_states := AStates.add a !available_states
  
let extend_env_state (vars1,preds1) (s,(vars2,preds2)) = (s,(vars1@vars2,preds1@preds2))  

let string_of_state (s,env) = match env with
  | ([],[]) -> string_of_int s
  | (vars,[]) -> "(" ^ (string_of_int s) ^ "," ^ (string_of_pmap ":" string_of_typeML vars) ^ ")"
  | (vars,preds) -> "(" ^ (string_of_int s) ^ "," ^ (string_of_pmap ":" string_of_typeML vars) ^ "," ^ (string_of_conj "/\\" string_of_arith_pred preds) ^ ")"

type sr = { 
    mutable states : States.t;
    mutable init_state : state;
    mutable incons_states : States.t;    
    mutable pintern_transitions : (state*state*label) list;
    mutable extern_transitions : (state*state) list;
    mutable wb_transitions : (state*state) list;        
    mutable peps_transitions : (state*state) list;
    mutable o_transitions : (state*state*polarity) list;     
    mutable oeps_transitions : (state*state) list;    
  }

let extend_env_list env l = 
  List.map (extend_env_state env) l

let extend_env_states env states = 
  List.fold_right States.add (extend_env_list env (States.elements states))  States.empty   
  
type esr = sr*States.t*States.t*States.t*arith_pred list 
  
let extend_env_esr env (sr,isExt,isWB,preds) =
  sr.states <- extend_env_states env sr.states;
  sr.init_state <- extend_env_state env sr.init_state;
  sr.incons_states <- extend_env_states env sr.incons_states;
  sr.pintern_transitions <- List.map (fun (s,s',label) -> (extend_env_state env s,extend_env_state env s',label)) sr.pintern_transitions;  
  sr.extern_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.extern_transitions;
  sr.wb_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.wb_transitions;  
  sr.peps_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.peps_transitions;
  sr.o_transitions <- List.map (fun (s,s',polarity) -> (extend_env_state env s,extend_env_state env s',polarity)) sr.o_transitions;
  sr.oeps_transitions <- List.map (fun (s,s') -> (extend_env_state env s,extend_env_state env s')) sr.oeps_transitions;      
  let isExt' = extend_env_states env isExt in  
  let isWB' = extend_env_states env isWB in
  (sr,isExt',isWB',preds)
                 
let string_of_intern_transition (s1,s2,label) =
  (string_of_state s1) ^ "-(" ^ (string_of_label label) ^")->" ^ (string_of_state s2)

let string_of_o_transition (s1,s2,polarity) =
  (string_of_state s1) ^ "-->" ^ (string_of_state s2)  

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
  List.iter (fun x -> print_endline (string_of_intern_transition x)) sr.pintern_transitions;
  print_newline ();
  print_endline ("External Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.extern_transitions;
  print_newline ();
  print_endline ("WB Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.wb_transitions;  
  print_newline ();      
  print_endline ("P-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.peps_transitions;
  print_newline ();
  print_endline ("O Transitions: ");
  List.iter (fun x -> print_endline (string_of_o_transition x)) sr.o_transitions;
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
(*  print_endline ("Adding new state " ^(string_of_state state));*)
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

let substitute_state_atom_trans old_state new_state (s1,s2) = 
  let s1' = (if compare_state s1 old_state then new_state else s1) in
  let s2' = (if compare_state s2 old_state then new_state else s2) in
  (s1',s2')   
   
let substitute_state_intern_trans old_state new_state (s1,s2,label) = 
  let (s1',s2') = substitute_state_atom_trans old_state new_state (s1,s2) in
  (s1',s2',label)
  
let substitute_state_o_trans old_state new_state (s1,s2,polarity) = 
  let (s1',s2') = substitute_state_atom_trans old_state new_state (s1,s2) in
  (s1',s2',polarity)  
  
let substitute_state_esr state (sr,isExt,isWB,preds) =
  let old_init = sr.init_state in
  let isExt' = states_swap old_init state isExt in  
  let isWB' = states_swap old_init state isWB in    
  let sr' = 
       { states = States.add state (States.remove old_init sr.states); 
         init_state = state;
         incons_states = states_swap old_init state sr.incons_states;
         pintern_transitions = List.map (substitute_state_intern_trans old_init state) sr.pintern_transitions;
         extern_transitions = List.map (substitute_state_atom_trans old_init state) sr.extern_transitions;
         wb_transitions = List.map (substitute_state_atom_trans old_init state) sr.wb_transitions;         
         peps_transitions = List.map (substitute_state_atom_trans old_init state) sr.peps_transitions;
         o_transitions = List.map (substitute_state_o_trans old_init state) sr.o_transitions;          
         oeps_transitions = List.map (substitute_state_atom_trans old_init state) sr.oeps_transitions }
  in free_state old_init;
     (sr',isExt',isWB',preds)       

let basic_union init_state (sr1,isExt1,isWB1,preds1) (sr2,isExt2,isWB2,preds2) =
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
  (sr,States.union isExt1 isExt2,States.union isWB1 isWB2,preds1@preds2)      
  
let union ((sr1,isExt1,isWB1,preds1) as esr1) ((sr2,isExt2,isWB2,preds2) as esr2) =
  let (i,_) = sr1.init_state in
  let (j,_) = sr2.init_state in
  match (i < j,i=j) with
   | (true,_) -> (*print_endline ("Merging1 " ^ (string_of_state sr2.init_state) ^ " into " ^ (string_of_state sr1.init_state));*)
                 let (sr2,isExt2,isWB2,pred2) = substitute_state_esr sr1.init_state esr2  in
                 basic_union sr1.init_state (sr1,isExt1,isWB1,preds1) (sr2,isExt2,isWB2,preds2)
   | (false,true) -> failwith "Error trying to build the WTS: We are trying to merge two ESRs which already share the same initial state"
   | (false,false) -> print_endline ("Merging2 " ^ (string_of_state sr1.init_state) ^ " into " ^ (string_of_state sr2.init_state));
                      let (sr1,isExt1,isWB1,pred1) = substitute_state_esr sr2.init_state esr1  in
                      basic_union sr2.init_state (sr1,isExt1,isWB1,preds1) (sr2,isExt2,isWB2,preds2)


let build_esr_ruleO polarity = function
  | [] -> failwith "Error trying to build the WTS: the rule V or K does not contain any premise"
  | esr::esrs ->
      let new_init_state = fresh_state () in
      let (sr,isExt,isWB,preds) = List.fold_left (basic_union new_init_state) esr esrs in      
      let wb_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isWB) in
      let extern_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isExt) in
      let o_transitions = List.map (fun (sr,_,_,_) -> (new_init_state, sr.init_state,polarity)) (esr::esrs) in
      sr.states <- States.add new_init_state sr.states;  
      sr.init_state <- new_init_state;        
      sr.wb_transitions <- wb_transitions@sr.wb_transitions;
      sr.extern_transitions <- wb_transitions@extern_transitions@sr.extern_transitions;      
      sr.o_transitions <- o_transitions@sr.o_transitions;
      begin match polarity with
        | OQ -> (sr,States.empty,States.empty,preds)
        | OA -> (sr,States.empty,isWB,preds)
        | _ -> failwith "Wrong polarity"
      end  

let build_pintern_trans sequent s ((sr,_,_,preds),(tag,hpre1,hpre2,hpost1,hpost2),sequent') =
  let (preds',vars) = newelem_of_sequents sequent' sequent in
  let polarity = polarity_from_tag tag in
  (s,sr.init_state,(hpre1,hpre2,hpost1,hpost2,preds@preds',polarity))

let build_pintern_trans_incons sequent s s' ((sr,_,_,_),(tag,hpre1,hpre2,hpost1,hpost2),sequent') =
  let (preds',vars) = newelem_of_sequents sequent' sequent in
  let polarity = polarity_from_tag tag in
  (s,s',(hpre1,hpre2,hpost1,hpost2,preds',polarity))  
        
let build_esr_ruleP sequent esrs_a = match esrs_a with
  | [] -> failwith "Error trying to build the WTS: the rule E does not contain any premise"
  | ((esr,annotation,sequent)::esrs_a') ->
      let new_init_state = fresh_state () in
      let new_incons_states = List.map (fun (esr,_,_) -> fresh_incons_state esr) esrs_a in
      let (sr,isExt,isWB,preds) = List.fold_left (fun esr1 (esr2,_,_) -> basic_union new_init_state esr1 esr2) esr esrs_a' in      
      let pintern_transitions = List.map (build_pintern_trans sequent new_init_state) esrs_a in
      let pintern_transitions_incons = List.map2 (build_pintern_trans_incons sequent new_init_state) new_incons_states esrs_a in
      sr.states <- States.add new_init_state sr.states;  
      List.iter (fun s -> sr.states <- States.add s sr.states) new_incons_states;
      List.iter (fun s -> sr.incons_states <- States.add s sr.incons_states) new_incons_states;      
      sr.init_state <- new_init_state;
      sr.pintern_transitions <- pintern_transitions@pintern_transitions_incons@sr.pintern_transitions;
      let isExt_esrs_a = List.filter (fun (_,(tag,_,_,_,_),_) -> tag = Extern) esrs_a in
      let isExt' = States.of_list (List.map (fun ((sr,_,_,preds),_,_) -> sr.init_state) isExt_esrs_a) in
      let isWB_esrs_a = List.filter (fun (_,(tag,_,_,_,_),_) -> tag = WB) esrs_a in
      let isWB' = States.of_list (List.map (fun ((sr,_,_,preds),_,_) -> sr.init_state) isWB_esrs_a) in      
      (sr,States.union isExt' isExt,States.union isWB' isWB,[])        
    
let rec build_esr = function
  | RuleVG sequent -> let sr = singleton_sr () in (sr,States.empty,States.empty,[extract_pred_from_vg sequent])
  | Stop sequent   -> let sr = singleton_sr () in  (sr,States.empty,States.empty,[negate_arith_pred (AAnd sequent.arith_ctx)])              
  | RuleVProd ((tc1,tc2),sequent) | RuleSext ((tc1,tc2),sequent) ->
      let esr1 = build_esr tc1 in
      let esr2 = build_esr tc2 in      
      union esr1 esr2
  | Unfold (tc,sequent) | LUnfold (tc,sequent) | RUnfold (tc,sequent)  | Rewrite (tc,sequent) -> build_esr tc    
  | RuleV (tcs,sequent) -> let esrs = List.map build_esr tcs in build_esr_ruleO OQ esrs
  | RuleK (tcs,sequent) -> let esrs = List.map build_esr tcs in build_esr_ruleO OA esrs
  | RuleE (tcs_a,sequent) -> 
      let esrs_a = List.map (fun (annotation,tc) -> (build_esr tc,annotation,get_root tc)) tcs_a in
      build_esr_ruleP sequent esrs_a
  | tc -> failwith ("Error trying to build the WTS: we cannot deal with the tc structure: " ^ (string_of_tc tc))     
  
let rec build_sr tc =
  let (sr,_,_,_) = build_esr tc in sr
