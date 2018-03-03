(* ocamlfind ocamlopt pmap.ml syntax.ml parser.ml lexer.ml logic.ml symb_red.ml skor.ml tcstruct.ml wts.ml wts_closure.ml wts_to_dot.ml type_checker.ml main.ml -o syteci -ccopt -static *)


open Tcstruct
open Logic
open Pmap
open Syntax
open Skor
open Unif

type polarity = PI | PQ | PA | OQ | OA

let string_of_polarity = function
  | PI -> "PI" 
  | PQ -> "PQ" 
  | PA -> "PA" 
  | OQ -> "OQ" 
  | OA -> "OA"


type label = (symbheap*symbheap*symbheap*symbheap*Logic.arith_pred*polarity)

let polarity_from_tag = function
  | Intern -> PI
  | Extern -> PQ
  | WB -> PA
  | Wrong -> PI

let string_of_label (heapPre1,heapPre2,heapPost1,heapPost2,preds,polarity) =
   let string_heaps = (string_of_symb_heap heapPre1) ^ "," ^(string_of_symb_heap heapPre2) ^ "," ^(string_of_symb_heap heapPost1) ^ "," ^ (string_of_symb_heap heapPost2) in
   let string_polarity = string_of_polarity polarity in
   let string_preds = string_of_arith_pred preds in
   match string_preds with
     | "" -> string_heaps ^ "," ^ string_polarity
     | _ -> string_heaps ^ "," ^ string_preds ^ "," ^ string_polarity

(*let simplify_label (heapPre1,heapPre2,heapPost1,heapPost2,pred) = (heapPre1,heapPre2,heapPost1,heapPost2,simplify_arith_pred pred) *)
      
type state = int

module States = Set.Make( 
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)
    type t = state
  end )

let count_state = ref 0
let available_states = ref States.empty
let fresh_state () = 
  match States.is_empty !available_states with
    | true -> let a = !count_state in
              count_state := !count_state + 1;
              a
    | false -> let a = States.min_elt !available_states in
               available_states := States.remove a !available_states;
               a



let free_state a =
  available_states := States.add a !available_states

let string_of_state = string_of_int

type sr = { 
    mutable states : States.t;
    mutable init_state : state;
    mutable incons_states : States.t;    
    mutable pintern_transitions : (state*state*label) list;
    mutable extern_transitions : (state*state) list;
    mutable wb_transitions : (state*state) list;        
    mutable peps_transitions : (state*state*gsubst) list;
    mutable o_transitions : (state*state*polarity) list;     
    mutable oeps_transitions : (state*state) list;    
  }

type esr = sr*States.t*States.t*States.t*arith_pred list 
                 
let string_of_intern_transition (s1,s2,label) =
  (string_of_state s1) ^ "-(" ^ (string_of_label label) ^")->" ^ (string_of_state s2)

let string_of_o_transition (s1,s2,polarity) =
  (string_of_state s1) ^ "-"^(string_of_polarity polarity)^"->" ^ (string_of_state s2)  

let string_of_peps_transition (s1,s2,gsubst) =
  (string_of_state s1) ^ "-"^(string_of_gsubst gsubst)^"->" ^ (string_of_state s2)  
  
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
  List.iter (fun x -> print_endline (string_of_peps_transition x)) sr.peps_transitions;
  print_newline ();
  print_endline ("O Transitions: ");
  List.iter (fun x -> print_endline (string_of_o_transition x)) sr.o_transitions;
  print_newline ();
  print_endline ("O-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (string_of_atomic_transition x)) sr.oeps_transitions;
  print_newline ()         
  
let set_swap old_state new_state states =
  let (states1,states2) = States.partition (fun z -> z = old_state) states in
  if (States.is_empty states1) then states2 else (States.add new_state states2)
  
let list_swap old_state new_state states =
  let (states1,states2) = List.partition (fun z -> z = old_state) states in
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
  let s1' = (if s1 = old_state then new_state else s1) in
  let s2' = (if s2 = old_state then new_state else s2) in
  (s1',s2')   
   
let substitute_state_complex_trans old_state new_state (s1,s2,u) = 
  let (s1',s2') = substitute_state_atom_trans old_state new_state (s1,s2) in
  (s1',s2',u)

let substitute_state_partial_trans old_state new_state (s,n,u) =
  let s' = (if s = old_state then new_state else s) in
  (s',n,u)
  
let substitute_state_esr state (sr,isExt,isWB,preds,p_back_trans) =
  let old_init = sr.init_state in
  let isExt' = set_swap old_init state isExt in  
  let isWB' = set_swap old_init state isWB in
  let p_back_trans' = List.map (substitute_state_partial_trans old_init state) p_back_trans in
  let sr' = 
       { states = States.add state (States.remove old_init sr.states); 
         init_state = state;
         incons_states = set_swap old_init state sr.incons_states;
         pintern_transitions = List.map (substitute_state_complex_trans old_init state) sr.pintern_transitions;
         extern_transitions = List.map (substitute_state_atom_trans old_init state) sr.extern_transitions;
         wb_transitions = List.map (substitute_state_atom_trans old_init state) sr.wb_transitions;         
         peps_transitions = List.map (substitute_state_complex_trans old_init state) sr.peps_transitions;
         o_transitions = List.map (substitute_state_complex_trans old_init state) sr.o_transitions;          
         oeps_transitions = List.map (substitute_state_atom_trans old_init state) sr.oeps_transitions }
  in free_state old_init;
     (sr',isExt',isWB',preds,p_back_trans')       

let basic_union init_state (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2) =
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
  (sr,States.union isExt1 isExt2,States.union isWB1 isWB2,preds1@preds2,p_back_trans1@p_back_trans2)      
  
let union ((sr1,isExt1,isWB1,preds1,p_back_trans1) as esr1) ((sr2,isExt2,isWB2,preds2,p_back_trans2) as esr2) =
  let i = sr1.init_state in
  let j = sr2.init_state in
  match (i < j,i=j) with
   | (true,_) -> (*print_endline ("Merging1 " ^ (string_of_state sr2.init_state) ^ " into " ^ (string_of_state sr1.init_state));*)
                 let (sr2,isExt2,isWB2,pred2,p_back_trans2) = substitute_state_esr sr1.init_state esr2  in
                 basic_union sr1.init_state (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2)
   | (false,true) -> failwith "Error trying to build the WTS: We are trying to merge two ESRs which already share the same initial state"
   | (false,false) -> print_endline ("Merging2 " ^ (string_of_state sr1.init_state) ^ " into " ^ (string_of_state sr2.init_state));
                      let (sr1,isExt1,isWB1,pred1,p_back_trans1) = substitute_state_esr sr2.init_state esr1  in
                      basic_union sr2.init_state (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2)


let build_esr_ruleO polarity = function
  | [] -> failwith "Error trying to build the WTS: the rule V or K does not contain any premise"
  | esr::esrs ->
      let new_init_state = fresh_state () in
      let (sr,isExt,isWB,preds,p_back_trans) = List.fold_left (basic_union new_init_state) esr esrs in      
      let wb_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isWB) in
      let extern_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isExt) in
      let o_transitions = List.map (fun (sr,_,_,_,_) -> (new_init_state, sr.init_state,polarity)) (esr::esrs) in
      sr.states <- States.add new_init_state sr.states;  
      sr.init_state <- new_init_state;        
      sr.wb_transitions <- wb_transitions@sr.wb_transitions;
      sr.extern_transitions <- wb_transitions@extern_transitions@sr.extern_transitions;      
      sr.o_transitions <- o_transitions@sr.o_transitions;
      begin match polarity with
        | OQ -> (sr,States.empty,States.empty,preds,p_back_trans)
        | OA -> (sr,States.empty,isWB,preds,p_back_trans)
        | _ -> failwith "Wrong polarity"
      end  

let build_pintern_trans sequent s ((sr,_,_,preds,_),(tag,hpre1,hpre2,hpost1,hpost2),sequent') =
  let (preds',vars) = newelem_of_sequents sequent sequent' in
  let polarity = polarity_from_tag tag in
  let full_preds = simplify_arith_pred (AAnd (preds@preds')) in
  (s,sr.init_state,(hpre1,hpre2,hpost1,hpost2,full_preds,polarity))

let build_pintern_trans_incons sequent s ((sr,_,_,preds,_),(tag,hpre1,hpre2,hpost1,hpost2),sequent') =
  let (preds',vars) = newelem_of_sequents sequent sequent' in
  let polarity = polarity_from_tag tag in
  let neg_preds = negate_arith_pred (AAnd preds) in
  let full_preds = simplify_arith_pred (simplify_arith_pred (AAnd (neg_preds::preds'))) in
  match full_preds with
   | AFalse -> []
   | _ -> let s' = fresh_state() in [((s,s',(hpre1,hpre2,hpost1,hpost2,full_preds,polarity)),s')]  
        
let build_esr_ruleP sequent esrs_a = match esrs_a with
  | [] -> failwith "Error trying to build the WTS: the rule E does not contain any premise"
  | ((esr,annotation,sequent)::esrs_a') ->
      let new_init_state = fresh_state () in
      let isExt_a = List.filter (fun (_,(tag,_,_,_,_),_) -> tag = Extern) esrs_a in
      let isExt' = States.of_list (List.map (fun ((sr',_,_,_,_),_,_) -> sr'.init_state) isExt_a) in
      let isWB_a = List.filter (fun (_,(tag,_,_,_,_),_) -> tag = WB) esrs_a in
      let isWB' = States.of_list (List.map (fun ((sr',_,_,_,_),_,_) -> sr'.init_state) isWB_a) in          
(*      let new_incons_states = List.map (fun _ -> fresh_state ()) esrs_a in*)
      let (sr,isExt,isWB,preds,p_back_trans) = List.fold_left (fun esr1 (esr2,_,_) -> basic_union new_init_state esr1 esr2) esr esrs_a' in      
      let pintern_transitions = List.map (build_pintern_trans sequent new_init_state) esrs_a in
      let (pintern_transitions_incons,new_incons_states) = List.split (List.flatten (List.map (build_pintern_trans_incons sequent new_init_state) esrs_a)) in
      sr.states <- States.add new_init_state sr.states;  
      List.iter (fun s -> sr.states <- States.add s sr.states) new_incons_states;
      List.iter (fun s -> sr.incons_states <- States.add s sr.incons_states) new_incons_states;      
      sr.init_state <- new_init_state;
      sr.pintern_transitions <- pintern_transitions@pintern_transitions_incons@sr.pintern_transitions;  
      (sr,States.union isExt' isExt,States.union isWB' isWB,[],p_back_trans)        
    
let rec build_esr = function
  | RuleVG sequent -> let sr = singleton_sr () in (sr,States.empty,States.empty,[extract_pred_from_vg sequent],[])
  | Stop sequent | ROut sequent   -> let sr = singleton_sr () in  (sr,States.empty,States.empty,[AFalse],[])
  | LOut sequent   -> let sr = singleton_sr () in  (sr,States.empty,States.empty,[ATrue],[])     
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
  | Gen (gsubst,tc,sequent) ->
      let (sr,isExt,isWB,preds,p_back_trans) = build_esr tc in
      let new_init_state = fresh_state () in
      sr.peps_transitions <- (new_init_state,sr.init_state,gsubst)::sr.peps_transitions;
      sr.init_state <- new_init_state;
      let premise_sequent = get_root tc in
      print_endline ("Gen id " ^ (string_of_int premise_sequent.id));
      let (p_back_trans_now,p_back_trans_later) = List.partition (fun (_,id,_) -> id = premise_sequent.id) p_back_trans in
      let back_trans = List.map (fun (state,_,gsubst) -> (state,new_init_state,gsubst)) p_back_trans_now in
      sr.peps_transitions <- back_trans@sr.peps_transitions;
      (sr,isExt,isWB,preds,p_back_trans_later)
  | Circ (gsubst,back_sequent,sequent) -> 
      let sr = singleton_sr () in 
      (sr,States.empty,States.empty,[],[(sr.init_state,back_sequent.id,gsubst)])   

  
let rec build_sr tc =
  match  build_esr tc with
    | (sr,_,_,_,[]) -> sr
    | (_,_,_,_,_) -> failwith ("Error, the generation of back-transition is not finished. "); 
(*    List.iter (fun (s,id,_) -> print_endline ("(" ^ (string_of_state s) ^ "," ^ (string_of_int id) ^ ") "))*)
  
