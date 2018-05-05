open Pmap
open Tcstruct
open Logic
open Syntax
open Skor
open Unif

type polarity = PI | PQ | PA | PE| OQ | OA | OE

let string_of_polarity = function
  | PI -> "PI" 
  | PQ -> "PQ" 
  | PA -> "PA"
  | PE -> "PE"
  | OQ -> "OQ" 
  | OA -> "OA"
  | OE -> "OE"


type label = (symbheap*symbheap*symbheap*symbheap*Logic.arith_pred*Syntax.var_ctx*polarity)

let polarity_from_tag = function
  | Intern -> PI
  | Extern -> PQ
  | WB -> PA
  | Wrong -> PI

let string_of_label (heapPre1,heapPre2,heapPost1,heapPost2,preds,_,polarity) =
   let string_heaps = (string_of_symb_heap heapPre1) ^ "↝" ^ (string_of_symb_heap heapPost1) ^ "," ^ (string_of_symb_heap heapPre2) ^ "↝" ^ (string_of_symb_heap heapPost2) in
   let string_polarity = string_of_polarity polarity in
   let string_preds = string_of_arith_pred preds in
   match string_preds with
     | "" -> string_heaps ^ "," ^ string_polarity
     | _ -> string_heaps ^ "," ^ string_preds ^ "," ^ string_polarity

(*let simplify_label (heapPre1,heapPre2,heapPost1,heapPost2,pred) = (heapPre1,heapPre2,heapPost1,heapPost2,simplify_arith_pred pred) *)

type invariant = (symbheap*symbheap*arith_pred*arith_pred*var_ctx) (* the second arith_pred is the real invariant *)

let string_of_invariant (heap1,heap2,preds1,preds2,_) =
   let string_heaps = (string_of_symb_heap heap1) ^ "," ^ (string_of_symb_heap heap2) in
   let string_preds1 = string_of_arith_pred preds1 in 
   let string_preds2 = string_of_arith_pred preds2 in    
   string_heaps ^ "," ^ string_preds1 ^ "," ^ string_preds2

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

type transition = 
  | PT of (state*label)
  | PET of (state*gsubst)
  | OT of (state*polarity)
  | OET of state

module StateMap = Map.Make(
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)    
    type t = state
  end)  

type sr = { 
    mutable states : States.t;
    mutable init_state : state;
    mutable incons_states : States.t;
    mutable var_map : (state,var_ctx) pmap;
    mutable trans_fun : (transition list) StateMap.t;
    mutable extern_transitions : (state*state) list;
    mutable wb_transitions : (state*state) list;     
    mutable invariants : invariant list;
  }  

let string_of_transition s1 = function
  | PT (s2,label) ->   (string_of_state s1) ^ "-(" ^ (string_of_label label) ^")->" ^ (string_of_state s2)
  | PET (s2,gsubst) -> (string_of_state s1) ^ "-"^(string_of_gsubst gsubst)^"->" ^ (string_of_state s2)
  | OT (s2,polarity) -> (string_of_state s1) ^ "-"^(string_of_polarity polarity)^"->" ^ (string_of_state s2)
  | OET s2 -> (string_of_state s1) ^ "-->" ^ (string_of_state s2)

let get_ltrans trans_fun s =
  try StateMap.find s trans_fun with
    | Not_found -> []  

let add_trans trans_fun s trans  =
  Debug.print_debug ("Adding a new transition " ^ (string_of_transition s trans));
  let ltrans = get_ltrans trans_fun s in
  StateMap.add s (trans::ltrans) trans_fun
  
let add_list_trans trans_fun s ltrans  =
  let ltrans' = get_ltrans trans_fun s in
  StateMap.add s (ltrans@ltrans') trans_fun  

(* Important invariant: There is never a transition going to the initial state *)

type esr = sr*States.t*States.t*States.t*arith_pred list 

  
  
let print_sr sr =
  print_endline ("States: ");
  States.iter (fun x -> print_string ((string_of_state x) ^ " ")) sr.states;
  print_newline ();
  print_endline ("Initial State: " ^ (string_of_state sr.init_state));    
  print_endline ("Inconsistent States: ");
  States.iter (fun x -> print_string ((string_of_state x) ^ " ")) sr.incons_states;
  print_newline ();      
  print_endline ("Transitions: ");
  List.iter (fun (s1,l) -> List.iter (fun trans -> print_endline (string_of_transition s1 trans)) l) (StateMap.bindings sr.trans_fun)    
  
let set_swap old_state new_state states =
  let (states1,states2) = States.partition (fun z -> z = old_state) states in
  if (States.is_empty states1) then states2 else (States.add new_state states2)
  
let list_swap old_state new_state states =
  let (states1,states2) = List.partition (fun z -> z = old_state) states in
  if (states1 = []) then states2 else  new_state::states2
  
let singleton_sr () = 
  let state = fresh_state () in
  { states = States.singleton state; 
    init_state = state;
    incons_states = States.empty;
    var_map = [];
    trans_fun = StateMap.empty;
    extern_transitions = [];
    wb_transitions = [];
    invariants = [];
 }

let substitute_state_atom_trans old_state new_state (s1,s2) = 
  let s1' = (if s1 = old_state then new_state else s1) in
  let s2' = (if s2 = old_state then new_state else s2) in
  (s1',s2') 
 
let substitute_state_trans trans_fun old_state new_state =
  match get_ltrans trans_fun old_state with
    | [] -> trans_fun
    | l -> 
      let trans_fun' = StateMap.remove old_state trans_fun in
      StateMap.add new_state l trans_fun'

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
         var_map = List.map (fun (s,f) -> ((if s = old_init then state else s),f)) sr.var_map;
         trans_fun = substitute_state_trans sr.trans_fun old_init state;
         extern_transitions = List.map (substitute_state_atom_trans old_init state) sr.extern_transitions;
         wb_transitions = List.map (substitute_state_atom_trans old_init state) sr.wb_transitions; 
         invariants = sr.invariants
       }
  in free_state old_init;
     (sr',isExt',isWB',preds,p_back_trans')       

let basic_union init_state (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2) =
  let aux _ l1 l2 = match (l1,l2) with
    | (None,None) -> None
    | (None,Some l2') -> Some l2'
    | (Some l1',None) -> Some l1'
    | (Some l1',Some l2') -> Some (l1'@l2') in
  let sr = 
    { states = States.union sr1.states sr2.states; 
      init_state = init_state;
      incons_states = States.union sr1.incons_states sr2.incons_states;
      var_map = sr1.var_map@sr2.var_map;
      trans_fun = StateMap.merge aux sr1.trans_fun sr2.trans_fun;
      extern_transitions = sr1.extern_transitions@sr2.extern_transitions;
      wb_transitions = sr1.wb_transitions@sr2.wb_transitions;      
      invariants = sr1.invariants@sr2.invariants 
    } in
  (sr,States.union isExt1 isExt2,States.union isWB1 isWB2,preds1@preds2,p_back_trans1@p_back_trans2)      
  
let union ((sr1,isExt1,isWB1,preds1,p_back_trans1) as esr1) ((sr2,isExt2,isWB2,preds2,p_back_trans2) as esr2) =
  let i = sr1.init_state in
  let j = sr2.init_state in
  match (i < j,i=j) with
   | (true,_) -> Debug.print_debug ("Merging1 " ^ (string_of_state j) ^ " into " ^ (string_of_state i));
                 let (sr2,isExt2,isWB2,preds2,p_back_trans2) = substitute_state_esr i esr2  in
                 basic_union i (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2)
   | (false,true) -> failwith "Error trying to build the WTS: We are trying to merge two ESRs which already share the same initial state"
   | (false,false) -> Debug.print_debug ("Merging2 " ^ (string_of_state i) ^ " into " ^ (string_of_state j));
                      let (sr1,isExt1,isWB1,preds1,p_back_trans1) = substitute_state_esr j esr1  in
                      basic_union j (sr1,isExt1,isWB1,preds1,p_back_trans1) (sr2,isExt2,isWB2,preds2,p_back_trans2)

let build_esr_ruleO polarity = function
  | [] -> failwith "Error trying to build the WTS: the rule V or K does not contain any premise"
  | esr::esrs ->
      let new_init_state = fresh_state () in
      let (sr,isExt,isWB,preds,p_back_trans) = List.fold_left (basic_union new_init_state) esr esrs in      
      let wb_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isWB) in
      let extern_transitions = List.map (fun state' -> (new_init_state, state')) (States.elements isExt) in
      let o_transitions = List.map (fun (sr,_,_,_,_) -> OT (sr.init_state,polarity)) (esr::esrs) in
      sr.states <- States.add new_init_state sr.states;  
      sr.init_state <- new_init_state;        
      sr.wb_transitions <- wb_transitions@sr.wb_transitions;
      sr.extern_transitions <- wb_transitions@extern_transitions@sr.extern_transitions;      
      sr.trans_fun <- add_list_trans sr.trans_fun new_init_state o_transitions;
      begin match polarity with
        | OQ -> (sr,States.empty,States.empty,preds,p_back_trans)
        | OA -> (sr,States.empty,isWB,preds,p_back_trans)
        | _ -> failwith "Wrong polarity"
      end  

let add_incons_trans (hpre1,hpre2,hpost1,hpost2,preds,preds',var_ctx,polarity) ((sr,_,_,_,_) as esr) =
  let s' = fresh_state() in
  let neg_preds = simplify_arith_pred (negate_arith_pred (AAnd preds)) in  
  let full_neg_preds = simplify_arith_pred (AAnd (neg_preds::preds')) in   
  let incons_trans = PT (s',(hpre1,hpre2,hpost1,hpost2,full_neg_preds,var_ctx,polarity)) in
  let preds = if preds = [AFalse] then [] else preds in
  let preds'' = simplify_arith_pred (AAnd (preds'@preds)) in
  let inv = freshen_inv (hpre1,hpre2,ATrue,preds'') in
  Debug.print_debug ("Possible invariant: " ^ (string_of_invariant inv));  
  sr.invariants <- inv::sr.invariants;
  sr.states <- States.add s' sr.states;
  sr.incons_states <- States.add s' sr.incons_states;
  sr.trans_fun <- add_trans sr.trans_fun sr.init_state incons_trans;
  esr  
     
let build_pintern_trans sequent ((sr1,_,_,_,_) as esr1) ((sr2,_,_,preds,_) as esr2,(tag,hpre1,hpre2,hpost1,hpost2),sequent') =
  let (preds',_) = newelem_of_sequents sequent sequent' in
  let polarity = polarity_from_tag tag in
  let full_preds = simplify_arith_pred (AAnd (preds@preds')) in
  let neg_preds = simplify_arith_pred (negate_arith_pred (AAnd preds)) in
  let full_neg_preds = simplify_arith_pred (AAnd (neg_preds::preds')) in  
  match (Logic_to_smt.check_sat sequent'.ground_var_ctx [full_preds],Logic_to_smt.check_sat sequent'.ground_var_ctx [full_neg_preds]) with
    | (false,false) -> esr1
    | (false,true) -> add_incons_trans (hpre1,hpre2,hpost1,hpost2,preds,preds',sequent'.ground_var_ctx,polarity) esr1
    | (_,b) ->
      let trans = PT (sr2.init_state,(hpre1,hpre2,hpost1,hpost2,full_preds,sequent'.ground_var_ctx,polarity)) in    
      let (sr,isExt,isWB,_,back_trans) = basic_union sr1.init_state esr1 esr2 in
      let isExt' = if (tag = Extern) then States.add (sr2.init_state) isExt else isExt in
      let isWB' = if (tag = WB) then States.add (sr2.init_state) isWB else isWB in      
      sr.trans_fun <- add_trans sr.trans_fun sr.init_state trans;
      let esr = (sr,isExt',isWB',[],back_trans) in
      if not b then esr else add_incons_trans (hpre1,hpre2,hpost1,hpost2,preds,preds',sequent'.ground_var_ctx,polarity) esr
     
let build_esr_ruleP sequent esrs_a = 
  let sr = singleton_sr () in 
  let esr = (sr,States.empty,States.empty,[],[]) in
  List.fold_left (build_pintern_trans sequent) esr esrs_a

let rec build_esr bc = function
  | RuleVG sequent -> let sr = singleton_sr () in (sr,States.empty,States.empty,[extract_pred_from_vg sequent],[])
  | Stop _ | ROut _   -> let sr = singleton_sr () in  (sr,States.empty,States.empty,[AFalse],[])
  | LOut _   -> 
    let sr = singleton_sr () in
    let preds = if bc then [ATrue] else [AFalse] in
    (sr,States.empty,States.empty,preds,[])     
  | RuleVProd ((tc1,tc2),_) | RuleSext ((tc1,tc2),_) ->
      let esr1 = build_esr bc tc1 in
      let esr2 = build_esr bc tc2 in      
      union esr1 esr2
  | Unfold (tc,_) | LUnfold (tc,_) | RUnfold (tc,_)  | Rewrite (tc,_) -> build_esr bc tc    
  | RuleV (tcs,_) -> let esrs = List.map (build_esr bc) tcs in build_esr_ruleO OQ esrs
  | RuleK (tcs,_) -> let esrs = List.map (build_esr bc) tcs in build_esr_ruleO OA esrs
  | RuleE (tcs_a,sequent) -> 
      let esrs_a = List.map (fun (annotation,tc) -> (build_esr bc tc,annotation,get_root tc)) tcs_a in
      build_esr_ruleP sequent esrs_a
  | Gen (gsubst,tc,_) ->
      let (sr,isExt,isWB,preds,p_back_trans) = build_esr bc tc in
      let new_init_state = fresh_state () in
      sr.trans_fun <- add_trans sr.trans_fun new_init_state (PET (sr.init_state,gsubst));
      sr.init_state <- new_init_state;
      let premise_sequent = get_root tc in
      Debug.print_debug ("Gen id " ^ (string_of_int premise_sequent.id));
      let (p_back_trans_now,p_back_trans_later) = List.partition (fun (_,id,_) -> id = premise_sequent.id) p_back_trans in
      let back_trans = List.map (fun (state,_,gsubst) -> (state,PET (new_init_state,gsubst))) p_back_trans_now in
      sr.trans_fun <- List.fold_left (fun trans_fun (s,trans) -> (add_trans trans_fun s trans)) sr.trans_fun back_trans;
      (sr,isExt,isWB,preds,p_back_trans_later)
  | Circ (gsubst,back_sequent,_) -> 
      let sr = singleton_sr () in 
      (sr,States.empty,States.empty,[],[(sr.init_state,back_sequent.id,gsubst)])   

(*let remove_useless_state tc =
    let n = (States.max_elt sr.states) in
    let visited = Array.make n false in*)
    

  
let build_sr bc tc =
  match  build_esr bc tc with
    | (sr,_,_,_,[]) -> sr
    | (_,_,_,_,_) -> failwith ("Error, the generation of back-transition is not finished. "); 
(*    List.iter (fun (s,id,_) -> print_endline ("(" ^ (string_of_state s) ^ "," ^ (string_of_int id) ^ ") "))*)
  
