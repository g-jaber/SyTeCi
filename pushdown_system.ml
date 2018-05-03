open Pmap
open Syntax
open Logic
open Wts

type pstate = int*bool list  

let string_of_pstate (s,pred_val) =
  "(" ^ (Wts.string_of_state s) ^ "," ^ (String.concat "," ((List.map string_of_bool) pred_val)) ^ ")"

module PStates = Set.Make( 
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)
    type t = pstate
  end )  
  
module PStateMap = Map.Make(
  struct
    let compare = Pervasives.compare (* TODO: Change this ! *)    
    type t = pstate
  end)

type pushdown_system = { 
    mutable pstates : PStates.t;
    mutable init_pstate : pstate;
    mutable incons_pstates : States.t;
    mutable transitions : ((pstate*polarity) list) PStateMap.t; 
  }

let get_ltrans mtrans s =
  try PStateMap.find s mtrans with
    | Not_found -> []

let add_trans mtrans (s,s',l)  =
  let ltrans = get_ltrans mtrans s in
  PStateMap.add s ((s',l)::ltrans) mtrans

let dlabel_from_pstate ps ((a,_) as s)  =
  let l = string_of_pstate s in
  let shape = if States.mem a ps.incons_pstates then "diamond" else if s = ps.init_pstate then "box" else "circle" in 
  l ^ "[shape = "^ shape ^", label=\""^ l ^ "\"];"

let degde_from_transition s1 (s2,polarity) = 
  let n1 = string_of_pstate s1 in
  let n2 = string_of_pstate s2 in
  n1 ^ " -> " ^ n2 ^ "[color=red,label=\""^ (string_of_polarity polarity) ^"\"];"
  
let dot_from_ps ps =
  print_endline "//DOT";
  print_endline "digraph R {";
  PStates.iter (fun x -> print_endline ((dlabel_from_pstate ps x) ^ " ")) ps.pstates;
  print_newline ();
  List.iter (fun (s1,l) -> List.iter (fun y -> print_endline (degde_from_transition s1 y)) l) (PStateMap.bindings ps.transitions);
  print_endline "}";
