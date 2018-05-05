open Wts
open Unif

let dlabel_from_state sr s =
  let l = string_of_state s in
  let shape = if States.mem s sr.incons_states then "diamond" else if s = sr.init_state then "box" else "circle" in 
  l ^ "[shape = "^ shape ^", label=\""^ l ^ "\"];"


let dedge_from_transition s1 = function
  | PT (s2,label) ->   (string_of_state s1) ^ " -> " ^ (string_of_state s2) ^ "[color=blue, label=\""^ (string_of_label label) ^ "\"];"
  | PET (s2,gsubst) -> (string_of_state s1) ^ " -> " ^ (string_of_state s2) ^ "[color=blue, style=dotted,label=\""^ (string_of_gsubst gsubst) ^"\"];"  
  | OT (s2,polarity) -> (string_of_state s1) ^ " -> " ^ (string_of_state s2) ^ "[color=red,label=\""^ (string_of_polarity polarity) ^"\"];"
  | OET s2 -> (string_of_state s1) ^ " -> " ^ (string_of_state s2)  ^ "[color=red, style=dotted];"

(*let degde_from_pintern_transition (s1,s2,label) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ "[color=blue, label=\""^ (string_of_label label) ^ "\"];"

let degde_from_external_transition (s1,s2) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ "[style=bold];"    
  
let degde_from_oeps_transition (s1,s2) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ "[color=red, style=dotted];"    
  
let degde_from_o_transition (s1,s2,polarity) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ "[color=red,label=\""^ (string_of_polarity polarity) ^"\"];"

let degde_from_peps_transition (s1,s2,gsubst) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ "[color=blue, style=dotted,label=\""^ (string_of_gsubst gsubst) ^"\"];"  *)
  
let degde_from_atomic_transition (s1,s2) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ ";"
  

let dot_from_sr sr =
  print_endline "//DOT";
  print_endline "digraph R {";
  States.iter (fun x -> print_endline ((dlabel_from_state sr x) ^ " ")) sr.states;
  print_newline ();
  print_endline ("//Transitions: ");  
  List.iter (fun (s1,l) -> List.iter (fun trans -> print_endline (dedge_from_transition s1 trans)) l) (StateMap.bindings sr.trans_fun);
(*  List.iter (fun x -> print_endline (degde_from_transition x)) sr.pintern_transitions;*)
(*  print_newline ();*)
(*  print_endline ("//External Transitions: ");
  List.iter (fun x -> print_endline (degde_from_atomic_transition x)) sr.extern_transitions;
  print_newline ();*)
(*  print_endline ("//WB Transitions: ");
  List.iter (fun x -> print_endline (degde_from_atomic_transition x)) sr.wb_transitions;  
  print_newline ();*)      
  print_endline "}";
