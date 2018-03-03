open Wts
open Unif

let dlabel_from_state sr s =
  let l = string_of_state s in
  let shape = if States.mem s sr.incons_states then "diamond" else if s = sr.init_state then "doublecircle" else "circle" in 
  l ^ "[shape = "^ shape ^", label=\""^ l ^ "\"];"


let degde_from_pintern_transition (s1,s2,label) = 
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
  n1 ^ " -> " ^ n2 ^ "[color=blue, style=dotted,label=\""^ (string_of_gsubst gsubst) ^"\"];"  
  
let degde_from_atomic_transition (s1,s2) = 
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ ";"
  

let dot_from_sr sr =
  print_endline "//DOT";
  print_endline "digraph R {";
  States.iter (fun x -> print_endline ((dlabel_from_state sr x) ^ " ")) sr.states;
  print_newline ();
  print_endline ("//P-Intern Transitions: ");  
  List.iter (fun x -> print_endline (degde_from_pintern_transition x)) sr.pintern_transitions;
  print_newline ();
(*  print_endline ("//External Transitions: ");
  List.iter (fun x -> print_endline (degde_from_atomic_transition x)) sr.extern_transitions;
  print_newline ();*)
(*  print_endline ("//WB Transitions: ");
  List.iter (fun x -> print_endline (degde_from_atomic_transition x)) sr.wb_transitions;  
  print_newline ();*)      
  print_endline ("//P-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (degde_from_peps_transition x)) sr.peps_transitions;
  print_newline ();
  print_endline ("//O Transitions: ");
  List.iter (fun x -> print_endline (degde_from_o_transition x)) sr.o_transitions;
  print_newline ();
  print_endline ("//O-Epsilon Transitions: ");
  List.iter (fun x -> print_endline (degde_from_oeps_transition x)) sr.oeps_transitions;
  print_newline ();
  print_endline "}";
