open Wts
open Unif

let dlabel_from_state sr s =
  let l = string_of_state s in
  let shape = if States.mem s sr.incons_states then "diamond"
    else if s = sr.init_state then "box" else "circle" in
  l ^ "[shape = "^ shape ^", label=\""^ l ^ "\"];"


let dedge_from_transition s1 = function
  | PT (s2,label) ->
    (string_of_state s1) ^ " -> " ^ (string_of_state s2)
    ^ "[color=blue, label=\""^ (string_of_label label) ^ "\"];"
  | PET (s2,gsubst) | PBT (s2,gsubst) ->
    (string_of_state s1) ^ " -> " ^ (string_of_state s2)
    ^ "[color=blue, style=dotted,label=\""^ (string_of_gsubst gsubst) ^"\"];"
  | OT (s2,polarity) ->
    (string_of_state s1) ^ " -> " ^ (string_of_state s2)
    ^ "[color=red,label=\""^ (string_of_opolarity polarity) ^"\"];"
  | OET s2 ->
    (string_of_state s1) ^ " -> " ^ (string_of_state s2)
    ^ "[color=red, style=dotted];"


let degde_from_atomic_transition (s1,s2) =
  let n1 = string_of_state s1 in
  let n2 = string_of_state s2 in
  n1 ^ " -> " ^ n2 ^ ";"


let dot_from_sr file sr =
  Printer.print_to_file file "//DOT";
  Printer.print_to_file file "digraph R {";
  States.iter (fun x -> Printer.print_to_file file ((dlabel_from_state sr x) ^ " ")) sr.states;
  Printer.print_to_file file "\n";
  Printer.print_to_file file ("//Transitions: ");
  List.iter (fun (s1,l) -> List.iter (fun trans -> Printer.print_to_file file (dedge_from_transition s1 trans)) l)
    (StateMap.bindings sr.trans_fun);
  Printer.print_to_file file "}";
