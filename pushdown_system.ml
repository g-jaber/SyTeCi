open Pmap
open Syntax
open Logic

type pstate = int*bool list

let string_of_pstate (s,pred_val) =
  "(" ^ (Wts.string_of_state s) ^ "," ^ (String.concat "," ((List.map string_of_bool) pred_val)) ^ ")"

let compare_pstate (n,_) (m,_) = Pervasives.compare n m

module PStates = Set.Make(
  struct
    let compare = compare_pstate
    type t = pstate
  end )

module PStateMap = Map.Make(
  struct
    let compare = compare_pstate
    type t = pstate
  end)

type polarity = PI | PQ | PA | PE | OA | OQ | OE

let string_of_polarity = function
  | PI -> "PI"
  | PQ -> "PQ"
  | PA -> "PA"
  | PE -> "PE"
  | OQ -> "OQ"
  | OA -> "OA"
  | OE -> "OE"

let ppolarity_to_polarity = function
  | Wts.PI -> PI
  | Wts.PQ -> PQ
  | Wts.PA -> PA

let opolarity_to_polarity = function
  | Wts.OQ -> OQ
  | Wts.OA -> OA

type pushdown_system = {
    mutable pstates : PStates.t;
    mutable init_pstate : pstate;
    mutable incons_pstates : Wts.States.t;
    mutable ptrans_fun : ((pstate*polarity) list) PStateMap.t;
  }

let get_ltrans trans_fun s =
  try PStateMap.find s trans_fun with
    | Not_found -> []

let add_trans trans_fun (s,s',l)  =
  let ltrans = get_ltrans trans_fun s in
  PStateMap.add s ((s',l)::ltrans) trans_fun

let add_list_trans trans_fun s ltrans  =
  let ltrans' = get_ltrans trans_fun s in
  PStateMap.add s (ltrans@ltrans') trans_fun


let dlabel_from_pstate push_sys state_map ((s,_) as ps)  =
  let l = string_of_pstate ps in
  let a = Wts.string_of_state (PStateMap.find ps state_map) in
  let shape = if Wts.States.mem s push_sys.incons_pstates then "diamond" else if ps = push_sys.init_pstate then "box" else "circle" in
  a ^ "[shape = "^ shape ^", label=\""^ l ^ "\"];"

let degde_from_transition state_map ps1 (ps2,polarity) =
  let n1 = Wts.string_of_state (PStateMap.find ps1 state_map) in
  let n2 = Wts.string_of_state (PStateMap.find ps2 state_map) in
  match polarity with
    | PI | PQ | PA | PE ->   n1 ^ " -> " ^ n2 ^ "[color=blue,label=\""^ (string_of_polarity polarity) ^"\"];"
    | OQ | OA -> n1 ^ " -> " ^ n2 ^ "[color=red,label=\""^ (string_of_polarity polarity) ^"\"];"
    | OE ->   n1 ^ " -> " ^ n2 ^ "[color=red, style=dotted, label=\""^ (string_of_polarity polarity) ^"\"];"

let generate_state_map push_sys =
  let state_map = PStateMap.empty in
  let aux ps state_map =
    let s = Wts.fresh_state () in
    PStateMap.add ps s state_map
  in PStates.fold aux push_sys.pstates state_map

let dot_from_push_sys push_sys =
  let state_map = generate_state_map push_sys in
  print_endline "//DOT";
  print_endline "digraph R {";
  PStates.iter (fun x -> print_endline ((dlabel_from_pstate push_sys state_map x) ^ " ")) push_sys.pstates;
  print_newline ();
  List.iter (fun (ps1,l) -> List.iter (fun y -> print_endline (degde_from_transition state_map ps1 y)) l) (PStateMap.bindings push_sys.ptrans_fun);
  print_endline "}";
