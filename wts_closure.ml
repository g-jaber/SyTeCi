open Wts

let adjacency_matrix sr =
  let n = (States.max_elt sr.states) in
  let matrix_extern = Array.make_matrix (n+1) (n+1) false in
  let matrix_oeps = Array.make_matrix (n+1) (n+1) false in  
  let matrix_wb = Array.make_matrix (n+1) (n+1) false in  
  List.iter (fun (i,j) -> matrix_extern.(i).(j) <- true) sr.extern_transitions;
(*  List.iter (fun (i,j) -> matrix_oeps.(i).(j) <- true) sr.oeps_transitions;  *)
  List.iter (fun (i,j) -> matrix_wb.(i).(j) <- true) sr.wb_transitions;  
  (matrix_extern,matrix_oeps,matrix_wb)

(* Careful: Impure function !! *)
let matrix_to_trans sr matrix =
  let n = Array.length matrix in
  let result = ref [] in
  for i = 0 to n-1 do
    result := [];
    for j = 0 to n-1 do
      if matrix.(i).(j) then result := (OET j)::!result else ()
    done;
    sr.trans_fun <- add_list_trans sr.trans_fun i !result;
  done

let print_matrix matrix = 
  let n = Array.length matrix in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      print_string ((string_of_bool matrix.(i).(j)) ^ " ");
    done;
    print_newline ();
  done  
  
let copy_matrix matrix =
  let n = Array.length matrix in
  let result = Array.make_matrix n n false in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      result.(i).(j) <- matrix.(i).(j)
    done;
  done;
  result

let product_matrix matrix1 matrix2 = 
  let n = Array.length matrix1 in (* We always consider square matrices *)
  let result = copy_matrix matrix2 in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
        result.(i).(j) <-result.(i).(j) || (matrix1.(i).(k) && matrix2.(k).(j))
      done;
    done;
  done;  
  result

let reflexive_closure matrix = 
  let n = Array.length matrix in (* We always consider square matrices *)
  let result = copy_matrix matrix in
  for i = 0 to n-1 do
    result.(i).(i) <- true
  done;
  result  
  

let transitive_closure matrix = (* The transitive closure is implemented via a standard Warshall algorithm *)
  let n = Array.length matrix in (* We always consider square matrices *)
  let result = copy_matrix matrix in
  for k = 0 to n-1 do
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        result.(i).(j) <- result.(i).(j) || (result.(i).(k) && result.(k).(j))
      done;
    done;
  done;
  result  
  
let isOQ = function
  | OT (_,OQ) -> true
  | _ -> false

let isOA = function
  | OT (_,OA) -> true
  | _ -> false

let get_isRuleV sr = List.map (fun (s,_) -> s) ((List.filter (fun (s,l) -> List.exists isOQ l)) (StateMap.bindings sr.trans_fun))
let get_isRuleK sr = List.map (fun (s,_) -> s) ((List.filter (fun (s,l) -> List.exists isOA l)) (StateMap.bindings sr.trans_fun))

(*let prune_noentry_states =
  let is_reach = ref false in
  let result = ref [] in
  let n = Array.length matrix in
  for i = 0 to n-1 do
    is_reach := false;
    for j = 0 to n-1 do
     is_reach := !is_reach || matrix.(j).(i)
    done;
    if (!is_reach) then () else result := i::!result 
  done;
  !result*)
  

let sr_closure_aux sr =
  let (matrix_extern,matrix_oeps,matrix_wb) = adjacency_matrix sr in
  let matrix_refl_oeps = reflexive_closure matrix_oeps in
  let matrix_trans_extern = transitive_closure (product_matrix matrix_refl_oeps matrix_extern) in
  let matrix_trans_wb = transitive_closure (product_matrix matrix_refl_oeps matrix_wb) in  
  let aux matrix i =
    let n = Array.length matrix in
    for j = 0 to n-1 do 
      if matrix.(i).(j) then matrix_oeps.(j).(i) <- true else ()
    done
  in
  List.iter (aux matrix_trans_extern) (get_isRuleV sr);
  List.iter (aux matrix_trans_wb)  (get_isRuleK sr);
  matrix_to_trans sr matrix_oeps;
  sr

let rec fixed_point f x =
  let x' = f x in if x' = x then x else fixed_point f x'
  
let sr_closure sr = fixed_point sr_closure_aux sr  
