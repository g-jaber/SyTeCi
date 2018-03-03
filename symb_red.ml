open Syntax
open Logic
open Pmap


let add_locvar heap v = let l = fresh_locvar () in (l,(l,v)::heap)  

let aux g (a,b,c,d,e,f) = (g a,b,c,d,e,f)

let aux_bin_red symbred cons_op = function
    | (expr1,expr2) when isval expr1 -> let (result,b) = symbred expr2 in (List.map (aux (fun x -> (cons_op (expr1,x)))) result,b)
    | (expr1,expr2) -> let (result,b) = symbred expr1 in (List.map (aux (fun x -> (cons_op (x,expr2)))) result,b)

let aux_bin_arith cons heapPost symbred = 
  let (expr1,expr2,arith_op,cons_op) = Syntax.get_aop_from_expr cons in match (expr1, expr2) with
    | (Int n1, Int n2) -> let n = arith_op n1 n2 in ([(Int n,[],[],heapPost,[],[])],true)    
    | (Int n, Var x) -> let newid = fresh_lvar () in 
                        let newvar = Var newid in
                        ([(newvar,[],[],heapPost,[(newid,TInt)],[AEqual (AExpr newvar, AExpr(cons_op (Int n,Var x)))])],true) 
    | (Var x, Int n) -> let newid = fresh_lvar () in 
                        let newvar = Var newid in
                        ([(newvar,[],[],heapPost,[(newid,TInt)],[AEqual (AExpr newvar, AExpr(cons_op  (Var x,Int n)))])],true)
    | (Var x1, Var x2) -> let newid = fresh_lvar () in 
                        let newvar = Var newid in
                        ([(newvar,[],[],heapPost,[(newid,TInt)],[AEqual (AExpr newvar, AExpr(cons_op (Var x1,Var x2)))])],true)
    | (expr1,expr2) -> aux_bin_red symbred cons_op (expr1,expr2)

let aux_bin_arithbool cons heapPost symbred = 
  let (expr1,expr2,arithbool_op,cons_op) = Syntax.get_abop_from_expr cons in match (expr1, expr2) with
    | (Int n1, Int n2) -> let b = arithbool_op n1 n2 in ([(Bool b,[],[],heapPost,[],[])],true)    
    | (Int n, Var x) -> ([(Bool true,[],[],heapPost,[],[expr_to_arith_pred (cons_op (Int n,Var x))]);(Bool false,[],[],heapPost,[],[negate_arith_pred (expr_to_arith_pred (cons_op (Int n,Var x)))])],true)
    | (Var x, Int n) -> ([(Bool true,[],[],heapPost,[],[expr_to_arith_pred (cons_op (Var x,Int n))]);(Bool false,[],[],heapPost,[],[negate_arith_pred (expr_to_arith_pred (cons_op (Var x,Int n)))])],true)
    | (Var x1, Var x2) -> ([(Bool true,[],[],heapPost,[],[expr_to_arith_pred (cons_op (Var x1,Var x2))]);(Bool false,[],[],heapPost,[],[negate_arith_pred (expr_to_arith_pred (cons_op (Var x1,Var x2)))])],true)
    | (expr1,expr2) -> aux_bin_red symbred cons_op (expr1,expr2) 
    
let aux_bin_bool cons heapPost symbred = 
  let (expr1,expr2,bool_op,cons_op) = Syntax.get_bop_from_expr cons in match (expr1, expr2) with
    | (Bool b1, Bool b2) -> let b = bool_op b1 b2 in ([(Bool b,[],[],heapPost,[],[])],true)    
(*    | (Bool b, Var x) -> let newvar = fresh_lvar () in [(newvar,[],[],heapPost,[AEqual (AExpr newvar, AExpr(cons_op (Bool b,Var x)))])]
    | (Var x, Bool b) -> let newvar = fresh_lvar () in [(newvar,[],[],heapPost,[AEqual (AExpr newvar, AExpr(cons_op  (Var x,Bool b)))])] 
    | (Var x1, Var x2) -> let newvar = fresh_lvar () in [(newvar,[],[],heapPost,[AEqual (AExpr newvar, AExpr(cons_op (Var x1,Var x2)))])]*)
    | _ -> aux_bin_red symbred cons_op (expr1,expr2)

    
let rec symbred heapPost expr = match expr with
  | App (Fun (var,_,expr1),expr2) when (isval expr2) -> ([(subst expr1 (Var var) expr2, [], [], heapPost, [], [])],true)
  | App (Fix (idfun,_,var,_,expr1) as expr',expr2) when (isval expr2) -> ([(subst expr1 (Var var) expr2, [(idfun,expr')], [], heapPost, [], [])],true)
  | App (expr1,expr2) -> aux_bin_red (symbred heapPost) (fun (x,y) -> App (x,y)) (expr1,expr2)
  | Seq (Unit,expr2) -> ([(expr2,[],[],heapPost,[], [])],true)
  | Seq (expr1,expr2) -> let (result,b) = symbred heapPost expr1 in (List.map (aux (fun x -> (Seq (x,expr2)))) result,b)
  | Pair (expr1,expr2) -> aux_bin_red (symbred heapPost) (fun (x,y) -> Pair (x,y)) (expr1,expr2)
  | Let (var,expr1,expr2) -> if (isval expr1) then ([(subst expr2 (Var var) expr1, [], [], heapPost, [], [])],true)
                             else let (result,b) = symbred heapPost expr1 in (List.map (aux (fun x -> (Let (var,x,expr2)))) result,b)
  | Newref expr -> if (isval expr) then let (l,heapPost') = add_locvar heapPost expr in ([(Var l,[],[],heapPost',[(l,TRef TInt)], [])],true) (* Fix This *)
                else let (result,b) = symbred heapPost expr in (List.map (aux (fun x -> (Newref x))) result,b)
  | Deref (Var l) -> begin match lookup_pmap l heapPost with
                       | Some value -> ([(value,[],[],heapPost,[], [])],true)
                       | None -> let x = Logic.fresh_lvar () in
                                 let heapPre = [(l,Var x)] in ([(Var x,[],heapPre,heapPre@heapPost,[(x,TInt)], [])],true) (* Fix This *)
                     end            
  | Deref expr -> let (result,b) = symbred heapPost expr in (List.map (aux (fun x -> (Deref x))) result,b)
  | Assign (Var l,expr2) when (isval expr2) ->  
      begin match lookup_pmap l heapPost with
        | Some _ ->  ([(Unit,[],[],modadd_pmap l expr2 heapPost,[], [])],true)
        | None -> let x = Logic.fresh_lvar () in
                  let heapPre = [(l,Var x)] in ([(Unit,[],heapPre,modadd_pmap l expr2 heapPost,[(x,TInt)], [])],true)
      end            
  | Assign (expr1,expr2) ->  aux_bin_red (symbred heapPost) (fun (x,y) -> Assign (x,y)) (expr1,expr2)
  | If (Bool b,expr1,expr2) -> if b then ([(expr1,[],[],heapPost,[], [])],true) else ([(expr2,[],[],heapPost,[], [])],true)
  | If ((Var _),_,_) -> failwith "Error: Boolean variables are not allowed in the symbolic reduction"
  | If (expr,expr1,expr2) -> let (result,b) = symbred heapPost expr in (List.map (aux (fun x -> (If (x,expr1,expr2)))) result,b)
  | Plus _ | Minus _ | Mult _ | Div _ -> aux_bin_arith expr heapPost (symbred heapPost)
  | And _ | Or _ -> aux_bin_bool expr heapPost (symbred heapPost)
  | Not (Bool b) -> ([(Bool (not b),[],[],heapPost,[], [])],true)
(*  | Not (Var b) -> [(Bool true, [],[],heapPost,[AEqual (AExpr(Var b)),AExpr (Bool false)]);(Bool false, [], [], heapPost,[AEqual (AExpr(Var b)),AExpr (Bool true)])]*)
  | Not expr -> let (result,b) = symbred heapPost expr in (List.map (aux (fun x -> (Not x))) result,b)  
  | Equal _ | NEqual _ | Less _ | LessEq _ | Great _ | GreatEq _ -> aux_bin_arithbool expr heapPost (symbred heapPost)
  | _ -> ([(expr,[],[],heapPost,[], [])],false)
  
  
let rec symbred_trans (expr,gamma) heapPre heapPost vars preds =
  let aux (expr',gamma',heapPre',heapPost',vars',preds') = ((expr',gamma'@gamma),heapPre'@heapPre,heapPost',vars'@vars,preds'@preds) in
  let (result,b) = symbred heapPost expr in
  if (not b) then List.map aux result 
  else List.flatten (List.map (fun (expr',gamma',heapPre',heapPost',vars',preds') -> symbred_trans (expr',gamma'@gamma) (heapPre'@heapPre) heapPost' (vars'@vars) (preds'@preds)) result) 
