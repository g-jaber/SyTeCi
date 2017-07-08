open Logic
open Syntax
open Pmap
open Skor


type kindTerm = IsVal | IsCallExtern of (id*typeML*exprML*eval_context) | IsRecCall of (id*exprML*exprML*eval_context)
  
let kind_of_term cb_context (expr,gamma) = 
  if isval expr then IsVal 
  else begin let (f,v,ctx) = extract_call expr in
    begin match Pmap.lookup_pmap f cb_context with
      | None -> begin match Pmap.lookup_pmap f gamma with
                  | None -> failwith ("The variable " ^ f ^ " comes from nowhere.")
                  | Some ty -> IsRecCall (f,ty,v,ctx)
                end
      | Some ty -> IsCallExtern (f,ty,v,ctx)
    end
  end 

let select_tag cb_context (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term cb_context (expr1,gamma1), kind_of_term cb_context (expr1,gamma1)) with
    | (IsVal,IsVal) -> WB
    | (IsCallExtern _,IsCallExtern _) -> Extern    
    | (IsRecCall _,_) -> Intern
    | (_,IsRecCall _) -> Intern
    | (_,_) -> Wrong
    
let get_elem_callexts cb_context (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term cb_context (expr1,gamma1), kind_of_term cb_context (expr2,gamma2)) with
   | (IsCallExtern (f1,ty1,v1,k1), IsCallExtern (f2,ty2,v2,k2)) when f1 = f2 -> Some (ty1,k1,v1,k2,v2)
   | _ -> None

  

type tc_struct = 
  | Stop of sequent
  | RuleVG of sequent
  | RuleVProd of (tc_struct * tc_struct) * sequent
  | RuleSext of (tc_struct * tc_struct) * sequent
  | LOut of sequent
  | ROut of sequent  
  | RuleV of (tc_struct list)*sequent
  | RuleK of (tc_struct list)*sequent
  | Unfold of tc_struct*sequent
  | LUnfold of tc_struct*sequent
  | RUnfold of tc_struct*sequent  
  | RuleE of (tc_struct list)*sequent
  | Rewrite of tc_struct*sequent
  | Circ of sequent*sequent  

let get_root = function
  | Stop sequent -> sequent    
  | RuleVG sequent -> sequent
  | RuleVProd (_, sequent) -> sequent
  | RuleSext (_, sequent) -> sequent  
  | LOut sequent -> sequent
  | ROut sequent -> sequent  
  | RuleV (_,sequent) -> sequent
  | RuleK (_,sequent) -> sequent
  | Unfold (_,sequent) -> sequent
  | LUnfold (_,sequent) -> sequent
  | RUnfold (_,sequent) -> sequent  
  | RuleE (_,sequent) -> sequent
  | Rewrite (_,sequent) -> sequent
  | Circ (_,sequent) -> sequent  
  
let rec extract_temporal tc = match tc with
  | Stop sequent -> tc
  | RuleVG _ -> tc
  | RuleVProd ((tc1,tc2),sequent) -> RuleVProd ((extract_temporal tc1,extract_temporal tc2),sequent)    
  | LOut sequent -> tc
  | ROut sequent -> tc  
  | RuleV (tcs,sequent) ->  RuleV (List.map extract_temporal tcs,sequent)
  | RuleK (tcs,sequent) -> RuleK (List.map extract_temporal tcs,sequent)
  | RuleSext ((tc1,tc2),sequent) -> RuleSext ((extract_temporal tc1,extract_temporal tc2),sequent)  
  | Unfold (tc',sequent) ->  Unfold (extract_temporal tc',sequent)
  | LUnfold (tc',sequent) -> LUnfold (extract_temporal tc',sequent)
  | RUnfold (tc',sequent) -> RUnfold (extract_temporal tc',sequent)
  | RuleE (tcs,sequent) -> RuleE (List.map extract_temporal tcs,sequent)
  | Rewrite (tc',sequent) -> Rewrite (extract_temporal tc',sequent)
  | Circ (_,sequent) -> failwith "Not supported yet"  

let rec string_of_list = function
  | [] -> ""
  | [str] -> str
  | str::tl -> str ^ "," ^ (string_of_list tl)
  
let rec string_of_tc = function
  | Stop sequent -> "Stop "
  | RuleVG sequent -> "RuleVG "
  | LOut sequent -> "LOut "
  | ROut sequent -> "LOut "
  | RuleVProd ((tc1,tc2),sequent) -> "RuleVx (" ^ (string_of_tc tc1) ^ "," ^ (string_of_tc tc2) ^")"    
  | RuleV (tcs,sequent) ->  "RuleV (" ^ (string_of_list (List.map string_of_tc tcs)) ^")"
  | RuleK (tcs,sequent) -> "RuleK (" ^ (string_of_list (List.map string_of_tc tcs)) ^")"
  | RuleSext ((tc1,tc2),sequent) -> "RuleSext (" ^ (string_of_tc tc1) ^ "," ^ (string_of_tc tc2) ^")"  
  | Unfold (tc',sequent) ->  "Unfold (" ^ (string_of_tc tc') ^")"
  | LUnfold (tc',sequent) -> "LUnfold (" ^ (string_of_tc tc') ^")"
  | RUnfold (tc',sequent) -> "RUnfold (" ^ (string_of_tc tc') ^")"
  | RuleE (tcs,sequent) -> "RuleE (" ^ (string_of_list (List.map string_of_tc tcs)) ^")"
  | Rewrite (tc',sequent) -> "Rewrite (" ^ (string_of_tc tc')  ^")"
  | Circ (_,sequent) -> "Circ "  
  
let rec mix_lists g list1 = function
  | [] -> []
  | hd::tl -> (List.map (fun x -> g (x,hd)) list1) @ (mix_lists g list1 tl)  
  

let rec symb_val = function
  | TUnit -> [(Unit,Unit,[])]
  | TBool -> [(Bool true,Bool true,[]);(Bool false, Bool false,[])]
  | TInt -> let x = (Logic.fresh_lvar ()) in
            [(Var x, Var x, [(x,TInt)])]
  | (TArrow (_,_)) as ty -> let x = Logic.fresh_lvar () in  [(Var x, Var x, [(x,ty)])]
  | TProd (ty1,ty2) -> let result1 = symb_val ty1 in
                       let result2 = symb_val ty2 in
                       let g = fun ((vall1,valr1,logenvc1),(vall2,valr2,logenvc2)) -> 
                          (Pair (vall1,vall2), Pair (valr1, valr2), logenvc1@logenvc2) in
                       mix_lists g result1 result2
  | TRef _ -> failwith "Error: Types with occurencences of ref subtypes are not supported."                    
                    
                       
let rec build_tc_rule hist sequent = 
  match (trivially_false sequent.logctx, sequent.formula) with
  | (true,_) -> print_endline ("Stop : " ^ (string_of_arith_pred (AAnd sequent.logctx)));   
                Stop sequent
  | (_,RelV (TArrow (ty1,ty2), cb_context, expr1, expr2, gamma1, gamma2)) ->
    print_endline ("RuleVA : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));     
          let result = symb_val ty1 in 
          let f = fun (sval_l,sval_r,cb_context') ->  new_sequent sequent [] [] [] [] (RelE (ty2, (cb_context @ cb_context'), (App (expr1,sval_l)),(App (expr2,sval_r)), gamma1, gamma2)) in
          let premises = List.map (build_tc_rule hist) (List.map f result) in
          RuleV (premises,sequent)
  | (_,RelV (TProd (ty1,ty2), cb_context, Pair (v11, v12), Pair (v21, v22), gamma1, gamma2)) ->
          let skor1 = RelV (ty1,cb_context,v11,v21,gamma1,gamma2) in
          let skor2 = RelV (ty2,cb_context,v12,v22,gamma1,gamma2) in
                let tc1 = build_tc_rule hist (new_sequent sequent [] [] [] [] skor1) in
                let tc2 = build_tc_rule hist (new_sequent sequent [] [] [] [] skor2) in            
          RuleVProd ((tc1,tc2), sequent)
  | (_,RelV (_, _, expr1, expr2, gamma1, gamma2)) -> RuleVG sequent
  
  | (_,RelSE (ty, cb_context, expr1, expr2, gamma1, gamma2)) ->
      print_endline ("RuleSE : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));  
       begin match (get_elem_callexts cb_context (expr1,gamma1) (expr2,gamma2)) with
            | Some (TArrow (ty1,ty2),k1,v1,k2,v2) -> 
                let skorv = RelV (ty1,cb_context,v1,v2,gamma1,gamma2) in
                let skork = RelK (ty2,ty,cb_context,k1,k2,gamma1,gamma2) in
                let tcv = build_tc_rule hist (new_sequent sequent [] [] [] [] skorv) in
                let tck = build_tc_rule hist (new_sequent sequent [] [] [] [] skork) in                
                RuleSext ((tcv,tck),sequent)
            | None -> print_endline ("Stop in RelSE : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2)); Stop sequent   
        end        
  | (_,RelK (ty1, ty2, cb_context, ctx1, ctx2, gamma1, gamma2)) ->
    print_endline ("RuleK : " ^ (string_of_exprML ctx1) ^ " and " ^ (string_of_exprML ctx2));   
          let result = symb_val ty1 in 
          let f = fun (sval_l,sval_r,logenv_c) ->  
                       new_sequent sequent logenv_c [] [] []
                       (RelE (ty2,cb_context, (fill_hole ctx1 sval_l),(fill_hole ctx2 sval_r), gamma1, gamma2)) in
          let premises = List.map (build_tc_rule hist) (List.map f result) in
          RuleK (premises,sequent)
(*  | (_,RelSI (ty, expr1, expr2, gamma1, gamma2)) ->  
     begin match (kind_of_term sequent.logenvc gamma1 expr1,kind_of_term sequent.logenvc gamma2 expr2,sequent.index) with
       | (IsCallIntern _, _,0) -> print_endline ("LOut : " ^ (string_of_arith_pred (AAnd sequent.logctx)) ^ " and " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2)); LOut sequent
       | (_, IsCallIntern _,0) -> print_endline ("LOut : " ^ (string_of_arith_pred (AAnd sequent.logctx)) ^ " and " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2)); LOut sequent          
       | (IsCallIntern (f1,body1,val1,ctx1), IsCallIntern (f2,body2,val2,ctx2),k) ->
         print_endline ("Unfold : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));       
         let ctx1' = iter 3 (Unif.rewrite_ac (sequent.logenvc@sequent.logenvl)) ctx1 in
         let ctx2' = iter 3 (Unif.rewrite_ac (sequent.logenvc@sequent.logenvr)) ctx2 in
         print_endline ("From : " ^ (string_of_exprML expr1) ^ " to " ^ (string_of_exprML (fill_hole ctx1' (App (Var f1,val1)))));
         print_endline ("From : " ^ (string_of_exprML expr2) ^ " to " ^ (string_of_exprML (fill_hole ctx2' (App (Var f2,val2)))));          
         let sequent' = new_sequent sequent [] [] [] [] (RelSI (ty,fill_hole ctx1' (App (Var f1,val1)),fill_hole ctx2' (App (Var f2,val2)),gamma1,gamma2)) in
         let hist' = sequent'::hist in         
         begin match (Unif.unif_in_hist sequent' hist) with
           | Some (circ_sequent,_,_) -> print_endline ("Circ ! "); Circ (circ_sequent,sequent')
           | None -> let expr1' = fill_hole ctx1' (App (body1,val1)) in
                     let expr2' = fill_hole ctx2' (App (body2,val2)) in
                     let formula = RelE (ty,expr1',expr2',gamma1,gamma2) in
                     let new_tc = build_tc_rule hist' (new_sequent sequent [] [] [] [] ?index:(Some (k-1)) formula) in
                     Rewrite (Unfold (new_tc,sequent'),sequent)
         end          
       | (IsCallIntern (f1,body1,val1,ctx1), _,k) -> 
         print_endline ("LUnfold : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));
         let expr1' = fill_hole ctx1 (App (body1,val1)) in
         let formula = RelE (ty,expr1',expr2,gamma1,gamma2) in
         let new_tc = build_tc_rule hist (new_sequent sequent [] [] [] [] ?index:(Some (k-1)) formula) in
         LUnfold (new_tc,sequent)
       | (_, IsCallIntern (f2,body2,val2,ctx2),k) ->
         print_endline ("RUnfold : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));       
         let expr2' = fill_hole ctx2 (App (body2,val2)) in
         let formula = RelE (ty,expr1,expr2',gamma1,gamma2) in
         let new_tc = build_tc_rule hist (new_sequent sequent [] [] [] [] ?index:(Some (k-1)) formula) in
         Unfold (new_tc,sequent)
       | (_,_,_) -> failwith "RelSI is applied on terms which are not recursive calls"   
     end   *) 
  | (_,RelE (ty, cb_context, expr1, expr2, gamma1, gamma2)) ->
         print_endline ("RelE : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));         
         let result1 = Symb_red.symbred_trans [] [] [] expr1 [] [] in
         let result2 = Symb_red.symbred_trans [] [] [] expr2 [] [] in
         let build_tc_rule_expr (expr1,expr2,gamma1,gamma2,heapPre1,heapPre2,heapPost1,heapPost2,vars,preds) = 
           begin match (select_tag cb_context (expr1,gamma1) (expr2,gamma2)) with
             | WB -> let skor' = RelV (ty,cb_context,expr1,expr2,gamma1,gamma2) in
                     build_tc_rule hist (new_sequent sequent vars [] [] preds ~annot:(Some (AnnotHeap (WB,heapPre1,heapPre2,heapPost1,heapPost2))) skor')            
             | Extern -> let skor' = RelSE (ty,cb_context,expr1,expr2,gamma1,gamma2) in
                   build_tc_rule hist (new_sequent sequent vars [] [] preds ~annot:(Some (AnnotHeap (Extern,heapPre1,heapPre2,heapPost1,heapPost2))) skor')
             | Intern -> 
                   let skor' = RelSI (ty,cb_context,expr1,expr2,gamma1,gamma2) in
                   build_tc_rule hist (new_sequent sequent vars [] [] preds ~annot:(Some (AnnotHeap (Intern,heapPre1,heapPre2,heapPost1,heapPost2))) skor')                
             | Wrong -> print_endline ("Stop in RelE : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2)); Stop sequent      
           end      
         in let g = fun ((expr1,gamma1',heapPre1,heapPost1,vars1,preds1),(expr2,gamma2',heapPre2,heapPost2,vars2,preds2)) 
                    -> (expr1,expr2,gamma1'@gamma1,gamma2'@gamma2,heapPre1,heapPre2,heapPost1,heapPost2,(vars1@vars2),(preds1@preds2)) in
         RuleE (List.map build_tc_rule_expr (mix_lists g result1 result2),sequent)

let diff list1 list2 = List.filter (fun x -> not ((List.mem x) list1)) list2
         
let newelem_of_sequents prev_sequent sequent = (diff prev_sequent.logctx sequent.logctx,diff prev_sequent.logenvc sequent.logenvc)

let newgroundelem_of_sequents prev_sequent sequent = 
  let aux (_,ty) = match ty with
    | TArrow _ -> false
    | _ -> true
  in  
  let env = diff prev_sequent.logenvc sequent.logenvc in
  let env' = List.filter aux env in
  let preds = diff prev_sequent.logctx sequent.logctx in   
  (env',preds)  

let extract_pred_from_vg sequent = 
    match sequent.formula with
        | RelV (TUnit,_,_,_,_,_)  -> ATrue      
        | RelV (TBool,_,Bool b1, Bool b2,_,_) when b1 = b2 -> ATrue
        | RelV (TBool,_,Bool b1, Bool b2,_,_) -> AFalse
        | RelV (TInt,_,v1, v2,_,_) -> (AEqual (AExpr v1,AExpr v2))
        | _ -> failwith "Error: The rule VG has been used on a non-ground type."  
