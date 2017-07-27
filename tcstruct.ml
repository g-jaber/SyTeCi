open Logic
open Syntax
open Pmap
open Skor
open Unif


type kindTerm = IsVal | IsCallExtern of (id*typeML*exprML*eval_context) | IsRecCall of (id*exprML*exprML*eval_context)
  
let kind_of_term funct_var_ctx (expr,gamma) = 
  if isval expr then IsVal 
  else begin let (f,v,ctx) = extract_call expr in
    begin match Pmap.lookup_pmap f funct_var_ctx with
      | None -> begin match Pmap.lookup_pmap f gamma with
                  | None -> failwith ("The variable " ^ f ^ " comes from nowhere.")
                  | Some ty -> IsRecCall (f,ty,v,ctx)
                end
      | Some ty -> IsCallExtern (f,ty,v,ctx)
    end
  end 

let select_tag funct_var_ctx (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term funct_var_ctx (expr1,gamma1), kind_of_term funct_var_ctx (expr1,gamma1)) with
    | (IsVal,IsVal) -> WB
    | (IsCallExtern _,IsCallExtern _) -> Extern    
    | (IsRecCall _,_) -> Intern
    | (_,IsRecCall _) -> Intern
    | (_,_) -> Wrong
    
let get_elem_callexts funct_var_ctx (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term funct_var_ctx (expr1,gamma1), kind_of_term funct_var_ctx (expr2,gamma2)) with
   | (IsCallExtern (f1,ty1,v1,k1), IsCallExtern (f2,ty2,v2,k2)) when f1 = f2 -> Some (ty1,(k1,gamma1),(v1,gamma1),(k2,gamma2),(v2,gamma2))
   | _ -> None

type annot_rele = tag*symbheap*symbheap*symbheap*symbheap  

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
  | RuleE of ((annot_rele*tc_struct) list)*sequent
  | Rewrite of tc_struct*sequent
  | Circ of gsubst*sequent*sequent
  | Gen of gsubst*tc_struct*sequent    

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
  | Circ (_,_,sequent) -> sequent 
  | Gen (_,_,sequent) -> sequent  

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
  | RuleE (tcs,sequent) -> "RuleE (" ^ (string_of_list (List.map (fun (_,tc) -> (string_of_tc tc)) tcs)) ^")"
  | Rewrite (tc',sequent) -> "Rewrite (" ^ (string_of_tc tc')  ^")"
  | Circ (_,_,sequent) -> "Circ "
  | Gen (_,tc,sequent) -> "Gen (" ^ (string_of_tc tc) ^ ")"    
  
let rec mix_lists g list1 = function
  | [] -> []
  | hd::tl -> (List.map (fun x -> g (x,hd)) list1) @ (mix_lists g list1 tl)  
  

let rec symb_val = function
  | TUnit -> [(Unit,empty_pmap,empty_pmap)]
  | TBool -> [(Bool true,empty_pmap,[]);(Bool false,empty_pmap,[])]
  | TInt -> let x = (Logic.fresh_lvar ()) in
            [(Var x, empty_pmap, [(x,TInt)])]
  | (TArrow (_,_)) as ty -> let x = Logic.fresh_lvar () in  [(Var x, [(x,ty)],empty_pmap)]
  | TProd (ty1,ty2) -> let result1 = symb_val ty1 in
                       let result2 = symb_val ty2 in
                       let g = fun ((val1,funct_var_ctx1,ground_var_ctx1),(val2,funct_var_ctx2,ground_var_ctx2)) -> 
                          (Pair (val1,val2), funct_var_ctx1@funct_var_ctx2, ground_var_ctx1@ground_var_ctx2) in
                       mix_lists g result1 result2
  | TRef _ -> failwith "Error: Types with occurencences of ref subtypes are not supported."                    
                    
                       
let rec build_tc_rule hist sequent = 
  match (trivially_false sequent.arith_ctx, sequent.formula) with
  | (true,_) -> Stop sequent
  | (_,RelV (TArrow (ty1,ty2), funct_var_ctx, (expr1,gamma1), (expr2,gamma2))) ->  
          let result = symb_val ty1 in 
          let f = fun (sval,funct_var_ctx',ground_var_ctx') ->  
                      new_sequent sequent ground_var_ctx' (RelE (ty2, (funct_var_ctx @ funct_var_ctx'), (App (expr1,sval),gamma1),(App (expr2,sval),gamma2))) in
          let premises = List.map (build_tc_rule hist) (List.map f result) in
          RuleV (premises,sequent)
          
  | (_,RelV (TProd (ty1,ty2), funct_var_ctx, (Pair (v11, v12), gamma1), (Pair (v21, v22), gamma2))) ->
          let skor1 = RelV (ty1,funct_var_ctx,(v11,gamma1),(v21,gamma2)) in
          let skor2 = RelV (ty2,funct_var_ctx,(v12,gamma1),(v22,gamma2)) in
                let tc1 = build_tc_rule hist (new_sequent sequent [] skor1) in
                let tc2 = build_tc_rule hist (new_sequent sequent [] skor2) in            
          RuleVProd ((tc1,tc2), sequent)
          
  | (_,RelV (_, _, _, _)) -> RuleVG sequent
  
  | (_,RelSE (ty, funct_var_ctx, fexpr1, fexpr2)) -> 
       begin match (get_elem_callexts funct_var_ctx fexpr1 fexpr2) with
            | Some (TArrow (ty1,ty2),fk1,fv1,fk2,fv2) -> 
                let skorv = RelV (ty1,funct_var_ctx,fv1,fv2) in
                let skork = RelK (ty2,ty,funct_var_ctx,fk1,fk2) in
                let tcv = build_tc_rule hist (new_sequent sequent [] skorv) in
                let tck = build_tc_rule hist (new_sequent sequent [] skork) in                
                RuleSext ((tcv,tck),sequent)
            | None -> Stop sequent
            | _ -> failwith "Error, trying to prove RelSE on a non-functional type"
        end
        
  | (_,RelK (ty1, ty2, funct_var_ctx, (ctx1,gamma1), (ctx2,gamma2))) -> 
          let result = symb_val ty1 in 
          let f = fun (sval,funct_var_ctx',ground_var_ctx') ->  
                       new_sequent sequent ground_var_ctx' 
                       (RelE (ty2,funct_var_ctx @ funct_var_ctx', (fill_hole ctx1 sval,gamma1),(fill_hole ctx2 sval,gamma2))) in
          let premises = List.map (build_tc_rule hist) (List.map f result) in
          RuleK (premises,sequent)
          
  | (_,RelSI (ty, funct_var_ctx, (expr1,gamma1), (expr2,gamma2))) ->  
     begin match (kind_of_term funct_var_ctx (expr1,gamma1),kind_of_term funct_var_ctx (expr2,gamma2),sequent.j,sequent.k) with
       | (IsRecCall _, _,0,_) -> LOut sequent
       | (_, IsRecCall _,_,0) -> ROut sequent          
       | (IsRecCall (f1,body1,val1,ctx1), IsRecCall (f2,body2,val2,ctx2),j,k) ->     
(*         let ctx1' = iter 3 (Unif.rewrite_ac (sequent.logenvc@sequent.logenvl)) ctx1 in
         let ctx2' = iter 3 (Unif.rewrite_ac (sequent.logenvc@sequent.logenvr)) ctx2 in
         print_endline ("From : " ^ (string_of_exprML expr1) ^ " to " ^ (string_of_exprML (fill_hole ctx1' (App (Var f1,val1)))));
         print_endline ("From : " ^ (string_of_exprML expr2) ^ " to " ^ (string_of_exprML (fill_hole ctx2' (App (Var f2,val2))))); *)         
         let sequent' = new_sequent sequent [] ~j:(j-1) ~k:(k-1) (RelSI (ty, funct_var_ctx, (fill_hole ctx1 (App (Var f1,val1)),gamma1), (fill_hole ctx2 (App (Var f2,val2)),gamma2))) in
         let hist' = sequent'::hist in
         let premise = build_tc_rule hist' sequent'
         in Unfold (premise,sequent)
(*         begin match (Unif.unif_in_hist sequent' hist) with
           | Some (circ_sequent,_,_) -> print_endline ("Circ ! "); Circ (circ_sequent,sequent')
           | None -> let expr1' = fill_hole ctx1' (App (body1,val1)) in
                     let expr2' = fill_hole ctx2' (App (body2,val2)) in
                     let sequent' = RelE (ty,expr1',expr2',gamma1,gamma2) in
                     let premise = build_tc_rule hist' (new_sequent sequent [] [] [] [] ?index:(Some (k-1)) sequent') in
                    Rewrite (Unfold (premise,sequent'),sequent)
         end     *)     
       | (IsRecCall (f1,body1,val1,ctx1), _,j,_) -> 
         print_endline ("LUnfold : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));
         let expr1' = fill_hole ctx1 (App (body1,val1)) in
         let sequent' = new_sequent sequent [] ~j:(j-1)  (RelE (ty,funct_var_ctx,(expr1',gamma1),(expr2,gamma2))) in
         let premise = build_tc_rule hist sequent' in
         LUnfold (premise,sequent)
       | (_, IsRecCall (f2,body2,val2,ctx2),_,k) ->
         print_endline ("RUnfold : " ^ (string_of_exprML expr1) ^ " and " ^ (string_of_exprML expr2));       
         let expr2' = fill_hole ctx2 (App (body2,val2)) in
         let sequent' = RelE (ty,funct_var_ctx,(expr1,gamma1),(expr2',gamma2)) in
         let premise = build_tc_rule hist (new_sequent sequent [] ~k:(k-1) sequent') in
         Unfold (premise,sequent)
       | (_,_,_,_) -> failwith "RelSI is applied on terms which are not recursive calls"   
     end
  | (_,RelE (ty, funct_var_ctx, fexpr1, fexpr2)) ->       
         let result1 = Symb_red.symbred_trans fexpr1 [] [] [] [] in
         let result2 = Symb_red.symbred_trans fexpr2 [] [] [] [] in
         let build_tc_rule_expr (fexpr1,fexpr2,heapPre1,heapPre2,heapPost1,heapPost2,vars,preds) = 
           begin match (select_tag funct_var_ctx fexpr1 fexpr2) with
             | WB -> let skor' = RelV (ty,funct_var_ctx,fexpr1,fexpr2) in
                     ((WB,heapPre1,heapPre2,heapPost1,heapPost2),build_tc_rule hist (new_sequent sequent vars ~arith_ctx:preds skor'))            
             | Extern -> let skor' = RelSE (ty,funct_var_ctx,fexpr1,fexpr2) in
                   ((Extern,heapPre1,heapPre2,heapPost1,heapPost2),build_tc_rule hist (new_sequent sequent vars ~arith_ctx:preds skor'))
             | Intern -> 
                   let skor' = RelSI (ty,funct_var_ctx,fexpr1,fexpr2) in
                   ((Intern,heapPre1,heapPre2,heapPost1,heapPost2),build_tc_rule hist (new_sequent sequent vars  ~arith_ctx:preds skor'))                
             | Wrong -> ((Wrong,heapPre1,heapPre2,heapPost1,heapPost2),Stop sequent)      
           end      
         in let g = fun ((fexpr1,heapPre1,heapPost1,vars1,preds1),(fexpr2,heapPre2,heapPost2,vars2,preds2)) 
                    -> (fexpr1,fexpr2,heapPre1,heapPre2,heapPost1,heapPost2,(vars1@vars2),(preds1@preds2)) in
         RuleE (List.map build_tc_rule_expr (mix_lists g result1 result2),sequent)

let diff list1 list2 = List.filter (fun x -> not ((List.mem x) list1)) list2
         
let newelem_of_sequents prev_sequent sequent = (diff prev_sequent.arith_ctx sequent.arith_ctx,diff prev_sequent.ground_var_ctx sequent.ground_var_ctx)


let extract_pred_from_vg sequent = 
    match sequent.formula with
        | RelV (TUnit,_,_,_)  -> ATrue      
        | RelV (TBool,_,(Bool b1,_), (Bool b2,_)) when b1 = b2 -> ATrue
        | RelV (TBool,_,(Bool b1,_), (Bool b2,_)) -> AFalse
        | RelV (TInt,_,(v1,_), (v2,_)) -> (AEqual (AExpr v1,AExpr v2))
        | _ -> failwith "Error: The rule VG has been used on a non-ground type."  
