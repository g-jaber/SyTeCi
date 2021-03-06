open Logic
open Syntax
open Skor
open Unif

type kindTerm =
    IsVal
  | IsCallExtern of (id*typeML*exprML*eval_context)
  | IsRecCall of (id*exprML*exprML*eval_context)

let kind_of_term funct_var_ctx (expr,gamma) =
  if isval expr then IsVal
  else begin let (f,v,ctx) = extract_call expr in
    begin match Pmap.lookup_pmap f funct_var_ctx with
      | None -> begin match Pmap.lookup_pmap f gamma with
          | None -> failwith ("The variable " ^ f ^ " comes from nowhere in " ^ (string_of_exprML expr))
          | Some valf -> IsRecCall (f,valf,v,ctx)
        end
      | Some ty -> IsCallExtern (f,ty,v,ctx)
    end
  end

let select_tag funct_var_ctx (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term funct_var_ctx (expr1,gamma1),
         kind_of_term funct_var_ctx (expr2,gamma2)) with
  | (IsVal,IsVal) -> WB
  | (IsCallExtern _,IsCallExtern _) -> Extern
  | (IsRecCall _,_) -> Intern
  | (_,IsRecCall _) -> Intern
  | (_,_) -> Wrong

let get_skor_from_tag (ty,funct_var_ctx,fexpr1,fexpr2) = function
  | WB -> RelV (ty,funct_var_ctx,fexpr1,fexpr2)
  | Extern -> RelSE (ty,funct_var_ctx,fexpr1,fexpr2)
  | Intern -> RelSI (ty,funct_var_ctx,fexpr1,fexpr2)
  | Wrong -> RelE (ty,funct_var_ctx,fexpr1,fexpr2)

let get_elem_callexts funct_var_ctx (expr1,gamma1) (expr2,gamma2) =
  match (kind_of_term funct_var_ctx (expr1,gamma1),
         kind_of_term funct_var_ctx (expr2,gamma2)) with
  | (IsCallExtern (f1,ty1,v1,k1), IsCallExtern (f2,_,v2,k2)) when f1 = f2 ->
    Some (ty1,(k1,gamma1),(v1,gamma1),(k2,gamma2),(v2,gamma2))
  | _ -> None

type annot_rele = tag*symbheap*symbheap*symbheap*symbheap

type tc_struct =
  | Stop of sequent
  | RuleVG of sequent
  | RuleVProd of (tc_struct * tc_struct) * sequent
  | RuleSext of (tc_struct * tc_struct) * sequent
  | RuleSwrong of sequent
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
  | RuleInit of (tc_struct list)*sequent

let get_root = function
  | Stop sequent -> sequent
  | RuleVG sequent -> sequent
  | RuleVProd (_, sequent) -> sequent
  | RuleSext (_, sequent) -> sequent
  | RuleSwrong sequent -> sequent
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
  | RuleInit (_,sequent) -> sequent

let rec string_of_list = function
  | [] -> ""
  | [str] -> str
  | str::tl -> str ^ "," ^ (string_of_list tl)

let rec string_of_tc tc = 
  let sequent = get_root tc in
  let str_formula = Skor.string_of_formula sequent.formula in
  match tc with
  | Stop _ -> "Stop[" ^ str_formula ^ "]"
  | RuleVG _ -> "RuleVG[" ^ str_formula ^ "]"
  | LOut _ -> "LOut[" ^ str_formula ^ "]"
  | ROut _ -> "LOut[" ^ str_formula ^ "]"
  | RuleVProd ((tc1,tc2),_) -> 
    "RuleVx[" ^ str_formula ^ "](" 
    ^ (string_of_tc tc1) ^ ","^ (string_of_tc tc2) ^")"
  | RuleV (tcs,_) -> 
    "RuleV[" ^ str_formula ^ "](" 
    ^ (string_of_list (List.map string_of_tc tcs)) ^")"
  | RuleK (tcs,_) -> 
    "RuleK[" ^ str_formula ^ "](" 
    ^ (string_of_list (List.map string_of_tc tcs)) ^")"
  | RuleSext ((tc1,tc2),_) -> 
    "RuleSext[" ^ str_formula ^ "](" 
    ^ (string_of_tc tc1) ^ "," ^ (string_of_tc tc2) ^")"
  | RuleSwrong _ -> "Swrong[" ^ str_formula ^ "]"
  | Unfold (tc',_) ->  "Unfold[" 
  ^ str_formula ^ "](" ^ (string_of_tc tc') ^")"
  | LUnfold (tc',_) -> "LUnfold[" 
  ^ str_formula ^ "](" ^ (string_of_tc tc') ^")"
  | RUnfold (tc',_) -> "RUnfold[" 
  ^ str_formula ^ "](" ^ (string_of_tc tc') ^")"
  | RuleE (tcs,_) -> 
    "RuleE[" ^ str_formula ^ "]("
    ^ (string_of_list (List.map (fun (_,tc) -> (string_of_tc tc)) tcs)) ^ ")"
  | Rewrite (tc',_) -> "Rewrite[" ^ str_formula ^ "](" ^ (string_of_tc tc')  ^")"
  | Circ (_,_,_) -> "Circ[" ^ str_formula ^ "]"
  | Gen (_,tc,_) -> "Gen[" ^ str_formula ^ "](" ^ (string_of_tc tc) ^ ")"
  | RuleInit (tcs,_)->  
    "RuleInit[" ^ str_formula ^ "](" ^ (string_of_list (List.map string_of_tc tcs))^")"

let rec mix_lists g list1 = function
  | [] -> []
  | hd::tl -> (List.map (fun x -> g (x,hd)) list1) @ (mix_lists g list1 tl)


let rec symb_val = function
  | TUnit -> [(Unit,Pmap.empty,Pmap.empty)]
  | TBool -> [(Bool true,Pmap.empty,Pmap.empty);(Bool false,Pmap.empty,Pmap.empty)]
  | TInt ->
    let x = (Logic.fresh_lvar ()) in
    [(Var x, Pmap.empty, [(x,TInt)])]
  | (TArrow (_,_)) as ty -> let x = Logic.fresh_lvar () in  [(Var x, [(x,ty)],Pmap.empty)]
  | TProd (ty1,ty2) ->
    let result1 = symb_val ty1 in
    let result2 = symb_val ty2 in
    let aux ((val1,funct_var_ctx1,ground_var_ctx1),
             (val2,funct_var_ctx2,ground_var_ctx2)) =
      (Pair (val1,val2), funct_var_ctx1@funct_var_ctx2,
       ground_var_ctx1@ground_var_ctx2) in
    mix_lists aux result1 result2
  | TRef _ ->
    failwith "Error: Types with occurencences of ref subtypes are not supported."
  | TVar _ ->
    Debug.print_debug "Generating symbolic value for a type variable";
    let x = (Logic.fresh_lvar ()) in
    [(Var x, Pmap.empty, [(x,TInt)])]
  | TUndef -> failwith "Error, undefined type ! Pleaser report."

type 'a result_build_tc =
  | Continue of 'a
  | Backtrack of (gsubst*sequent*sequent)

type 'a result_build_ltc =
  | LContinue of ('a list)
  | LBacktrack of (gsubst*sequent*sequent)

type result_build_ptc =
  | PContinue of (tc_struct*tc_struct)
  | PBacktrack of (gsubst*sequent*sequent)

let commute_result u = function
  | Continue v -> Continue (u,v)
  | Backtrack _ as b -> b

let rec select_if_backtrack_list f = function
  | [] -> LContinue []
  | sequent::lsequent ->
    begin match f sequent with
      | Backtrack sequent -> LBacktrack sequent
      | Continue tc_struct ->
        begin match select_if_backtrack_list f lsequent with
          | LBacktrack _ as result -> result
          | LContinue ltc_struct -> LContinue (tc_struct::ltc_struct)
        end
    end

let select_if_backtrack_pair f sequent1 sequent2 =
  match f sequent1 with
  | Backtrack sequent -> PBacktrack sequent
  | Continue tc_struct1 ->
    begin match f sequent2 with
      | Backtrack sequent -> PBacktrack sequent
      | Continue tc_struct2 -> PContinue (tc_struct1,tc_struct2)
    end

let rec gen_symb_subst = function
  | [] -> [[],[],[]]
  | (var,ty)::var_ctx ->
    let result_symb_val = symb_val ty in
    let result_symb_subst = gen_symb_subst var_ctx in
    let aux_symb_val (sval,funct_var_ctx,ground_var_ctx) =
      let aux_symb_subst (symb_subst,funct_var_ctx',ground_var_ctx') =
        ((var,sval)::symb_subst,funct_var_ctx@funct_var_ctx',
         ground_var_ctx@ground_var_ctx') in
      List.map aux_symb_subst result_symb_subst in
    List.flatten (List.map aux_symb_val result_symb_val)


let rec build_tc_rule asym_unfold flag hist sequent =
  match sequent.formula with
  | RelEquiv (ty,var_ctx,(expr1,gamma1),(expr2,gamma2)) ->
    let result = gen_symb_subst var_ctx in
    let aux (symb_subst,funct_var_ctx,ground_var_ctx) =
      let expr1 = subst_list expr1 symb_subst in
      let expr2 = subst_list expr2 symb_subst in
      update_sequent sequent ground_var_ctx
        (RelE (ty, funct_var_ctx, (expr1,gamma1),(expr2,gamma2))) in
    let list_sequents = List.map aux result in
    let premises = select_if_backtrack_list
        (build_tc_rule asym_unfold false hist) list_sequents in
    begin match premises with
      | LBacktrack (_,bt_sequent,gen_sequent) ->
        failwith ("Error: Uncaught backtracking " ^ (string_of_int bt_sequent.id)
                  ^ " and " ^ (string_of_int gen_sequent.id) ^ ". Please report.")
      | LContinue ltc_struct -> Continue (RuleInit (ltc_struct,sequent))
    end

  | RelV (TArrow (ty1,ty2), funct_var_ctx, (expr1,gamma1), (expr2,gamma2)) ->
    let result = symb_val ty1 in
    let aux (sval,funct_var_ctx',ground_var_ctx') =
      update_sequent sequent ground_var_ctx'
        (RelE (ty2, (funct_var_ctx @ funct_var_ctx'),
               (App (expr1,sval),gamma1),(App (expr2,sval),gamma2))) in
    let list_sequents = List.map aux result in
    let premises = select_if_backtrack_list
        (build_tc_rule asym_unfold false hist) list_sequents in
    begin match premises with
      | LBacktrack sequent -> Backtrack sequent
      | LContinue ltc_struct -> Continue (RuleV (ltc_struct,sequent))
    end

  | RelV (TProd (ty1,ty2), funct_var_ctx, (Pair (v11, v12), gamma1),
          (Pair (v21, v22), gamma2)) ->
    let skor1 = RelV (ty1,funct_var_ctx,(v11,gamma1),(v21,gamma2)) in
    let skor2 = RelV (ty2,funct_var_ctx,(v12,gamma1),(v22,gamma2)) in
    let sequent1 = update_sequent sequent [] skor1 in
    let sequent2 = update_sequent sequent [] skor2 in
    let premises = select_if_backtrack_pair (build_tc_rule asym_unfold false hist)
        sequent1 sequent2  in
    begin match premises with
      | PBacktrack sequent -> Backtrack sequent
      | PContinue (tc1,tc2) -> Continue (RuleVProd ((tc1,tc2), sequent))
    end

  | RelV (_, _, _, _) -> Continue (RuleVG sequent)

  | RelSE (ty, funct_var_ctx, fexpr1, fexpr2) ->
    begin match (get_elem_callexts funct_var_ctx fexpr1 fexpr2) with
      | Some (TArrow (ty1,ty2),fk1,fv1,fk2,fv2) ->
        let skorv = RelV (ty1,funct_var_ctx,fv1,fv2) in
        let skork = RelK (ty2,ty,funct_var_ctx,fk1,fk2) in
        let sequentv = update_sequent sequent [] skorv in
        let sequentk = update_sequent sequent [] skork in
        let premises = select_if_backtrack_pair (build_tc_rule asym_unfold false hist)
            sequentv sequentk in
        begin match  premises with
          | PBacktrack sequent -> Backtrack sequent
          | PContinue (tc1,tc2) -> Continue (RuleSext ((tc1,tc2), sequent))
        end
      | None -> Continue (Stop sequent)
      | _ -> failwith "Error, trying to prove RelSE on a non-functional type"
    end

  | RelK (ty1, ty2, funct_var_ctx, (ctx1,gamma1), (ctx2,gamma2)) ->
    let result = symb_val ty1 in
    let aux (sval,funct_var_ctx',ground_var_ctx') = update_sequent
        sequent ground_var_ctx' (RelE (ty2,funct_var_ctx @ funct_var_ctx',
                                       (fill_hole ctx1 sval,gamma1),
                                       (fill_hole ctx2 sval,gamma2))) in
    let list_sequents = List.map aux result in
    let premises = select_if_backtrack_list (build_tc_rule asym_unfold false hist)
        list_sequents in
    begin match premises with
      | LBacktrack sequent -> Backtrack sequent
      | LContinue ltc_struct -> Continue (RuleK (ltc_struct,sequent))
    end

  | RelSI (ty, funct_var_ctx, (expr1,gamma1), (expr2,gamma2)) ->
    begin match (kind_of_term funct_var_ctx (expr1,gamma1),
                 kind_of_term funct_var_ctx (expr2,gamma2),sequent.j,sequent.k,flag) with
    | (IsRecCall _, _,0,_,_) ->
      Debug.print_debug ("LOut reached: " ^ (string_of_formula sequent.formula));
      Continue (LOut sequent)
    | (_, IsRecCall _,_,0,_) ->
      Debug.print_debug "ROut reached"; Continue (ROut sequent)
    | (IsRecCall (_,body1,val1,ctx1), IsRecCall (_,body2,val2,ctx2),j,k,true) ->
      let expr1' = fill_hole ctx1 (App (body1,val1)) in
      let expr2' = fill_hole ctx2 (App (body2,val2)) in
      let sequent' = update_sequent sequent [] ~j:(j-1) ~k:(k-1)
          (RelE (ty, funct_var_ctx, (expr1',gamma1), (expr2',gamma2))) in
      let premise = build_tc_rule asym_unfold false hist sequent' in
      begin match premise with
        | Continue tc_struct ->  Continue (Unfold (tc_struct,sequent))
        | Backtrack (_,bt_sequent,_) as bt ->
          Debug.print_debug ("Possible dubious backtracking "
                             ^ (string_of_int bt_sequent.id) ^ " and "
                             ^ (string_of_int sequent'.id));
          bt
      end
    | (IsRecCall (f1,body1,val1,ctx1), IsRecCall (f2,body2,val2,ctx2),j,k,false) ->
      (* We rewrite contexts *)
      let ctx1' = iter 3 (Rewrite.rewrite_ac sequent.ground_var_ctx) ctx1 in
      let ctx2' = iter 3 (Rewrite.rewrite_ac sequent.ground_var_ctx) ctx2 in
      let expr1' = fill_hole ctx1' (App (Var f1,val1)) in
      let expr2' = fill_hole ctx2' (App (Var f2,val2)) in
      Debug.print_debug ("From : " ^ (string_of_exprML expr1) ^ " to "
                         ^ (string_of_exprML expr1'));
      Debug.print_debug ("From : " ^ (string_of_exprML expr2) ^ " to "
                         ^ (string_of_exprML expr2'));
      let sequent' = update_sequent sequent []
          (RelSI (ty, funct_var_ctx, (expr1',gamma1), (expr2',gamma2))) in
      begin match (Unif.unif_in_hist sequent' hist) with
        (* We check in the history if we have already seen the rewritten sequent *)
        | (Some (gsubst1,[],_,gen_sequent)) ->
          Debug.print_debug ("Circ with " ^ (string_of_gsubst gsubst1));
          Continue (Circ (gsubst1,gen_sequent,sequent'))
        | (Some (_,gsubst2,bt_sequent,gen_sequent)) ->
          Debug.print_debug "Backtrack";
          Backtrack (gsubst2,bt_sequent,gen_sequent)
        | None -> let expr1'' = fill_hole ctx1' (App (body1,val1)) in
          (* If we have not seen it, we unfold the fixed-points *)
          let expr2'' = fill_hole ctx2' (App (body2,val2)) in
          let sequent'' = update_sequent sequent [] ~j:(j-1) ~k:(k-1)
              (RelE (ty, funct_var_ctx, (expr1'',gamma1), (expr2'',gamma2))) in
          let hist' = if flag then hist else (sequent',false)::hist in
          let premise = build_tc_rule asym_unfold false hist' sequent'' in
          begin match premise with
            | Continue tc_struct ->  Continue (Unfold (tc_struct,sequent'))
            | Backtrack (gsubst,bt_sequent,gen_sequent) as bt ->
              (* When we have to backtrack, we check that the current sequent
                 is the one we have to backtrack to *)
              Debug.print_debug ("Backtracking " ^ (string_of_int bt_sequent.id)
                                 ^ " and " ^ (string_of_int sequent'.id));
              if bt_sequent.id = sequent'.id then begin
                let hist'' =  (gen_sequent,true)::hist in
                let premise = build_tc_rule asym_unfold true hist'' gen_sequent in
                begin match premise with
                  | Continue tc_struct ->  Continue (Gen (gsubst,tc_struct,sequent))
                  | Backtrack _ -> failwith "Dubious backtracking."
                end
              end else bt
          end
      end
    | (IsRecCall (_,body1,val1,ctx1), _,j,_,_) ->
      Debug.print_debug ("LUnfold : " ^ (string_of_exprML expr1) ^ " and "
                         ^ (string_of_exprML expr2));
      if asym_unfold then begin
        let expr1' = fill_hole ctx1 (App (body1,val1)) in
        let sequent' = update_sequent sequent [] ~j:(j-1)
            (RelE (ty,funct_var_ctx,(expr1',gamma1),(expr2,gamma2))) in
        let premise = build_tc_rule asym_unfold false hist sequent' in
        begin match premise with
          | Continue tc_struct ->  Continue (LUnfold (tc_struct,sequent))
          | Backtrack _ -> premise
        end
      end
      else Continue (Stop sequent)
    | (_, IsRecCall (_,body2,val2,ctx2),_,k,_) ->
      Debug.print_debug ("RUnfold : " ^ (string_of_exprML expr1) ^ " and "
                         ^ (string_of_exprML expr2));
      if asym_unfold then begin
        let expr2' = fill_hole ctx2 (App (body2,val2)) in
        let sequent' = update_sequent sequent [] ~k:(k-1)
            (RelE (ty,funct_var_ctx,(expr1,gamma1),(expr2',gamma2))) in
        let premise = build_tc_rule asym_unfold false hist sequent' in
        begin match premise with
          | Continue tc_struct ->  Continue (RUnfold (tc_struct,sequent))
          | Backtrack _ -> premise
        end
      end
      else Continue (Stop sequent)
    | (_,_,_,_,_) -> failwith "Error: RelSI is applied on terms which are
                               not recursive calls. Please report."
    end
  | RelE (ty, funct_var_ctx, fexpr1, fexpr2) ->
    let result1 = Symb_red.symbred_trans fexpr1 [] [] [] [] in
    let result2 = Symb_red.symbred_trans fexpr2 [] [] [] [] in
    let build_tc_rule_expr (fexpr1,fexpr2,heapPre1,heapPre2,
                            heapPost1,heapPost2,vars,preds) =
      let tag = select_tag funct_var_ctx fexpr1 fexpr2 in
      let skor' = get_skor_from_tag (ty,funct_var_ctx,fexpr1,fexpr2) tag in
      let sequent' = update_sequent sequent vars ~arith_ctx:preds skor' in
      begin match (tag,Logic_to_z3.check_sat (vars@sequent.ground_var_ctx)
                     (preds@sequent.arith_ctx)) with
      | (_,false) ->
        Continue ((tag,heapPre1,heapPre2,heapPost1,heapPost2),Stop sequent')
      | (Wrong,_) ->
        Continue ((tag,heapPre1,heapPre2,heapPost1,heapPost2),RuleSwrong sequent')
      | (_,_) ->
        let premise = build_tc_rule asym_unfold false hist sequent' in
        commute_result (tag,heapPre1,heapPre2,heapPost1,heapPost2) premise
      end in
    let aux ((fexpr1,heapPre1,heapPost1,vars1,preds1),(fexpr2,heapPre2,heapPost2,vars2,preds2)) =
      (fexpr1,fexpr2,heapPre1,heapPre2,heapPost1,heapPost2,(vars1@vars2),(preds1@preds2)) in
    let premises = select_if_backtrack_list build_tc_rule_expr
        (mix_lists aux result1 result2) in
    begin match premises with
      | LBacktrack sequent -> Backtrack sequent
      | LContinue ltc_struct -> Continue (RuleE (ltc_struct,sequent))
    end

let diff list1 list2 = List.filter (fun x -> not ((List.mem x) list1)) list2

let newelem_of_sequents prev_sequent sequent =
  (diff prev_sequent.arith_ctx sequent.arith_ctx
  ,diff prev_sequent.ground_var_ctx sequent.ground_var_ctx)


let extract_pred_from_vg sequent =
  match sequent.formula with
  | RelV (TUnit,_,_,_)  -> ATrue
  | RelV (TBool,_,(Bool b1,_), (Bool b2,_)) when b1 = b2 -> ATrue
  | RelV (TBool,_,(Bool _,_), (Bool _,_)) -> AFalse
  | RelV (TInt,_,(v1,_), (v2,_)) -> AEqual (v1,v2)
  | _ -> failwith "Error: The rule VG has been used on a non-ground type."

let rec extensional_reasoning ty expr1 expr2 =
  match (ty, isval expr1 && isval expr2) with
  | (TArrow (ty1,ty2),true) ->
    Debug.print_debug "Performing extensional reasoning";
    let (var1,(expr1,gamma1)) = extract_body expr1 in
    let (var2,(expr2,gamma2)) = extract_body expr2 in
    let expr2 = subst expr2 (Var var2) (Var var1) in
    let (env,(expr1,gamma1'),(expr2,gamma2'),ty) = extensional_reasoning ty2
        expr1 expr2 in
    ((var1,ty1)::env,(expr1,gamma1@gamma1'),(expr2,gamma2@gamma2'),ty)
  | _ -> 
    Debug.print_debug "No extensional reasoning performed";
     ([],(expr1,[]),(expr2,[]),ty)

let build_tc ext_reason asym_unfold ty expr1 expr2 step1 step2 =
  let skor = if ext_reason then begin
      let (var_ctx,fexpr1,fexpr2,ty) = extensional_reasoning ty expr1 expr2 in
      RelEquiv (ty,var_ctx,fexpr1,fexpr2)
    end
    else RelEquiv (ty,[],(expr1,[]),(expr2,[])) in
  let sequent = create_sequent step1 step2 skor in
  match build_tc_rule asym_unfold false [] sequent with
  | Continue tc -> tc
  | Backtrack (_,bt_sequent,gen_sequent) ->
    failwith ("Error: Uncaught backtracking " ^ (string_of_int bt_sequent.id)
              ^ " and " ^ (string_of_int gen_sequent.id) ^ ". Please report.")
