open Syntax
open Logic
open Symb_red

(*type history = (exprML * exprML) list*)



  
(*and equiv_expr ty logenv expr1 expr2 env1 env2 hist k =
  print_endline "New Step !";
  let result1 = Symb_red.symbred_trans [] [] [] expr1 [] in
  let result2 = Symb_red.symbred_trans [] [] [] expr2 [] in
  let hist' = add_hist hist expr1 expr2 in
  let equiv_expr_aux (expr1,expr2,env1,env2,heapPre1,heapPre2,heapPost1,heapPost2,preds) = 
    match (isval expr1,isval expr2) with
      | (false,false) -> let (f1,val1,ctx1) = (extract_callback expr1) in
                         let (f2,val2,ctx2) = (extract_callback expr2) in
                         let hist'' = add_hist hist' expr1 expr2 in
                         begin match (lookup_logenv logenv (f1,f2)) with
(*                           | Some (TArrow (ty1,ty2)) -> Next (heapPre1,heapPre2,heapPost1,heapPost2,Pred preds,
                                                         LogAnd [equiv_val ty1 logenv val1 val2 env1 env2 hist'' k; 
                                                                 equiv_ctx ty2 ty logenv ctx1 ctx2 env1 env2 hist'' k])*)
                           | None -> begin match (lookup_recenv env1 f1, lookup_recenv env2 f2,k) with
                                       | (Some expr1', Some expr2', 0) -> let (expr1'',_) = abstr_fo expr1 in
                                                                let (expr2'',_) = abstr_fo expr2 in
                                                                 print_endline ("LLooking for " ^ (string_of_exprML expr1'') ^ " AND " ^ (string_of_exprML expr2''));
                                                                (if ((match_expr hist expr1'' expr2'') <> []) then print_endline "wE HAVE A MATCH !" else ()); 
                                                                failwith "Cannot prove equivalence, we need to unwind more time the fixpoints."
                                       | (Some expr1', Some expr2', k) -> let (expr1'',_) = abstr_fo expr1 in
                                                                          let (expr2'',_) = abstr_fo expr2 in
                                                                          print_endline ("Looking for " ^ (string_of_exprML expr1) ^ " AND " ^ (string_of_exprML expr2));
                                                                          print_endline ("Or Looking for " ^ (string_of_exprML expr1'') ^ " AND " ^ (string_of_exprML expr2''));
                                                                          (if ((match_expr hist expr1'' expr2'') <> []) then print_endline "wE HAVE A MATCH !" else ());
(*                                                                          print_newline;
                                                                          print_endline ("We are substituting  " ^ f1 ^ " to " ^ (string_of_exprML expr1'));
                                                                          print_endline ("We apply it to  " ^ (string_of_exprML val1));
                                                                          print_endline ("The result:  " ^ (string_of_exprML (ctx1 (App (expr1',val1)))));                                                                
                                                                          print_endline ("We are substituting  " ^ f2 ^ " to " ^ (string_of_exprML expr2'));
                                                                          print_endline ("We apply it to  " ^ (string_of_exprML val2));
                                                                          print_endline ("The result:  " ^ (string_of_exprML (ctx2 (App (expr2',val2)))));
                                                                          print_endline ("Test:  " ^ (string_of_exprML (ctx2 (Var "?"))));         *)                                                                                                                                        
                                                                          equiv_expr ty logenv (ctx1 (App (expr1',val1))) (ctx2 (App (expr2',val2))) env1 env2 hist'' (k-1)
                                       | (None,None,_) ->  Next (heapPre1,heapPre2,heapPost1,heapPost2,Pred preds,AFalse)
                                       | (_,_,_) -> failwith ("Something went wrong with " ^ (string_of_exprML expr1) ^ " AND " ^ (string_of_exprML expr2));
                                     end  
                         end
      | (false,_) | (_,false) -> Next (heapPre1,heapPre2,heapPost1,heapPost2,Pred preds,AFalse)
(*      | (true,true) -> Next (heapPre1,heapPre2,heapPost1,heapPost2,Pred preds,equiv_val ty logenv expr1 expr2 env1 env2 hist' k)*)
  in let g = fun ((expr1,env1',heapPre1,heapPost1,preds1),(expr2,env2',heapPre2,heapPost2,preds2)) -> (expr1,expr2,env1'@env1,env2'@env2,heapPre1,heapPre2,heapPost1,heapPost2,(preds1@preds2))in
  LogAnd (List.map equiv_expr_aux (merge_pair_lists g result1 result2))

and equiv_ctx ty1 ty2 logenv ctx1 ctx2 env1 env2 hist k =
  let result = abstr_pair_vals ty1 in
     SquarePub (LogAnd (List.map (fun (val1,val2,preds,logenv') -> LogImpl (LogAnd preds,(equiv_expr ty2 (logenv@logenv') (ctx1 val1) (ctx2 val2) env1 env2 hist k))) result))*)
  
