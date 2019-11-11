open Syntax

exception TypingError of string

let check_tyvar = function
  | (var,TUndef) ->
    let tvar = fresh_typevar () in (var,tvar)
  | tyvar -> tyvar

let rec typing vctx lctx tsubst expr = match expr with
  | Var x ->
    begin match Pmap.lookup_pmap x vctx with
      | Some ty -> (ty,vctx,tsubst)
      | None -> Error.fail_error ("Error: the variable " ^ x ^ " is not defined.")
    end
  | Loc l ->
    begin match Pmap.lookup_pmap l lctx with
      | Some ty -> (ty,vctx,tsubst)
      | None -> Error.fail_error ("Error: the location " ^ (string_of_int l) ^ " is not defined.")
    end
  | Unit -> (TUnit,vctx,tsubst)
  | Int _ -> (TInt,vctx,tsubst)
  | Bool _ -> (TBool,vctx,tsubst)
  | Plus (e1,e2) | Minus (e1,e2) | Mult (e1,e2) | Div (e1,e2) -> begin
      try of_type_bin vctx lctx tsubst TInt e1 e2 TInt
      with TypingError msg -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ msg) end
  | And (e1,e2) | Or (e1,e2) -> of_type_bin vctx lctx tsubst TBool e1 e2 TBool
  | Equal (e1,e2) | NEqual (e1,e2) | Less (e1,e2) | LessEq (e1,e2) | Great (e1,e2) | GreatEq (e1,e2)  -> 
    of_type_bin vctx lctx tsubst TInt e1 e2 TBool
  | Not e -> of_type vctx lctx tsubst e TBool
  | If (e1,e2,e3) ->
    let (_,vctx1,tsubst1) = of_type vctx lctx tsubst e1 TBool in
    let (ty2,vctx2,tsubst2) = typing vctx1 lctx tsubst1 e2 in
    let (ty3,vctx3,tsubst3) = typing vctx2 lctx tsubst2 e3 in
    let ty2' = apply_type_subst ty2 tsubst3 in
    Debug.print_debug ("Typing of the first branch:" ^ (string_of_typeML ty2') ^ " (was " ^ (string_of_typeML ty2) ^ ")");
    Debug.print_debug ("Typing of the second branch:" ^ (string_of_typeML ty3));
    begin match unify_type tsubst3 (ty2',ty3) with
      | Some (ty,tsubst4) -> 
        Debug.print_debug ("Typing If:" ^ (string_of_typeML ty)); 
        (ty,lsubst_vctx tsubst4 vctx3,tsubst4)
      | None -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                  ^(string_of_typeML ty2) ^ " is not equal to "
                                  ^(string_of_typeML ty3))
    end
  | Fun ((var,TUndef),e) ->
    let tvar = fresh_typevar () in
    let (ty',vctx',tsubst') = typing (Pmap.modadd_pmap (var,tvar) vctx) lctx tsubst e in
    begin match Pmap.lookup_pmap var vctx' with
      | Some ty -> (TArrow (ty,ty'),vctx',tsubst')
      | None -> failwith ("Variable " ^ var ^ " not found in type-checking. Please report.")
    end
  | Fun ((var,ty),e) ->
    let (ty',vctx',tsubst') = typing (Pmap.modadd_pmap (var,ty) vctx) lctx tsubst e in
    (TArrow (ty,ty'),vctx',tsubst')
  | Fix ((idfun,TUndef),(var,TUndef),e) ->
    let tvar1 = fresh_typevar () in
    let tvar2 = fresh_typevar () in
    let new_vctx = Pmap.modadd_pmap2 (var,tvar1) (idfun,TArrow (tvar1,tvar2)) vctx in
    let (rty,vctx',tsubst') = typing new_vctx lctx  tsubst e in
    begin match (Pmap.lookup_pmap var vctx',Pmap.lookup_pmap idfun vctx') with
      | (Some aty, Some fty) ->
        begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
          | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
          | None -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                      ^(string_of_typeML fty) ^ " is not equal to "
                                      ^(string_of_typeML (TArrow (aty,rty))))
        end
      | _ -> failwith "Variables not found in type-checking. Please report."
    end
  | Fix ((idfun,TUndef),(var,aty),e) ->
    let tvar2 = fresh_typevar () in
    let new_vctx = Pmap.modadd_pmap2 (var,aty) (idfun,TArrow (aty,tvar2)) vctx in
    let (rty,vctx',tsubst') = typing new_vctx lctx tsubst e in
    begin match Pmap.lookup_pmap idfun vctx' with
      | Some fty -> 
        begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
          | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
          | None -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                      ^(string_of_typeML fty) ^ " is not equal to "
                                      ^(string_of_typeML (TArrow (aty,rty))))
        end
      | None -> failwith ("Variable " ^ idfun ^ " not found type-checking. Please report.")
    end
  | Fix ((idfun,fty),(var,aty),e) ->
    let new_vctx = Pmap.modadd_pmap2 (var,aty) (idfun,fty) vctx in
    let (rty,vctx',tsubst') =  typing new_vctx lctx tsubst e in
    begin match unify_type tsubst' (fty,TArrow (aty,rty)) with
      | Some (ty,tsubst'') -> (ty,lsubst_vctx tsubst'' vctx',tsubst'')
      | None -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                                  ^(string_of_typeML fty) ^ " is not equal to "
                                  ^(string_of_typeML (TArrow (aty,rty))))
    end
  | Let (var,e1,e2) -> 
    let (ty,vctx',tsubst') = typing vctx lctx tsubst e1 in
    let new_vctx = Pmap.modadd_pmap (var,ty) vctx' in
    typing new_vctx lctx tsubst' e2
  | LetPair (var1,var2,e1,e2) ->
    let (ty,vctx',tsubst') = typing vctx lctx tsubst e1 in
    begin match ty with
      | TProd (ty1,ty2) ->
        let new_vctx = Pmap.modadd_pmap2 (var1,ty1) (var2,ty2) vctx' in
        typing new_vctx lctx tsubst' e2
      | TVar tvar ->
        let tvar1 = fresh_typevar () in
        let tvar2 = fresh_typevar () in
        let vctx'' = subst_vctx tvar (TProd (tvar1,tvar2)) vctx' in
        let new_vctx = Pmap.modadd_pmap2 (var1,tvar1) (var2,tvar2) vctx'' in
        let new_tsubst = Pmap.modadd_pmap (tvar,TProd (tvar1,tvar2)) tsubst' in
        typing new_vctx lctx new_tsubst e2
      | _ -> Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr)
                               ^ " : " ^ (string_of_typeML ty) ^ " is not a product type")
    end
  | App (e1,e2) ->
    let (aty,vctx',tsubst') = typing vctx lctx tsubst e2 in
    let (fty,vctx'',tsubst'') = typing vctx' lctx tsubst' e1 in
    let  aty' = apply_type_subst aty tsubst'' in
    Debug.print_debug ("Typing App:" ^ (Syntax.string_of_exprML expr));
    Debug.print_debug ((Syntax.string_of_exprML e1) ^ " is of type " ^ (string_of_typeML fty));
    Debug.print_debug ((Syntax.string_of_exprML e2) ^ " is of type " ^ (string_of_typeML aty') ^ " (was " ^ (string_of_typeML aty) ^")");
    begin match fty with
      | TVar a ->
        let tvar = fresh_typevar () in
        let new_type_a = TArrow (aty',tvar) in
        (tvar,subst_vctx a new_type_a vctx'',Pmap.modadd_pmap (a,new_type_a) tsubst'')
      | TArrow (ty',rty) ->
        begin match unify_type tsubst'' (ty',aty') with
          | Some (_,tsubst''') -> (rty,lsubst_vctx tsubst''' vctx'',tsubst''')
          | None -> Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": "
                                      ^ (string_of_typeML ty') ^ " is not equal to "
                                      ^(string_of_typeML aty'))
        end
      | _ -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                               ^ (string_of_typeML fty) ^ " is not an arrow type")
    end
  | Seq (e1,e2) -> let (_,vctx',tsubst') = of_type vctx lctx tsubst e1 TUnit in  typing vctx' lctx tsubst' e2
  (*    if (ty = TUnit) then (typing vctx' lctx e2) else raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty) ^ "  is not equal to " ^(string_of_typeML TUnit)))*)
  | Pair (e1,e2) -> let (ty1,vctx',tsubst') = typing vctx lctx tsubst e1 in
    let (ty2,vctx'',tsubst'') = typing vctx' lctx tsubst' e2 in (TProd (ty1,ty2),vctx'',tsubst'')
  | Newref e -> 
    let (ty ,vctx',tsubst')= typing vctx lctx tsubst e in
    begin match ty with
    | TVar a -> (TRef TInt,subst_vctx a TInt vctx',Pmap.modadd_pmap (a,TInt) tsubst')
    | TInt -> (TRef ty,vctx',tsubst')
    | _ -> Error.fail_error ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                               ^ (string_of_typeML ty) ^ " is not of type int. Only integers can be stored in references.")
  end
  | Deref e ->
    let (ty,vctx',tsubst') = typing vctx lctx tsubst e in
    begin match ty with
      | TRef ty -> (ty,vctx',tsubst')
      | _ -> Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : "
                               ^ (string_of_typeML ty) ^ " is not a ref type")
    end
  | Assign (e1,e2) ->
    let (ty1,vctx',tsubst') = typing vctx lctx tsubst e1 in
    let (ty2,vctx'',tsubst'') = typing vctx' lctx tsubst' e2 in
    begin match (ty1,ty2) with
      | (TRef ty1',_) when ty1' = ty2 -> (TUnit,vctx'',tsubst'')
      | (TRef ty1',TVar a) -> (TUnit,subst_vctx a ty1' vctx'',tsubst'')
      | (TRef (TVar a),_) -> (TUnit,subst_vctx a ty2 vctx'',tsubst'')
      | (_,_) -> Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr)
                                   ^ " : " ^ (string_of_typeML ty1) ^ " is not a ref type")
    end
  | Hole -> failwith ("Error: The typechecker cannot type a hole")

and of_type vctx lctx tsubst expr resty =
  let (ty ,vctx',tsubst')= typing vctx lctx tsubst expr in
  match ty with
  | TVar a -> (resty,subst_vctx a resty vctx',Pmap.modadd_pmap (a,resty) tsubst')
  | _ when ty = resty -> (ty,vctx',tsubst')
  | _ -> Error.fail_error ("Error typing " ^ (Syntax.string_of_exprML expr) 
                           ^ " : " ^ ((string_of_typeML ty) ^ " is not equal to " 
                                      ^(string_of_typeML resty)))

and of_type_bin vctx lctx tsubst com_ty expr1 expr2 res_ty =
  let (ty1,vctx',tsubst') = typing vctx lctx tsubst expr1 in
  let (ty2,vctx'',tsubst'') = typing vctx' lctx tsubst' expr2 in
  let ty1' = apply_type_subst ty1 tsubst'' in
  match (ty1',ty2) with
  | (TVar a1,TVar a2) -> 
    let vctx_new = subst_vctx a1 com_ty vctx'' in
    let vctx_new' = subst_vctx a2 com_ty vctx_new in
    (res_ty, vctx_new',Pmap.modadd_pmap2 (a1,com_ty) (a2,com_ty) tsubst'')
  | (TVar a1, _) when ty2 = com_ty -> 
    let vctx_new = subst_vctx a1 com_ty vctx'' in
    (res_ty,vctx_new,Pmap.modadd_pmap (a1,com_ty) tsubst'')
  | (_,TVar a2) when ty1 = com_ty -> 
    let vctx_new = subst_vctx a2 com_ty vctx'' in
    (res_ty,vctx_new,Pmap.modadd_pmap (a2,com_ty) tsubst'')
  | (_,_) when (ty1 = com_ty) && (ty2 = com_ty) -> (res_ty,vctx'',tsubst'')
  | (_,_) when (ty1 <> com_ty) -> raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML com_ty)))
  | (_,_) -> raise (TypingError ((string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML com_ty)))

let typing_full polyflag intflag expr =
  let (ty,_,_) = typing Pmap.empty Pmap.empty Pmap.empty expr in
  if polyflag then ty
  else if intflag then close_type TInt ty
  else close_type TUnit ty
