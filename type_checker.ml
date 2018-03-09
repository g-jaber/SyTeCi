open Syntax

exception TypingError of string


let rec typing vctx lctx expr = match expr with
  | Var x -> 
    begin match Pmap.lookup_pmap x vctx with
      | Some ty -> (ty,vctx)
      | None -> failwith ("Error: the variable " ^ x ^ " is not defined")
    end  
  | Loc l ->
      begin match Pmap.lookup_pmap l lctx with
      | Some ty -> (ty,vctx)
      | None -> failwith ("Error: the location " ^ (string_of_int l) ^ " is not defined")
    end  
  | Unit -> (TUnit,vctx)
  | Int _ -> (TInt,vctx)
  | Bool _ -> (TBool,vctx)
  | Plus (e1,e2) | Minus (e1,e2) | Mult (e1,e2) | Div (e1,e2) -> begin
    try of_type_bin vctx lctx TInt e1 e2 TInt
    with TypingError msg -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ msg) end
  | And (e1,e2) | Or (e1,e2) -> of_type_bin vctx lctx TBool e1 e2 TBool
  | Equal (e1,e2) | NEqual (e1,e2) | Less (e1,e2) | LessEq (e1,e2) | Great (e1,e2) | GreatEq (e1,e2)  -> of_type_bin vctx lctx TInt e1 e2 TBool
  | Not e -> of_type vctx lctx e TBool
  | If (e1,e2,e3) -> 
    let (ty1,vctx1) = of_type vctx lctx e1 TBool in
    let (ty2,vctx2) = typing vctx1 lctx e2 in
    let (ty3,vctx3) = typing vctx2 lctx e3 in
    begin match (ty2,ty3) with
      | (_,TVar a3) -> (ty2,subst_tctx a3 ty2 vctx3)
      | (TVar a2,_) -> (ty3,subst_tctx a2 ty3 vctx3)
      | (_,_) when (ty2 = ty3) -> (ty2,vctx3)
      | _ -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^(string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML ty3))
    end  
  | Fun ((var,TUndef),e) ->
    let tvar = fresh_typevar () in
    let (ty',vctx') = typing ((var,tvar)::vctx) lctx e in
    let ty = List.assoc var vctx' in 
    (TArrow (ty,ty'),vctx')
  | Fun ((var,ty),e) ->
    let (ty',vctx') = typing ((var,ty)::vctx) lctx e in
    (TArrow (ty,ty'),vctx')
  | Fix ((idfun,TUndef),(var,TUndef),e) ->
    let tvar1 = fresh_typevar () in
    let tvar2 = fresh_typevar () in    
    let (ty',vctx') = typing ((var,tvar1)::(idfun,TArrow (tvar1,tvar2))::vctx) lctx e in
    let ty = List.assoc var vctx' in (TArrow (ty,ty'),vctx')  
  | Fix ((idfun,tyf),(var,tyv),e) -> 
    let (_,vctx') =  typing ((var,tyv)::(idfun,tyf)::vctx) lctx e in (tyf,vctx')
  | Let (var,e1,e2) -> let (ty,vctx') = typing vctx lctx e1 in typing ((var,ty)::vctx') lctx e2
  | App (e1,e2) -> 
    let (fty,vctx') = typing vctx lctx e1 in
    let (aty,vctx'') = typing vctx' lctx e2 in
    begin match (fty,aty) with
      | (TVar a,_) -> 
        let tvar = fresh_typevar () in
        (tvar,subst_tctx a (TArrow (aty,tvar)) vctx'')      
      | (TArrow (ty',ty),_) when ty' = aty -> (ty,vctx'')
      | (TArrow (TVar a,ty),_) -> (ty,subst_tctx a aty vctx'')
      | (TArrow (ty',ty),TVar a) -> (ty,subst_tctx a ty' vctx'')
      | (TArrow (ty',ty),_) when ty' <> aty -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty') ^ " is not equal to " ^(string_of_typeML aty))
      | _ -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML fty) ^ " is not an arrow type")
    end  
  | Seq (e1,e2) -> let (_,vctx') = of_type vctx lctx e1 TUnit in  typing vctx' lctx e2
(*    if (ty = TUnit) then (typing vctx' lctx e2) else raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty) ^ "  is not equal to " ^(string_of_typeML TUnit)))*)
  | Pair (e1,e2) -> let (ty1,vctx') = typing vctx lctx e1 in
                    let (ty2,vctx'') = typing vctx' lctx e2 in (TProd (ty1,ty2),vctx'')
  | Newref e -> let (ty,vctx') = typing vctx lctx e in (TRef ty,vctx')
  | Deref e -> 
    let (ty,vctx') = typing vctx lctx e in
    begin match ty with 
      | TRef ty -> (ty,vctx')
      | _ -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ (string_of_typeML ty) ^ " is not a ref type")
    end       
  | Assign (e1,e2) ->
    let (ty1,vctx') = typing vctx lctx e1 in
    let (ty2,vctx'') = typing vctx' lctx e2 in    
    begin match (ty1,ty2) with
      | (TRef ty1',_) when ty1' = ty2 -> (TUnit,vctx'')
      | (TRef ty1',TVar tvar) -> (TUnit,subst_tctx tvar ty1' vctx'')
      | (_,_) -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ (string_of_typeML ty1) ^ " is not a ref type")
    end
  | Hole -> failwith ("Error: The typechecker cannot type a hole")

and of_type vctx lctx expr resty = 
  let (ty ,vctx')= typing vctx lctx expr in
  match ty with
  | TVar tvar -> (resty,subst_tctx tvar resty vctx')
  | _ when ty = resty -> (ty,vctx')
  | _ -> failwith ("Error  typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ ((string_of_typeML ty) ^ " is not equal to " ^(string_of_typeML resty)))  
  
and of_type_bin vctx lctx com_ty expr1 expr2 res_ty =
  let (ty1,vctx') = typing vctx lctx expr1 in
  let (ty2,vctx'') = typing vctx' lctx expr2 in  
  match (ty1,ty2) with
    | (TVar a1,TVar a2) -> (res_ty,subst_tctx a2 com_ty (subst_tctx a1 com_ty vctx''))
    | (TVar a1, _) when ty2 = com_ty -> (res_ty,subst_tctx a1 com_ty vctx'')
    | (_,TVar a2) when ty1 = com_ty -> (res_ty,subst_tctx a2 com_ty vctx'')
    | (_,_) when (ty1 = com_ty) && (ty2 = com_ty) -> (res_ty,vctx'')
    | (_,_) when (ty1 <> com_ty) -> raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML com_ty)))
    | (_,_) -> raise (TypingError ((string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML com_ty)))  
