open Syntax

exception TypingError of string

let check_tyvar = function
  | (var,TUndef) ->
    let tvar = fresh_typevar () in (var,tvar)
  | tyvar -> tyvar

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
    let (_,vctx1) = of_type vctx lctx e1 TBool in
    let (ty2,vctx2) = typing vctx1 lctx e2 in
    let (ty3,vctx3) = typing vctx2 lctx e3 in
    begin match unify_type Pmap.empty (ty2,ty3) with
      | Some (ty,lsubst) -> (ty,lsubst_vctx lsubst vctx3)
      | None -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                          ^(string_of_typeML ty2) ^ " is not equal to "
                          ^(string_of_typeML ty3))
    end
  | Fun ((var,TUndef),e) ->
    let tvar = fresh_typevar () in
    let (ty',vctx') = typing (Pmap.modadd_pmap (var,tvar) vctx) lctx e in
    begin match Pmap.lookup_pmap var vctx' with
      | Some ty -> (TArrow (ty,ty'),vctx')
      | None -> failwith ("Variable " ^ var ^ " not found.")
    end
  | Fun ((var,ty),e) ->
    let (ty',vctx') = typing (Pmap.modadd_pmap (var,ty) vctx) lctx e in
    (TArrow (ty,ty'),vctx')
  | Fix ((idfun,TUndef),(var,TUndef),e) ->
    let tvar1 = fresh_typevar () in
    let tvar2 = fresh_typevar () in
    let (rty,vctx') = typing (Pmap.modadd_pmap2 (var,tvar1) (idfun,TArrow (tvar1,tvar2)) vctx) lctx e in
    begin match (Pmap.lookup_pmap var vctx',Pmap.lookup_pmap idfun vctx') with
      | (Some aty, Some fty) ->
        begin match unify_type Pmap.empty (fty,TArrow (aty,rty)) with
          | Some (ty,lsubst) -> (ty,lsubst_vctx lsubst vctx')
          | None -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                              ^(string_of_typeML fty) ^ " is not equal to "
                              ^(string_of_typeML (TArrow (aty,rty))))
        end
      | _ -> failwith "Variables not found"
      end
  | Fix ((idfun,TUndef),(var,aty),e) ->
    let tvar2 = fresh_typevar () in
    let (rty,vctx') = typing (Pmap.modadd_pmap2 (var,aty) (idfun,TArrow (aty,tvar2)) vctx) lctx e in
    begin match Pmap.lookup_pmap idfun vctx' with
      | Some fty -> 
        begin match unify_type Pmap.empty (fty,TArrow (aty,rty)) with
          | Some (ty,lsubst) -> (ty,lsubst_vctx lsubst vctx')
          | None -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                              ^(string_of_typeML fty) ^ " is not equal to "
                              ^(string_of_typeML (TArrow (aty,rty))))
        end
      | None -> failwith ("Variable " ^ idfun ^ " not found.")
      end
  | Fix ((idfun,fty),(var,aty),e) ->
    let (rty,vctx') =  typing (Pmap.modadd_pmap2 (var,aty) (idfun,fty) vctx) lctx e in
    begin match unify_type Pmap.empty (fty,TArrow (aty,rty)) with
      | Some (ty,lsubst) -> (ty,lsubst_vctx lsubst vctx')
      | None -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                          ^(string_of_typeML fty) ^ " is not equal to "
                          ^(string_of_typeML (TArrow (aty,rty))))
    end
  | Let (var,e1,e2) -> let (ty,vctx') = typing vctx lctx e1 in typing (Pmap.modadd_pmap (var,ty) vctx') lctx e2
  | LetPair (var1,var2,e1,e2) ->
    let (ty,vctx') = typing vctx lctx e1 in
    begin match ty with
      | TProd (ty1,ty2) -> typing (Pmap.modadd_pmap2 (var1,ty1) (var2,ty2) vctx') lctx e2
      | TVar tvar ->
        let tvar1 = fresh_typevar () in
        let tvar2 = fresh_typevar () in
        let vctx'' = subst_vctx tvar (TProd (tvar1,tvar2)) vctx' in
        typing (Pmap.modadd_pmap2 (var1,tvar1) (var2,tvar2) vctx'') lctx e2
      | _ -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr)
                       ^ " : " ^ (string_of_typeML ty) ^ " is not a product type")
    end
  | App (e1,e2) ->
    let (fty,vctx') = typing vctx lctx e1 in
    let (aty,vctx'') = typing vctx' lctx e2 in
    Debug.print_debug ("Typing App:" ^ (Syntax.string_of_exprML expr));
    Debug.print_debug ((Syntax.string_of_exprML e1) ^ " is of type " ^ (string_of_typeML fty));
    Debug.print_debug ((Syntax.string_of_exprML e2) ^ " is of type " ^ (string_of_typeML aty));
    begin match fty with
      | TVar a ->
        let tvar = fresh_typevar () in
        (tvar,subst_vctx a (TArrow (aty,tvar)) vctx'')
      | TArrow (ty',ty) ->
        begin match unify_type Pmap.empty (ty',aty) with
          | Some (_,lsubst) -> (ty,lsubst_vctx lsubst vctx'')
          | None -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": "
                              ^ (string_of_typeML ty') ^ " is not equal to "
                              ^(string_of_typeML aty))
        end
          (*
      | (TArrow (ty',ty),_) when ty' = aty -> (ty,vctx'')
      | (TArrow (TVar a,ty),_) -> (ty,subst_vctx a aty vctx'')
      | (TArrow (ty',ty),TVar a) -> (ty,subst_vctx a ty' vctx'')
      | (TArrow (ty',ty),_) when ty' <> aty ->
        failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": "
                  ^ (string_of_typeML ty') ^ " is not equal to "
                  ^(string_of_typeML aty)) *)
      | _ -> failwith ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": "
                       ^ (string_of_typeML fty) ^ " is not an arrow type")
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
      | _ -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : "
                       ^ (string_of_typeML ty) ^ " is not a ref type")
    end
  | Assign (e1,e2) ->
    let (ty1,vctx') = typing vctx lctx e1 in
    let (ty2,vctx'') = typing vctx' lctx e2 in
    begin match (ty1,ty2) with
      | (TRef ty1',_) when ty1' = ty2 -> (TUnit,vctx'')
      | (TRef ty1',TVar tvar) -> (TUnit,subst_vctx tvar ty1' vctx'')
      | (TRef (TVar tvar),_) -> (TUnit,subst_vctx tvar ty2 vctx'')
      | (_,_) -> failwith ("Error typing " ^ (Syntax.string_of_exprML expr)
                           ^ " : " ^ (string_of_typeML ty1) ^ " is not a ref type")
    end
  | Hole -> failwith ("Error: The typechecker cannot type a hole")

and of_type vctx lctx expr resty =
  let (ty ,vctx')= typing vctx lctx expr in
  match ty with
  | TVar tvar -> (resty,subst_vctx tvar resty vctx')
  | _ when ty = resty -> (ty,vctx')
  | _ -> failwith ("Error  typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ ((string_of_typeML ty) ^ " is not equal to " ^(string_of_typeML resty)))

and of_type_bin vctx lctx com_ty expr1 expr2 res_ty =
  let (ty1,vctx') = typing vctx lctx expr1 in
  let (ty2,vctx'') = typing vctx' lctx expr2 in
  match (ty1,ty2) with
    | (TVar a1,TVar a2) -> (res_ty,subst_vctx a2 com_ty (subst_vctx a1 com_ty vctx''))
    | (TVar a1, _) when ty2 = com_ty -> (res_ty,subst_vctx a1 com_ty vctx'')
    | (_,TVar a2) when ty1 = com_ty -> (res_ty,subst_vctx a2 com_ty vctx'')
    | (_,_) when (ty1 = com_ty) && (ty2 = com_ty) -> (res_ty,vctx'')
    | (_,_) when (ty1 <> com_ty) -> raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML com_ty)))
    | (_,_) -> raise (TypingError ((string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML com_ty)))

let typing_full polyflag expr =
  let (ty,_) = typing Pmap.empty Pmap.empty expr in
  if polyflag then ty
  else close_type TInt ty
