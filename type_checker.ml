open Syntax

exception TypingError of string

type var_ctx = (typeML * Syntax.id) list
type loc_ctx = (typeML * Syntax.loc) list

let of_type ty ty1 =
  if (ty = ty1)  then ty else raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML ty)))

let of_type_bin com_ty ty1 ty2 res_ty =
  if (com_ty = ty1)  then begin
     if (com_ty = ty2) then res_ty else raise (TypingError ((string_of_typeML ty2) ^ " is not equal to " ^(string_of_typeML com_ty)))
  end else raise (TypingError ((string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML com_ty)))

let rec typing vctx lctx expr = match expr with
  | Var x -> List.assoc x vctx 
  | Loc l -> List.assoc l lctx  
  | Unit -> TUnit
  | Int _ -> TInt
  | Bool _ -> TBool
  | Plus (e1,e2) | Minus (e1,e2) | Mult (e1,e2) | Div (e1,e2) -> begin
    try of_type_bin TInt (typing vctx lctx e1) (typing vctx lctx e2) TInt
    with TypingError msg -> raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ msg)) end
  | And (e1,e2) | Or (e1,e2) -> of_type_bin TBool (typing vctx lctx e1) (typing vctx lctx e2) TBool
  | Equal (e1,e2) | NEqual (e1,e2) | Less (e1,e2) | LessEq (e1,e2) | Great (e1,e2) | GreatEq (e1,e2)  -> of_type_bin TInt (typing vctx lctx e1) (typing vctx lctx e2) TBool
  | Not e -> of_type TBool (typing vctx lctx e)
  | If (e1,e2,e3) -> let ty1 = typing vctx lctx e1 in
                     let ty2 = typing vctx lctx e2 in
                     let ty3 = typing vctx lctx e3 in
                     if ((ty1 = TBool) && (ty2 = ty3)) then ty2 else raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^(string_of_typeML ty1) ^ " is not equal to " ^(string_of_typeML TBool)))
  | Fun (var,ty,e) -> let ty' = typing ((var,ty)::vctx) lctx e in TArrow (ty,ty')
  | Fix (idfun,tyf,var,tyv,e) -> let _ =  typing ((var,tyv)::(idfun,tyf)::vctx) lctx e in tyf 
  | Let (var,e1,e2) -> let ty = typing vctx lctx e1 in typing ((var,ty)::vctx) lctx e2
  | App (e1,e2) -> begin match (typing vctx lctx e1,typing vctx lctx e2) with
                       | (TArrow (ty1,ty2),ty3) -> if (ty1 = ty3) then ty2 else raise (TypingError ("Error typing " ^ (Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty1) ^ "  is not equal to " ^(string_of_typeML ty3)))
                       | (ty,_) -> raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty) ^ " is not an arrow type"))
                     end
  | Seq (e1,e2) -> let ty = typing vctx lctx e1 in
      if (ty = TUnit) then (typing vctx lctx e2) else raise (TypingError ("Error typing " ^(Syntax.string_of_exprML expr) ^ ": " ^ (string_of_typeML ty) ^ "  is not equal to " ^(string_of_typeML TUnit)))
  | Pair (e1,e2) -> let ty1 = typing vctx lctx e1 in
                    let ty2 = typing vctx lctx e2 in TProd (ty1,ty2)
  | Newref e -> let ty = typing vctx lctx e in TRef ty
  | Deref e -> begin match typing vctx lctx e with 
                      | TRef ty -> ty
                      | ty -> raise (TypingError ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ (string_of_typeML ty) ^ " is not a ref type"))
               end       
  | Assign (e1,e2) -> begin match (typing vctx lctx e1,typing vctx lctx e2) with
                        | (TRef ty1,ty2) when ty1 = ty2 -> TUnit
                        | (ty,_) -> raise (TypingError ("Error typing " ^ (Syntax.string_of_exprML expr) ^ " : " ^ (string_of_typeML ty) ^ " is not a ref type"))
                      end
  | Hole -> failwith ("Error: The typechecker cannot type a hole")                    
