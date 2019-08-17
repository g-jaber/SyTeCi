open Pmap
open Syntax
open Logic
open Skor

type gsubst = (id,exprML) pmap

let string_of_gsubst = string_of_pmap "=" string_of_exprML

let rec is_ground_term vars = function
  | Var x -> begin match lookup_pmap x vars with
      | None -> false
      | Some _ -> true
    end
  | Loc _ | Unit | Int _ | Bool _ -> true
  | Plus (expr1,expr2) | Minus (expr1,expr2) | Mult (expr1,expr2) | Div (expr1,expr2)
  | And (expr1,expr2) | Or (expr1,expr2)
  | Equal (expr1,expr2) | NEqual (expr1,expr2)
  | Less (expr1,expr2) | LessEq (expr1,expr2) | Great (expr1,expr2)
  | GreatEq (expr1,expr2) | Pair (expr1,expr2) ->
    (is_ground_term vars expr1) && (is_ground_term vars expr2)
  | Not expr -> is_ground_term vars expr
  | If (expr1,expr2,expr3) ->
    (is_ground_term vars expr1) && (is_ground_term vars expr2)
    && (is_ground_term vars expr3)
  | _ -> false


let is_ground_term_with_hole vars expr =
  let bin_bool_or (b1,b2) (b3,b4) = (b1 || b3, b2 && b4) in
  let rec aux = function
    (* First boolean indicates if the term contain a Hole, the second boolean
       indicates if the term is ground *)
    | Hole -> (true,true)
    | Var x -> begin match lookup_pmap x vars with
        | None -> (false,false)
        | Some _ -> (false,true)
      end
    | Loc _ | Unit | Int _ | Bool _ -> (false,true)
    | Plus (expr1,expr2) | Minus (expr1,expr2) | Mult (expr1,expr2)
    | Div (expr1,expr2) | And (expr1,expr2) | Or (expr1,expr2)
    | Equal (expr1,expr2) | NEqual (expr1,expr2) | Less (expr1,expr2)
    | LessEq (expr1,expr2) | Great (expr1,expr2)
    | GreatEq (expr1,expr2) | Pair (expr1,expr2) ->
      bin_bool_or (aux expr1) (aux expr2)
    | Not expr -> aux expr
    | If (expr1,expr2,expr3) ->
      bin_bool_or (aux expr1) (bin_bool_or (aux expr2) (aux expr3))
    | _ -> (false,false)
  in let (b1,b2) = aux expr
  in b1 && b2

let rec abstr_fo expr = match expr with
  | Mult (Var x1, Int n) ->
    let newvar = Var (fresh_lvar ()) in
    (newvar,[AEqual (newvar, Mult (Var x1,Int n))])
  | Mult (Var x1, Var x2) ->
    let newvar = Var (fresh_lvar ()) in
    (newvar,[AEqual (newvar, Mult (Var x1,Var x2))])
  | Mult (Var x1, Mult (Var x2, (App (_,_) as expr'))) ->
    let newvar = Var (fresh_lvar ()) in
    (Mult (newvar,expr'),[AEqual (newvar, Mult (Var x1,Var x2))])
  | App (expr1, expr2) ->
    let (expr2',preds) = abstr_fo expr2 in (App (expr1,expr2'),preds)
  | _ -> (expr,[])

let apply_fourth f = function
  | None -> None
  | Some (support,gsubst1,gsubst2,e) -> Some (support,gsubst1,gsubst2,f e)

let rec unif flag vars support gsubst1 gsubst2 (expr1,expr2) =
  match (expr1,expr2) with
  | (Var x, Var y) when x = y -> Some (support,gsubst1,gsubst2,expr2)
  | (expr, Var x) when (is_ground_term vars expr) ->
    if flag then Some (support,(x,expr)::gsubst1,gsubst2,expr2)
    else begin
      let y = fresh_lvar () in
      Some ((y,TInt)::support,(y,expr)::gsubst1,(y,expr2)::gsubst2,Var y)
    end
  | (Loc l1, Loc l2) ->
    if (l1 = l2) then Some (support,gsubst1,gsubst2,expr2)
    else None
  | (Unit,Unit) -> Some (support,gsubst1,gsubst2,expr2)
  | (Int n1,Int n2) ->
    if (n1 = n2) then Some (support,gsubst1,gsubst2,expr2)
    else None
  | (Bool b1, Bool b2) ->
    if (b1 = b2) then Some (support,gsubst1,gsubst2,expr2)
    else None
  | (Plus (e11,e12),Plus (e21,e22)) | (Minus (e11,e12),Minus (e21,e22))
  | (Mult (e11,e12),Mult (e21,e22)) | (Div (e11,e12),Div (e21,e22))
  | (And (e11,e12),And (e21,e22)) | (Or (e11,e12),Or (e21,e22))
  | (Equal (e11,e12),Equal (e21,e22)) | (NEqual (e11,e12),NEqual (e21,e22))
  | (Less (e11,e12),Less (e21,e22)) | (LessEq (e11,e12),LessEq (e21,e22))
  | (Great (e11,e12),Great (e21,e22)) | (GreatEq (e11,e12),GreatEq (e21,e22))
  | (App (e11,e12),App (e21,e22)) | (Seq (e11,e12),Seq (e21,e22))
  | (Pair (e11,e12),Pair (e21,e22))
  | (Assign (e11,e12),Assign (e21,e22))
  | (Let (_,e11,e12),Let (_,e21,e22)) ->
    let funconstr = get_consfun_from_binexpr expr2 in
    unif_aux_bin flag funconstr vars support gsubst1 gsubst2 (e11,e21) (e12,e22)
  | (Not e1,Not e2) | (Newref e1,Newref e2) | (Deref e1,Deref e2) ->
    let funconstr = get_consfun_from_unexpr expr2 in
    apply_fourth funconstr (unif flag vars support gsubst1  gsubst2 (e1,e2))
  | (Fun ((_,_),e1),Fun ((var2,ty2),e2)) ->
    apply_fourth (fun x -> Fun ((var2,ty2),x))
      (unif flag vars support gsubst1 gsubst2 (e1,e2))
  | (Fix ((_,_),(_,_),e1),Fix ((idfun2,ty2),(var2,tyv2),e2)) ->
    apply_fourth (fun x -> Fix ((idfun2,ty2),(var2,tyv2),x))
      (unif flag vars support gsubst1 gsubst2 (e1,e2))
  | (If (e11,e12,e13), If (e21,e22,e23)) ->
    begin match (unif flag vars support gsubst1 gsubst2 (e11,e21)) with
      | None -> None
      | Some (support',gsubst1',gsubst2',e21') ->
        unif_aux_bin flag (fun (x,y) -> If (e21',x,y))
          vars support' gsubst1' gsubst2' (e12,e22) (e13,e23)
    end
  | (_,_) -> None

and unif_aux_bin flag funconstr vars support gsubst1 gsubst2 u v =
  match (unif flag vars support gsubst1 gsubst2 u) with
  | None -> None
  | Some (support',gsubst1',gsubst2',e1) ->
    apply_fourth (fun y -> funconstr (e1,y))
      (unif flag vars support' gsubst1' gsubst2' v)

let unif_sequent flag sequent_1 sequent_2 =
  match (sequent_1.formula,sequent_2.formula) with
  | (RelSI (ty1, _, (expr11, _), (expr12, _)),
     RelSI (ty2, cb_context2, (expr21,rec_env21), (expr22, rec_env22))) ->
    if (ty1 != ty2) then None
    else begin
      Debug.print_debug ("Comparing : " ^ (string_of_exprML expr11)
                         ^ " and " ^ (string_of_exprML expr21));
      Debug.print_debug ("Comparing : " ^ (string_of_exprML expr12)
                         ^ " and " ^ (string_of_exprML expr22));
      let result1 = unif flag sequent_1.ground_var_ctx [] [] []
          (expr11,expr21) in
      let result2 = unif flag sequent_1.ground_var_ctx [] [] []
          (expr12,expr22) in
      begin match (result1,result2,flag) with
        | (Some (support1,gsubst11,gsubst12,expr21'),
           Some (support2,gsubst21,gsubst22,expr22'),false) ->
          let sequent' = update_sequent sequent_2 (support1@support2)
              (RelSI (ty2, cb_context2, (expr21',rec_env21),
                      (expr22', rec_env22))) in
          Some (gsubst11@gsubst21,gsubst12@gsubst22,sequent')
        | (Some ([],gsubst11,[],_),Some ([],gsubst21,[],_),true) ->
          Some (gsubst11@gsubst21,[],sequent_2)
        | (Some _,Some _,true) -> failwith "Error in unif_sequent"
        | (_,_,_) -> None
      end
    end
  | _ -> failwith "Cannot unify sequents whose SKOR is not RelSI"

let unif_in_hist sequent hist =
  let hist' = List.filter (fun (seq,_) -> seq.j > sequent.j) hist in
  let rec aux = function
    | [] -> None
    | (sequent',flag)::tl -> begin match unif_sequent flag sequent sequent' with
        | None -> aux tl
        | Some (gsubst1,gsubst2,sequent'') ->
          Some (gsubst1,gsubst2,sequent',sequent'')
      end
  in aux hist'
