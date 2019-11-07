open Syntax

let rec generate_kierstead_aux str_var i n =
  if n = 0 then begin 
    let var = Var (str_var ^ (string_of_int i)) in 
    App (var, Unit)
  end
  else begin
    let var_id = (str_var ^ (string_of_int n)) in
    App (Var "f",Fun ((var_id,TUndef),generate_kierstead_aux str_var  i (n-1)))
  end

let generate_kierstead str_var i n =
  Fun (("f",TUndef),generate_kierstead_aux str_var  i n)
