let x = ref 0 in let b = ref false in 
  (fun (f:Unit->Unit) -> if !b then () else (b:=true;f(); x:= (!x+1); b:=false)
   ,fun (y:Unit) -> !x)