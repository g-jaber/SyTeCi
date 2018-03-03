let x = ref 0 in let b = ref 0 in 
  (fun (f:Unit->Unit) -> if !b = 0 then b:=1;f(); x:=!x+1; b:=0 else ()
   ,fun (y:Unit) -> !x)
