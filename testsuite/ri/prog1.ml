let x = ref 0 in (fun (y:Unit) -> x:=!x+1
                  ,fun (y:Unit) -> !x)
