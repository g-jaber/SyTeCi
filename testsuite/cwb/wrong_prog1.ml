let x = ref 0 in (fun (f:Unit->Unit) -> f(); x:= (!x+1),fun (y:Unit) -> !x)
