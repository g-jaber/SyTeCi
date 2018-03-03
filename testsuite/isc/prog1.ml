let x = ref 0 in fun (f:Unit->Unit) -> x:=1; f(); !x
