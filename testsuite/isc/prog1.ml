let x = ref 0 in
fun f -> x:=1; f(); !x
