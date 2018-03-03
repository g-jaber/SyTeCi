fun (n:Int) ->
  let res = ref 1 in
  let aux = fix aux_fact(m:Int):(Int -> Unit) ->
     if (m <= 1) then ()
     else res := !res*m; aux_fact (m-1)
  in aux n; !res
