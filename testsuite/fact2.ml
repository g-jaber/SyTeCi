fun (n:Int) ->
  let aux = fix aux_fact(m:Int):(Int -> (Int -> Int)) -> fun (res:Int) ->
     if (m <= 1) then res
     else aux_fact (m-1) (m * res)
  in aux n 1