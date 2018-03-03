fix fact(n:Int):(Int -> Int) -> 
  if (n <= 1) then 1
  else n * fact (n-1)
