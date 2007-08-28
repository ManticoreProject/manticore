fun f (n : int) = 
  if (n < 0) then 
      f (n+1) 
  else if (n > 0) then
      f (0-n)
  else
      2;

(| f(1000 : int), f(~1000 : int) |) 
