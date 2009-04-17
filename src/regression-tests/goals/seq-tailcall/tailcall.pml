fun f (x, y) = 
  if x = 90000000 then
   (Print.printLn ("still going... " ^ Int.toString y);
    f (0, y+1))
  else
    f (x+1, y)

val _ = f (0, 0)
