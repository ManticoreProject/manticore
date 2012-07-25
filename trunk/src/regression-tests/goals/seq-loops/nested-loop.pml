fun loop1 x =
  if x = 0
  then Print.printLn "in loop1"
  else (let
    fun loop2 y =
      if y = 0
      then Print.print "\n"
      else (Print.print "in loop2 ";
	    loop2 (y-1))
    in 
      loop2 x
    end;
    loop1 (x - 1))

val _ = loop1 6
