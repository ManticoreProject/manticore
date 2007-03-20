module Hi
  extern void printInt (int); 
  fun init (arg : [int], k : cont(any), exh : cont(any)) =

      fun fib (i : int, k : cont(int), exh : cont(any)) =
	  if I32Lte(i, 0:int) then throw k(0:int)
	  else if I32Eq(i, 1:int) then throw k(1:int)
	  else
	    cont k1 (a : int) =
		 cont k2 (b : int) = throw k (I32Add(a, b))
		      apply fib (I32Sub(i, 2:int), k2, exh)
            apply fib (I32Sub(i, 1:int), k1, exh)

      fun printn (i : int, k : cont(any), exh : cont(any)) =
	  if I32Eq (i, 0:int) 
	  then let wi : [int] = wrap (1:int)
  	       throw k(wi)
	  else
	      cont k1 (x : int) =	            
 	           let () = ccall printInt (x)	     
  		   apply printn (I32Sub (i, 1:int), k, exh)
              apply fib (i, k1, exh)

      apply printn (unwrap (arg), k, exh)
