(* pfib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

            fun pFib (n : int) = (case n
         of 0 => 0
	  | 1 => 1
	  | n => let
              dval x = pFib (n-1)
	      val y = pFib (n-2)
	      in
	        x + y
	      end
         (* end case *))
;


fun timeTest () = let
    val n = readint()

    val b = gettimeofday ()
    val _ = pFib(n)
    val e = gettimeofday ()
    in
        print (dtos (e-b)^"\n")
    end
;

timeTest()
