(* fib-test.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)




            fun seqFib (n : int) =  (case n
         of 0 => 0
	  | 1 => 1
	  | n => let
              val x = seqFib (n-1)
	      val y = seqFib (n-2)
	      in
	        x + y
	      end
         (* end case *))
;


fun timeTest () = let
    val n = readint()

    val b = gettimeofday ()
    val _ = seqFib(n)
    val e = gettimeofday ()
    in
        print (dtos (e-b)^"\n")
    end
;

timeTest()
