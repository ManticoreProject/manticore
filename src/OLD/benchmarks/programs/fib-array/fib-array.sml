(* fib-array.sml
 * 
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Here is a synthetic benchmark that seeds a parallel array with repeated fib(i) 
 * computations. Supposing that the optimizer does not hoist this computation outside
 * the parallel-array map, we should expect a near linear speedup of the computation.
 *)

fun time susp = let
  fun now_us () = Time.toMicroseconds (Time.now ())
  val t0 = now_us ()
  val x = susp ()
  val t1 = now_us ()
  in
    (x, t1-t0)
  end

fun array_map f a =
 (case Array.length a
    of 0 => a
     | n => let
         val b = Array.array (n, f (Array.sub (a, 0)))
         fun lp i = 
           if i >= n then b
	   else let
             val _ = Array.update (b, i, f (Array.sub (a, i)))
             in
               lp (i+1)
	     end
         in
	   lp 1 (* did 0 already *)
         end)

fun sFib (n : int) = (
    case n
     of 0 => 0
      | 1 => 1
      | n => let
            val x = sFib (n-1)
            val y = sFib (n-2)
	    in
	      x + y
	    end
    (* end case *))

fun bench (i, n) = let
    val a = Array.tabulate (n, fn _ => i)
    val (a', t) = time (fn () => array_map sFib a)
    in
      print(LargeInt.toString t);
      print("\n")
    end

