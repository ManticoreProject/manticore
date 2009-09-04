(* pfib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

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


fun pFib (n : int) = (
    case n
     of 0 => 0
      | 1 => 1
      | n => let
            val (x, y) = (| pFib (n-1), pFib (n-2) |)
	    in
	      x + y
	    end
    (* end case *))

fun bench () = let
    val _ = PrimIO.readInt()
    val n = PrimIO.readInt()
    val b = Time.now ()
    val pf = pFib n
    val e = Time.now ()
    in
    (* sanity test *)
      if pf = sFib n then () else (raise Fail "incorrect answer"; ());
      Print.print(Time.toString (e-b))
    end

val _ = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), bench)

