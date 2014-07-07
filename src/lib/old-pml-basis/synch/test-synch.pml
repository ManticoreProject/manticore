(* test-synch.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Test the synchronization routines.
 *)

structure PT = PrimTypes 
structure O = Option
structure Q = LockedQueue

(*** locked queues: check that elements do not get dropped ***)
fun queueToList q = let
      fun acc ls = (
	    case LockedQueue.dequeue q
	     of O.NONE => List.rev ls
	      | O.SOME x => acc(x :: ls)
            (* end case *))
      in
         acc nil
      end

fun listToQueue ls = let
      val q = Q.new()
      in
        List.app(fn x => Q.enqueue(q, x)) ls;
	q
      end

fun check1 n = let
      val ls = List.tabulate(n, fn i => i)
      val q = listToQueue ls
      val ls' = queueToList q
      in
        List.length ls = List.length ls' andalso List.all(List.zipWith(fn (x, y) => x = y, ls, ls'))
      end
      
val () = pml_assert(check1 14)

(*** locked queues: use across processors ***) 
fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

val n = 100
val q = Q.new()
fun thd () = let
      fun doit i = if i < 0 then ()
		   else (
		       delay 20;
		       Q.enqueue(q, i);
		       doit(i-1))
    in
      doit n
    end
fun wait i =
      if i < 0 then ()
      else (case Q.dequeue q
	     of O.NONE => wait i
	      | O.SOME x => wait(i-1))

val () = List.app (fn vp => VProcExtras.spawnOn thd vp) (VProcExtras.vprocs())
val () = wait n

val () = Print.printLn "Synch tests finished"
