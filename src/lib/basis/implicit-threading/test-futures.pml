fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));
(* sequential fib (for a correctness check) *)
fun fib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => fib(i-1) + fib(i-2)
      (* end case *))

fun doit pfib x i = (
Print.printLn "doit";
    if i < 0 then true
    else if pfib x = fib x
    then doit pfib x (i-1)
    else false)


(* eager futures & work stealing & no cancelation *)
fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val fut = EagerFuture.future(fn () => pfib(i-1), false)
	      in
	        pfib(i-2) + EagerFuture.touch fut
	      end
       (* end case *))
val () = Print.printLn "cilk5"
val cilk5 = MultiprogrammedWorkStealing.workGroup()
val () = Print.printLn "eager futures"
val () = ImplicitThread.runWithGroup(cilk5, fn () => pml_assert(doit pfib 21 4))

(* lazy futures & global BFS & no cancelation *)

val globalBFS = GlobalBFSScheduler.workGroup()
(*
val x = ImplicitThread.runWithGroup(globalBFS, fn () =>
	   let 
	       val fut = LazyFuture.delay(fn () => fib 20, false)
	       val () = LazyFuture.run fut
	       val x = fib 20
	   in
	       x + LazyFuture.force fut
	   end)
val () = pml_assert(x = fib 20 * 2)
*)

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val fut = LazyFuture.delay(fn () => pfib(i-1), false)
	      val () = LazyFuture.run fut
	      in
	        pfib(i-2) + LazyFuture.force fut
	      end
      (* end case *))

val x = ImplicitThread.runWithGroup(globalBFS, fn () => pml_assert(doit pfib 24 6))

val () = Print.printLn "Finished futures tests"

