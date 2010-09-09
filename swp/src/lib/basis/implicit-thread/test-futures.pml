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
	      val fut = MultilispFuture.future(fn () => pfib(i-1), false)
	      in
	        pfib(i-2) + MultilispFuture.touch fut
	      end
       (* end case *))

val () = Print.printLn "Initializing swp scheduler"
val swp = WorkStealing.workGroup()
val () = Print.printLn "Testing multilisp futures"
val () = ImplicitThread.runOnWorkGroup(swp, fn () => pml_assert(doit pfib 21 4))

(* Parallel suspensions (no cancelation) *)

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val susp = ParSusp.delay(fn () => pfib(i-1), false)
	      val () = ParSusp.run susp
	      in
	        pfib(i-2) + ParSusp.force susp
	      end
      (* end case *))

val () = Print.printLn "Initializing global BFS scheduler"
val globalBFS = GlobalBFSScheduler.workGroup()
val () = Print.printLn "Testing lazy futures"
val x = ImplicitThread.runOnWorkGroup(globalBFS, fn () => pml_assert(doit pfib 21 2))

(* Single-toucher parallel suspensions (no cancelation) *)

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val susp = ParSusp1.delay(fn () => pfib(i-1), false)
	      val () = ParSusp1.run susp
	      in
	        pfib(i-2) + ParSusp1.force susp
	      end
      (* end case *))

val x = ImplicitThread.runOnWorkGroup(globalBFS, fn () => pml_assert(doit pfib 21 2))

fun cancel1 () = let
      val susp1 = ParSusp.delay(fn () => (
				  Print.printLn "starting susp1";
				  fib 23;
				  Print.printLn "finished susp1"
				), true)
      val susp2 = ParSusp.delay(fn () => (
				  Print.printLn "starting susp2";
				  fib 23;
				  Print.printLn "finished susp2"
				), true)
      in
        ParSusp.run susp1;
        fib 20;
	ParSusp.cancel susp1;
        ParSusp.run susp2;
	ParSusp.force susp2;
	Print.printLn "canceled susp1";
	()
      end

val () = ImplicitThread.runOnWorkGroup(globalBFS, cancel1)

val () = Print.printLn "Finished futures tests"

