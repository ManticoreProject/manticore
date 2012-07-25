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

val () = Print.printLn "Finished futures tests"

