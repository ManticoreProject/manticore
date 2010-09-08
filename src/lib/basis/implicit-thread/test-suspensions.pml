(* sequential fib (for a correctness check) *)
fun fib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => fib(i-1) + fib(i-2)
      (* end case *))

fun doit pfib x i = (
    if i < 0 then true
    else if pfib x = fib x
    then doit pfib x (i-1)
    else false)

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val susp = ParallelSuspensions.new (fn () => pfib(i-1))
	      val () = ParallelSuspensions.spawn susp
	      in
	        pfib(i-2) + ParallelSuspensions.force susp
	      end
      (* end case *))

val () = Print.printLn "Initializing work stealing scheduler"
val ws = WorkStealing.workGroup ()
val () = Print.printLn "Testing Parallel Suspensions"
val x = ImplicitThread.runOnWorkGroup(ws, fn () => pml_assert(doit pfib 21 2))
