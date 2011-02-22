
structure Main =
  struct

structure P = ParallelSuspensions

    val dfltN = 22

fun fib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => fib(i-1) + fib(i-2)
      (* end case *))

fun doit pfib x i = (
    if i < 0 then ()
    else if pfib x = fib x
    then doit pfib x (i-1)
    else Print.printLn "ERROR")

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val susp = P.new (fn () => pfib(i-2))
	      val task = P.spawnTask susp
	      val v = pfib (i-1)
	      val _ = P.removeTask task
	      in
	        v + P.force susp
	      end
      (* end case *))
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    val () = Print.printLn "Initializing work stealing scheduler"

	    val ws = WorkStealing.workGroup ()
	    val () = Print.printLn "Testing Parallel Suspensions"
	    val x = ImplicitThread.runOnWorkGroup(ws, fn () => (pfib n; ()))
	in
	    Print.printLn "done"
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())


