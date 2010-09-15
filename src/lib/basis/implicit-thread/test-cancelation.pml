structure Main =
  struct

    structure P = ParallelSuspensions

    val dfltN = 22

fun d () = (Print.printLn "d"; d (); 0)

fun fib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => fib(i-1) + fib(i-2)
      (* end case *))

fun f () =
  let
      val s1 = P.new (fn _ => (P.spawnTask (P.new (fn _ => d ()));
			       d ()))
      val _ = P.spawnTask s1
      val _ = Print.printLn "spawned s1"
      val _ = fib 39
      val _ = P.cancel s1
      val _ = Print.printLn "cancelled s1"
      val _ = PrimIO.readInt ()
  in
      ()
  end
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    val () = Print.printLn "Initializing work stealing scheduler"

	    val ws = WorkStealing.workGroup ()
	    val () = Print.printLn "Testing Parallel Suspensions"
	    val x = ImplicitThread.runOnWorkGroup(ws, f)
	in
	    Print.printLn "done"
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
