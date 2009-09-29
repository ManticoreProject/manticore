fun p2s (x, y) = "(" ^ Float.toString x ^ "," ^ Float.toString y ^ ")"
(*
DEBUGGING
val S = [| (0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.5, 0.5), (0.5, 1.5) |]
val H = quickhull S
val _ = Print.printLn (PArray.toString p2s ", " H)
*)

fun bench (seqSz, n) = 
    let

	val b = Time.now ()
	val _ = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), fn () => 
		   let									   
		       val pts = tabP (n, fn _ => (Rand.randFloat (~100000.0, 100000.0), Rand.randFloat (~100000.0, 100000.0)))
		   in 
		       quickhull pts
		   end)
	val e = Time.now ()
    in
	(*
	Print.printLn (PArray.toString p2s ", " pts);
	Print.printLn (PArray.toString p2s ", " hull);
	 *)
	Print.printLn (Time.toString (e - b));
	()
    end

val () = bench(PrimIO.readInt(), PrimIO.readInt())


