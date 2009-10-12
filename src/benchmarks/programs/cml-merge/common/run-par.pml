structure RunPar (* sig
  val run : (unit -> unit) -> unit
  end *) = struct

    fun run f =
	let
	    val b = Time.now ()
#ifndef SEQUENTIAL
	    val _ = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), f)
#else
	    val _ = f ()
#endif
	    val e = Time.now ()
	in
	    Print.printLn (Time.toString (e - b));
	    ()
	end

  end
