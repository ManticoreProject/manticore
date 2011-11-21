(* run-par.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RunPar (* sig
  val run : (unit -> 'a) -> 'a
  val runSilent : (unit -> 'a) -> 'a
  end *) = struct

  fun mkRun optToStr f = let
    val b = Time.now ()
#ifndef SEQUENTIAL
    val ans = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), f)
#else
    val ans = f ()
#endif
    val e = Time.now ()
    val _ = (case optToStr
      of SOME tos => Print.printLn (tos (e-b))
       | NONE => ()
      (* end case *))
    in
      ans
    end

  val run = mkRun (SOME Time.toString)
	    
  val runMicrosec = mkRun (SOME Time.toStringMicrosec)

  val runSilent = mkRun NONE

end
