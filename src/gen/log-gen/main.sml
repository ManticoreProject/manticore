(* main.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Main =
  struct

    fun main _ = let
	  val info = LoadFile.loadFile "log-events.json"
	  in
GenLogEventsDef.gen (TextIO.stdOut, info);
	    ()
	  end

  end

