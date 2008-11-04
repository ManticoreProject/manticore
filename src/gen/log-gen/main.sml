(* main.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Main =
  struct

    structure GenInlineLogH = GeneratorFn (GenInlineLogH)
    structure GenLogEventsDef = GeneratorFn (GenLogEventsDef)
    structure GenLogEventsH = GeneratorFn (GenLogEventsH)

    fun main _ = let
	  val info = LoadFile.loadFile "log-events.json"
	  in
	    ()
	  end

  end

