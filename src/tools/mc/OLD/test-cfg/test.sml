(* test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Test =
  struct

    structure Opt = CFGOptFn (DummySpec)

    fun test module = let
	  val _ = PrintCFG.print module
	  val module = Opt.optimize module
	  in
	    CheckCG.check module;
	    PrintCFG.print module
	  end

  end
