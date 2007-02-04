(* test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Test =
  struct

    structure Opt = CFGOptFn (DummySpec)

    fun prHdr msg = print(concat["******************** ", msg,  "********************\n"])

    fun doit file = let
	  val cps = CPSParser.parse file
	  val _ = (
		prHdr "CPS after expand";
		PrintCPS.print cps)
	  val cfg = FlatClosure.convert cps
	  val _ = (
		prHdr "CFG after closure";
		PrintCFG.print cfg;
		CheckCFG.check cfg)
	  val cfg = Opt.optimize cfg
	  val _ = (
		prHdr "CFG after cfg-opt";
		PrintCFG.print cfg;
		CheckCFG.check cfg)
	  in
	    cfg
	  end

  end
