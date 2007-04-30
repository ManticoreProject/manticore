(* test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Test =
  struct

    structure BOMOpt = BOMOptFn (DummySpec)
    structure CPSOpt = CPSOptFn (DummySpec)
    structure CFGOpt = CFGOptFn (DummySpec)

    fun prHdr msg = print(concat["******************** ", msg,  "********************\n"])

    fun cvt file = let
	  val bom = BOMParser.parse file
	  val _ = (
		prHdr "BOM after expand";
		PrintBOM.print bom)
	  val bom = BOMOpt.optimize bom
	  val _ = (
		prHdr "BOM after optimization";
		PrintBOM.print bom)
	  val cps = Convert.transform bom
	  val _ = (
		prHdr "CPS after convert";
		PrintCPS.print cps)
	  val cps = CPSOpt.optimize cps
	  val _ = (
		prHdr "CPS after optimization";
		PrintCPS.print cps)
	  val cfg = FlatClosure.convert cps
	  val _ = (
		prHdr "CFG after closure";
		PrintCFG.output {types=true} (TextIO.stdOut, cfg))
          val _ = CheckCFG.check cfg
	  in
	    cfg
	  end

  end
