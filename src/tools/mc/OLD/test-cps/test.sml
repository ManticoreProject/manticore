(* test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Test =
  struct

    datatype value = datatype InterpCFG.value
    datatype raw_value = datatype InterpCFG.raw_value

    structure CPSOpt = CPSOptFn (DummySpec)
    structure CFGOpt = CFGOptFn (DummySpec)
    structure AMD64Gen = AMD64GenFn (structure Spec = DummySpec)

    fun prHdr msg = print(concat["******************** ", msg,  "********************\n"])

    fun cvt file = let
	  val cps = CPSParser.parse file
	  val _ = (
		prHdr "CPS after expand";
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

    fun load file = let
	  val cfg = cvt file
	  val cfg = CFGOpt.optimize cfg
	  val _ = (
		prHdr "CFG after cfg-opt";
		PrintCFG.output {types=true} (TextIO.stdOut, cfg))
          val _ = CheckCFG.check cfg
	  in
	    cfg
	  end

    fun init file = let
	  val cMap = InterpCFG.runtime()
	  val cfg = load file
	  in
	    InterpCFG.traceFlg := true;
	    (cMap, InterpCFG.load cMap cfg)
	  end

    fun apply (cMap, clos) value = InterpCFG.applyClos cMap (clos, value)

    val all = ["add1.cps","fact.cps","fib.cps",
               "float1.cps","float2.cps","float3.cps",
               "length.cps","map.cps","map2.cps",
               (*"mult-ls.cps",*)"sum-ls-foldr.cps","sum-ls.cps",
               "mutual-recursion.cps","mutual-recursion2.cps"]

    fun compile file = 
	let val cfg = load file
	    val outStrm = TextIO.openOut (file^".s")
	in
	    AMD64Gen.Gen.codeGen {dst=outStrm, code=cfg};
	    TextIO.closeOut outStrm
	end

    fun cvtAll () = List.map cvt all
    fun loadAll () = List.map load all
    fun initAll () = List.map init all
    fun compileAll () = List.map compile all

  end

