(* test.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TestCompile =
  struct

  val _ = (
      SMLofNJ.Internals.TDP.mode := true;
      Coverage.install ();
      BackTrace.install() )

    structure AMD64TargetSpec = AMD64TargetSpecFn (
    val abiName = "SVID"
    val osName = "linux" )
    structure AMD64CG = AMD64GenFn (structure Spec = AMD64TargetSpec)

    structure Opt = CFGOptFn (AMD64TargetSpec)

    fun prHdr msg = print(concat["******************** ", msg,  "********************\n"])

    fun load file = let
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

    fun init file = BackTrace.monitor (fn () =>
	let val cMap = InterpCFG.runtime()
	    val cfg = load file
	in
	    T.compile (cfg, file ^".s")
	end)

  end
