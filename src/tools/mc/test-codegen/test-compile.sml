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

  fun compile (cfg, outFile) = let
	val outStrm = TextIO.openOut outFile
	val outStrmFG = TextIO.openOut (outFile^".fg")
	fun doit () = AMD64CG.Gen.codeGen {dst=outStrm, code=cfg}
	in	  
	  MLRiscControl.debug_stream := outStrmFG;
(*	  (MLRiscControl.flag "amd64-cfg-debug") := true;*)
	  (MLRiscControl.flag "dump-initial-cfg") := true;
	  AsmStream.withStream outStrm doit ();
	  TextIO.closeOut outStrm
	end (* compile *)

    fun init file = BackTrace.monitor (fn () => let
	  val cfg = load file
	  val asmFile = (case OS.Path.splitBaseExt file
		 of {base, ext=SOME "cps"} => OS.Path.joinBaseExt{base=base, ext=SOME "s"}
		  | _ => OS.Path.joinBaseExt{base=file, ext=SOME "s"}
		(* end case *))
	  in
	    compile (cfg, asmFile)
	  end)

    fun main (cmd, args) = (List.app init args; OS.Process.success)

  end
