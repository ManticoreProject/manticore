(* main-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor MainFn (

    structure Spec : TARGET_SPEC
    structure CG : CODE_GEN

  ) : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    val _ = (
        SMLofNJ.Internals.TDP.mode := true;
        Coverage.install ();
        BackTrace.install ())

    structure Version = VersionFn (Spec)
    structure BOMOpt = BOMOptFn (Spec)
    structure CPSOpt = CPSOptFn (Spec)
    structure CFGOpt = CFGOptFn (Spec)

    fun prHdr msg = print(concat["******************** ", msg,  "********************\n"])

  (* *)
    fun bomToCFG bom = let
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
		PrintCFG.output {types=true} (TextIO.stdOut, cfg);
		CheckCFG.check cfg)
	  val cfg = CFGOpt.optimize cfg
	  val _ = (
		prHdr "CFG after cfg-opt";
		PrintCFG.print cfg;
		CheckCFG.check cfg)
	  in
	    cfg
	  end

    fun codegen (outFile, cfg) = let
	  val outStrm = TextIO.openOut outFile
	  fun doit () = CG.codeGen {dst=outStrm, code=cfg}
	  in	  
	    AsmStream.withStream outStrm doit ();
	    TextIO.closeOut outStrm
	  end (* compile *)

    fun bomC (bomFile, asmFile) = let
	  val bom = BOMParser.parse bomFile
	  val _ = (
		prHdr "BOM after expand";
		PrintBOM.print bom)
	  in
	    codegen (asmFile, bomToCFG bom)
	  end

    fun mantC (srcFile, asmFile) = raise Fail "Manticore frontend not done yet"

    fun doFile file = BackTrace.monitor (fn () =>let
	  fun asmFile stem = OS.Path.joinBaseExt{base=stem, ext=SOME ".s"}
	  in
	    case OS.Path.splitBaseExt file
	     of {base, ext=SOME "bom"} => bomC(file, asmFile base)
	      | {base, ext=SOME "pml"} => mantC(file, asmFile base)
	      | _ => raise Fail "unknown source file extension"
	    (* end case *)
	  end)

    fun main (cmd, [file]) = (doFile file; OS.Process.success)
      | main (cmd, _) = (
	  TextIO.output(TextIO.stdErr, concat["usage: ", cmd, " file\n"]);
	  OS.Process.failure)
 
  end
