(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

    fun contract module = let
	  val _ = Census.census module;
	  val module = Contract.contract module
	  in
	    print "******************** after contract ********************\n";
	    PrintBOM.print module;
	    module
	  end

    fun expandAll module = (case ExpandHLOps.expand module
	   of SOME module => (
		print "******************** after expand ********************\n";
		PrintBOM.print module;
		contract(expandAll module))
	    | NONE => module
	  (* end case *))

    fun optimize module = let
	  val module = contract module
	  val module = expandAll module
	  val module = CaseSimplify.transform module
	  in
	    module
	  end

  end
