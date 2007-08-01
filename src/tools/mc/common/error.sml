(* error.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Common infrastructure for error reporting in the Manticore compiler.
 *)

structure Error :> sig

  (* logical positions in the input stream *)
    type pos

    type err_stream

  (* make an error stream. *)
    val mkErrStream : string -> err_stream

  (* global flag to record the existance of errors *)
    val anyErrors : bool ref

  (* the current input file *)
    val sourceFile : string ref

  (* the current sourcemap *)
    val sourceMap : AntlrStreamPos.sourcemap ref

  (* print an error message and record the fact that there was an error *)
    val say : string list -> unit

  end = struct

    structure SP = AntlrStreamPos
    structure Repair = AntlrRepair
    structure F = Format

    type pos = SP.pos
    type span = SP.span

    datatype severity = WARN | ERR

    type error = {
	kind : severity,
	pos : span option,
	msg : string
      }

  (* an error stream collects the errors and warnings generated for
   * a compilation unit.
   *)
    datatype err_stream = ES of {
	sm		: SP.sourcemap,	(* the source map for mapping positions to *)
					(* source-file positions *)
	errors		: error list ref,
	numErrors	: int ref,
	numWarnings	: int ref
      }

  (* make an error stream. *)
    fun mkErrStream filename = ES{
	    sm = SP.mkSourcemap' filename,
	    errors = ref [],
	    numErrors = ref 0,
	    numWarnings = ref 0
	  }

    fun addErr (ES{errors, numErrors, ...}, pos, msg) = (
	  numErrors := !numErrors + 1;
	  errors := {kind=ERR, pos=pos, msg=msg} :: !errors)
	  
    fun addWarn (ES{errors, numWarnings, ...}, pos, msg) = (
	  numWarnings := !numWarnings + 1;
	  errors := {kind=WARN, pos=pos, msg=msg} :: !errors)

    fun parseError tok2str es (pos, repair) = let
	  val toksToStr = (String.concatWith " ") o (List.map tok2str)
	  val msg = (case repair
		 of Repair.Insert toks => ["syntax error; try inserting \"", toksToStr toks, "\""]
		  | Repair.Delete toks => ["syntax error; try deleting \"", toksToStr toks, "\""]
		  | Repair.Subst{old, new} => [
			"syntax error; try substituting \"", toksToStr new, "\" for \"",
			toksToStr old, "\""
		      ]
		  | Repair.FailureAt tok => ["syntax error at ", tok2str tok]
		(* end case *))
	  in
	    addErr (es, SOME(pos, pos), String.concat msg)
	  end

    fun printError (ES{sm, ...}) = let
	  fun pr {kind, pos, msg} = let
		val kind = (case kind of ERR => "Error" | Warn => "Warning")
		val pos = (case pos
		       of SOME(p1, p2) => if (p1 = p2)
			    then let
			      val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p1
			      in
				F.format "[%s:%d.%d] " [
				    F.STR f, F.INT lineNo, F.INT colNo
				  ]
			      end
			    else let
			      val {fileName=SOME f1, lineNo=l1, colNo=c1} = SP.sourceLoc sm p1
			      val {fileName=SOME f2, lineNo=l2, colNo=c2} = SP.sourceLoc sm p2
			      in
				if (f1 <> f2)
				  then F.format "[%s:%d.%d-%s:%d.%d] " [
				      F.STR f1, F.INT l1, F.INT c1,
				      F.STR f2, F.INT l2, F.INT c2
				    ]
				else if (l1 <> l2)
				  then F.format "[%s:%d.%d-%d.%d] " [
				      F.STR f1, F.INT l1, F.INT c1,
				      F.INT l2, F.INT c2
				    ]
				  else F.format "[%s:%d.%d-%d] " [
				      F.STR f1, F.INT l1, F.INT c1, F.INT c2
				    ]
			      end
		      (* end case *))
		in
		  TextIO.output (TextIO.stdOut, String.concat [pos, kind, ": ", msg])
		end
	  in
	    pr
	  end

  (* global flag to record the existance of errors *)
    val anyErrors = ref false

  (* the current input file *)
    val sourceFile = ref ""

  (* the current sourcemap *)
    val sourceMap = ref(AntlrStreamPos.mkSourcemap())

  (* print an error message and record the fact that there was an error *)
    fun say l = (
	  anyErrors := true;
	  TextIO.output(TextIO.stdErr, String.concat l);
	  TextIO.output1(TextIO.stdErr, #"\n"))

  end
