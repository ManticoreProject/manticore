(* error.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Common infrastructure for error reporting in the Manticore compiler.
 *)

structure Error :> sig

  (* logical positions in the input stream *)
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span

    type err_stream

  (* make an error stream. *)
    val mkErrStream : string -> err_stream

    val anyErrors : err_stream -> bool
    val sourceFile : err_stream -> string
    val sourceMap : err_stream -> AntlrStreamPos.sourcemap

  (* add error messages to the error stream *)
    val error : err_stream * string list -> unit
    val errorAt : err_stream * span * string list -> unit

  (* add warning messages to the error stream *)
    val warning : err_stream * string list -> unit
    val warningAt : err_stream * span * string list -> unit

  (* add an ml-antlr parse error to the error stream *)
    val parseError : ('tok -> string)
	  -> err_stream
	    -> (pos * 'tok AntlrRepair.repair_action)
	      -> unit

  (* print the errors to an output stream *)
    val report : TextIO.outstream * err_stream -> unit

  (* source-code locations *)
    datatype location
      = UNKNOWN
      | LOC of {file : string, l1 : int, c1 : int, l2 : int, c2 : int}

    val location : err_stream * span -> location
    val position : err_stream * pos -> location

    val locToString : location -> string

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

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
	srcFile		: string,
	sm		: SP.sourcemap,	(* the source map for mapping positions to *)
					(* source-file positions *)
	errors		: error list ref,
	numErrors	: int ref,
	numWarnings	: int ref
      }

  (* make an error stream. *)
    fun mkErrStream filename = ES{
	    srcFile = filename,
	    sm = SP.mkSourcemap' filename,
	    errors = ref [],
	    numErrors = ref 0,
	    numWarnings = ref 0
	  }

    fun anyErrors (ES{numErrors, ...}) = (!numErrors > 0)
    fun sourceFile (ES{srcFile, ...}) = srcFile
    fun sourceMap (ES{sm, ...}) = sm

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

  (* add error messages to the error stream *)
    fun error (es, msg) = addErr (es, NONE, String.concat msg)
    fun errorAt (es, span, msg) = addErr (es, SOME span, String.concat msg)

  (* add warning messages to the error stream *)
    fun warning (es, msg) = addWarn (es, NONE, String.concat msg)
    fun warningAt (es, span, msg) = addWarn (es, SOME span, String.concat msg)

  (* sort a list of errors by position in the source file *)
    val sort = let
	  fun lt (NONE, NONE) = false
	    | lt (NONE, _) = true
	    | lt (_, NONE) = false
	    | lt (SOME(l1, r1), SOME(l2, r2)) = (case Position.compare(l1, l2)
		 of LESS => true
		  | EQUAL => (Position.compare(r1, r2) = LESS)
		  | GREATER => false
		(* end case *))
	  fun cmp (e1 : error, e2 : error) = lt(#pos e1, #pos e2)
	  in
	    ListMergeSort.sort cmp
	  end

  (* source-code locations *)
    datatype location
      = UNKNOWN
      | LOC of {file : string, l1 : int, c1 : int, l2 : int, c2 : int}

    fun location (ES{sm, ...}, (p1, p2) : span) =
	  if (p1 = p2)
	    then let
	      val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p1
	      in
		LOC{file=f, l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	      end
	    else let
	      val {fileName=SOME f1, lineNo=l1, colNo=c1} = SP.sourceLoc sm p1
	      val {fileName=SOME f2, lineNo=l2, colNo=c2} = SP.sourceLoc sm p2
	      in
		if (f1 <> f2)
		  then LOC{file=f1, l1=l1, c1=c1, l2=l1, c2=c1}
		  else LOC{file=f1, l1=l1, c1=c1, l2=l2, c2=c2}
	      end

    fun position (ES{sm, ...}, p : pos) = let
	  val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p
	  in
	    LOC{file=f, l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	  end

    fun locToString UNKNOWN = "<unknown>"
      | locToString (LOC{file, l1, l2, c1, c2}) =
	  if (l1 = l2)
	    then if (c1 = c2)
	      then F.format "[%s:%d.%d] " [F.STR file, F.INT l1, F.INT c1]
	      else F.format "[%s:%d.%d-%d] " [F.STR file, F.INT l1, F.INT c1, F.INT c2]
	    else F.format "[%s:%d.%d-%d.%d] " [
		F.STR file, F.INT l1, F.INT c1, F.INT l2, F.INT c2
	      ]

    fun printError (outStrm, ES{sm, ...}) = let
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
		  TextIO.output (outStrm, String.concat [pos, kind, ": ", msg, "\n"])
		end
	  in
	    pr
	  end

    fun report (outStrm, es as ES{errors, ...}) =
	  List.app (printError (outStrm, es)) (sort (!errors))

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

  end
