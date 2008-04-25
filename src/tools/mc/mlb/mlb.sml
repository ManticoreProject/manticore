(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load the compilation unit of an MLB file *)
    val load : (Error.err_stream * string) -> ParseTree.program list

  end = struct

    structure PT = MLBParseTree

  (* process a basis expression *)
    fun processBasExp (errStrm, mlb, pts) = (case mlb
        of PT.MarkBasExp {span, tree} => processBasExp (errStrm, tree, pts)
	 | PT.DecBasExp basDec => processBasDec (errStrm, basDec, pts)
	 | _ => raise Fail "todo"
        (* end case *))

  (* process a basis declaration *)
    and processBasDec (errStrm, basDec, pts) = (case basDec
        of PT.MarkBasDec {span, tree} => processBasDec (errStrm, tree, pts)
	 (* imports can be pml or mlb files *)
	 | PT.ImportBasDec file => let
           in
	      case OS.Path.splitBaseExt (Atom.toString file)
	       of {base, ext=SOME "mlb"} => loadMLB (errStrm, base, pts)
		| {base, ext=SOME "pml"} => loadPML (errStrm, base, pts)
		| _ => raise Fail "unknown source file extension"
           end
	 | _ => raise Fail "todo"
        (* end case *))

  (* load an MLB file *)
    and loadMLB (errStrm, base, pts) = raise Fail "todo"

  (* load a PML file *)
    and loadPML (errStrm, base, pts) = (case FrontEnd.load'(errStrm, base^".pml")
        of NONE => pts
	 | SOME pt => pt :: pts
       (* end case *))

  (* load the MLB file *)
    and load (errStrm, file) = let
        val basDecs = MLBParser.parseFile (errStrm, file)
	fun f (basDec, pts) = processBasDec (errStrm, basDec, pts)
	val pts = List.foldl f [] basDecs
        in 
           List.rev pts
        end

  end (* MLB *)
