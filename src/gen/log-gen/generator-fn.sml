(* generator-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor GeneratorFn (G : GENERATOR) : sig

  (* name of template file *)
    val template : string

  (* destination path relative to root of Manticore source tree *)
    val path : string

    val gen : {
	    logSpec : LoadFile.log_file_desc,
	    template : string,
	    target : string
	  } -> unit

  end = struct

    structure TIO = TextIO
    structure SS = Substring
    structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = BackTrackEngine);
    structure M = MatchTree

    val template = G.template
    val path = G.path

    val placeholderRE = RE.compileString "[\\t ]*@([a-zA-Z][-a-zA-Z0-9_]*)@[\\t ]*"
    val prefixPlaceholder = RE.prefix placeholderRE SS.getc

    fun findPlaceholder s = (case prefixPlaceholder(SS.full s)
	   of SOME(M.Match(_, [M.Match({pos, len}, _)]), _) =>
		SOME(SS.string(SS.slice(pos, 0, SOME len)))
	    | _ => NONE
	  (* end case *))

  (* copy from inStrm to outStrm expanding placeholders *)
    fun copy (inStrm, outStrm, hooks) = let
	  fun lp () = (case TIO.inputLine inStrm
		 of NONE => ()
		  | SOME s => (
		    case findPlaceholder s
		     of NONE => TIO.output (outStrm, s)
		      | (SOME id) => (
			  case (List.find (fn (id', h) => id = id') hooks)
			   of (SOME(_, h)) => h ()
			    | NONE => raise Fail "bogus placeholder"
			  (* end case *))
		    (* end case *);
		    lp())
		(* end case *))
	  in
	    lp()
	  end

    exception OpenOut

    fun gen {logSpec, template, target} = (let
	  val templateStrm = TIO.openIn template
	  val targetStrm = TIO.openOut target
		handle ex => (
		  TIO.closeIn templateStrm;
		  TIO.output(TIO.stdOut, concat[
		      "Warning: unable to open output file \"",
		      target, "\"\n"
		    ]);
		  raise OpenOut)
	  val hooks = G.hooks (targetStrm, logSpec)
	  fun done () = (TIO.closeIn templateStrm; TIO.closeOut targetStrm)
	  in
	    copy (templateStrm, targetStrm, hooks) handle ex => (done(); raise ex);
	    done()
	  end
	    handle OpenOut => ())

  end
