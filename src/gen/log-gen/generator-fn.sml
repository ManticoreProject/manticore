(* generator-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor GeneratorFn (G : GENERATOR) : sig

    val gen : LoadFile.log_file_desc -> unit

  end = struct

    structure TIO = TextIO
    structure SS = Substring
    structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = BackTrackEngine);
    structure M = MatchTree

    type hook = TextIO.outstream -> unit

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

    fun expand {logSpec, src, dst} = (let
	  val srcStrm = TIO.openIn src
	  val dstStrm = TIO.openOut dst
		handle ex => (
		  TIO.closeIn srcStrm;
		  TIO.output(TIO.stdOut, concat[
		      "Warning: unable to open output file \"",
		      dst, "\"\n"
		    ]);
		  raise OpenOut)
	  val hooks = G.hooks (dstStrm, logSpec)
	  fun done () = (TIO.closeIn srcStrm; TIO.closeOut dstStrm)
	  in
	    copy (srcStrm, dstStrm, hooks) handle ex => (done(); raise ex);
	    done()
	  end
	    handle OpenOut => ())

    fun gen logSpec = expand {
	    logSpec = logSpec,
(* FIXME: need to adjust paths for our location *)
	    src = G.template,
	    dst = G.path
	  }

  end
