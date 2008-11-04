(* gen-inline-log-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate the "inline-log.h" file.
 *)

structure GenInlineLogH : GENERATOR =
  struct

    structure Map = EventSig.Map

    val template = "inline-log_h.in"
    val path = "src/lib/parallel-rt/include/inline-log.h"

  (* generate the inline logging function for a given signature *)
    fun genForSig outS (sign, args) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl l = TextIO.output(outS, concat l)
	(* generate params for the event arguments *)
	  fun genParams ([], _)= ()
	    | genParams ((_, ty)::r, i) = let
		val ty = (case ty
		       of EventSig.ADDR => "void *"
			| EventSig.INT => "int32_t "
			| EventSig.WORD => "uint32_t "
			| EventSig.FLOAT => "float "
			| EventSig.DOUBLE => "double "
			| EventSig.STR _ => "const char *"
		      (* end case *))
		in
		  prl [", ", ty, "a", Int.toString i];
		  genParams (r, i+1)
		end
	(* generate code to copy the event arguments into the event structure *)
	  fun genCopy ([], _) = ()
	    | genCopy ((loc, ty)::r, i) = let
		val param = "a" ^ Int.toString i
		val loc = loc - EventSig.argStart
		val index = Word.fmt StringCvt.DEC (Word.>>(loc, 0w2))
		in
		  pr "    ";
		  case ty
		   of EventSig.ADDR => prl[
			  "*((void **)&ep->data[", index, "]) = ", param, ";\n"
			]
		    | EventSig.INT => prl["ep->data[", index, "] = (uint32_t)", param, ";\n"]
		    | EventSig.WORD => prl["ep->data[", index, "] = ", param, ";\n"]
		    | EventSig.FLOAT => prl[
			  "*((float *)&ep->data[", index, "]) = ", param, ";\n"
			]
		    | EventSig.DOUBLE => prl[
			  "*((double *)&ep->data[", index, "]) = ", param, ";\n"
			]
		    | EventSig.STR n => prl[
			  "memcpy (((char *)(ep->data)) + ", Word.fmt StringCvt.DEC loc, ", ",
			  param, ", ", Int.toString n, ");\n"
			]
		  (* end case *);
		  genCopy (r, i+1)
		end
	  in
	    prl [
	      "STATIC_INLINE void LogEvent", sign, " (VProc_t *vp"
	      ];
	    genParams (args, 0);
	    pr "\
	      \)\n\
	      \{\n\
	      \    LogEvent_t *ep = NextLogEvent(vp);\n\
	      \\n\
	      \    LogTimestamp (&(ep->timestamp));\n\
	      \    ep->event = evt;\n";
	    genCopy (args, 0);
	    pr "\n}\n\n"
	  end

  (* generate an event-specific logging macro *)
    fun genLogMacro outS ({id=0, ...}) = ()
      | genLogMacro outS (ed : LoadFile.event_desc) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl l = TextIO.output(outS, concat l)
	  fun prParams [] = ()
	    | prParams ((a : EventSig.arg_desc)::r) = (prl [",", #name a]; prParams r)
	  fun prArgs [] = ()
	    | prArgs ((a : EventSig.arg_desc)::r) = (prl [", (", #name a, ")"]; prArgs r)
	  in
	    prl ["#define Log", #name ed, "(vp"];
	    prParams (#args ed);
	    prl [") LogEvent", #sign ed, " ((vp), ", #name ed, "Evt"];
	    prArgs (EventSig.sortArgs(#args ed)); (* NOTE: location order here! *)
	    pr ")\n"
	  end

  (* generate a dummy logging macro for when logging is disabled *)
    fun genDummyLogMacro outS ({id=0, ...}) = ()
      | genDummyLogMacro outS (ed : LoadFile.event_desc) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl l = TextIO.output(outS, concat l)
	  fun prParams [] = ()
	    | prParams ((a : EventSig.arg_desc)::r) = (prl [",", #name a]; prParams r)
	  fun prArgs [] = ()
	    | prArgs ((a : EventSig.arg_desc)::r) = (prl [", (", #name a, ")"]; prArgs r)
	  in
	    prl ["#define Log", #name ed, "(vp"];
	    prParams (#args ed);
	    pr ")\n"
	  end

  (* compute a mapping from signatures to their argument info from the list of event
   * descriptors.
   *)
    fun computeSigMap events = let
	  fun doEvent (evt : LoadFile.event_desc, map) = (case Map.find(map, #sign evt)
		 of SOME _ => map
		  | NONE => let
		      val argInfo = List.map (fn {loc, ty, ...} => (loc, ty)) (#args evt)
		      in
			Map.insert (map, #sign evt, argInfo)
		      end
		(* end case *))
	  in
	    List.foldl doEvent Map.empty events
	  end

    fun hooks (outS, logDesc : LoadFile.log_file_desc) = let
	  val sigMap = computeSigMap (#events logDesc)
	  fun genericLogFuns () = Map.appi (genForSig outS) sigMap
	  fun logFunctions () = List.app (genLogMacro outS) (#events logDesc)
	  fun dummyLogFunctions () = List.app (genDummyLogMacro outS) (#events logDesc)
	  in [
	    ("GENERIC-LOG-FUNCTIONS", genericLogFuns),
	    ("LOG-FUNCTIONS", logFunctions),
	    ("DUMMY-LOG-FUNCTIONS", dummyLogFunctions)
	  ] end

  end
