(* gen-log-events-def.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenLogEventsDef : GENERATOR =
  struct

    val template = "log-events_def.in"
    val path = "src/lib/basis/include/log-events.def"

    fun hooks (outS, logDesc as {date, version, events}) = let
	  fun prl l = TextIO.output(outS, concat l)
	  val isRTOnly = LoadFile.hasAttr LoadFile.ATTR_RT
	  fun genDef (evt as LoadFile.EVT{id, name, desc, ...}) =
		if isRTOnly evt
		  then prl [
		      "/*      ", name, "Evt ", Int.toString id, " ** ", desc, " */\n"
		    ]
		  else prl [
		      "#define ", name, "Evt ", Int.toString id, " /* ", desc, " */\n"
		    ]
	  in [
	    ("LOG-EVENTS", fn () => LoadFile.applyToEvents genDef logDesc)
	  ] end

  end

