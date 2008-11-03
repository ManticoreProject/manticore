(* gen-log-events-def.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenLogEventsDef : GENERATOR =
  struct

    val path = "src/basis/include/log-events.def"

    fun gen (outS, {date, version, events}) = let
	  fun prl l = TextIO.output(outS, concat l)
	  fun prDef (name, id, desc) = prl [
		  "#define ", name, " ", Int.toString id, " /* ", desc, " */\n"
		]
	  fun genDef ({id = 0, name, desc, ...} : LoadFile.event_desc) = prDef (name, 0, desc)
	    | genDef ({id, name, desc, ...}) = prDef (name^"Evt", id, desc)
	  in
	    List.app genDef events
	  end

  end

