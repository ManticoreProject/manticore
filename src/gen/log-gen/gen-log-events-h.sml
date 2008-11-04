(* gen-log-events-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenLogEventsH : GENERATOR =
  struct

    val template = "log-events_h.in"
    val path = "include/log-events.h"

    fun hooks (outS, {date, version, events} : LoadFile.log_file_desc) = let
	  fun prl l = TextIO.output(outS, concat l)
	  fun genVersion () = (
		prl ["#define LOG_VERSION_MAJOR ", Int.toString (#major version), "\n"];
		prl ["#define LOG_VERSION_MINOR ", Int.toString (#minor version), "\n"];
		prl ["#define LOG_VERSION_PATCH ", Int.toString (#patch version), "\n"];
		prl [
		    "#define LOG_VERSION 0x",
		    StringCvt.padLeft #"0" 3 (Int.fmt StringCvt.HEX (#major version)),
		    StringCvt.padLeft #"0" 3 (Int.fmt StringCvt.HEX (#minor version)),
		    StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (#patch version)),
		    "\n"
		  ])
	  fun prDef (name, id, desc) = prl [
		  "#define ", name, " ", Int.toString id, " /* ", desc, " */\n"
		]
	  fun genDef ({id = 0, name, desc, ...} : LoadFile.event_desc) = prDef (name, 0, desc)
	    | genDef ({id, name, desc, ...}) = prDef (name^"Evt", id, desc)
	  in [
	    ("DATE", fn () => prl ["#define LOG_VERSION_DATE ", date, "\n"]),
	    ("VERSION", genVersion),
	    ("LOG-EVENTS", fn () => List.app genDef events)
	  ] end

  end

