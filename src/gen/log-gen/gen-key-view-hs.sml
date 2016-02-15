(* gen-log-events-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenKeyViewHS : GENERATOR =
  struct

    val template = "KeyView_hs.in"
    val path = "src/tools/threadscope/GUI/KeyView.hs"
		   
    structure Sig = EventSig

    structure CM = LoadFile.ColorMap
			
    fun hooks (outS, logDesc as {date, version, events} : LoadFile.log_file_desc) = let
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

	  fun genKeyData(LoadFile.EVT{name,args,color=SOME (c,_),...}) =
	      prl["  ,(\"", name, "\", KEvent, ", c, ", \"", name, "\")\n"]
	    | genKeyData _ = ()
				  
    in [
	("DATE", fn () => prl ["#define LOG_VERSION_DATE ", date, "\n"]),
	("VERSION", genVersion),
	("MANTICORE-KEYS", fn () => LoadFile.applyToEvents genKeyData logDesc)
    ] end
											
  end

