(* gen-log-events-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenViewColoursHS : GENERATOR =
  struct

    val template = "ViewerColours_hs.in"
    val path = "src/tools/threadscope/GUI/ViewerColours.hs"
		   
    fun allCaps s = String.implode(List.map Char.toUpper (String.explode s))

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

	  fun genColor(c, (r, g, b)) =
	      (
		prl[c, " :: Color\n"];
		prl[c, " = Color ", Int.toString r, " ", Int.toString g, " ",
		    Int.toString b, "\n\n"]
	      )
		  
				  
    in [
	("DATE", fn () => prl ["#define LOG_VERSION_DATE ", date, "\n"]),
	("VERSION", genVersion),
	("COLORS", fn () => CM.appi genColor LoadFile.colors)
    ] end
											
  end

