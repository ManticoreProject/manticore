(* gen-log-events-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenEventsHS : GENERATOR =
  struct

    val template = "Events_hs.in"
    val path = "src/tools/event-parser/GHC/RTS/Events.hs"
				  
    fun allCaps s = String.implode(List.map Char.toUpper (String.explode s))

    structure Sig = EventSig
				  
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
	  fun prDef (name, id, desc) = prl [
		  "    ", name, " = ", Int.toString id, ", /* ", desc, " */\n"
		]
	  fun genDef (LoadFile.EVT{id = 0, name, desc, ...}) = prDef (name, 0, desc)
	    | genDef (LoadFile.EVT{id, name, desc, attrs, ...}) =
	      if List.exists (fn attr => attr = LoadFile.ATTR_GHC) attrs
	      then prDef (name, id, desc)
	      else prDef (name^"Evt", id, desc)

	  fun prDesc(name, desc) = prl [
		  "     [", name, "] = \"", desc, "\",\n" 
	      ]
	  fun genDesc (LoadFile.EVT{id = 0, name, desc, ...}) = prDesc(name,desc)
	    | genDesc (LoadFile.EVT{id, name, desc, ...}) = prDesc(name ^ "Evt", desc)

	  fun prSizes(name, size) = prl [
		  "    [", name, "] = ", size, ",\n"]

	  fun computeSize(args : EventSig.arg_desc list) =
	      case args
	       of [] => "0"
		| {ty, ...}::args => EventSig.strSizeofTy ty ^ " + " ^ computeSize args
		  				
	  fun genSizes(LoadFile.EVT{name, args, ...}) = (print ("Looking up " ^ name ^ "\n"); prSizes(name ^ "Evt", computeSize args))


	  fun genTypeNum(LoadFile.EVT{name,id, ...}) =
	      prl ["#define ", allCaps name, " ", Int.toString id, "\n"]

	  val numEvents = LoadFile.foldEvents (fn (_, y) => 1+y) 0 logDesc
		  
	  fun prNumEvents() = prl["#define NUM_GHC_EVENT_TAGS ", Int.toString(numEvents + 1), "\n\n"]

	  fun genArgs(args : Sig.arg_desc list) =
	      case args
	       of [] => []
		| {ty=Sig.INT, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word32"] :: genArgs tys
		| {name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word64"] :: genArgs tys


	  fun ghc attrs = List.exists(fn attr => attr = LoadFile.ATTR_GHC) attrs
													     
	  fun printSimpleParser(id, name) = prl["  (FixedSizeParser ", Int.toString id, " 0 (trace \"inside ",
						name, "\" (return ", name , ")))"]
					       
	  fun printComplexParser(id, name, args) =
	      let
		  fun mkExp(args : Sig.arg_desc list, exp, bindings, size) =
		      case args
		       of [] => (String.concat["(", exp, "return $ ", name, "{", String.concatWith ", " bindings, "})"], size)
			| {name=n, ty, ...} :: tys =>
			  let val {sz, ...} = Sig.alignAndSize ty
			  in mkExp(tys, String.concat[exp, "getE >>= \\ ", n, " -> "],
				   String.concat[n, " = ", n] :: bindings, sz + size)
			  end
		  val (monadicExp, size) = mkExp(args, "traceM \"inside " ^ name ^ "\" >> " , [], Word.fromInt 0)
	      in TextIO.output(outS, String.concatWith " " ["  (FixedSizeParser", Int.toString id, Int.toString (Word.toInt size), monadicExp, ")"])
	      end
		  
	  fun printParser(id, name, args) =
	      case args
	       of [] => printSimpleParser(id, name)
		| _ => printComplexParser(id, name, args)
					   
	  fun genParser(LoadFile.EVT{id,name,args, attrs, ...}) =
	      if ghc attrs
	      then ()
	      else printParser(id, name, args)

	  fun genParsers events =
	      case events
	       of [] => ()
		| [e] => genParser e
		| e :: es => (genParser e; TextIO.output(outS, ",\n"); genParsers es)

	  fun genEventNum(LoadFile.EVT{name, args, ...}) =
	      prl["    ", name, String.concat(List.map (fn _ => " _ ") args), " -> ", allCaps name, "\n"]

	  fun genPutEventSpec (LoadFile.EVT{id,name,args,attrs,...}) =
	      if ghc attrs
	      then ()
	      else 
		  let fun putArgs(args : Sig.arg_desc list) =
			  case args
			   of [] => prl ["    return()\n"]
			    | {name=n, ...}::tys =>
			      (prl ["    putE ", n, "\n"]; putArgs tys)
		      val argString = String.concatWith " " (List.map #name args)
		      val _ = prl ["putEventSpec (", name, " ", argString, ") = do\n"]
		  in putArgs args end

	  fun genShowEventInfo(LoadFile.EVT{name,attrs,args,format,desc,...}) =
	      if ghc attrs
	      then ()
	      else
		  case format
		   of NONE => 
		      prl ["        ", name, " ", String.concatWith " " (List.map #name args), " -> printf \"", desc, "\"\n"]
		    | SOME fmt => 
		      prl ["        ", name, " ", String.concatWith " " (List.map #name args), " -> printf ", fmt, "\n"]
			  
		      
	  val events = List.filter(fn (LoadFile.EVT{attrs, ...}) => not(ghc attrs)) events
    in [
	("DATE", fn () => prl ["#define LOG_VERSION_DATE ", date, "\n"]),
	("VERSION", genVersion),
	("MANTICORE-PARSERS", fn () => genParsers events),
	("EVENT-TYPE-NUMS", fn () => LoadFile.applyToEvents genEventNum logDesc),
	("MANTICORE-PUT-E", fn () => LoadFile.applyToEvents genPutEventSpec logDesc),
	("SHOW-MANT-INFO", fn () => LoadFile.applyToEvents genShowEventInfo logDesc)
    ] end
											
  end

