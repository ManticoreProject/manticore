(* gen-log-events-h.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure GenEventTypesHS : GENERATOR =
  struct

    val template = "EventTypes_hs.in"
    val path = "src/tools/event-parser/GHC/RTS/EventTypes.hs"

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
		| {ty=Sig.ADDR, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word64"] :: genArgs tys
		| {ty=Sig.INT, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word32"] :: genArgs tys
		| {ty=Sig.WORD, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word32"] :: genArgs tys
		| {ty=Sig.FLOAT, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word32"] :: genArgs tys
		| {ty=Sig.DOUBLE, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word64"] :: genArgs tys
		| {ty=Sig.NEW_ID, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word64"] :: genArgs tys
		| {ty=Sig.EVENT_ID, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word64"] :: genArgs tys
		| {ty=Sig.WORD8 , name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word8"] :: genArgs tys
		| {ty=Sig.WORD16, name=n, ...} :: tys =>
		  String.concat[n, " :: {-# UNPACK #-}!Word16"] :: genArgs tys
		| {ty=Sig.STR _, name=n, ...} :: tys =>
		  raise Fail "string not implemented yet"
					     
	  fun ghc attrs = List.exists(fn attr => attr = LoadFile.ATTR_GHC) attrs
													     
									   
	  fun genConstructor(LoadFile.EVT{id, name, args, attrs, ...}) =
	      if ghc attrs
	      then ()
	      else prl ["  | ", name, "{", String.concatWith ", " (genArgs args), "}\n"]
		       
    in [
	("DATE", fn () => prl ["#define LOG_VERSION_DATE ", date, "\n"]),
	("VERSION", genVersion),
	("MANTICORE-EVENTS", fn () => LoadFile.applyToEvents genConstructor logDesc)
    ] end
											
  end

