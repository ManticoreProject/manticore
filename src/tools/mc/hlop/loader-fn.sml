(* loader-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * TODO:
 *	Eventually, we should support overriding/augmenting the search path
 *	using the command-line and shell environment.
 *)

signature FILE_TYPE =
  sig

    type file

    val parse : (string * TextIO.instream) -> file option

  (* search-path info *)
    val defaultSearchPath : string

  end

functor LoaderFn (F : FILE_TYPE) : sig

    val load : string -> F.file option

  end = struct

    val defaultIncludes =
	  String.fields (fn #":" => true | _ => false) Paths.includeSearchPath

    val defaultSearchPath =
	  String.fields (fn #":" => true | _ => false) F.defaultSearchPath

    fun err msg = TextIO.print(concat("Error: " :: msg))

  (* search for a file *)
    fun findFile (searchPath, file) = let
	  fun exists path = let
		val path = OS.Path.concat(path, file)
		in
		  if OS.FileSys.access(path, [OS.FileSys.A_READ])
		    then SOME path
		    else NONE
		end
	  fun look [] = (
		err["unable to find \"", file, "\" in path \"", String.concatWith ":" searchPath, "\n"];
		NONE)
	    | look (p::ps) = (case exists p
		 of NONE => look ps
		  | somePath => somePath
		(* end case *))
	  in
	    look searchPath
	  end

    fun load file = (case findFile (defaultSearchPath, file)
	   of SOME path => let
		val {inStrm, reap} = RunCPP.run{
			noLines = false,
			defs = [],
			includes = defaultIncludes,
			input = path
		      }
		in
		  (F.parse(path, inStrm) before reap()) handle ex => (reap(); raise ex)
		end
	    | NONE => NONE
	  (* end case *))

  end
