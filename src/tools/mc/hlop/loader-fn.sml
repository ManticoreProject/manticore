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

    val parse : string -> file option

  (* search-path info *)
    val defaultSearchPath : string

  end

functor LoaderFn (F : FILE_TYPE) : sig

    val load : string -> F.file option

  end = struct

    val defaultSearchPath =
	  String.fields (fn #":" => true | _ => false) F.defaultSearchPath

  (* search for a file *)
    fun findFile (searchPath, file) = let
	  fun exists path = let
		val path = OS.Path.concat(path, file)
		in
		  if OS.FileSys.access(path, [OS.FileSys.A_READ])
		    then SOME path
		    else NONE
		end
	  fun look [] = NONE
	    | look (p::ps) = (case exists p
		 of NONE => look ps
		  | somePath => somePath
		(* end case *))
	  in
	    look searchPath
	  end

    fun load file = (case findFile (defaultSearchPath, file)
	   of SOME path => F.parse path
	    | NONE => NONE
	  (* end case *)

  end
