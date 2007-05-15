(* loader-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
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

    structure FileTbl = HashTableFn(struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey : (string * string) -> bool = (op =)
      end)

    val tbl : F.file FileTbl.hash_table  = FileTbl.mkTable (64, Fail "file table")

    fun load file = (case FileTbl.find tbl file
	   of SOME file => SOME file
	    | NONE => let
		fun loadFile path = (case F.parse path
		       of NONE => NONE
			| SOME file => (
			    FileTbl.insert tbl (path, file);
			    SOME file)
		      (* end case *))
		in
		  case findFile (defaultSearchPath, file)
		   of SOME path => loadFile path
		    | NONE => NONE
		end
	  (* end case *))

  end
