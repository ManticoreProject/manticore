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

    structure SS = Substring
    structure STbl = HashTableFn (
      struct
	type hash_key = string
	val hashVal = HashString.hashString
	fun sameKey (s1 : string, s2) = (s1 = s2)
      end)

    val defaultIncludes =
	  String.fields (fn #":" => true | _ => false) Paths.includeSearchPath

    val defaultSearchPath =
	  String.fields (fn #":" => true | _ => false) F.defaultSearchPath

    fun err msg = TextIO.print(concat("Error: " :: msg))
    fun warn msg = TextIO.print(concat("Warning: " :: msg))

  (* does a file exist and is it readable *)
    fun exists path = OS.FileSys.access(path, [OS.FileSys.A_READ])

    fun joinDirFile (dir, file) = OS.Path.joinDirFile{dir=dir, file=file}

  (* a cache maps a filename to a complete path for the file *)
    type cache = string STbl.hash_table

    val emptyCache : cache = STbl.mkTable (1, Fail "empty cache")

  (* load a cache file from the given directory; return the emptyCache if the file
   * does not exist or if there is an error.
   *)
    fun loadCache dirPath = let
	  val cachePath = joinDirFile(dirPath, ".cache")
	  fun load () = let
		val cache = STbl.mkTable (128, Fail cachePath)
		val insert = STbl.insert cache
		val inStrm = TextIO.openIn cachePath
		fun read () = (case TextIO.inputLine inStrm
		       of NONE => ()
			| SOME ln => let
			    val ln = SS.string(SS.dropl Char.isSpace (SS.dropr Char.isSpace (SS.full ln)))
			    val file = OS.Path.file ln
			    in
			      if String.isPrefix dirPath ln andalso (file <> "")
				then insert(OS.Path.file file, ln)
				else warn["bogus entry in ", cachePath, " ignored\n"];
			      read ()
			    end
		      (* end case *))
		in
		  read ();
		  TextIO.closeIn inStrm;
		  cache
		end
		  handle ex => (
		    err["uncaught exception reading ", cachePath, ": ", exnMessage ex, "\n"];
		    emptyCache)
	  in
	    if exists cachePath
	      then load ()
	      else (
		warn["unable to access cache file ", cachePath, "\n"];
		emptyCache)
	  end

  (* a mapping from paths to caches *)
    val cacheMap : cache STbl.hash_table = STbl.mkTable (32, Fail "cacheMap")

    val findCache = STbl.find cacheMap

  (* do a recursive search for a file using the available caches *)
    fun findFileInTree (rootPath, file) = let
	  fun getCache dirPath = (case findCache dirPath
		 of NONE => let
		      val cache = loadCache dirPath
		      in
			STbl.insert cacheMap (dirPath, cache);
			cache
		      end
		  | SOME cache => cache
		(* end case *))
	(* check the directory for the file.  We first see if the file is in the directory,
	 * then we check the cache (loading it if necessary), and then we do a recursive search
	 * of any subdirectories.
	 *)
	  fun findFileInDir dirPath = let
		val path = joinDirFile(dirPath, file)
		in
		  if (exists path)
		    then SOME path
		    else (case STbl.find (getCache dirPath) file
		       of NONE => searchDir dirPath
			| somePath => somePath
		      (* end case *))
		end
	(* recursively search the directory *)
	  and searchDir dirPath = let
		val dirStrm = OS.FileSys.openDir dirPath
		fun search () = (case OS.FileSys.readDir dirStrm
		       of SOME ".svn" => search ()
			| SOME "CVS" => search ()
			| SOME file => let
			    val path = joinDirFile(dirPath, file)
			    in
			      if OS.FileSys.isDir path
				then (case findFileInDir path
				   of NONE => search()
				    | someFile => someFile
				  (* end case *))
				else search()
			    end
			| NONE => (OS.FileSys.closeDir dirStrm; NONE)
		      (* end case *))
		in
		  search ()
		end
	  in
	    findFileInDir rootPath
	  end

  (* search for a file *)
    fun findFile (searchPath, file) = let
	  fun look [] = (
		err["unable to find \"", file, "\" in path \"", String.concatWith ":" searchPath, "\n"];
		NONE)
	    | look ("."::ps) = if exists file
		then SOME file
		else look ps
	    | look (p::ps) = (case findFileInTree (p, file)
		 of NONE => look ps
		  | somePath => somePath
		(* end case *))
	  in
	    look searchPath
	  end

  (* compute the list of #defines for the CPP *)
    fun defines () = let
	  fun add (true, d, l) = (d, NONE)::l
	    | add (false, d, l) = l
	  in
	    add (Controls.get BasicControl.logging, "ENABLE_LOGGING",
	    add (not(Controls.get BasicControl.debug), "NDEBUG",
	      []))
	  end

    fun load file = (case findFile (defaultSearchPath, file)
	   of SOME path => let
		val {inStrm, reap} = RunCPP.run{
			noLines = false,
			defs = defines(),
			includes = defaultIncludes,
			input = path
		      }
		val content = F.parse(path, inStrm) handle ex => (reap(); raise ex)
		in
		  reap();
		  content
		end
	    | NONE => NONE
	  (* end case *))

  end
