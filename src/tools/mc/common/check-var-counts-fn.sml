(* check-var-counts-fn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for checking census counts on variables.
 *)

functor CheckVarCountsFn (V : sig

    type var

    val useCntOf : var -> int
    val appCntOf : var -> int

    val toString : var -> string

    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end) : sig

    type var = V.var
    type table

    val init : (string list -> unit) -> table

    val bind : table -> var -> unit
    val use : table -> var -> unit
    val appUse : table -> var -> unit
    val checkCounts : table -> bool

  end = struct

    structure Tbl = V.Tbl

    type var = V.var

    datatype table = T of {
	tbl : {appCnt : int ref, useCnt : int ref} Tbl.hash_table,
	error : string list -> unit
      }

    fun init errFn = T{
	    tbl = Tbl.mkTable (1024, Fail "count table"),
	    error = errFn
	  }

    fun bind (T{tbl, error}) = let
	  val find = Tbl.find tbl
	  val insert = Tbl.insert tbl
	  in
	    fn x => (case find x
		 of SOME _ => error ["multiple bindings of ", V.toString x, "\n"]
		  | NONE => insert (x, {appCnt = ref 0, useCnt = ref 0})
		(* end case *))
	  end

    fun use (T{tbl, error}) = let
	  val find = Tbl.find tbl
	  in
	    fn x => (case find x
		 of NONE => error ["unbound variable ", V.toString x, "\n"]
		  | SOME{useCnt, ...} => useCnt := !useCnt + 1
		(* end case *))
	  end

    fun appUse (T{tbl, error}) = let
	  val find = Tbl.find tbl
	  in
	    fn x => (case find x
		 of NONE => error ["unbound variable ", V.toString x, "\n"]
		  | SOME{appCnt, useCnt} => (appCnt := !appCnt + 1; useCnt := !useCnt + 1)
		(* end case *))
	  end

    fun checkCounts (T{tbl, error}) = let
	  val anyErrors = ref false
	(* check old counts against new counts *)
	  fun checkCnt (x, {appCnt, useCnt}) =
		if (!appCnt <> V.appCntOf x) orelse (!useCnt <> V.useCntOf x)
		  then (
		    anyErrors := true;
		    error[
			"inconsistant counts for ", V.toString x, ": recorded <",
			Int.toString(!useCnt), ":", Int.toString(!appCnt),
			"> vs. actual <", Int.toString(V.useCntOf x), ":",
			Int.toString(V.appCntOf x), ">\n"
		      ])
		  else ()
	  in
	    Tbl.appi checkCnt tbl;
	    !anyErrors
	  end

  end
