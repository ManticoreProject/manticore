(* load-file.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure LoadFile : sig

    datatype event_kind = EVENT | START | END

    type event_desc = {
	name : string,
	id : int,
	args : EventSig.arg_desc list,
	sign : string,
	kind : event_kind,
	desc : string
      }

    type log_file_desc = {
	date : string,
	version : {major : int, minor : int, patch : int},
	events : event_desc list
      }

    val loadFile : string -> log_file_desc

  end = struct

    structure J = JSON

    datatype event_kind = EVENT | START | END

    type event_desc = {
	name : string,
	id : int,
	args : EventSig.arg_desc list,
	sign : string,
	kind : event_kind,
	desc : string
      }

    type log_file_desc = {
	date : string,
	version : {major : int, minor : int, patch : int},
	events : event_desc list
      }

    fun findField (J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => NONE
		  | SOME(_, v) => SOME v
		(* end case *))
	  in
	    find
	  end
      | findField _ = raise Fail "expected object"

    fun lookupField findFn lab = (case findFn lab
	   of NONE => raise Fail(concat["no definition for field \"", lab, "\""])
	    | SOME v => v
	  (* end case *))

    fun cvtArray cvtFn (J.ARRAY vl) = List.map cvtFn vl
      | cvtArray cvtFn _ = raise Fail "expected array"

    fun tabulate cvtFn (J.ARRAY vl) =
	  List.rev (#2(List.foldl (fn (v, (i, vl)) => (i+1, cvtFn(i, v)::vl)) (0, []) vl))
      | tabulate cvtFn _ = raise Fail "expected array"

  (* fold a function over a JSON array value *)
    fun foldl cvtFn init (J.ARRAY vl) = List.foldl cvtFn init vl
      | foldl _ _ _ = raise Fail "expected array"

    fun findInt find = let
	  fun get lab = (case find lab of J.INT r => r | _ => raise Fail "expected integer")
	  in
	    get
	  end

    fun findString find = let
	  fun get lab = (case find lab
		 of J.STRING s => s
		  | _ => raise Fail "expected string"
		(* end case *))
	  in
	    get
	  end

    fun cvtArg (obj, (loc, ads)) = let
	  val find = findField obj
	  val lookup = lookupField find
	  val name = findString lookup "name"
	  val ty = (case EventSig.tyFromString (findString lookup "ty")
		 of SOME ty => ty
		  | NONE => raise Fail "bogus type"
		(* end case *))
	  val (loc, nextLoc) = (case find "loc"
		 of SOME(J.INT n) => let
		      val loc = Word.fromLargeInt n
		      in
		      (* NOTE: we don't check that loc is properly aligned here;
		       * that is checked later when we compute the signature.
		       *)
			(loc, loc + #sz(EventSig.alignAndSize ty))
		      end
		  | SOME _ => raise Fail "expected integer for \"loc\" field"
		  | NONE => let
		      val {align, sz, ...} = EventSig.alignAndSize ty
		      val loc = EventSig.alignLoc (loc, align)
		      in
			(loc, loc+sz)
		      end
		(* end case *))
	  val desc = findString lookup "desc"
	  val ad = {
		  name = name,
		  ty = ty,
		  loc = loc,
		  desc = desc
		}
	  in
	    (nextLoc, ad::ads)
	  end

    fun cvtEvent (i, obj) = let
	  val find = lookupField(findField obj)
	  val args = let
		val (_, args) = foldl cvtArg (EventSig.argStart, []) (find "args")
		in
		  List.rev args
		end
	  in {
	    name = findString find "name",
	    id = i,
	    args = args,
	    sign = EventSig.signOf args,
	    kind = (case findString find "kind"
		 of "EVENT" => EVENT
		  | "START" => START
		  | "END" => END
		  | s => raise Fail(concat["unknown event kind \"", String.toString s, "\""])
		(* end case *)),
	    desc = findString find "desc"
	  } end

    fun cvt obj = let
	  val find = lookupField(findField obj)
	  val version = (case find "version"
		 of J.ARRAY[J.INT v1, J.INT v2, J.INT v3] => {
			major = Int.fromLarge v1,
			minor = Int.fromLarge v2,
			patch = Int.fromLarge v3
		      }
		  | _ => raise Fail "bogus version"
		(* end case *))
	  in {
	    date = findString find "date",
	    version = version,
	    events = tabulate cvtEvent (find "events")
	  } end

    fun loadFile file = cvt (JSONParser.parseFile file)

  end
