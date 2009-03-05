(* load-file.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure LoadFile : sig

    datatype event_kind = EVENT | START | END | SRC | DST

    type event = {
	name : string,
	id : int,
	args : EventSig.arg_desc list,
	sign : string,
	kind : event_kind,
	desc : string
      }

    datatype event_desc
      = GRP of {
	  name : string,
	  events : event_desc list
	}
      | EVT of event

    type log_file_desc = {
	date : string,
	version : {major : int, minor : int, patch : int},
	events : event_desc list
      }

    val loadFile : string -> log_file_desc

  (* helper functions *)
    val applyToEvents : (event -> unit) -> log_file_desc -> unit
    val apply : (event_desc -> unit) -> log_file_desc -> unit
    val foldEvents : ((event * 'a) -> 'a) -> 'a -> log_file_desc -> 'a

  end = struct

    structure J = JSON

    datatype event_kind = EVENT | START | END | SRC | DST

    type event = {
	name : string,
	id : int,
	args : EventSig.arg_desc list,
	sign : string,
	kind : event_kind,
	desc : string
      }

    datatype event_desc
      = GRP of {
	  name : string,
	  events : event_desc list
	}
      | EVT of event


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
	  val nextId = ref 0
	  fun cvtEventOrGroup obj = let
		val find = lookupField (findField obj)
		val name = findString find "name"
		val kind = findString find "kind"
		fun cvtEvent kind = let
		      val args = let
			    val (_, args) = foldl cvtArg (EventSig.argStart, []) (find "args")
			    in
			      List.rev args
			    end
		      val id = !nextId
		      in
			nextId := id + 1;
			EVT{
			    name = name, kind = kind, id = id, args = args,
			    sign = EventSig.signOf args, desc = findString find "desc"
			  }
		      end
		in
		  case findString find "kind"
		   of "GROUP" => GRP{
			  name = name,
			  events = cvtArray cvtEventOrGroup (find "events")
			}
		    | "EVENT" => cvtEvent EVENT
		    | "START" => cvtEvent START
		    | "END" => cvtEvent END
		    | "SRC" => cvtEvent SRC
		    | "DST" => cvtEvent DST
		    | s => raise Fail(concat["unknown event kind \"", String.toString s, "\""])
		  (* end case *)
		end
	  in {
	    date = findString find "date",
	    version = version,
	    events = cvtArray cvtEventOrGroup (find "events")
	  } end

    fun loadFile file = cvt (JSONParser.parseFile file)

  (* helper functions *)
    fun applyToEvents f {date, version, events} = let
	  fun appf (GRP{events, ...}) = List.app appf events
	    | appf (EVT ev) = f ev
	  in
	    List.app appf events
	  end

    fun apply f {date, version, events} = let
	  fun appf ed = (
		f ed;
		case ed
		 of GRP{events, ...} => List.app appf events
		  | _ => ()
		(* end case *))
	  in
	    List.app appf events
	  end

    fun foldEvents f init {date, version, events} = let
	  fun foldf (GRP{events, ...}, acc) = List.foldl foldf acc events
	    | foldf (EVT evt, acc) = f(evt, acc)
	  in
	    List.foldl foldf init events
	  end

  end
