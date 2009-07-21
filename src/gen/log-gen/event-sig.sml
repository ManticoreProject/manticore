(* event-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An event signature is the layout of its arguments.  Since there are many
 * more possible layouts than actual layouts, we use the signatures to determine
 * which layouts we need to generate code for.
 *)

structure EventSig : sig

  (* the field types *)
    datatype ty
      = ADDR
      | INT
      | WORD
      | FLOAT
      | DOUBLE
      | NEW_ID
      | EVENT_ID
      | STR of int

    type arg_desc = {
	name : string,
	ty : ty,
	loc : word,
	desc : string
      }

    val argStart : word

    val tyFromString : string -> ty option

  (* sort a list of arguments by increasing location *)
    val sortArgs : arg_desc list -> arg_desc list

    val alignLoc : word * word -> word
    val alignAndSize : ty -> {align:word, sz:word, tag:string}
    val signOf : arg_desc list -> string

  (* is an argument a new-id? *)
    val isNewIdArg : arg_desc -> bool

    structure Set : ORD_SET where type Key.ord_key = string
    structure Map : ORD_MAP where type Key.ord_key = string

  end = struct

    structure SS = Substring

    val eventSzb = 0w32		(* the size of an log event *)
    val argStart = 0w12		(* start of argument area *)

  (* the field types *)
    datatype ty
      = ADDR
      | INT
      | WORD
      | FLOAT
      | DOUBLE
      | NEW_ID
      | EVENT_ID
      | STR of int

    fun tyFromString s = (case String.map Char.toLower s
	   of "addr" => SOME ADDR
	    | "int" => SOME INT
	    | "word" => SOME WORD
	    | "float" => SOME FLOAT
	    | "double" => SOME DOUBLE
	    | "new-id" => SOME NEW_ID
	    | "id" => SOME EVENT_ID
	    | s => let
		val ss = SS.full s
		in
		  if SS.isPrefix "str" ss
		    then (case Int.scan StringCvt.DEC SS.getc (SS.triml 3 ss)
		       of SOME(n, ss) => if (0 < n) andalso (n <= 20) andalso SS.isEmpty ss
			    then SOME(STR n)
			    else NONE
			| NONE => NONE
		      (* end case *))
		    else NONE
		end
	  (* end case *))

    fun tyToString ADDR = "addr"
      | tyToString INT = "int"
      | tyToString WORD = "word"
      | tyToString FLOAT = "float"
      | tyToString DOUBLE = "double"
      | tyToString NEW_ID = "new-id"
      | tyToString EVENT_ID = "id"
      | tyToString (STR n) = "str" ^ Int.toString n

  (* field tag, alignment, and size in bytes *)
    fun alignAndSize ADDR = {tag = "A", align = 0w8, sz = 0w8}
      | alignAndSize INT = {tag = "i", align = 0w4, sz = 0w4}
      | alignAndSize WORD = {tag = "u", align = 0w4, sz = 0w4}
      | alignAndSize FLOAT = {tag = "f", align = 0w4, sz = 0w4}
      | alignAndSize DOUBLE = {tag = "D", align = 0w8, sz = 0w8}
      | alignAndSize NEW_ID = {tag = "N", align = 0w8, sz = 0w8}
      | alignAndSize EVENT_ID = {tag = "I", align = 0w8, sz = 0w8}
      | alignAndSize (STR n) =
	  {tag = "s"^Int.toString n, align = 0w1, sz = Word.fromInt n}

  (* an argument descriptor *)
    type arg_desc = {
	name : string,
	ty : ty,
	loc : word,
	desc : string
      }

  (* align a location *)
    fun alignLoc (loc, align) = Word.andb(loc + (align-0w1), Word.notb(align-0w1))

  (* sort a list of arguments by increasing location *)
    fun sortArgs (ads : arg_desc list) =
	  ListMergeSort.sort (fn (a, b) => #loc a > #loc b) ads

  (* we use a string to represent the signature of an event; this function
   * takes a list of argument descriptors and returns the signature.
   *)
    fun signOf ads = let
	(* compute the signature *)
	  fun f (loc, []) = if (loc <= eventSzb)
		then []
		else raise Fail "event arguments too large"
	    | f (loc, (ad : arg_desc)::ads) = let
		val {tag, align, sz} = alignAndSize (#ty ad)
		val loc' = alignLoc (loc, align)
		val tag = if (loc' <> loc)
		      then concat[
			  "p", (Word.fmt StringCvt.DEC (loc'-loc)), tag
			]
		      else tag
		in
		  if (loc' <> #loc ad)
		    then raise Fail(concat[
			"badly aligned argument \"", #name ad, "\" @ ",
			Word.fmt StringCvt.DEC (#loc ad)
		      ])
		    else ();
		  tag :: f(loc' + sz, ads)
		end
	  in
	    if List.length(List.filter (fn {ty=NEW_ID, ...} => true | _ => false) ads) > 1
	      then raise Fail "multiple new-id arguments (at most one allowed)"
	      else ();
	    String.concat(f (argStart, sortArgs ads))
	  end

  (* is an argument a new-id? *)
    fun isNewIdArg (ad : arg_desc) = (case #ty ad of NEW_ID => true | _ => false)

  (* sets and maps keyed by signature *)
    local
      structure Key =
	struct
	  type ord_key = string
	  fun compare (s1, s2) = (case Int.compare(size s1, size s2)
		 of EQUAL => String.compare(s1, s2)
		  | order => order
		(* end case *))
	end
    in
    structure Set = RedBlackSetFn (Key)
    structure Map = RedBlackMapFn (Key)
    end

  end
