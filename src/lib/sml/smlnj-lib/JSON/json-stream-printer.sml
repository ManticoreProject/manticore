(* json-stream-printer.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONStreamPrinter : sig

    type printer

    val null : printer -> unit
    val boolean : printer * bool -> unit
    val integer : printer * IntInf.int -> unit
    val float : printer * real -> unit
    val string : printer * string -> unit
    val beginObject : printer -> unit
    val objectKey : printer * string -> unit
    val endObject : printer -> unit
    val beginArray : printer -> unit
    val endArray : printer -> unit

    val new : TextIO.outstream -> printer
    val new' : {strm : TextIO.outstream, pretty : bool} -> printer
    val close : printer -> unit

  end = struct

    structure F = Format

    datatype printer = P of {
	strm : TextIO.outstream,
	indent : int ref,
	ctx : context ref,
	pretty : bool
      }

  (* the context is used to keep track of the printing state for indentation
   * and punctuation, etc.
   *)
    and context
      = TOP			(* top-most context *)
      | FIRST of context	(* first element of object or array; the argument *)
				(* must be one of OBJECT or ARRAY. *)
      | OBJECT of context	(* in an object (after the first element) *)
      | ARRAY of context	(* in an array (after the first element) *)
      | KEY of context		(* after the key of a object field *)

    fun new' {strm, pretty} = P{
	    strm = strm,
	    indent = ref 0,
	    ctx = ref TOP,
	    pretty = pretty
	  }

    fun new strm = new' {strm = strm, pretty = false}

    fun close (P{ctx = ref TOP, strm, ...}) = TextIO.output(strm, "\n")
      | close _ = raise Fail "premature close"

    fun pr (P{strm, ...}, s) = TextIO.output(strm, s)

    fun indent (P{pretty = false, ...}, _) = ()
      | indent (P{strm, indent, ...}, offset) = let
	  val tenSpaces = "          "
	  fun prIndent n = if (n <= 10)
		then TextIO.output(strm, String.extract(tenSpaces, 10-n, NONE))
		else (TextIO.output(strm, tenSpaces); prIndent(n-10))
	  in
	    prIndent ((!indent+offset) * 2)
	  end

    fun incIndent (P{indent, ...}, n) = indent := !indent + n;
    fun decIndent (P{indent, ...}, n) = indent := !indent - n;

    fun nl (P{pretty = false, ...}) = ()
      | nl (P{strm, ...}) = TextIO.output(strm, "\n")

    fun comma (P{strm, pretty = false, ...}) = TextIO.output(strm, ",")
      | comma (p as P{strm, ...}) = (
	  TextIO.output(strm, ",\n"); indent(p, 0))

    fun optComma (p as P{ctx, pretty, ...}) = (case !ctx
	   of FIRST ctx' => (indent(p, 0); ctx := ctx')
	    | OBJECT _ => comma p
	    | ARRAY _ => comma p
	    | KEY ctx' => (
		pr (p, if pretty then " : " else ":");
		ctx := ctx')
	    | _ => ()
	  (* end case *))

  (* print a value, which may be proceeded by a comma if it is in a sequence *)
    fun prVal (p, v) = (optComma p; pr(p, v))

    fun null p = prVal (p, "null")
    fun boolean (p, false) = prVal (p, "false")
      | boolean (p, true) = prVal (p, "true")
    fun integer (p, n) = prVal (p, F.format "%d" [F.LINT n])
    fun float (p, f) = prVal (p, F.format "%g" [F.REAL f])
(* FIXME: need to deal with UTF-* escapes *)
    fun string (p, s) = prVal (p, F.format "\"%s\"" [F.STR(String.toCString s)])

    fun beginObject (p as P{ctx, ...}) = (
	  optComma p;
	  pr (p, "{"); incIndent(p, 2); nl p;
	  ctx := FIRST(OBJECT(!ctx)))

    fun objectKey (p as P{ctx = ref(KEY _), ...}, field) =
	  raise Fail(concat["objectKey \"", field, "\" where value was expected"])
      | objectKey (p as P{ctx, ...}, field) = (
	  string (p, field);
	  ctx := KEY(!ctx))

    fun endObject (p as P{ctx, ...}) = let
	  fun prEnd ctx' = (
		ctx := ctx';
		indent(p, ~1); pr(p, "}"); decIndent (p, 2))
	  in
	    case !ctx
	     of OBJECT ctx' => (nl p; prEnd ctx')
	      | FIRST(OBJECT ctx') => prEnd ctx'
	      | _ => raise Fail "endObject not in object context"
	    (* end case *)
	  end

    fun beginArray (p as P{ctx, ...}) = (
	  optComma p;
	  pr (p, "["); incIndent(p, 2); nl p;
	  ctx := FIRST(ARRAY(!ctx)))

    fun endArray (p as P{ctx, ...}) = let
	  fun prEnd ctx' = (
		ctx := ctx';
		nl p; indent(p, ~1); pr(p, "]"); decIndent (p, 2))
	  in
	    case !ctx
	     of ARRAY ctx' => prEnd ctx'
	      | FIRST(ARRAY ctx') => prEnd ctx'
	      | _ => raise Fail "endArray not in array context"
	    (* end case *)
	  end

  end
