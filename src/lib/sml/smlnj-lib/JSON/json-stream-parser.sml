(* json-stream-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONStreamParser : sig

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx,
	error : 'ctx * string -> 'ctx
      }

    val parser : 'ctx callbacks -> (TextIO.instream * 'ctx) -> unit

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx,
	error : 'ctx * string -> 'ctx
      }

    fun error (cb : 'a callbacks, ctx, msg) = (
	  #error cb (ctx, msg);
	  raise Fail "error")

    fun parser (cb : 'a callbacks) (inStrm, ctx) = let
	  val lexer = Lex.lex (AntlrStreamPos.mkSourcemap ())
	  fun parseValue (strm : Lex.strm, ctx) = let
		val (tok, pos, strm) = lexer strm
		in
		  case tok
		   of T.LB => parseArray (strm, ctx)
		    | T.LCB => parseObject (strm, ctx)
		    | T.KW_null => (strm, #null cb ctx)
		    | T.KW_true => (strm, #boolean cb (ctx, true))
		    | T.KW_false => (strm, #boolean cb (ctx, false))
		    | T.INT n => (strm, #integer cb (ctx, n))
		    | T.FLOAT f => (strm, #float cb (ctx, f))
		    | T.STRING s => (strm, #string cb (ctx, s))
		    | _ => error (cb, ctx, "error parsing value")
		  (* end case *)
		end
	  and parseArray (strm : Lex.strm, ctx) = (case lexer strm
		 of (T.RB, _, strm) => (strm, #endArray cb (#startArray cb ctx))
		  | _ => let
		      fun loop (strm, ctx) = let
			    val (strm, ctx) = parseValue (strm, ctx)
			  (* expect either a "," or a "]" *)
			    val (tok, pos, strm) = lexer strm
			    in
			      case tok
			       of T.RB => (strm, ctx)
				| T.COMMA => loop (strm, ctx)
				| _ => error (cb, ctx, "error parsing array")
			      (* end case *)
			    end
		      val ctx = #startArray cb ctx
		      val (strm, ctx) = loop (strm, #startArray cb ctx)
		      in
			(strm, #endArray cb ctx)
		      end
		(* end case *))
	  and parseObject (strm : Lex.strm, ctx) = let
		fun parseField (strm, ctx) = (case lexer strm
		       of (T.STRING s, pos, strm) => let
			    val ctx = #objectKey cb (ctx, s)
			    in
			      case lexer strm
			       of (T.COLON, _, strm) => parseValue (strm, ctx)
				| _ => error (cb, ctx, "error parsing field")
			      (* end case *)
			    end
			| _ => (strm, ctx)
		      (* end case *))
		fun loop (strm, ctx) = let
		      val (strm, ctx) = parseField (strm, ctx)
		      in
			(* expect either "," or "}" *)
			case lexer strm
			 of (T.RCB, pos, strm) => (strm, ctx)
			  | (T.COMMA, pos, strm) => loop (strm, ctx)
			  | _ => error (cb, ctx, "error parsing object")
			(* end case *)
		      end
		val ctx = #startObject cb ctx
		val (strm, ctx) = loop (strm, #startObject cb ctx)
		in
		  (strm, #endObject cb ctx)
		end
	  in
	    ignore (parseValue (Lex.streamifyInstream inStrm, ctx))
	  end

  end
