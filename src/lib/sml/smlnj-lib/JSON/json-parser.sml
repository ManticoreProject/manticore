(* json-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONParser : sig

    val parse : TextIO.instream -> JSON.value

    val parseFile : string -> JSON.value

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens
    structure J = JSON

    fun parse' (srcMap, inStrm) = let
	  fun error (pos, msg, tok) = raise Fail(concat[
		  "error ", AntlrStreamPos.spanToString srcMap pos, ": ",
		  msg, ", found '", JSONTokens.toString tok, "'"
		])
	  val lexer = Lex.lex srcMap
	  fun parseValue (strm : Lex.strm) = let
		val (tok, pos, strm) = lexer strm
		in
		  case tok
		   of T.LB => parseArray strm
		    | T.LCB => parseObject strm
		    | T.KW_null => (strm, J.NULL)
		    | T.KW_true => (strm, J.BOOL true)
		    | T.KW_false => (strm, J.BOOL false)
		    | T.INT n => (strm, J.INT n)
		    | T.FLOAT f => (strm, J.FLOAT f)
		    | T.STRING s => (strm, J.STRING s)
		    | _ => error (pos, "parsing value", tok)
		  (* end case *)
		end
	  and parseArray (strm : Lex.strm) = (case lexer strm
		 of (T.RB, _, strm) => (strm, J.ARRAY[])
		  | _ => let
		      fun loop (strm, items) = let
			    val (strm, v) = parseValue strm
			  (* expect either a "," or a "]" *)
			    val (tok, pos, strm) = lexer strm
			    in
			      case tok
			       of T.RB => (strm, v::items)
				| T.COMMA => loop (strm, v::items)
				| _ => error (pos, "parsing array", tok)
			      (* end case *)
			    end
		      val (strm, items) = loop (strm, [])
		      in
			(strm, J.ARRAY(List.rev items))
		      end
		(* end case *))
	  and parseObject (strm : Lex.strm) = let
		fun parseField strm = (case lexer strm
		       of (T.STRING s, pos, strm) => (case lexer strm
			     of (T.COLON, _, strm) => let
				  val (strm, v) = parseValue strm
				  in
				    SOME(strm, (s, v))
				  end
			      | (tok, pos, _) => error (pos, "parsing field", tok)
			    (* end case *))
			| _ => NONE
		      (* end case *))
		fun loop (strm, flds) = (case parseField strm
		       of SOME(strm, fld) => (
			  (* expect either "," or "}" *)
			    case lexer strm
			     of (T.RCB, pos, strm) => (strm, fld::flds)
			      | (T.COMMA, pos, strm) => loop (strm, fld::flds)
			      | (tok, pos, _) => error (pos, "parsing object", tok)
			    (* end case *))
			| NONE => (strm, flds)
		      (* end case *))
		val (strm, flds) = loop (strm, [])
		in
		  (strm, J.OBJECT(List.rev flds))
		end
	  in
	    #2 (parseValue (Lex.streamifyInstream inStrm))
	  end

    fun parse inStrm = parse' (AntlrStreamPos.mkSourcemap (), inStrm)

    fun parseFile fileName = let
	  val inStrm = TextIO.openIn fileName
	  val v = parse' (AntlrStreamPos.mkSourcemap' fileName, inStrm)
	  in
	    TextIO.closeIn inStrm;
	    v
	  end

  end

