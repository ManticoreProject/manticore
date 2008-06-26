(* keywords.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Keywords : sig

    val smlIdToken : string -> ManticoreTokens.token
    val bomIdToken : string -> ManticoreTokens.token

  end = struct

    structure T = ManticoreTokens

  (* SML keywords *)
    val smlKeywords = [
	    ("and",		T.KW_and),
	    ("andalso",		T.KW_andalso),
	    ("case",		T.KW_case),
	    ("datatype",	T.KW_datatype),
	    ("div",		T.KW_div),
	    ("else",		T.KW_else),
	    ("end",		T.KW_end),
	    ("exception",	T.KW_exception),
	    ("fn",		T.KW_fn),
	    ("fun",		T.KW_fun),
	    ("handle",		T.KW_handle),
	    ("if",		T.KW_if),
	    ("in",		T.KW_in),
	    ("let",		T.KW_let),
	    ("local",		T.KW_let),
	    ("mod",		T.KW_mod),
	    ("of",		T.KW_of),
	    ("orelse",		T.KW_orelse),
	    ("raise",		T.KW_raise),
	    ("sig",		T.KW_sig),
	    ("signature",	T.KW_signature),
	    ("struct",		T.KW_struct),
	    ("structure",	T.KW_structure),
	    ("then",		T.KW_then),
	    ("type",		T.KW_type),
	    ("val",		T.KW_val),
	    ("where",		T.KW_where)
	  ]

  (* Additional Manticore keywords *)
    val manticoreKeywords = [
	    ("by",		T.KW_by),
	    ("otherwise",	T.KW_otherwise),
	    ("pcase",		T.KW_pcase),
	    ("pval",		T.KW_pval),
	    ("spawn",		T.KW_spawn),
	    ("to",		T.KW_to)
	  ]

  (* BOM keywords; some of these are also SML keywords *)
    val bomKeywords = [
	    ("addr",		T.KW_addr),
	    ("alloc",		T.KW_alloc),
	    ("and",		T.KW_and),
	    ("any",		T.KW_any),
	    ("apply",		T.KW_apply),
	    ("hlop",		T.KW_hlop),
	    ("byte",		T.KW_byte),
	    ("case",		T.KW_case),
	    ("ccall",		T.KW_ccall),
	    ("cont",		T.KW_cont),
	    ("datatype",	T.KW_datatype),
	    ("define",		T.KW_define),
	    ("do",		T.KW_do),
	    ("double",		T.KW_double),
	    ("else",		T.KW_else),
	    ("end",		T.KW_end),
	    ("enum",		T.KW_enum),
	    ("extern",		T.KW_extern),
	    ("float",		T.KW_float),
	    ("fun",		T.KW_fun),
	    ("host_vproc",	T.KW_host_vproc),
	    ("if",		T.KW_if),
	    ("inline",		T.KW_inline),
	    ("int",		T.KW_int),
	    ("let",		T.KW_let),
	    ("long",		T.KW_long),
	    ("module",		T.KW_module),
	    ("noreturn",	T.KW_noreturn),
	    ("of",		T.KW_of),
	    ("promote",		T.KW_promote),
	    ("pure",		T.KW_pure),
	    ("return",		T.KW_return),
	    ("short",		T.KW_short),
	    ("tag",		T.KW_tag),
	    ("then",		T.KW_then),
	    ("throw",		T.KW_throw),
	    ("typedef",		T.KW_typedef),
	    ("unwrap",		T.KW_unwrap),
	    ("use_rw",		T.KW_use_rw),
	    ("vec128",		T.KW_vec128),
	    ("void",		T.KW_void),
	    ("vproc",		T.KW_vproc),
	    ("vpload",		T.KW_vpload),
	    ("vpstore",		T.KW_vpstore),
	    ("wrap",		T.KW_wrap),
	    ("pmlvar",          T.KW_pmlvar)
	  ]

  (* create a keyword lookup table *)
    local
      fun mkFind kws = let
	    val tbl = AtomTable.mkTable (17, Fail "keywords")
	    fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	    val find = AtomTable.find tbl
	    fun idToken id = let
		  val ida = Atom.atom id
		  in
		    case find ida
		     of NONE => T.ID ida
		      | SOME kw => kw
		    (* end case *)
		  end
	    in
	      List.app (List.app ins) kws;
	      idToken
	    end
    in
  (* return either a Manticore keyword token or a ID token *)
      val smlIdToken = mkFind [smlKeywords, manticoreKeywords]
  (* return either a BOM keyword token or a ID token *)
      val bomIdToken = mkFind [bomKeywords]
    end

  end

