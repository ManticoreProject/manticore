structure 
MLBTokens = struct

    datatype token = EOF
      | STRING of string
      | NAME of Atom.atom
      | SEMI
      | EQ
      | KW_structure
      | KW_functor
      | KW_signature
      | KW_local
      | KW_open
      | KW_end
      | KW_let
      | KW_in
      | KW_basis
      | KW_bas
      | KW_and

    val allToks = [EOF, SEMI, EQ, KW_structure, KW_functor, KW_signature, KW_local, KW_open, KW_end, KW_let, KW_in, KW_basis, KW_bas, KW_and]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (STRING(_)) => "STRING"
  | (NAME(_)) => "NAME"
  | (SEMI) => ";"
  | (EQ) => "="
  | (KW_structure) => "structure"
  | (KW_functor) => "functor"
  | (KW_signature) => "signature"
  | (KW_local) => "local"
  | (KW_open) => "open"
  | (KW_end) => "end"
  | (KW_let) => "let"
  | (KW_in) => "in"
  | (KW_basis) => "basis"
  | (KW_bas) => "bas"
  | (KW_and) => "and"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (STRING(_)) => false
  | (NAME(_)) => false
  | (SEMI) => false
  | (EQ) => false
  | (KW_structure) => false
  | (KW_functor) => false
  | (KW_signature) => false
  | (KW_local) => false
  | (KW_open) => false
  | (KW_end) => false
  | (KW_let) => false
  | (KW_in) => false
  | (KW_basis) => false
  | (KW_bas) => false
  | (KW_and) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor MLBParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
MLBTokens
    structure UserCode = struct

 
  structure PT = ParseTree


  fun mark cons (span : AntlrStreamPos.span, tr) = cons{span = span, tree = tr}

  val markBasDec = mark PT.MarkBasDec



fun BasExp_PROD_1_ACT (BasDec, KW_bas, KW_end, BasDec_SPAN : (Lex.pos * Lex.pos), KW_bas_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkBasExp (FULL_SPAN, PT.DecBasExp BasDec))
fun BasExp_PROD_2_ACT (NAME, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkBasExp (FULL_SPAN, PT.IdBasExp NAME))
fun BasExp_PROD_3_ACT (KW_in, BasDec, BasExp, KW_end, KW_let, KW_in_SPAN : (Lex.pos * Lex.pos), BasDec_SPAN : (Lex.pos * Lex.pos), BasExp_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkBasExp (FULL_SPAN, PT.LocalDeclBasExp(BasDec, BasExp)))
fun BasDec_PROD_1_ACT (KW_basis, BasBind, KW_basis_SPAN : (Lex.pos * Lex.pos), BasBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.BasisBasDec BasBind))
fun BasDec_PROD_2_ACT (KW_local, BasDec1, BasDec2, KW_in, KW_end, KW_local_SPAN : (Lex.pos * Lex.pos), BasDec1_SPAN : (Lex.pos * Lex.pos), BasDec2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.LocalBasDec (BasDec1, BasDec2)))
fun BasDec_PROD_3_ACT (KW_structure, ModBind, KW_structure_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.StructureBasDec ModBind))
fun BasDec_PROD_4_ACT (KW_signature, ModBind, KW_signature_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.SignatureBasDec ModBind))
fun BasDec_PROD_5_ACT (KW_functor, ModBind, KW_functor_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.FunctorBasDec ModBind))
fun BasDec_PROD_6_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markBasDec(FULL_SPAN, PT.ImportBasDec STRING))
fun BasBind_PROD_1_ACT (EQ, SR, NAME, BasExp, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), BasExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkBasBind (FULL_SPAN, PT.BindBasBind ((NAME, BasExp) :: SR)))
fun ModBind_PROD_1_ACT (EQ, SR, NAME1, NAME2, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME1_SPAN : (Lex.pos * Lex.pos), NAME2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkModBind (FULL_SPAN, PT.BindModBind ((NAME1, NAME2) :: SR)))

    end

    structure Err = AntlrErrHandler(Tok)(Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNAME strm = (case (lex(strm))
 of (Tok.NAME(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_structure strm = (case (lex(strm))
 of (Tok.KW_structure, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_functor strm = (case (lex(strm))
 of (Tok.KW_functor, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_signature strm = (case (lex(strm))
 of (Tok.KW_signature, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_local strm = (case (lex(strm))
 of (Tok.KW_local, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_open strm = (case (lex(strm))
 of (Tok.KW_open, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_basis strm = (case (lex(strm))
 of (Tok.KW_basis, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_bas strm = (case (lex(strm))
 of (Tok.KW_bas, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_and strm = (case (lex(strm))
 of (Tok.KW_and, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (BasExp_NT) = 
let
fun ModBind_NT (strm) = let
      val (NAME1_RES, NAME1_SPAN, strm') = matchNAME(strm)
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (NAME2_RES, NAME2_SPAN, strm') = matchNAME(strm')
      fun ModBind_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_and_RES, KW_and_SPAN, strm') = matchKW_and(strm)
            val (NAME1_RES, NAME1_SPAN, strm') = matchNAME(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (NAME2_RES, NAME2_SPAN, strm') = matchNAME(strm')
            val FULL_SPAN = (#1(KW_and_SPAN), #2(NAME2_SPAN))
            in
              ((NAME1_RES, NAME2_RES), FULL_SPAN, strm')
            end
      fun ModBind_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_and, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(ModBind_PROD_1_SUBRULE_1_PRED, ModBind_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(NAME1_SPAN), #2(SR_SPAN))
      in
        (UserCode.ModBind_PROD_1_ACT (EQ_RES, SR_RES, NAME1_RES, NAME2_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME1_SPAN : (Lex.pos * Lex.pos), NAME2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun BasExp_NT (strm) = let
      fun BasExp_PROD_1 (strm) = let
            val (KW_bas_RES, KW_bas_SPAN, strm') = matchKW_bas(strm)
            val (BasDec_RES, BasDec_SPAN, strm') = BasDec_NT(strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_bas_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.BasExp_PROD_1_ACT (BasDec_RES, KW_bas_RES, KW_end_RES, BasDec_SPAN : (Lex.pos * Lex.pos), KW_bas_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasExp_PROD_2 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
            in
              (UserCode.BasExp_PROD_2_ACT (NAME_RES, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasExp_PROD_3 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (BasDec_RES, BasDec_SPAN, strm') = BasDec_NT(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (BasExp_RES, BasExp_SPAN, strm') = BasExp_NT(strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.BasExp_PROD_3_ACT (KW_in_RES, BasDec_RES, BasExp_RES, KW_end_RES, KW_let_RES, KW_in_SPAN : (Lex.pos * Lex.pos), BasDec_SPAN : (Lex.pos * Lex.pos), BasExp_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') => BasExp_PROD_3(strm)
          | (Tok.KW_bas, _, strm') => BasExp_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => BasExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and BasDec_NT (strm) = let
      fun BasDec_PROD_1 (strm) = let
            val (KW_basis_RES, KW_basis_SPAN, strm') = matchKW_basis(strm)
            val (BasBind_RES, BasBind_SPAN, strm') = BasBind_NT(strm')
            val FULL_SPAN = (#1(KW_basis_SPAN), #2(BasBind_SPAN))
            in
              (UserCode.BasDec_PROD_1_ACT (KW_basis_RES, BasBind_RES, KW_basis_SPAN : (Lex.pos * Lex.pos), BasBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasDec_PROD_2 (strm) = let
            val (KW_local_RES, KW_local_SPAN, strm') = matchKW_local(strm)
            val (BasDec1_RES, BasDec1_SPAN, strm') = BasDec_NT(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (BasDec2_RES, BasDec2_SPAN, strm') = BasDec_NT(strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_local_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.BasDec_PROD_2_ACT (KW_local_RES, BasDec1_RES, BasDec2_RES, KW_in_RES, KW_end_RES, KW_local_SPAN : (Lex.pos * Lex.pos), BasDec1_SPAN : (Lex.pos * Lex.pos), BasDec2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasDec_PROD_3 (strm) = let
            val (KW_structure_RES, KW_structure_SPAN, strm') = matchKW_structure(strm)
            val (ModBind_RES, ModBind_SPAN, strm') = ModBind_NT(strm')
            val FULL_SPAN = (#1(KW_structure_SPAN), #2(ModBind_SPAN))
            in
              (UserCode.BasDec_PROD_3_ACT (KW_structure_RES, ModBind_RES, KW_structure_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasDec_PROD_4 (strm) = let
            val (KW_signature_RES, KW_signature_SPAN, strm') = matchKW_signature(strm)
            val (ModBind_RES, ModBind_SPAN, strm') = ModBind_NT(strm')
            val FULL_SPAN = (#1(KW_signature_SPAN), #2(ModBind_SPAN))
            in
              (UserCode.BasDec_PROD_4_ACT (KW_signature_RES, ModBind_RES, KW_signature_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasDec_PROD_5 (strm) = let
            val (KW_functor_RES, KW_functor_SPAN, strm') = matchKW_functor(strm)
            val (ModBind_RES, ModBind_SPAN, strm') = ModBind_NT(strm')
            val FULL_SPAN = (#1(KW_functor_SPAN), #2(ModBind_SPAN))
            in
              (UserCode.BasDec_PROD_5_ACT (KW_functor_RES, ModBind_RES, KW_functor_SPAN : (Lex.pos * Lex.pos), ModBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BasDec_PROD_6 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.BasDec_PROD_6_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STRING(_), _, strm') => BasDec_PROD_6(strm)
          | (Tok.KW_signature, _, strm') => BasDec_PROD_4(strm)
          | (Tok.KW_local, _, strm') => BasDec_PROD_2(strm)
          | (Tok.KW_basis, _, strm') => BasDec_PROD_1(strm)
          | (Tok.KW_structure, _, strm') => BasDec_PROD_3(strm)
          | (Tok.KW_functor, _, strm') => BasDec_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
and BasBind_NT (strm) = let
      val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (BasExp_RES, BasExp_SPAN, strm') = BasExp_NT(strm')
      fun BasBind_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_and_RES, KW_and_SPAN, strm') = matchKW_and(strm)
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (BasExp_RES, BasExp_SPAN, strm') = BasExp_NT(strm')
            val FULL_SPAN = (#1(KW_and_SPAN), #2(BasExp_SPAN))
            in
              ((NAME_RES, BasExp_RES), FULL_SPAN, strm')
            end
      fun BasBind_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_and, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(BasBind_PROD_1_SUBRULE_1_PRED, BasBind_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(NAME_SPAN), #2(SR_SPAN))
      in
        (UserCode.BasBind_PROD_1_ACT (EQ_RES, SR_RES, NAME_RES, BasExp_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), BasExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (BasExp_NT)
end
val BasExp_NT =  fn s => unwrap (Err.launch (eh, lexFn, BasExp_NT , true) s)

in (BasExp_NT) end
  in
fun parse lexFn  s = let val (BasExp_NT) = mk lexFn in BasExp_NT s end

  end

end
