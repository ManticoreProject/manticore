(* ltc-dval.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate dvals into lazy-task creation primitives.
 *)

structure LTCDVal :> sig

    val transform : AST.module -> AST.module

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure RB = RuntimeBasis
    structure AU = ASTUtil

    val --> = T.FunTy
    infixr 8 -->

    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infixr 8 **

   (*

    The rule [| e |] expands dval bindings in e into explicit concurrent operations.
    
    [|  
        let dval x = e1
        in
	   e2
        end
    |]

     =

    callcc(fn k => let
	val iv = iVar()

        fun e2Susp (selFn) = 
            [| e2[x -> selFn()] |]

        fun ctx () = 
            throw k(e2Susp(fn () => iGet(iv)))
	in
           ltcPush(ctx);
           let val x = [| e1 |]
           in
              if (ltcPop())
                 then e2Susp(fn () => x)
                 else (
                    ltcIPut(iv, x);
		    threadExit())
            end
	end)

    The rule applies inductively for the other syntactic forms.

    *)

    fun mkBindVar (v, e) = A.ValBind(A.VarPat v, e)
    fun mkBindVars (vs, es) = ListPair.map mkBindVar (vs, es)

    fun ivarTy ty = AST.ConTy([ty], Basis.ivarTyc)
    fun dummyVar () = BasisUtils.monoVar("dummyVar", Basis.unitTy)
    val unitExp = A.TupleExp []

    fun expand (x, e1, e2) = let
        val e1Ty = TypeOf.exp(e1)
	val e2Ty = TypeOf.exp(e2)
	val ivar = BasisUtils.monoVar("ivar", ivarTy(e1Ty))
	val retK = BasisUtils.monoVar("retK", RB.contTy(e2Ty))
	val ctx = BasisUtils.monoVar("ctx", Basis.unitTy --> Basis.unitTy)

	val e2Susp = BasisUtils.monoVar("e2Susp", (Basis.unitTy --> e1Ty) --> e2Ty)
	val selFn = BasisUtils.monoVar("selFn", Basis.unitTy --> e1Ty)
	val e2SuspFun = AU.mkFunWithParams (e2Susp, [selFn], 
			    trExp(VarSubst.exp' (VarSubst.singleton(x, x)) (AU.mkApplyExp(A.VarExp(selFn,[]), [unitExp])) 
					  e2))

        val eBody = 
	    AU.mkSeqExp([RuntimeBasis.mkLtcPush(ctx)],
		AU.mkLetExp(mkBindVars([x], [trExp(e1)]),	
		  AU.mkIfExp (RB.mkLtcPop,
			      AU.mkApplyExp(A.VarExp(e2Susp, []), [A.FunExp (dummyVar(), A.VarExp(x, []), e1Ty)]),
			      AU.mkSeqExp([RB.mkLtcIPut(e1Ty, ivar, A.VarExp(x,[]))],
					  RB.mkThreadExit))))
		     
        val ctxExp =
	    A.FunExp (dummyVar(),
		      RB.mkThrowcc(e2Ty, retK, 
		            AU.mkApplyExp(A.VarExp(e2Susp, []), [A.FunExp(dummyVar(), RB.mkIVarGet(e1Ty, ivar), e1Ty)])),
		Basis.unitTy)
        in
	   RB.mkCallcc(e2Ty,
	      A.FunExp (retK,
			 AU.mkLetExp(A.FunBind [e2SuspFun] ::
				     mkBindVars ([ivar,             ctx], 
						 [RB.mkIVarNew(e1Ty), ctxExp]),
				     eBody),
			e2Ty))
        end

    and trExp (e) = (case e
        of A.LetExp (A.DValBind (pat, e1), e2) => (case pat
           of A.VarPat v => expand(v, e1, e2)
	    | A.WildPat ty => expand(Var.new("wild", ty), e1, e2)
	    | _ => raise Fail "todo"
           (* end case *))	   
	 | A.LetExp (binding, e) => A.LetExp(trBinding(binding), trExp(e))
	 | A.IfExp(e1, e2, e3, ty) => A.IfExp(trExp(e1), trExp(e2), trExp(e3), ty)
	 | A.CaseExp(e, ms, ty) => A.CaseExp(trExp(e), List.map trMatch ms, ty)
	 | A.HandleExp(e, ms, ty) => A.HandleExp(trExp(e), List.map trMatch ms, ty)
	 | A.RaiseExp(e, ty) => A.RaiseExp(trExp(e), ty)
	 | A.FunExp(v, e, ty) => A.FunExp(v, trExp(e), ty)
	 | A.ApplyExp(e1, e2, ty) => A.ApplyExp(trExp(e1), trExp(e2), ty) 
	 | A.VarArityOpExp (v, i, ty) => A.VarArityOpExp (v, i, ty)
	 | A.TupleExp(es) => A.TupleExp(List.map trExp es)
	 | A.RangeExp(e1, e2, eOpt, ty) => A.RangeExp(trExp(e1), trExp(e2), Option.map trExp eOpt, ty) 
	 | A.PTupleExp (es) => A.PTupleExp (List.map trExp es)
	 | A.PArrayExp (es, ty) => A.PArrayExp (List.map trExp es, ty)
	 | A.PCompExp (e, pes, eOpt) => A.PCompExp(trExp(e), List.map (fn (pat, e) => (pat, trExp(e))) pes, Option.map trExp eOpt)
	 | A.PChoiceExp(es, ty) => A.PChoiceExp(List.map trExp es, ty) 
	 | A.SpawnExp(e) => A.SpawnExp(trExp(e))
	 | A.SeqExp(e1, e2) => A.SeqExp(trExp(e1), trExp(e2))
	 | e => e
       (* end case *))

    and trBinding (binding) = (case binding
        of A.ValBind (pat, e) => A.ValBind (pat, trExp(e))
	 | A.PValBind (pat, e) => A.PValBind (pat, trExp(e))
	 | A.DValBind (pat, e) => raise Fail "impossible"
	 | A.FunBind (ls) => A.FunBind (List.map (fn A.FB(v1, v2, e) => A.FB(v1, v2, trExp(e))) ls)
        (* end case *))

    and trMatch (m) = (case m
        of A.PatMatch (pat, e) => A.PatMatch(pat, trExp(e))
	 | A.CondMatch(pat, e1, e2) => A.CondMatch(pat, trExp(e1), trExp(e2))
        (* end case *))

    fun transform (A.Module {exns, body}) = 
	   A.Module {exns=exns, body=trExp(body)}

  end (* LTCDval *)
