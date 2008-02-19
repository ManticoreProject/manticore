structure LTCPVal =
  struct

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

    The rule [| e |] expands pval bindings in e into explicit concurrent operations.
    
    [|  
        let pval x = e1
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
           let val x = e1
           in
              if (ltcPop())
                 then [| e2Susp(fn () => x) |]
                 else (
                    iPut(iv, x);
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
					    VarSubst.exp' (VarSubst.singleton(x, x)) (AU.mkApplyExp(A.VarExp(selFn,[]), [unitExp])) e2)

        val eBody = 
	    AU.mkSeqExp([RuntimeBasis.mkLtcPush(ctx)],
		AU.mkLetExp(mkBindVars([x], [e1]),	
		  AU.mkIfExp (RB.mkLtcPop,
			      AU.mkApplyExp(A.VarExp(e2Susp, []), [A.FunExp (dummyVar(), A.VarExp(x, []), e1Ty)]),
			      AU.mkSeqExp([RB.mkIVarPut(e1Ty, ivar, A.VarExp(x,[]))],
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

    and trPval (A.VarPat v, e1, e2) = expand (v, e1, e2)
      | trPval _ = raise Fail "todo"

  end (* LTCPVal *)
