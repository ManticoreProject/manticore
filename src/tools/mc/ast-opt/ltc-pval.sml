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
	val iv = ivarNew()
        fun ctx () = 
            throw k( [| e2[x -> ivarGet(iv)] |] )
	in
           ltcPush(ctx);
           let val x = e1
           in
              if (ltcPop())
                 then [| e2 |]
                 else (
                    ivarPut(iv, x);
		    threadExit())
            end
	end)

    The rule applies inductively for the other syntactic forms.

    *)

    fun mkBindVar (v, e) = A.ValBind(A.VarPat v, e)
    fun mkBindVars (vs, es) = ListPair.map mkBindVar (vs, es)

    fun ivarTy ty = AST.ConTy([ty], Basis.ivarTyc)

    fun expand (x, e1, e2) = let
        val ty = TypeOf.exp(e1)
	val e2Ty = TypeOf.exp(e2)
	val ivar = BasisUtils.monoVar("ivar", ivarTy(ty))
	val retK = BasisUtils.monoVar("retK", RB.contTy(e2Ty))
	val ctx = BasisUtils.monoVar("ctx", Basis.unitTy --> Basis.unitTy)
	val e2' = VarSubst.exp' (VarSubst.singleton(x, x)) (RB.mkIVarGet(ty, ivar)) e2
        val ctxExp =
	    A.FunExp (BasisUtils.monoVar("dummyVar", Basis.unitTy),
		      RB.mkThrowcc(e2Ty, retK, 
		            AU.copyExp(e2')),
		Basis.unitTy)
        val e2 = 
	    AU.mkSeqExp([RuntimeBasis.mkLtcPush(ctx)],
		AU.mkLetExp(mkBindVars([x], [e1]),	
		  AU.mkIfExp (RB.mkLtcPop,
			      VarSubst.exp' (VarSubst.singleton(x, x)) (A.VarExp(x,[])) e2,
			      AU.mkSeqExp([RB.mkIVarPut(ty, ivar, A.VarExp(x,[]))],
					  RB.mkThreadExit))))
        in
	   RB.mkCallcc(e2Ty,
	      A.FunExp (retK,
			 AU.mkLetExp(mkBindVars ([ivar, ctx], [RB.mkIVarNew(ty), ctxExp]),
				     e2),
			e2Ty))
        end

    and trPval (A.VarPat v, e1, e2) = expand (v, e1, e2)
      | trPval _ = raise Fail "todo"

  end (* LTCPVal *)
