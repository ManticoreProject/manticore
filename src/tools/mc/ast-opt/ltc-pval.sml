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
                    ivarPut(iv, e1);
		    threadExit())
            end
	end)

    The rule applies inductively for the other syntactic forms.

    *)

    fun mkBindVar (v, e) = A.ValBind(A.VarPat v, e)
    fun mkBindVars (vs, es) = ListPair.map mkBindVar (vs, es)

    fun ivarTy ty = AST.ConTy([ty], Basis.ivarTyc)

    fun expand (x, e, body) = let
        val ty = TypeOf.exp(e)
	val bodyTy = TypeOf.exp(body)
	val ivar = RB.monoVar("ivar", ivarTy(ty))
	val retK = RB.monoVar("retK", RB.contTy(bodyTy))
	val ctx = RB.monoVar("ctx", Basis.unitTy --> RB.voidTy)
        val ctxExp =
	    A.FunExp (Var.new("x", Basis.unitTy),
		      RB.mkThrowcc(bodyTy, retK, 
		            VarSubst.exp' (VarSubst.singleton(x, x)) (RB.mkIVarGet(ty, ivar)) body),
		Basis.unitTy --> RB.voidTy)
        val body = AU.mkInt(10000)
(*	    AU.mkSeqExp([RuntimeBasis.mkLtcPush(ctx)],
		AU.mkLetExp(mkBindVars([x], [e]),	
		  AU.mkIfExp (RB.mkLtcPop,
		      VarSubst.exp' (VarSubst.singleton(x, x)) (RB.mkIVarGet(ty, ivar)) body,
			      AU.mkSeqExp([RB.mkIVarPut(ty, ivar, e)],
					  RB.mkLtcPop))))*)
        in
	   RB.mkCallcc(bodyTy,
	      A.FunExp (retK,
			body,
(*			 AU.mkLetExp(mkBindVars ([ivar(*, ctx*)], [RB.mkIVarNew(ty)(*, ctxExp*)]),
				     body), *)
			bodyTy))
        end

    and trPval (A.VarPat v, e, body) = expand (v, e, body)
      | trPval _ = raise Fail "todo"

  end (* LTCPVal *)
