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
        val doneCell = setOnceCell()
	val iv = ivarNew()
        fun ctx () = if setCell(doneCell)
            then ltcPop()
            else throw k( [| e2[x -> ivarGet(iv)] |] )
	in
           ltcPush(ctx);
           ivarPut(iv, e1);
           ltcPop();
	end)

    The rules applies inductively for the other expression forms.

    *)

    fun mkBindVar (v, e) = A.ValBind(A.VarPat v, e)
    fun mkBindVars (vs, es) = ListPair.map mkBindVar (vs, es)

    fun expand (v, e, body) = let
        val ty = TypeOf.exp(e)
	val doneCell = RB.setOnceCellVar
	val ivar = RB.ivarVar("ivar", ty)
	val retK = RB.mkContVar(ty, "retK")
	val ctx = RB.monoVar("ctx", Basis.unitTy --> RB.voidTy)
        val ctxExp =
	    A.FunExp (ctx,
		AU.mkIfExp(RB.mkSetCell(doneCell),
			   RB.mkLtcPop,
			   RB.mkThrowcc(ty, retK, VarSubst.exp' (VarSubst.singleton(v, v)) (RB.mkIvarGet(ivar)) body)),
		Basis.unitTy --> RB.voidTy)
        val body = AU.mkSeqExp(
		     RuntimeBasis.mkLtcPush(ctx),
		   [ RB.mkIvarPut(ivar, e),
		     RB.mkLtcPop
		   ])
        in
	   RB.mkCallcc(ty,
	      AU.mkLetExp(mkBindVars ([doneCell, #1(ivar), ctx], [RB.mkSetOnceCell, RB.mkIvar(ty), ctxExp]),
			  body))
        end

    and trPval (A.VarPat v, e, body) = expand (v, e, body)
      | trPval _ = raise Fail "todo"

  end (* LTCPVal *)
