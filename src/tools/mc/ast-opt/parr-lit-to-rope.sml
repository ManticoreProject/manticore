(* parr-lit-to-rope.sml
 *
 * COPYRIGHT (c) 2008 Manticore Group
 * All rights reserved.
 *
 * Translation of parallel array literals to ropes.
 *)

structure ParrLitToRope : sig

  (* takes a list of rope elements and their types, and packages them into a rope 
   *   [| e1, ..., en |]
   *      ==>
   *   let pval x0 = e0
   *       ...
   *       pval xn = en
   *   in
   *     Ropes.fromSeq(Seq.concat(Seq.singleton x0, ..., Seq.singleton xn))
   *   end
   *)
    val tr : AST.exp list * Types.ty -> AST.exp

  end = struct

    fun newVar e = Var.new("x", TypeOf.exp e)

    fun mkPValBind (x, e) = AST.PValBind(AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])

  (* make an expression that creates a rope from a sequence *)
    fun mkRopesFromSeq (ty, seq) = let
	  val ModuleEnv.Var fromSeq = BasisEnv.getValFromBasis["Ropes", "fromSeq"]
          in
	    ASTUtil.mkApplyExp(AST.VarExp(fromSeq, [ty]), seq)
	  end

  (* make an expression that creates a singleton sequence containing the expression e *)
    fun mkSingletonSeq (ty, e) = let
	  val ModuleEnv.Var singleton = BasisEnv.getValFromBasis["Seq", "singleton"]
          in
	    ASTUtil.mkApplyExp(AST.VarExp(singleton, [ty]), e)
	  end

  (* make an expression that concatenates two sequences *)
    fun mkSeqConcat (ty, s1, s2) = let
	  val ModuleEnv.Var concat = BasisEnv.getValFromBasis["Seq", "concat"]
          in
	    AST.mkApplyExp(AST.VarExp(concat, [ty]), ASTUtil.mkTupleExp[s1, s2])
	  end

  (* make an expression that creates a sequence containing the expressions in es *)
    fun mkSeqFromExps (ty, es) = let
	  val ModuleEnv.Var empty = BasisEnv.getValFromBasis["Seq", "empty"]
	  val emptySeq = AST.VarExp(empty, [ty])
	  fun f (e, seq) = mkSeqConcat(mkSingletonSeq e, seq)
          in
	    List.foldl f emptySeq es
	  end

    fun tr (es, ty) = let
	  val xs = List.map newVar es
	  val binds = ListPair.mapEq mkPValBind (xs, es)
	  in
	    AST.mkLetExp(binds, mkRopesFromSeq(ty, mkSeqFromExps(ty, List.map mkVarExp xs)))
	  end
		     
  end
