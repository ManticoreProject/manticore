(* parr-lit-to-rope.sml
 *
 * COPYRIGHT (c) 2008 Manticore Group
 * All rights reserved.
 *
 * Translation of parallel array literals to ropes.
 *)

structure ParrLitToRope : sig

  (* takes a list of rope elements and their types, and packages them into a rope 
   *   tr [| e1, ..., en |]
   *      ==>
   *   let pval x0 = tr e0
   *       ...
   *       pval xn = tr en
   *   in
   *     Ropes.fromList [x0, x1, ..., xn]
   *   end
   *)
    val tr : AST.exp list * Types.ty -> AST.exp

  end = struct

    structure U = ASTUtil
    structure MEnv = ModuleEnv
    structure BEnv = BasisEnv

    val ropeFromList = 
     (case BEnv.getValFromBasis ["Ropes", "fromList"]
        of MEnv.Var x => x
	 | _ => raise Fail "expected a ModuleEnv.val_bing Var variant"
        (* end case *))

    fun newVar e = Var.new("x", TypeOf.exp e)
    fun mkPValBind (x, e) = AST.PValBind(AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])
    val mkApply = U.mkApplyExp

  (* mkRopeFromList : ty * exp -> exp *)
  (* Make a rope expression from an expression which is a list in the surface language. *)
    fun mkRopeFromList (ty, listExp) = mkApply (AST.VarExp (ropeFromList, [ty]), [listExp])

  (* tr : exp list * ty -> exp *)
  (* Given a list of expressions, which were in a parallel array, and their type, *)
  (* build a rope out of them. *)
    fun tr (es, ty) = let
	  val xs  = map newVar es
	  val binds = ListPair.mapEq mkPValBind (xs, es)
	  val xsList = U.mkList (map mkVarExp xs, ty)
	  in
	    U.mkLetExp (binds, mkRopeFromList (ty, xsList))
	  end
		     
  end
