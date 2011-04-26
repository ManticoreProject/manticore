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
   *   let val (x1, ..., xn) = (| e1, ..., en |)
   *   in
   *     Rope.fromList [x0, ..., xn]
   *   end
   *)
    val tr : AST.exp list * Types.ty -> AST.exp

    val mkRope    : AST.exp list * Types.ty -> AST.exp

  end = struct

    structure U = ASTUtil
    structure MEnv = ModuleEnv
    structure BEnv = BasisEnv

    val itos = Int.toString

    val getVar = BEnv.getVarFromBasis
    val rVar = fn v => getVar ("Rope"::[v])

    val ropeFromList  = Memo.new' (fn () => rVar "fromList")
    val ropeEmpty     = Memo.new' (fn () => rVar "empty")
    val ropeSingleton = Memo.new' (fn () => rVar "singleton")

    fun newVar e = Var.new ("x", TypeOf.exp e)
    fun mkPValBind (x, e) = AST.PValBind (AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])
    val mkApply = U.mkApplyExp

  (* mkRopeFromList : ty * exp -> exp *)
  (* Make a rope expression from an expression which is a list in the surface language. *)
    fun mkRopeFromList (t, listExp) = let
      val rfl = AST.VarExp (ropeFromList (), [t])
      in
        U.mkApplyExp (rfl, [listExp])
      end

  (* newVars : int * ty -> var list *)
  (* Generate [x0, x1, ..., xn], all of type t. *)
    fun newVars (n, t) = List.tabulate (n, fn i => Var.new ("x" ^ itos i, t))
      
  (* tr : exp list * ty -> exp *)
  (* Given a list of expressions, which were in a parallel array, and their type, *)
  (* build a rope out of them. *)
    fun tr ([], ty) = AST.VarExp (ropeEmpty (), [ty])
      | tr ([e], ty) = U.mkApplyExp (AST.VarExp (ropeSingleton (), [ty]), [e])
      | tr (es, ty) = let
          val isLit = (fn AST.ConstExp (AST.LConst _) => true
			| _ => false)
          in
            if List.all isLit es then (* don't build a ptuple *)
              mkRopeFromList (ty, U.mkList (es, ty))
	    else let
              val xs = newVars (List.length es, ty)
	      val tupPat = AST.TuplePat (List.map AST.VarPat xs)
	      val bind = AST.ValBind (tupPat, AST.PTupleExp es)
	      val xsList = U.mkList (List.map mkVarExp xs, ty)
	      in
                U.mkLetExp ([bind], mkRopeFromList (ty, xsList))
	      end
	  end

  (* mkRope : exp list * ty -> exp *)
    fun mkRope (es, t) = tr (es, t)

(*
(* HACK for testing the rope-of-tuples translation *)
    fun trans (es, ty) = let
      val rope = tr (es, ty)
      val rope' = RopeOfTuples.transform rope
      in
       (PrintAST.printExpNoTypesNoStamps rope;
	PrintAST.printExpNoTypesNoStamps rope';
	raise Fail "You've been ROPED.")
      end

    fun tr (es, ty) = trans (es, ty)
*)
	     
  end
