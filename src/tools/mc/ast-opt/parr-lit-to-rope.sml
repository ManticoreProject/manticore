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

  end = struct

    structure U = ASTUtil
    structure D = DelayedBasis
    structure DV = D.Var

    structure MEnv = ModuleEnv

    fun newVar e = Var.new ("x", TypeOf.exp e)
    fun mkPValBind (x, e) = AST.PValBind (AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])
    val mkApply = U.mkApplyExp

  (* mkRopeFromList : ty * exp -> exp *)
  (* Make a rope expression from an expression which is a list in the surface language. *)
    fun mkRopeFromList (ty, listExp) =
      U.mkApplyExp (AST.VarExp (DV.ropeFromList (), [ty]), [listExp])

  (* newVars : ty -> exp list -> var list *)
  (* Given [e0, e1, ..., en], generate [x0, x1, ..., xn]. *)
  (* We are in a special case where we know all the types are the same. *)
    fun newVars ty es = let
      fun lp ([], _, xs) = List.rev xs
        | lp (e::es, i, xs) = let
            val x = Var.new ("x" ^ Int.toString i, ty) 
            in
              lp (es, i+1, x::xs)
            end
      in
	lp (es, 0, [])
      end

  (* tr : exp list * ty -> exp *)
  (* Given a list of expressions, which were in a parallel array, and their type, *)
  (* build a rope out of them. *)
    fun tr ([], ty) = AST.VarExp (D.Var.ropeEmpty (), [ty])
      | tr ([e], ty) = let
          val sing = D.Var.ropeSingleton ()
          in
	    AST.ApplyExp (AST.VarExp (sing, [ty]), e, Basis.parrayTy ty)
          end
      | tr (es, ty) = let
          val xs = newVars ty es
	  val tupPat = AST.TuplePat (List.map AST.VarPat xs)
	  val bind = AST.ValBind (tupPat, AST.PTupleExp es)
	  val xsList = U.mkList (List.map mkVarExp xs, ty)
	  in
            U.mkLetExp ([bind], mkRopeFromList (ty, xsList))
	  end

(*
(* HORRIBLE HACK for testing the rope-of-tuples translation *)
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
