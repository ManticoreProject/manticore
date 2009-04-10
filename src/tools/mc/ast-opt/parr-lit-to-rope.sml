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
   *     Ropes.fromList [x0, ..., xn]
   *   end
   *)
    val tr : AST.exp list * Types.ty -> AST.exp

  end = struct

    structure U = ASTUtil
    structure MEnv = ModuleEnv
    structure BEnv = BasisEnv

    fun newVar e = Var.new ("x", TypeOf.exp e)
    fun mkPValBind (x, e) = AST.PValBind (AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])
    val mkApply = U.mkApplyExp

  (* mkRopeFromList : ty * exp -> exp *)
  (* Make a rope expression from an expression which is a list in the surface language. *)
    fun mkRopeFromList (ty, listExp) = let
      val ropeFromList = 
       (case BEnv.getValFromBasis ["Ropes", "fromList"]
          of MEnv.Var x => x
	   | _ => raise Fail "expected a ModuleEnv.val_bind Var variant"
       (* end case *))
      in
	mkApply (AST.VarExp (ropeFromList, [ty]), [listExp])
      end

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
    fun tr ([], ty) = let
          val e = BEnv.getVarFromBasis ["Ropes", "empty"]
          in
            AST.VarExp (e, [ty])
          end
      | tr ([e], ty) = let
          val sing = BEnv.getVarFromBasis ["Ropes", "singleton"]
          in
	    AST.ApplyExp (AST.VarExp (sing, [ty]), e, PArray.parrayTy ty)
          end
      | tr (es, ty) = let
          val xs = newVars ty es
	  val tupPat = AST.TuplePat (List.map AST.VarPat xs)
	  val bind = AST.ValBind (tupPat, AST.PTupleExp es)
	  val xsList = U.mkList (List.map mkVarExp xs, ty)
	  in
            U.mkLetExp ([bind], mkRopeFromList (ty, xsList))
	  end
		     
  end
