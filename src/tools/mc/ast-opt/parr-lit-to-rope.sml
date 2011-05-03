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

    structure A = AST
    structure U = ASTUtil
    structure DV = DelayedBasis.Var
    structure MEnv = ModuleEnv

    val itos = Int.toString

    fun newVar e = Var.new ("x", TypeOf.exp e)
    fun mkPValBind (x, e) = AST.PValBind (AST.VarPat x, e)
    fun mkVarExp v = AST.VarExp(v, [])
    val mkApply = U.mkApplyExp

  (* mkRopeFromList : ty * exp -> exp *)
  (* Make a rope expression from an expression which is a list in the surface language. *)
    fun mkRopeFromList (t, listExp) = let
      val rfl = AST.VarExp (DV.ropeFromList (), [t])
      in
        U.mkApplyExp (rfl, [listExp])
      end

  (* newVars : int * ty -> var list *)
  (* Generate [x0, x1, ..., xn], all of type t. *)
    fun newVars (n, t) = List.tabulate (n, fn i => Var.new ("x" ^ itos i, t))

  (* Is there some function like "Option.map with valOf or default value"? *)
  (* Seems like there should be. -ams *)
    fun optMap' (f, default) = fn NONE => default | SOME x => f x
 
  (* shouldNotPar : exp list -> bool *)
  (* given: the expression list in a PArrayExp *)
  (* compute: whether the PArray should not be parallelized *)
  (* ex: a list of constants should not be parallelized *)
  (* ex: a list of lists of constants should not be parallelized *)
  (* ex: a list of applications should be parallelized *)
  (* Note this function can be refined and improved arbitrarily much! *)
  (* As it stands, this function effectively searches for ApplyExps. *)
    fun shouldNotPar (es : AST.exp list) : bool = let
      val $ = List.all
      fun exp (A.LetExp (b, e)) = binding b andalso exp e
	| exp (A.IfExp (e1, e2, e3, _)) = $exp [e1, e2, e3]
	| exp (A.CaseExp (e, ms, _)) = exp e andalso $match ms
	| exp (A.PCaseExp (es, ms, _)) = $exp es andalso $pmatch ms
	| exp (A.HandleExp (e, ms, _)) = exp e andalso $match ms
	| exp (A.RaiseExp (e, _)) = exp e
	| exp (A.FunExp (_, e, _)) = true
	| exp (A.ApplyExp _) = false (* TODO improve this. 1+1 should not be parallelized. *)
	| exp (A.VarArityOpExp _) = true
	| exp (A.TupleExp es) = $exp es
	| exp (A.RangeExp (e1, e2, optE, _)) = 
	    exp e1 andalso exp e2 andalso optMap' (exp, true) optE
	| exp (A.PTupleExp es) = $exp es
	| exp (A.PArrayExp (es, _)) = $exp es
	| exp (A.PCompExp (e, pes, optE)) =
            exp e andalso $(exp o #2) pes andalso optMap' (exp, true) optE	    
	| exp (A.PChoiceExp (es, _)) = $exp es
	| exp (A.SpawnExp e) = exp e
	| exp (A.ConstExp _) = true
	| exp (A.VarExp _) = true
	| exp (A.SeqExp (e1, e2)) = exp e1 andalso exp e2
	| exp (A.OverloadExp _) = true
	| exp (A.ExpansionOptsExp (_, e)) = exp e
(* note: the following expression forms (and some above) are unlikely to appear *)
(* in a parray. nevertheless, all cases are covered here for simplicity. *)
	| exp (A.PArrayOp _) = true
	| exp (A.FTupleExp es) = $exp es
	| exp (A.FArrayExp (es, n, _)) = $exp es andalso ntree n
	| exp (A.FlOp _) = true
      and ntree (A.Lf (e1, e2)) = exp e1 andalso exp e2
	| ntree (A.Nd ts) = $ntree ts
      and binding (A.ValBind (_, e)) = exp e
	| binding (A.PValBind (_, e)) = exp e
	| binding (A.FunBind lams) = true
	| binding (A.PrimVBind _) = true
	| binding (A.PrimCodeBind _) = true
      and match (A.PatMatch (_, e)) = exp e
	| match (A.CondMatch (_, e1, e2)) = exp e1 andalso exp e2
      and pmatch (A.PMatch (_, e)) = exp e
	| pmatch (A.Otherwise (_, e)) = exp e
      in
        $exp es
      end

  (* tr : exp list * ty -> exp *)
  (* Given a list of expressions, which were in a parallel array, and their type, *)
  (* build a rope out of them. *)
    fun tr ([], ty) = AST.VarExp (DV.ropeEmpty (), [ty])
      | tr ([e], ty) = U.mkApplyExp (AST.VarExp (DV.ropeSingleton (), [ty]), [e])
      | tr (es, ty) = let
          val isLit = (fn AST.ConstExp (AST.LConst _) => true
			| _ => false)
          in
            if shouldNotPar es then (* don't build a ptuple *)
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
