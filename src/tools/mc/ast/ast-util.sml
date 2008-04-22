(* ast-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for manipulating AST terms.
 *)

structure ASTUtil : sig

  (* create a tuple expression, with singleton tuples mapping to their element expression *)
    val mkTupleExp : AST.exp list-> AST.exp

  (* create a series of let expressions from a list of bindings and a body *)
    val mkLetExp : AST.binding list * AST.exp -> AST.exp

  (* create a tuple pattern, with singleton tuples mapping to their element pattern *)
    val mkTuplePat : AST.pat list-> AST.pat

  (* create a function given the function's name, a list of parameter variables, and its body *)
    val mkFunWithParams : AST.var * AST.var list * AST.exp -> AST.lambda

  (* create a function given the function's name, a parameter pattern, and its body *)
    val mkFunWithPat : AST.var * AST.pat * AST.exp -> AST.lambda

  (* create an AST list given a list of expressions and a type *)
    val mkList : AST.exp list * AST.ty -> AST.exp

  (* create an AST int based on an SML int *)
    val mkInt : int -> AST.exp

  (* create an expression that applies a function *)
    val mkApplyExp : (AST.exp * AST.exp list) -> AST.exp

  (* create a sequence of expressions *)
    val mkSeqExp : (AST.exp list * AST.exp) -> AST.exp

  (* create an if expression *)
    val mkIfExp : (AST.exp * AST.exp * AST.exp) -> AST.exp

  (* create an expression for a variable and its concete types *)
    val mkVarExp : (AST.var * AST.ty list) -> AST.exp

  (* make a fresh copy of an expression *)
    val copyExp : AST.exp -> AST.exp

  end = struct

    structure A = AST
    structure B = Basis

    fun mkTupleExp [e] = e
      | mkTupleExp es = AST.TupleExp es

    fun mkTuplePat [p] = p
      | mkTuplePat ps = AST.TuplePat ps

    fun mkLetExp ([], e) = e
      | mkLetExp (bind::r, e) = AST.LetExp(bind, mkLetExp(r, e))

    fun mkFunWithParams (f, [], e) = let
	  val param = Var.new ("param", Basis.unitTy)
	  in
	    AST.FB(f, param, e)
	  end
      | mkFunWithParams (f, [x], e) = AST.FB(f, x, e)
      | mkFunWithParams (f, params, e) = let
	  val AST.TyScheme(_, AST.FunTy(argTy, resTy)) = Var.typeOf f
	  val param = Var.new ("param", argTy)
	  val pat = AST.TuplePat(List.map AST.VarPat params)
	  in
	    AST.FB(f, param, AST.CaseExp(AST.VarExp(param, []), [AST.PatMatch(pat, e)], resTy))
	  end

  (* create a single-parameter lambda from one that has a pattern parameter *)
    fun mkFunWithPat (f, AST.VarPat x, e) = AST.FB(f, x, e)
      | mkFunWithPat (f, AST.TuplePat[], e) = let
	  val param = Var.new ("param", Basis.unitTy)
	  in
	    AST.FB(f, param, e)
	  end
      | mkFunWithPat (f, pat, e) = let
	  val AST.TyScheme(_, AST.FunTy(argTy, resTy)) = Var.typeOf f
	  val param = Var.new ("param", argTy)
	  in
	    AST.FB(f, param, AST.CaseExp(AST.VarExp(param, []), [AST.PatMatch(pat, e)], resTy))
	  end

    fun mkList ([], ty) = A.ConstExp (A.DConst (B.listNil, [ty]))
      | mkList (exps, ty) = let
          val ::! = A.ConstExp (A.DConst (B.listCons, [ty]))
	  fun cons' (x, xs) = A.ApplyExp (::!, A.TupleExp [x, xs], B.listTy ty)
	  val nil' = mkList ([], ty)
	  in
	    List.foldr cons' nil' exps
	  end

    fun mkInt n = A.ConstExp (A.LConst (Literal.Int (IntInf.fromInt n), Basis.intTy))

    fun mkApplyExp (e, es) = 
	A.ApplyExp (e, mkTupleExp(es), TypeUtil.rangeType(TypeOf.exp(e)))

    fun mkSeqExp (es, e) = 
	List.foldr (fn (e, seqExp) => A.SeqExp (e, seqExp)) e es

    fun mkIfExp (e1, e2, e3) = A.IfExp(e1, e2, e3, TypeOf.exp(e2))

    fun mkVarExp (v, tys) =
	A.VarExp (v, tys)

    fun copyPat s p =
	let fun f (A.ConPat (c, ts, p)) = A.ConPat (c, ts, f p)
	      | f (A.TuplePat ps) = A.TuplePat (map f ps)
	      | f (v as A.VarPat x) = 
		let val x' = Var.copy x
		in
		    Var.Tbl.insert s (x, x');
		    A.VarPat x'
		end
	      | f (A.WildPat t) = A.WildPat t
	      | f (k as A.ConstPat _) = k
	in
	    f p
	end

    fun copyExpWalk s e =
	let fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
	      | exp (A.IfExp (e1, e2, e3, t)) =
		  A.IfExp (exp e1, exp e2, exp e3, t)
	      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
	      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
	      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
	      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
	      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
	      | exp (m as A.VarArityOpExp _) = m
	      | exp (A.TupleExp es) = A.TupleExp (map exp es)
	      | exp (A.RangeExp (e1, e2, oe3, t)) =
		  A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
	      | exp (A.PTupleExp es) = A.PTupleExp (map exp es)
	      | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
	      | exp (A.PCompExp (e, pes, opred)) = 
		  A.PCompExp (exp e, 
			      map (fn (p,e) => (copyPat s p, exp e)) pes,
			      Option.map exp opred)
	      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
	      | exp (A.SpawnExp e) = A.SpawnExp (exp e) 
	      | exp (k as A.ConstExp _) = k
	      | exp (v as A.VarExp (x, ts)) = 
		(case Var.Tbl.find s x
		  of NONE => v
		   | SOME v' => A.VarExp (v', ts))
	      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	      | exp (ov as A.OverloadExp _) = ov
	    and match (A.PatMatch (p, e)) = A.PatMatch (copyPat s p, exp e)
	      | match (A.CondMatch (p, cond, e)) = A.CondMatch (copyPat s p, exp cond, exp e)
	    and binding (A.ValBind (p, e)) = A.ValBind (copyPat s p, exp e)
	      | binding (A.PValBind (p, e)) = A.PValBind (copyPat s p, exp e)
	      | binding _ = raise Fail "todo"
	in
	    exp e
	end

    fun copyExp e =
	let val s = Var.Tbl.mkTable (256, Fail "copyExp")
	in
	    copyExpWalk s e
	end

  end
