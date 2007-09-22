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

  end = struct

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

  end
