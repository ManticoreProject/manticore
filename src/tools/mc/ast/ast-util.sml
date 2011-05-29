(* ast-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for manipulating AST terms.
 *)

structure ASTUtil : sig

  (* create a tuple expression, with singleton tuples mapping to their element expression *)
    val mkTupleExp : AST.exp list -> AST.exp

  (* create a parallel tuple, with singleton tuples mapping to their element expression *)
    val mkPTupleExp : AST.exp list -> AST.exp

  (* create a series of let expressions from a list of bindings and a body *)
    val mkLetExp : AST.binding list * AST.exp -> AST.exp

  (* create a case expression *)
    val mkCaseExp : AST.exp * AST.match list -> AST.exp 

  (* create a tuple pattern, with singleton tuples mapping to their element pattern *)
    val mkTuplePat : AST.pat list -> AST.pat

  (* create a function given the function's name, a list of parameter variables, and its body *)
    val mkFunWithParams : AST.var * AST.var list * AST.exp -> AST.lambda

  (* create a function given the function's name, a parameter pattern, and its body *)
    val mkFunWithPat : AST.var * AST.pat * AST.exp -> AST.lambda

  (* make an anonymous function expression *)
    val mkFunExp : AST.var * AST.exp -> AST.exp

  (* create an AST list given a list of expressions and a type *)
    val mkList : AST.exp list * AST.ty -> AST.exp

  (* create various flavors of AST ints from an SML int *)
    val mkIntLit   : int -> Literal.literal
    val mkIntConst : int -> AST.const
    val mkIntPat   : int -> AST.pat
    val mkInt      : int -> AST.exp

  (* generate code for arithmetic *)
    val zero       : AST.exp (* integer 0 *)
    val one        : AST.exp (* integer 1 *)
    val plus       : AST.exp -> AST.exp -> AST.exp
    val plusOne    : AST.exp -> AST.exp
    val minus      : AST.exp -> AST.exp -> AST.exp
    val minusOne   : AST.exp -> AST.exp
    val times      : AST.exp -> AST.exp -> AST.exp
    val intNeg     : AST.exp -> AST.exp
    val intDiv     : AST.exp * AST.exp -> AST.exp
    val intMod     : AST.exp * AST.exp -> AST.exp
    val intGTE     : AST.exp * AST.exp -> AST.exp
    val intGT      : AST.exp * AST.exp -> AST.exp
    val intLT      : AST.exp * AST.exp -> AST.exp

  (* boolean constants *)
    val trueConst  : AST.const
    val falseConst : AST.const
    val trueExp    : AST.exp
    val falseExp   : AST.exp

  (* bool utils *)
    val mkNot : AST.exp -> AST.exp

  (* strings *)
    val mkString : string -> AST.exp 

  (* exceptions *)
    val exnMatchExp : AST.exp
    val mkFail : string * AST.ty -> AST.exp

  (* unit *)
    val unitExp   : AST.exp

  (* operations on boolean expressions *)
    val boolEq     : AST.exp * AST.exp -> bool

  (* create an expression that applies a function *)
    val mkApplyExp : (AST.exp * AST.exp list) -> AST.exp

  (* create an expression that applies a curried function *)
    val mkCurriedApplyExp : (AST.exp * AST.exp list) -> AST.exp

  (* create a sequence of expressions *)
    val mkSeqExp : (AST.exp list * AST.exp) -> AST.exp

  (* make a FunExp which is the composition of two functional expressions *)
    val mkCompose : AST.exp * AST.exp -> AST.exp

  (* return the application of given expression to unit *)
    val mkForce : AST.exp -> AST.exp

  (* create an if expression *)
    val mkIfExp : (AST.exp * AST.exp * AST.exp) -> AST.exp

  (* create an expression for a variable and its concrete types *)
    val mkVarExp : (AST.var * AST.ty list) -> AST.exp

  (* make a fresh copy of an expression *)
    val copyExp : AST.exp -> AST.exp

  (* make an array out of given expressions *)
    val mkArray : AST.exp list * AST.ty -> AST.exp

  (* debugging util *)
    val patToString : AST.pat -> string

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types

    structure TU = TypeUtil

    fun mkTupleExp [e] = e
      | mkTupleExp es = AST.TupleExp es

    fun mkTuplePat [p] = p
      | mkTuplePat ps = AST.TuplePat ps

    fun mkPTupleExp [e] = e
      | mkPTupleExp es = AST.PTupleExp es

    fun mkLetExp ([], e) = e
      | mkLetExp (bind::r, e) = AST.LetExp (bind, mkLetExp (r, e))

  (* pre: all matches have same type *)
    fun mkCaseExp (e, ms as m::_) = A.CaseExp (e, ms, TypeOf.match m)
      | mkCaseExp _ = raise Fail "must provide at least one match"

    fun mkFunWithParams (f, [], e) = let
	  val param = Var.new ("param", Basis.unitTy)
	  in
	    AST.FB(f, param, e)
	  end
      | mkFunWithParams (f, [x], e) = AST.FB(f, x, e)
      | mkFunWithParams (f, params, e) = let
	  val (argTy, resTy) =
	    (case Var.typeOf f
	      of AST.TyScheme(_, AST.FunTy(a,r)) => (a,r)
	       | _ => raise Fail "not a function" (* shouldn't happen *)
	     (* end case *))
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
	  val (argTy, resTy) = (case Var.typeOf f
	    of AST.TyScheme(_, AST.FunTy(a,r)) => (a,r)
	     | _ => raise Fail "not a function" (* shouldn't happen *)
	    (* end case *))
	  val param = Var.new ("param", argTy)
	  in
	    AST.FB(f, param, AST.CaseExp(AST.VarExp(param, []), [AST.PatMatch(pat, e)], resTy))
	  end

  (* make a FunExp from a var and an exp *)
    fun mkFunExp (x, body) = A.FunExp (x, body, TypeOf.exp body)

    fun mkArray (es, ty) = raise Fail "todo: ASTUtil.mkArray"

    val unit = A.TupleExp []

    fun mkIntLit n = Literal.Int (IntInf.fromInt n)
    fun mkIntConst n = A.LConst (mkIntLit n, Basis.intTy)
    fun mkIntPat n = A.ConstPat (mkIntConst n)
    fun mkInt n = A.ConstExp (mkIntConst n)

    val trueConst  = A.DConst (Basis.boolTrue, [])
    val falseConst = A.DConst (Basis.boolFalse, [])
    val trueExp    = A.ConstExp trueConst
    val falseExp   = A.ConstExp falseConst

    fun mkNot e = 
      A.CaseExp (e, 
        [A.PatMatch (A.ConstPat trueConst, falseExp),
	 A.PatMatch (A.ConstPat falseConst, trueExp)],
        Basis.boolTy)

    val unitExp = A.TupleExp []

    fun boolEq (A.ConstExp k1, A.ConstExp k2) =
          (case (k1, k2)
	     of (A.LConst (lit1, ty1), A.LConst (lit2, ty2)) =>
                  if isBool ty1 andalso isBool ty2 
		  then Literal.same (lit1, lit2)
		  else false
	      | _ => false)
      | boolEq _ = false
    and isBool ty = TypeUtil.same (ty, Basis.boolTy)

    fun mkApplyExp (e, es) = 
	A.ApplyExp (e, mkTupleExp(es), TypeUtil.rangeType(TypeOf.exp(e)))

    fun mkCurriedApplyExp (e, es) = (case es
      of arg::[]   => mkApplyExp (e, [arg])
       | arg::args => mkCurriedApplyExp (mkApplyExp (e, [arg]), args)
       | [] => raise Fail "mkCurriedApplyExp"
      (* end case *))

    fun mkSeqExp (es, e) = 
	List.foldr (fn (e, seqExp) => A.SeqExp (e, seqExp)) e es

    fun mkIfExp (e1, e2, e3) = A.IfExp(e1, e2, e3, TypeOf.exp(e2))

    fun mkVarExp (v, tys) = A.VarExp (v, tys)

    val exnMatchExp = A.ConstExp (A.DConst (Basis.exnMatch, []))

    fun mkString s = A.ConstExp (A.LConst (Literal.String s, Basis.stringTy))

    fun mkFail (s, t) = let
      val failConst = A.ConstExp (A.DConst (Basis.exnFail, []))
      val exn = mkApplyExp (failConst, [mkString s])
      in
        A.RaiseExp (exn, t)
      end

    fun mkList ([], t) = A.ConstExp (A.DConst (B.listNil, [t]))
      | mkList (exps, t) = let
          val ::: = A.ConstExp (A.DConst (B.listCons, [t]))
	  fun cons' (x, xs) = mkApplyExp (:::, [x, xs])
	  val nil' = mkList ([], t)
	  in
	    List.foldr cons' nil' exps
	  end

  (* make a FunExp which is the composition of two functional expressions *)
  (* ... and check types along the way *)
    fun mkCompose (f, g) = (case (TypeOf.exp f, TypeOf.exp g)
      of (A.FunTy (fDom, fRng), A.FunTy (gDom, gRng)) =>
	   if TypeUtil.same (fDom, gRng) then let
             val arg = Var.new ("arg", gDom)
             fun v x = A.VarExp (x, [])
	     val body = mkApplyExp (f, [mkApplyExp (g, [v arg])])
             in
               A.FunExp (arg, body, fRng)
	     end
	   else
	     raise Fail "mkCompose: f's domain <> g's range"
       | _ => raise Fail "mkCompose"
      (* end case *))

    fun mkForce thunk = mkApplyExp (thunk, [A.TupleExp []])

    val zero = mkInt 0
    val one = mkInt 1

    local
      fun intBin binop = fn args => mkApplyExp (A.VarExp (binop, []), args) 
      val mkPlus = intBin B.int_plus
      val mkMinus = intBin B.int_minus
      val mkTimes = intBin B.int_times
      val mkDiv = intBin B.int_div
      val mkMod = intBin B.int_mod
      val mkGT = intBin B.int_gt
      val mkGTE = intBin B.int_gte
      val mkLT = intBin B.int_lt
    in
      fun plusOne n = mkPlus [n, one]
      fun plus n m = mkPlus [n, m]
      fun minusOne n = mkMinus [n, one]
      fun minus n m = mkMinus [n, m]
      fun times n m = mkTimes [n, m]
      fun intDiv (n, m) = mkDiv [n, m]
      fun intMod (n, m) = mkMod [n, m]
      fun intGTE (n, m) = mkGTE [n, m]
      fun intGT (n, m) = mkGT [n, m]
      fun intLT (n, m) = mkLT [n, m]
      fun intNeg n = mkApplyExp (A.VarExp (B.int_neg, []), [n])
    end (* local *)

    val lower: string -> string = implode o List.map Char.toLower o explode    

    val isBool: AST.ty -> bool = (fn t => TypeUtil.same (Basis.boolTy, t))
    val isUnit: AST.ty -> bool = (fn t => TypeUtil.same (Basis.unitTy, t))

    val patToString : AST.pat -> string = let
      fun tos (AST.ConPat (c, ts, p)) = (case p
	    of AST.TuplePat _ => DataCon.nameOf c ^ tos p
	     | _ => DataCon.nameOf c ^ "(" ^ tos p ^ ")")
	| tos (AST.TuplePat []) = "()"
	| tos (AST.TuplePat ps) = 
            String.concat ["(", String.concatWith "," (List.map tos ps), ")"]
	| tos (AST.VarPat x) = Var.nameOf x
	| tos (AST.WildPat ty) = "_" (* "(_:" ^ TypeUtil.toString ty ^ ")" *)
	| tos (AST.ConstPat (AST.DConst (c, _))) = DataCon.nameOf c
	| tos (p as AST.ConstPat (AST.LConst (lit, ty))) = let
            val s = Literal.toString lit
            in
              if isBool ty then lower s 
	      else if isUnit ty then "()"
	      else s
            end
      in
        tos
      end

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
	      | exp (A.PCaseExp (es, pms, t)) = A.PCaseExp (map exp es, map pmatch pms, t)
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
	      | exp (A.ExpansionOptsExp(opts, e)) = A.ExpansionOptsExp(opts, exp e)
	      | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
	      | exp (A.FArrayExp (es, n, t)) = A.FArrayExp (List.map exp es, ntree n, t)
	      | exp (A.FlOp flop) = A.FlOp flop
	      | exp (A.PArrayOp pop) = A.PArrayOp pop 
	    and ntree (A.Lf (e1, e2)) = A.Lf (exp e1, exp e2)
	      | ntree (A.Nd ns) = A.Nd (List.map ntree ns)
	    and match (A.PatMatch (p, e)) = A.PatMatch (copyPat s p, exp e)
	      | match (A.CondMatch (p, cond, e)) = A.CondMatch (copyPat s p, exp cond, exp e)
	    and binding (A.ValBind (p, e)) = A.ValBind (copyPat s p, exp e)
	      | binding (A.PValBind (p, e)) = A.PValBind (copyPat s p, exp e)
	      | binding _ = raise Fail "todo"
	    and pmatch (A.PMatch (ps, e)) = A.PMatch (map ppat ps, exp e)
	      | pmatch (A.Otherwise (ts, e)) = A.Otherwise (ts, exp e)
	    and ppat (w as A.NDWildPat _) = w
	      | ppat (A.HandlePat (p, t)) = A.HandlePat (copyPat s p, t)
	      | ppat (A.Pat p) = A.Pat (copyPat s p)
	in
	    exp e
	end

    fun copyExp e =
	let val s = Var.Tbl.mkTable (256, Fail "copyExp")
	in
	    copyExpWalk s e
	end

  end
