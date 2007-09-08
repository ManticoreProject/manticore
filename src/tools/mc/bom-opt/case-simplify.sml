(* case-simplify.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This phase simplifies cases and applications of data constructors in
 * the BOM representation.  The resulting code will be free of data constructors
 * and all cases will be over enumeration tags.
 *)

structure CaseSimplify : sig

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = BOM.Var
    structure BTy = BOMTy
    structure Lit = Literal
    structure BU = BOMUtil

  (* case conversion structures *)

    local
    (* generate numeric comparisons *)
      fun genNumTest (ty, ltPrim, eqPrim, const) {arg, key, ltAct, eqAct, gtAct} = let
	    val v = BV.new("_caseLbl", ty)
	    val isLess = BV.new("_isLess", BTy.boolTy)
	    val isEq = BV.new("_isEq", BTy.boolTy)
	    in
	      B.mkStmt([v], const key,
	      B.mkStmt([isLess], B.E_Prim(ltPrim(arg, v)),
		B.mkIf(isLess,
		  ltAct,
		  B.mkStmt([isEq], B.E_Prim(eqPrim(arg, v)),
		    B.mkIf(isEq, eqAct, gtAct)))))
	    end

    (* generate numeric order test *)
      fun genNumOrdTest (ty, cmpPrim, const) {arg, key, eqAct, neqAct} = let
	    val v = BV.new("_caseLbl", ty)
	    val isOrd = BV.new("_isOrd", BTy.boolTy)
	    in
	      B.mkStmt([v], const key,
	      B.mkStmt([isOrd], B.E_Prim(cmpPrim(arg, v)),
		B.mkIf(isOrd, eqAct, neqAct)))
	    end
    (* label types *)
      val intTy = BTy.T_Raw BTy.T_Int
      val longTy = BTy.T_Raw BTy.T_Int
      val floatTy = BTy.T_Raw BTy.T_Int
      val doubleTy = BTy.T_Raw BTy.T_Int
    (* make an integer constant *)
      fun iConst ty i = B.E_Const(Literal.Int i, ty)
      val i32Const = iConst intTy
      val i64Const = iConst longTy
    (* make a float constant *)
      fun fConst ty f = B.E_Const(Literal.Float f, ty)
      val f32Const = fConst floatTy
      val f64Const = fConst doubleTy
    in
    structure I32Tst = LiteralCaseFn (
      struct
	type label = IntInf.int
	val equal : IntInf.int * IntInf.int -> bool = (op =)
	val greater = IntInf.>
	fun succ (i : IntInf.int) = SOME(i+1)
	fun pred (i : IntInf.int) = SOME(i-1)
	val genCmpTest = genNumTest (intTy, Prim.I32Lt, Prim.I32Eq, i32Const)
	val genEqTest = genNumOrdTest (intTy, Prim.I32Eq, i32Const)
      end)

    structure I64Tst = LiteralCaseFn (
      struct
	type label = IntInf.int
	val equal : IntInf.int * IntInf.int -> bool = (op =)
	val greater = IntInf.>
	fun succ (i : IntInf.int) = SOME(i+1)
	fun pred (i : IntInf.int) = SOME(i-1)
	val genCmpTest = genNumTest (longTy, Prim.I64Lt, Prim.I64Eq, i64Const)
	val genEqTest = genNumOrdTest (longTy, Prim.I64Eq, i64Const)
      end)

    structure F32Tst = LiteralCaseFn (
      struct
	type label = FloatLit.float
	fun equal (a, b) = (case FloatLit.compare(a, b) of EQUAL => true | _ => false)
	fun greater (a, b) = (case FloatLit.compare(a, b) of GREATER => true | _ => false)
	fun succ _ = NONE
	fun pred _ = NONE
	fun genCmpTest _ = raise Fail "F32Tst.genTest"
	val genEqTest = genNumOrdTest (floatTy, Prim.F32Eq, f32Const)
      end)

    structure F64Tst = LiteralCaseFn (
      struct
	type label = FloatLit.float
	fun equal (a, b) = (case FloatLit.compare(a, b) of EQUAL => true | _ => false)
	fun greater (a, b) = (case FloatLit.compare(a, b) of GREATER => true | _ => false)
	fun succ _ = NONE
	fun pred _ = NONE
	fun genCmpTest _ = raise Fail "F64Tst.genTest"
	val genEqTest = genNumOrdTest (doubleTy, Prim.F64Eq, f64Const)
      end)
   end (* local *)

    fun numEnumsOfTyc (BTy.DataTyc{nNullary, ...}) = nNullary
    fun numConsOfTyc (BTy.DataTyc{cons, ...}) = List.length(!cons)

    fun repOf (B.DCon{rep, ...}) = rep

    val typeOf = BV.typeOf
    val typesOf = List.map typeOf

  (* does a BOM type contain type constructors? *)
    fun hasTyc (BTy.T_Tuple(_, tys)) = List.exists hasTyc tys
      | hasTyc (BTy.T_Addr ty) = hasTyc ty
      | hasTyc (BTy.T_Fun(tys1, tys2, tys3)) =
	  List.exists hasTyc tys1 orelse List.exists hasTyc tys2
	    orelse List.exists hasTyc tys3
      | hasTyc (BTy.T_Cont tys) = List.exists hasTyc tys
      | hasTyc (BTy.T_TyCon _) = true
      | hasTyc _ = false

  (* convert datatypes to their representation types.  We assume that hasTyc is true
   * for ty.
   *)
    fun tyToRepTy ty = let
	  fun ty2ty (BTy.T_Tuple(mut, tys)) = BTy.T_Tuple(mut, List.map ty2ty tys)
	    | ty2ty (BTy.T_Addr ty) = BTy.T_Addr(ty2ty ty)
	    | ty2ty (BTy.T_Fun(argTys, exh, resTys)) =
		BTy.T_Fun(tys2tys argTys, tys2tys exh, tys2tys resTys)
	    | ty2ty (BTy.T_Cont tys) = BTy.T_Cont(tys2tys tys)
	    | ty2ty (BTy.T_TyCon tyc) = tyc2ty tyc
	    | ty2ty ty = ty
	  and tys2tys [] = []
	    | tys2tys (ty::r) = ty2ty ty :: tys2tys r
	  and tyc2ty (BTy.DataTyc{rep=ref(SOME ty), ...}) = ty
	    | tyc2ty (BTy.DataTyc{nNullary, cons, rep, ...}) = let
		val _ = (rep := SOME BTy.T_Any);  (* to avoid infinite recursion *)
		val ty = (case (nNullary, !cons)
		       of (0, [BTy.DCon{rep=BTy.Transparent, argTy=[ty], ...}]) => ty2ty ty
			| (0, [BTy.DCon{rep=BTy.Tuple, argTy, ...}]) => BTy.T_Tuple(false, tys2tys argTy)
			| (0, [BTy.DCon{rep=BTy.TaggedTuple tag, argTy, ...}]) =>
			    BTy.T_Tuple(false, BTy.T_Enum tag :: tys2tys argTy)
			| (_, []) => BTy.T_Enum(Word.fromInt nNullary - 0w1)
(* FIXME: we need a union type in BOM for this situation *)
			| _ => BOMTy.T_Any
		      (* end case *))
		in
		  rep := SOME ty;
		  ty
		end
	  in
	    ty2ty ty
	  end

  (* return the low-level BOM type that describes the representation of a data constructor *)
    fun dconToRepTy (BTy.DCon{name, rep, argTy, ...}) = (case (rep, argTy)
	   of (BTy.Transparent, [ty]) => tyToRepTy ty
	    | (B.Transparent, _) => raise Fail("bogus application of transparent dcon "^name)
	    | (BTy.Tuple, _) => BTy.T_Tuple(false, List.map tyToRepTy argTy)
	    | (BTy.TaggedTuple tag, _) => BTy.T_Tuple(false, BTy.T_Enum tag :: List.map tyToRepTy argTy)
	  (* end case *))

  (* variable to variable substitution *)
    val subst = BU.subst
    fun retype (s, x, ty) = let
	  val x' = BV.new(BV.nameOf x, ty)
	  in
(*DEBUG
print(concat["retype(_, ", BV.toString x, ", ", BTy.toString ty, ") = ", BV.toString x', "\n"]);
DEBUG*)
	    (BU.extend(s, x, x'), x')
	  end
  (* if a variable has a TyCon type, the retype it *)
    fun xformVar (s, x) = if hasTyc(typeOf x)
	  then retype(s, x, tyToRepTy(typeOf x))
	  else (s, x)

  (* apply xformVar over a list of variables *)
    fun xformVars (s, xs) = let
	  fun xform ([], s, xs') = (s, List.rev xs')
	    | xform (x::xs, s, xs') = let
		val (s, x') = xformVar(s, x)
		in
		  xform(xs, s, x'::xs')
		end
	  in
	    xform (xs, s, [])
	  end

  (* transform an expression.  The parameters are:
   *	s	-- a substitution used to rename variables whose types have changed.
   *	tys	-- the result type(s) of the expression.
   *	e	-- the expresssion to transform
   *)
    fun xformE (s, tys, B.E_Pt(_, e)) = (case e
	   of B.E_Let(lhs, e1, e2) => let
		val (s', lhs) = xformVars (s, lhs)
		in
		  B.mkLet(lhs, xformE(s, typesOf lhs, e1), xformE(s', tys, e2))
		end
	    | B.E_Stmt([y], B.E_DCon(B.DCon{name, rep, argTy, ...}, xs), e) => (
		case (rep, xs)
		 of (B.Transparent, [x]) => let
		      val (s', y') = retype (s, y, tyToRepTy(typeOf x))
		      in
			B.mkLet([y'], B.mkRet[subst s x], xformE(s', tys, e))
		      end
		  | (B.Tuple, _) => let
		      val ty = tyToRepTy(BTy.T_Tuple(false, argTy))
		      val (s', y') = retype (s, y, ty)
		      in
			B.mkStmt([y'], B.E_Alloc(ty, BU.subst'(s, xs)), xformE(s', tys, e))
		      end
		  | (B.TaggedTuple tag, _) => let
		      val tagTy = BTy.T_Enum tag
		      val tag' = BV.new(name, tagTy)
		      val ty = tyToRepTy(BTy.T_Tuple(false, tagTy :: argTy))
		      val (s, y) = retype (s, y, ty)
		      in
			B.mkStmts([
			    ([tag'], B.E_Const(Lit.Enum tag, tagTy)),
			    ([y], B.E_Alloc(ty, tag' :: BU.subst'(s, xs)))
			  ], xformE(s, tys, e))
		      end
		  | (B.Transparent, _) => raise Fail "bogus transparent dcon application"
		(* end case *))
	    | B.E_Stmt(lhs, rhs, e) => let
		val (s', lhs) = xformVars (s, lhs)
		in
		  B.mkStmt(lhs, xformRHS(s, rhs), xformE(s', tys, e))
		end
	    | B.E_Fun(fbs, e) => let
		val (s, fbs) = xformLambdas (s, fbs)
		in
		  B.mkFun(fbs, xformE(s, tys, e))
		end
	    | B.E_Cont(fb, e) => let
		val (s, fb) = xformLambda (s, fb)
		in
		  B.mkCont(fb, xformE(s, tys, e))
		end
	    | B.E_If(x, e1, e2) =>
		B.mkIf(subst s x, xformE(s, tys, e1), xformE(s, tys, e2))
	    | B.E_Case(x, rules, dflt) => xformCase (s, tys, x, rules, dflt)
	    | B.E_Apply(f, args, exh) => let
		val subst = subst s
		in
		  B.mkApply(subst f, List.map subst args, List.map subst exh)
		end
	    | B.E_Throw(k, args) => B.mkThrow(subst s k, List.map (subst s) args)
	    | B.E_Ret args => B.mkRet(List.map (subst s) args)
	    | B.E_HLOp _ => raise Fail "unexpected HLOp"
	  (* end case *))

    and xformRHS (s, rhs) = (case rhs
	   of B.E_Const(lit, ty) => if hasTyc ty
		then B.E_Const(lit, tyToRepTy ty)
		else BU.substRHS(s, rhs)
	    | B.E_Cast(ty, x) => if hasTyc ty
		then B.E_Cast(tyToRepTy ty, subst s x)
		else BU.substRHS(s, rhs)
	    | B.E_Alloc(ty, xs) => if hasTyc ty
		then B.E_Alloc(tyToRepTy ty, List.map (subst s) xs)
		else BU.substRHS(s, rhs)
	    | B.E_DCon _ => raise Fail "impossible"
	    | _ => BU.substRHS(s, rhs)
	  (* end case *))

    and xformLambdas (s, fbs) = let
	  val s = List.foldl (fn (B.FB{f, ...}, s) => (#1(xformVar(s, f)))) s fbs
	  fun xformLambda (B.FB{f, params, exh, body}) = let
		val f = subst s f
		val (s, params) = xformVars (s, params)
		val (s, exh) = xformVars (s, exh)
		in
		  B.FB{
		      f = f, params = params, exh = exh,
		      body = xformE(s, BTy.returnTy(typeOf f), body)
		    }
		end
	  in
	    (s, List.map xformLambda fbs)
	  end

    and xformLambda (s, fb) = let
	  val (s, [fb]) = xformLambdas(s, [fb])
	  in
	    (s, fb)
	  end

    and xformCase (s, tys, x, rules, dflt) = let
	  val argument = subst s x
	  val dflt = Option.map (fn e => xformE(s, tys, e)) dflt
	(* classify the rules into a list of those with enum patterns, a list
	 * of literal patterns, and a list of data constructor patterns.
	 *)
	  fun classify ([], enums, lits, cons) = (enums, lits, cons)
	    | classify ((B.P_Const(Lit.Enum w, ty), e)::rules, enums, lits, cons) =
		classify (rules, (w, ty, e)::enums, lits, cons)
	    | classify ((B.P_Const c, e)::rules, enums, lits, cons) =
		classify (rules, enums, (c, e)::lits, cons)
	    | classify ((B.P_DCon(dc, y), e)::rules, enums, lits, cons) =
		classify (rules, enums, lits, (dc, y, e)::cons)
	(* generate a case for a list of one or more data constructors, plus an
	 * optional default case.
	 *)
	  fun consCase ([(dc, ys, e)], dflt) = let
		val (s, ys) = xformVars(s, ys)
		val (s, argument') = retype(s, argument, dconToRepTy dc)
		fun sel ([], _) = xformE(s, tys, e)
		  | sel (y::ys, i) = let
		      val ty = typeOf y
		      val ty' = BTy.select(typeOf argument', i)
		      in
			if BTy.match(ty', ty)
			  then B.mkStmt([y], B.E_Select(i, argument'), sel(ys, i+1))
			  else let
			    val y' = BV.new("_t", ty')
			    in
			      B.mkStmt([y'], B.E_Select(i, argument'),
			      B.mkStmt([y], B.E_Cast(ty, y'),
				sel(ys, i+1)))
			    end
		      end
		in
		  case (repOf dc, dflt)
		   of (B.Transparent, NONE) => (case ys
			 of [y] => B.mkStmt([y], B.E_Cast(typeOf y, argument), xformE(s, tys, e))
			  | _ => raise Fail "bogus transparent rep"
			(* end case *))
		    | (B.Tuple, NONE) => B.mkStmt(
			[argument'], B.E_Cast(BV.typeOf argument', argument),
			sel (ys, 0))
		    | (B.TaggedTuple tag, SOME dflt) => let
			val ty = BTy.T_Enum tag
			val tag' = BV.new("tag", ty)
			val tmp = BV.new("tmp", ty)
			val eq = BV.new("eq", BTy.boolTy)
			in
			  B.mkStmts([
			      ([argument'], B.E_Cast(BV.typeOf argument', argument)),
			      ([tag'], B.E_Select(0, argument')),
			      ([tmp], B.E_Const(Lit.Enum tag, ty)),
			      ([eq], B.E_Prim(Prim.I32NEq(argument, tmp)))
			    ],
			    B.mkIf(eq, sel(ys, 1), dflt))
			end
		    | _ => raise Fail "bogus dcon rep"
		  (* end case *)
		end
	    | consCase (cons, dflt) = let
	      (* here we have two, or more, constructors and they must all have the
	       * TaggedBox representation.
	       *)
		fun mkAlt (dc, ys, e) = (case repOf dc
		       of B.TaggedTuple tag => let
			    val (s, ys) = xformVars(s, ys)
			    fun sel ([], _) = xformE(s, tys, e)
			      | sel (y::ys, i) = B.mkStmt([y], B.E_Select(i, argument), sel(ys, i+1))
			    in
			      (B.P_Const(Lit.Enum tag, BTy.T_Enum tag), sel (ys, 1))
			    end
			| _ => raise Fail "expected TaggedBox representation"
		      (* end case *))
		in
		  B.mkCase(argument, List.map mkAlt cons, dflt)
		end
	  fun enumCase (w, ty, e) = let
		val ty = if hasTyc ty then tyToRepTy ty else ty
		in
		  (B.P_Const(Lit.Enum w, ty), xformE(s, tys, e))
		end
	  in
	    case classify (rules, [], [], [])
	     of (enums, [], []) => B.mkCase(argument, List.map enumCase enums, dflt)
	      | ([], lits, []) => literalCase (s, tys, argument, lits, dflt)
	      | ([], [], cons) => consCase (cons, dflt)
	      | (enums, [], cons) => let
		  val tyc = BTy.asTyc(BV.typeOf x)
		  val enumCover = (List.length enums = numEnumsOfTyc tyc)
		  val consCover = (List.length cons = numConsOfTyc tyc)
		  val isBoxed = BV.new("isBoxed", BTy.boolTy)
		  in
		    case (enumCover, consCover)
		     of (true, _) => let
			  val case1 = consCase (cons, dflt)
			  in
			    if (numEnumsOfTyc tyc = 1)
			      then let
			      (* when there is only one possible enum value, we just do
			       * an equality test.
			       *)
				val [(w, ty, e)] = enums
				val ty = if hasTyc ty then tyToRepTy ty else ty
				val tmp = BV.new("t", ty)
				in
				  B.mkStmts([
				      ([tmp], B.E_Const(Lit.Enum w, ty)),
				      ([isBoxed], B.E_Prim(Prim.I32NEq(argument, tmp)))
				    ],
				    B.mkIf(isBoxed, case1, xformE(s, tys, e)))
				end
			      else B.mkStmt([isBoxed], B.E_Prim(Prim.isBoxed argument),
				B.mkIf(isBoxed, case1,
				  B.mkCase(argument,
				    List.map enumCase enums,
				    NONE)))
			  end
		      | (false, true) =>
			  B.mkStmt([isBoxed], B.E_Prim(Prim.isBoxed argument),
			    B.mkIf(isBoxed,
			      consCase (cons, NONE),
			      B.mkCase(argument,
				List.map enumCase enums,
				dflt)))
		      | (false, false) => let
			(* the default case is shared by both the boxed and unboxed
			 * sub cases, so we have to wrap it in a function abstraction.
			 *)
			  val join = BV.new("join", BTy.T_Fun([], [], tys))
			  val joinFB = B.FB{f=join, params=[], exh=[], body=valOf dflt}
			  in
			    B.mkCont(joinFB,
			    B.mkStmt([isBoxed], B.E_Prim(Prim.isBoxed argument),
			      B.mkIf(isBoxed,
				consCase (cons, SOME(B.mkThrow(join, []))),
				B.mkCase(argument,
				  List.map enumCase enums,
				  SOME(B.mkThrow(join, []))))))
			  end
		    (* end case *)
		  end
	      | _ => raise Fail "strange case"
	    (* end case *)
	  end

  (* convert a case on literals to a if-then-else tree; note that the default case
   * is required and has already been transformed.
   *)
    and literalCase (s, tys, argument, cases as (((_, ty), _)::_), SOME dflt) = let
	  val arcs = List.map (fn ((lit, _), e) => (lit, xformE(s, tys, e))) cases
	  fun convert (proj, cvt) = cvt{
		  arg = argument,
		  arcs = List.map (fn ((lit, _), e) => (proj lit, xformE(s, tys, e))) cases,
		  default = dflt
		}
	  in
	    case ty
	     of BTy.T_Raw BTy.T_Int => convert(fn (Literal.Int i) => i, I32Tst.convert)
	      | BTy.T_Raw BTy.T_Long => convert(fn (Literal.Int i) => i, I64Tst.convert)
	      | BTy.T_Raw BTy.T_Float => convert(fn (Literal.Float f) => f, F32Tst.convert)
	      | BTy.T_Raw BTy.T_Double => convert(fn (Literal.Float f) => f, F64Tst.convert)
	      | _ => raise Fail("literal case on unsupported type "^ BTy.toString ty)
	    (* end case *)
	  end
      | literalCase _ = raise Fail "ill-formed literal case"

    fun transform (B.MODULE{name, externs, body}) = let
	  val module = B.mkModule(name, externs, #2 (xformLambda (BV.Map.empty, body)))
	  in
(* FIXME: eventually, this pass should preserve census info! *)
	    Census.census module;
	    module
	  end

  end
