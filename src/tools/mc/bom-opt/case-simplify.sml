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

    fun numEnumsOfTyc (B.DataTyc{nNullary, ...}) = nNullary
    fun numConsOfTyc (B.DataTycP{cons, ...}) = List.length cons

    val typeOf = BV.typeOf
    val typesOf = List.map typeOf

  (* variable to variable substitution *)
    fun subst s x = (case BV.Map.find x of NONE => x | SOME y => y)
    fun retype (s, x, ty) = let
	  val x' = BV.new(BV.nameOf x, ty)
	  in
	    (BV.Map.insert(s, x, x'), x')
	  end
  (* transform an expression.  The parameters are:
   *	s	-- a substitution used to rename variables whose types have changed.
   *	tys	-- the result type(s) of the expression.
   *	e	-- the expresssion to transform
   *)
    fun xformE (s, tys, B.E_Pt(_, e)) = (case e
	   of B.E_Let(lhs, e1, e2) =>
		B.mkLet(lhs, xformE(s, typesOf lhs, e1), xformE(s, tys, e2))
	    | B.E_Stmt([y], B.E_DCon(B.DCon{name, rep, ...}, x), e) => (
		case rep
		 of B.Transparent => let
		      val (s', y') = retype (s, y, typeOf x)
		      in
			B.mkLet([y'], B.mkRet[x], xFormE(s', tys, e))
		      end
		  | B.Boxed => let
		      val ty = BTy.T_Tuple[typeOf x]
		      val (s', y') = retype (s, y, ty)
		      in
			B.mkStmt([y'], B.E_Alloc(ty, [x]), xFormE(s', tys, e))
		      end
		  | B.TaggedBox tag => let
		      val tagTy = BTy.T_Enum tag
		      val tag' = BV.new(name, tagTy)
		      val ty = BTy.T_Tuple[tagTy, typeOf x]
		      val (s', y') = retype (s, y, ty)
		      in
			B.mkStmts([
			    ([tag'], B.E_Const(B.E_EnumConst(tag, tagTy))),
			    (lhs, B.E_Alloc(ty, [tag', x]))
			  ], xFormE([y'], tys, e))
		      end
		(* end case *))
(* FIXME: need to apply the substitution to the RHS *)
	    | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, xformE e)
	    | B.E_Fun(fbs, e) => B.mkFun(List.map (xformLambda s) fbs, xformE(s, tys, e))
	    | B.E_Cont(fb, e) => B.mkCont(xformLamda s fb, xformE(s, tys, e))
	    | B.E_If(x, e1, e2) =>
		B.mkIf(x, xformE(s, tys, e1), xformE(s, tys, x2))
	    | B.E_Case(x, rules, dflt) => xformCase (s, tys, x, rules, dflt)
	    | e => B.mkExp e
	  (* end case *))

    and xformLambda s (B.FB{f, params, exh, body}) = B.FB{
	    f = f, params = params, exh = exh,
	    body = xformE(s, [BTy.returnTy(typeOf f)], body)
	  }

    and xformCase (s, tys, x, rules, dflt) = let
	  val dflt = Option.map xformE dflt
	(* classify the rules into a list of those with enum patterns, a list
	 * of literal patterns, and a list of data constructor patterns.
	 *)
	  fun classify ([], enums, lits, cons) = (enums, lits, cons)
	    | classify ((B.D_Const(B.EnumConst(w, ty)), e):: rules) =
		classify (rules, (w, ty, xformE e)::enums, lits, cons)
	    | classify ((B.D_Const c), e):: rules) =
		classify (rules, enums, (c, xformE e)::lits, cons)
	    | classify ((B.P_DCon(dc, y), e)::rules, enums, lits, cons) =
		classify (rules, enums, lits, (dc, y, xformE e)::cons)
	(* classify data constructors *)
	  fun dcClassify ([], trans, boxed, tagged) =
	    | dcClassify (dc::rest, trans, boxed, tagged) =
		case dc
		 of (B.DCon{rep=Transparent, ...}, y, e) =>
	  fun consCase ([
	  in
	    case classify (rules, [], [], [])
	     of (enums, [], []) => B.mkCase(x, rules, dflt)
	      | ([], lits, []) => (* build binary search tree *)
	      | ([], [], cons) => consCase (x, cons, dflt)
	      | (enums, [], cons) => let
		  val tyc = BTy.asTyc(BV.typeOf x)
		  val enumCover = (List.length enums = numEnumsOfTyc tyc)
		  val consCover = (List.length cons = numConsOfTyc tyc)
		  val isBoxed = BV.new("isBoxed", BTy.boolTy)
		  in
		    case (enumCover, consCover)
		     of (true, _) => let
			  val case1 = consCase (x, cons, dflt)
			  in
			    if (numEnumsOfTyc tyc = 1)
			      then let
			      (* when there is only one possible enum value, we just do
			       * an equality test.
			       *)
				val [(w, ty, e)] = enums
				val tmp = BV.new("t", ty)
				in
				  B.mkStms([
				      ([tmp], B.E_Const(B.E_EnumConst(w, ty))),
				      ([isBoxed], B.E_Prim(Primop.P_I32Neq(x, tmp)))
				    ],
				    B.mkIf(isBoxed, case1, e))
				end
			      else B.mkStmt([isBoxed], B.E_Prim(Primop.P_isBoxed x),
				B.mkIf(isBoxed, case1,
				  B.mkCase(x,
				    List.map (fn (w, ty, e) => (B.D_Const(B.EnumConst(w, ty)), e)) enums,
				    NONE)))
			  end
		      | (false, true) =>
			  B.mkStmt([isBoxed], B.E_Prim(Primop.P_isBoxed x),
			    B.mkIf(isBoxed,
			      consCase (x, cons, NONE),
			      B.mkCase(x,
				List.map (fn (w, ty, e) => (B.D_Const(B.EnumConst(w, ty)), e)) enums,
				dflt)))
		      | (false, false) => let
			(* the default case is shared by both the boxed and unboxed
			 * sub cases, so we have to wrap it in a function abstraction.
			 *)
			  val join = BV.new("join", BTy.T_Fun([], [], tys))
			  val joinFB = B.FB{f=join, params=[], exh=[], body=valOf dflt}
			  in
			    B.mkCont(joinFB,
			    B.mkStmt([isBoxed], B.E_Prim(Primop.P_isBoxed x),
			      B.mkIf(isBoxed,
				consCase (x, cons, B.mkThrow(join, [])),
				B.mkCase(x,
				  List.map (fn (w, ty, e) => (B.D_Const(B.EnumConst(w, ty)), e)) enums,
				  B.mkThrow(join, [])))))
			  end
		    (* end case *)
		  end
	      | _ => raise Fail "strange case"
	    (* end case *)
	  end

    fun transform (B.MODULE{name, externs, body}) =
	  B.mkModule(name, externs, xformLambda BV.Map.empty body)

  end
