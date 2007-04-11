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

    fun xformE (B.E_Pt(_, e)) = (case e
	   of B.E_Let(lhs, e1, e2) => B.mkLet(lhs, xformE exp, xformE e2)
	    | B.E_Stmt(lhs, B.E_DCon(dc, xs), e) => ??
	    | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, xformE e)
	    | B.E_Fun(fbs, e) => B.mkFun(List.map xformLambda fbs, xformE e)
	    | B.E_Cont(fb, e) => B.mkCont(List.map xformLamda
	    | B.E_If(x, e1, e2) => B.mkIf(x, xformE e1, xformE x2)
	    | B.E_Case(x, rules, dflt) => xformCase (x, rules, dflt)
	    | e => B.mkExp e
	  (* end case *))

    and xformLambda (B.FB{f, params, exh, body}) =
	  B.FB{f = f, params = params, exh = exh, body = xformE body}

    and xformCase (x, rules, dflt) = let
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
		  in
		    case (enumCover, consCover)
		     of (true, _) => let
			  val isBoxed = BV.new("isBoxed", BTy.boolTy)
			  val case1 = consCase (x, cons, dflt)
			  in
			    if (numEnumsOfTyc tyc = 1)
			      then let
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
		      | (_, true) =>
		      | (false, false) => let
			(* the default case is shared by both the boxed and unboxed
			 * sub cases.
			 *)
			  in
			  end
		    (* end case *)
		  end
	      | _ => raise Fail "strange case"
	    (* end case *)
	  end

    fun transform (B.MODULE{name, externs, body}) =
	  B.mkModule(name, externs, xformLambda body)

  end
