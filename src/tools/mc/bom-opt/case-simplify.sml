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

    fun numEnumsOfTyc (BTy.DataTyc{nNullary, ...}) = nNullary
    fun numConsOfTyc (BTy.DataTyc{cons, ...}) = List.length(!cons)

    fun repOf (B.DCon{rep, ...}) = rep

    val typeOf = BV.typeOf
    val typesOf = List.map typeOf

  (* does a BOM type contain type constructors? *)
    fun hasTyc (BTy.T_Tuple tys) = List.exists hasTyc tys
      | hasTyc (BTy.T_Fun(tys1, tys2, tys3)) =
	  List.exists hasTyc tys1 orelse List.exists hasTyc tys2
	    orelse List.exists hasTyc tys3
      | hasTyc (BTy.T_Cont tys) = List.exists hasTyc tys
      | hasTyc (BTy.T_TyCon _) = true
      | hasTyc _ = false

  (* convert datatypes to their representation types.  We assume that hasTyc is true
   * for ty.
   *)
    fun toRepTy ty = let
	  fun ty2ty (BTy.T_Tuple tys) = BTy.T_Tuple(List.map ty2ty tys)
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
			| (0, [BTy.DCon{rep=BTy.Tuple, argTy, ...}]) => BTy.T_Tuple(tys2tys argTy)
			| (0, [BTy.DCon{rep=BTy.TaggedTuple tag, argTy, ...}]) =>
			    BTy.T_Tuple(BTy.T_Enum tag :: tys2tys argTy)
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

  (* variable to variable substitution *)
    fun subst s x = (case BV.Map.find(s, x) of NONE => x | SOME y => y)
    fun retype (s, x, ty) = let
	  val x' = BV.new(BV.nameOf x, ty)
	  in
	    (BV.Map.insert(s, x, x'), x')
	  end

  (* if a variable has a TyCon type, the retype it *)
    fun xformVar (s, x) = if hasTyc(typeOf x)
	  then retype(s, x, toRepTy(typeOf x))
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
		      val (s', y') = retype (s, y, typeOf x)
		      in
			B.mkLet([y'], B.mkRet[x], xformE(s', tys, e))
		      end
		  | (B.Tuple, _) => let
		      val ty = BTy.T_Tuple argTy
		      val (s', y') = retype (s, y, ty)
		      in
			B.mkStmt([y'], B.E_Alloc(ty, xs), xformE(s', tys, e))
		      end
		  | (B.TaggedTuple tag, _) => let
		      val tagTy = BTy.T_Enum tag
		      val tag' = BV.new(name, tagTy)
		      val ty = BTy.T_Tuple(tagTy :: argTy)
		      val (s, y) = retype (s, y, ty)
		      in
			B.mkStmts([
			    ([tag'], B.E_Const(Lit.Enum tag, tagTy)),
			    ([y], B.E_Alloc(ty, tag' :: xs))
			  ], xformE(s, tys, e))
		      end
		  | (B.Transparent, _) => raise Fail "bogus transparent dcon application"
		(* end case *))
	    | B.E_Stmt(lhs, rhs, e) => let
		val (s', lhs) = xformVars (s, lhs)
		in
(* FIXME: need to apply the substitution s to the RHS *)
		  B.mkStmt(lhs, rhs, xformE(s', tys, e))
		end
	    | B.E_Fun(fbs, e) => B.mkFun(xformLambdas (s, fbs), xformE(s, tys, e))
	    | B.E_Cont(fb, e) => B.mkCont(xformLambda (s, fb), xformE(s, tys, e))
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

    and xformRHS (s, rhs) = let
	  val subst = subst s
	  in
	    case rhs
	     of B.E_Const _ => rhs
	      | B.E_Cast(ty, x) => if hasTyc ty
		  then B.E_Cast(toRepTy ty, subst x)
		  else rhs
	      | B.E_Select(i, x)  => B.E_Select(i, subst x)
	      | B.E_Alloc(ty, xs) => if hasTyc ty
		  then B.E_Alloc(toRepTy ty, List.map subst xs)
		  else rhs
	      | B.E_Wrap _ => rhs
	      | B.E_Unwrap _ => rhs
	      | B.E_Prim p => B.E_Prim(PrimUtil.map subst p)
	      | B.E_DCon _ => raise Fail "impossible"
	      | B.E_CCall _ => rhs
	      | B.E_HostVProc => rhs
	      | B.E_VPLoad _ => rhs
	      | B.E_VPStore _ => rhs
	    (* end case *)
	  end

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
	    List.map xformLambda fbs
	  end

    and xformLambda (s, fb) = hd(xformLambdas(s, [fb]))

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
		fun sel ([], _) = xformE(s, tys, e)
		  | sel (y::ys, i) = B.mkStmt([y], B.E_Select(i, argument), sel(ys, i+1))
		in
		  case (repOf dc, dflt)
		   of (B.Transparent, NONE) => (case ys
			 of [y] => B.mkStmt([y], B.E_Cast(typeOf y, argument), xformE(s, tys, e))
			  | _ => sel (ys, 0)
			(* end case *))
		    | (B.Tuple, NONE) => sel (ys, 0)
		    | (B.TaggedTuple tag, SOME dflt) => let
			val ty = BTy.T_Enum tag
			val tag' = BV.new("tag", ty)
			val tmp = BV.new("tmp", ty)
			val eq = BV.new("eq", BTy.boolTy)
			in
			  B.mkStmts([
			      ([tag'], B.E_Select(0, argument)),
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
	  in
	    case classify (rules, [], [], [])
	     of (enums, [], []) => B.mkCase(argument, rules, dflt)
	      | ([], lits, []) => raise Fail "unimplemented" (* build binary search tree *)
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
				val tmp = BV.new("t", ty)
				in
				  B.mkStmts([
				      ([tmp], B.E_Const(Lit.Enum w, ty)),
				      ([isBoxed], B.E_Prim(Prim.I32NEq(argument, tmp)))
				    ],
				    B.mkIf(isBoxed, case1, e))
				end
			      else B.mkStmt([isBoxed], B.E_Prim(Prim.isBoxed argument),
				B.mkIf(isBoxed, case1,
				  B.mkCase(argument,
				    List.map (fn (w, ty, e) => (B.P_Const(Lit.Enum w, ty), e)) enums,
				    NONE)))
			  end
		      | (false, true) =>
			  B.mkStmt([isBoxed], B.E_Prim(Prim.isBoxed argument),
			    B.mkIf(isBoxed,
			      consCase (cons, NONE),
			      B.mkCase(argument,
				List.map (fn (w, ty, e) => (B.P_Const(Lit.Enum w, ty), e)) enums,
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
				  List.map (fn (w, ty, e) => (B.P_Const(Lit.Enum w, ty), e)) enums,
				  SOME(B.mkThrow(join, []))))))
			  end
		    (* end case *)
		  end
	      | _ => raise Fail "strange case"
	    (* end case *)
	  end

    fun transform (B.MODULE{name, externs, body}) =
	  B.mkModule(name, externs, xformLambda (BV.Map.empty, body))

  end
