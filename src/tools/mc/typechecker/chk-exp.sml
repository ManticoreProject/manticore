(* chk-exp.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type check expression and value declarations.
 *)

structure ChkExp :> sig 

  (* type check an expression *)
    val checkExp : (Error.span * ProgramParseTree.PML2.exp) 
		       -> (AST.exp * Types.ty)

  (* type check a value declaration *)
    val checkValDecl : (Error.span * ProgramParseTree.PML2.val_decl) 
		           -> AST.binding

  end = struct

    structure PPT = ProgramParseTree
    structure PT = PPT.PML2
    structure U = Unify
    structure TU = TypeUtil
    structure Ty = Types
    structure Env = ModuleEnv

    val error = ErrorStream.error

    fun unzip3 tups = let
      fun u ([], xs, ys, zs) = (xs, ys, zs)
	| u ((x,y,z)::t, xs, ys, zs) = u (t, x::xs, y::ys, z::zs)
      in
        u (List.rev tups, [], [], [])
      end

  (* a type expression for when there is an error *)
    val bogusTy = AST.ErrorTy

  (* an expression/type pair for when there is an error *)
    val bogusExp = (AST.TupleExp[], bogusTy)

  (* a pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

    val idToString = ProgramParseTree.Var.toString

  (* typecheck a literal *)
    fun chkLit (_, PT.IntLit i) = let
	  val ty = TypeClass.new Ty.Int
	  in
	    Overload.addLit (ty, Basis.IntClass);
	    (AST.LConst(Literal.Int i, ty), ty)
	  end
      | chkLit (_, PT.FltLit f) = let
	  val ty = TypeClass.new Ty.Float
	  in
	    Overload.addLit (ty, Basis.FloatClass);
	    (AST.LConst(Literal.Float f, ty), ty)
	  end
      | chkLit (_, PT.StrLit s) = (AST.LConst(Literal.String s, Basis.stringTy), Basis.stringTy)

    fun chkFunBinds (loc, depth, fbs) = let
	  val depth' = depth+1
	(* create variable bindings for the functions *)
          fun bindClause ((f, _, _, _), (fs, names)) = 
                if PPT.Var.Set.member(names, f)
                then (fs, names)
                else let
                  val f' = Var.new(PPT.Var.nameOf f,
                          AST.FunTy(
                             AST.MetaTy(MetaVar.new depth'),
                             AST.MetaTy(MetaVar.new depth')))
                  in
                     ((f, f')::fs, PPT.Var.Set.add(names, f))
                  end
	  fun bindFun (fb, (fs, names)) = (case fb
		 of PT.MarkFunct{span, tree} => bindFun (tree, (fs, names))
                  | PT.Funct clauses => let
                      val (cfs, cnames) = List.foldr bindClause ([], PPT.Var.Set.empty) clauses
                      val dnames = PPT.Var.Set.intersection (names, cnames)
                      in
                        if PPT.Var.Set.numItems cnames > 1
                          then error(loc, [
                                     "different names ",
                                     String.concatWith "," (List.map PPT.Var.nameOf (PPT.Var.Set.listItems cnames)),
                                     " in function binding"
                                     ])
                          else ();
                        if not (PPT.Var.Set.isEmpty dnames)
                          then error(loc, [
                                     "duplicate name(s) ",
                                     String.concatWith "," (List.map PPT.Var.nameOf (PPT.Var.Set.listItems dnames)),
                                     " in function binding"
                                     ])
                          else ();
                        (cfs @ fs, PPT.Var.Set.union (cnames, names))
		      end
		(* end case *))
	  val (fs, _) = List.foldr bindFun ([], PPT.Var.Set.empty) fbs
	(* insert the function variables into an environment for checking
	 * the function bodies.
	 *)
	  val _ = List.app (fn (f, f') => Env.bindVal(f, Env.Var f')) fs
	(* typecheck the functions *)
          fun chkClause (nargs', argTys', bodyTy', funTy') (f, pats, bodyTyAscrip, body) = let
                val SOME(Env.Var f') = Env.getValBind f
                val AST.TyScheme(_, funTy) = Var.typeOf f'
                val npats = List.length pats
                val pats = if npats < nargs'
                               then (
                                 error(loc, [
                                       "function defined with different numbers of arguments"
                                       ]);
                                 pats @ (List.tabulate (nargs' - npats, fn _ => PT.WildPat)))
                               else pats
                val (pats, argTys) = chkPats' (loc, depth', pats)
                val (body, bodyTy) = chkExp (loc, depth', body)
                in
                  if not(U.unify(funTy, funTy'))
                    then error(loc, ["type mismatch in function ", PPT.Var.nameOf f])
                    else ();
                  ListPair.app (fn (argTy, argTy') =>
                                  if not(U.unify(argTy, argTy'))
                                    then error(loc, [
                                               "function defined with argument of different types"
                                          ])
                                     else ())
                               (argTys, argTys');
                  case bodyTyAscrip
                   of NONE => ()
                    | SOME bodyTyAscrip => let
                        val bodyTyAscrip = #2(ChkTy.checkTy (loc, [], bodyTyAscrip))
                        in
                          if not(U.unify(bodyTy, bodyTyAscrip))
                            then error(loc, [
                                       "body type does not match the ascribed type of function ",
                                       PPT.Var.nameOf f
                                       ])
                            else ()
                         end
                  (* end case *);
                  if not(U.unify(bodyTy, bodyTy'))
                    then error(loc, [
                               "function defined with result of different types"
                               ])
                    else ();
                  (pats, body)
                end
	  fun chkFun loc (fb, fbs) = (case fb
		 of PT.MarkFunct{span, tree} => chkFun span (tree, fbs)
                  | PT.Funct clauses => let
		      val SOME(Env.Var f') = Env.getValBind (#1 (hd clauses))
                      val nargs = List.foldl (fn ((_, pats, _, _), nargs) => Int.max (List.length pats, nargs)) 0 clauses
                      val argTys = List.tabulate (nargs, fn _ => AST.MetaTy(MetaVar.new depth'))
                      val bodyTy = AST.MetaTy(MetaVar.new depth')
                      val funTy = List.foldr AST.FunTy bodyTy argTys
                      val clauses = List.map (chkClause (nargs,argTys,bodyTy,funTy)) clauses
                      fun makeExp ([],vars) = let
                            val vars = List.rev vars
                            in
                              (AST.CaseExp
                               (ASTUtil.mkTupleExp (List.map (fn var => ASTUtil.mkVarExp (var, [])) vars),
                                List.map (fn (pats, body) => AST.PatMatch (ASTUtil.mkTuplePat pats, body)) clauses,
                                bodyTy),
                               bodyTy)
                            end
                        | makeExp (argTy::argTys, vars) = let
                            val var = Var.new ("param", argTy)
                            val (body,bodyTy) = makeExp (argTys, var::vars)
                            in
                              (AST.FunExp (var, body, bodyTy),
                               AST.FunTy (argTy, bodyTy))
                            end
                      fun makeLambda [] = raise Fail "compiler bug"
                        | makeLambda (argTy::argTys) = let
                            val var = Var.new ("param", argTy)
                            val (body,bodyTy) = makeExp (argTys, [var])
                            in
                              AST.FB (f', var, body)
                            end
                      val fb = makeLambda argTys
                      in
                        fb :: fbs
                      end
		(* end case *))
	  val fbs' = List.foldr (chkFun loc) [] fbs
	(* close over the types of the functions and build an environment
	 * for checking the scope of the declaration.
	 *)
	  fun close (f, f') = (
		Var.closeTypeOf (depth, f');
		Env.bindVal(f, Env.Var f'))
	  val _ = List.app close fs
	  in
	    fbs'
	  end

  (* close the types of all variables occuring in a pattern.
   * QUESTION: because match wants mono-types for pattern variables, we must close them to mono
   * types here. is this necessary?
   *)
    and generalizePat (depth, pat) = (case pat
	   of AST.VarPat v => Var.closeTypeOf(depth, v)
	    | AST.ConPat (_, _, pat) => generalizePat(depth, pat)
	    | AST.TuplePat pats => List.app (fn pat => generalizePat(depth, pat)) pats
	    | _ => ()
	  (* end case *))

  (* typecheck value declarations as described in Section 6.6 *)
    and chkValDcl (loc, depth, decl) = (case decl
	   of PT.MarkVDecl{span, tree} => chkValDcl (span, depth, tree)
	    | PT.ValVDecl(pat, e) => let
		val depth' = depth+1
		val (pat', lhsTy) = chkPat(loc, depth', pat)
		val lhsTy = TU.openTy(depth', lhsTy)
		val (e', rhsTy) = chkExp (loc, depth', e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, [
	              	"type mismatch in val binding:\n\
                        \  lhs: ", TypeUtil.toString lhsTy, "\n\
                        \  rhs: ", TypeUtil.toString rhsTy, ".\n"
		      ])
		    else ();
		  generalizePat (depth, pat');
		  AST.ValBind(pat', e')
		end
	    | PT.PValVDecl(pat, e) => let
		val depth' = depth+1
		val (pat', lhsTy) = chkPat(loc, depth', pat)
		val (e', rhsTy) = chkExp (loc, depth', e)
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then error (loc, ["type mismatch in pval binding"])
		    else ();
		  generalizePat (depth, pat');
		  AST.PValBind(pat', e')
		end
	    | PT.FunVDecl fbs => AST.FunBind (chkFunBinds (loc, depth, fbs))
	    | PT.PrimVDecl (pat, primRhs) => let
		fun hasTypeAscription (PT.MarkPat{tree, ...}) = hasTypeAscription tree
		  | hasTypeAscription (PT.ConstraintPat _) = true
		  | hasTypeAscription _ = false
		val _ = if hasTypeAscription pat
			   then ()
			else error(loc, ["need type ascription to import inline BOM"])
		val depth' = depth+1
		val (AST.VarPat v, lhsTy) = chkPat(loc, depth', pat)
		in
		  Var.closeTypeOf(depth, v);
		  AST.PrimVBind (v, primRhs)
	        end
	  (* end case *))

  (* typecheck expressions as described in Section 6.8 *)
    and chkExp (loc, depth, exp) : AST.exp * AST.ty = (case exp
	   of PT.MarkExp{span, tree} => chkExp (span, depth, tree)
	    | PT.LetExp(valDcls, exp) => let
		  fun chkDcls [] = chkExp (loc, depth, exp)
		    | chkDcls (vd::vds) = let
			  val bind = chkValDcl (loc, depth, vd)
			  val (e', ty) = chkDcls vds
		      in
		          (AST.LetExp(bind, e'), ty)
		      end
	      in
		  chkDcls valDcls
	      end
	    | PT.IfExp(e1, e2, e3) => let
		val (e1', ty1) = chkExp (loc, depth, e1)
		val (e2', ty2) = chkExp (loc, depth, e2)
		val (e3', ty3) = chkExp (loc, depth, e3)
		in
		  if not(U.unify(ty1, Basis.boolTy))
		    then error(loc, ["type of conditional not bool"])
		    else ();
		  if not(U.unify(ty2, ty3))
		    then (
		      error(loc, ["types of then and else clauses do not match.\n\
                                  \  then branch: " ^ TypeUtil.toString ty2 ^ "\n\
                                  \  else branch: " ^ TypeUtil.toString ty3]);
		      bogusExp)
		    else (AST.IfExp(e1', e2', e3', ty2), ty2)
		end
	    | PT.CaseExp(e, cases) => let
		val (e', argTy) = chkExp (loc, depth, e)
		val resTy = AST.MetaTy(MetaVar.new depth)
		val matches = List.map
		      (fn m => chkMatch(loc, depth, argTy, resTy, m))
			cases
		in
		  (AST.CaseExp(e', matches, resTy), resTy)
		end
	    | PT.PCaseExp(es, pms) => let
                val (es', tys) = let
                  fun chk e = chkExp (loc, depth, e)
                  in
                    ListPair.unzip (List.map chk es)
		  end
		val resTy = AST.MetaTy(MetaVar.new depth)
		val pms' = let
                  fun chk m = chkPMatch(loc, depth, tys, resTy, m)
                  in
                    List.map chk pms
                  end
                in
                  (AST.PCaseExp(es', pms', resTy), resTy)
                end
	    | PT.HandleExp(e, cases) => let
		val (e', resTy) = chkExp (loc, depth, e)
		val matches = List.map
		      (fn m => chkMatch(loc, depth, Basis.exnTy, resTy, m))
			cases
		in
		  (AST.HandleExp(e', matches, resTy), resTy)
		end
	    | PT.RaiseExp e => let
		val (e', ty) = chkExp(loc, depth, e)
		val resTy = AST.MetaTy(MetaVar.new depth)
		in
		  if not(U.unify(ty, Basis.exnTy))
		    then error(loc, ["argument of raise must be an exception"])
		    else ();
		  (AST.RaiseExp(e', resTy), resTy)
		end
	    | PT.PChoiceExp es => let
		fun chk (e, (es, ty)) = let
		      val (e', ty') = chkExp(loc, depth, e)
		      in
			if not(U.unify (ty, ty'))
			  then error(loc, ["type mismatch in parallel choice"])
			  else ();
			(e'::es, ty')
		      end
		val (es', ty) = List.foldr chk ([], Ty.MetaTy (MetaVar.new depth)) es
		in
		  (AST.PChoiceExp(es', ty), ty)
		end
	    | PT.OrElseExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, e1)
		  val (e2', ty2) = chkExp (loc, depth, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of orelse must have type bool"])
		  else ();
		  (AST.IfExp(e1', AST.ConstExp(AST.DConst(Basis.boolTrue, [])), e2', Basis.boolTy), Basis.boolTy)
	      end
	    | PT.AndAlsoExp(e1, e2) => let
		  val (e1', ty1) = chkExp (loc, depth, e1)
		  val (e2', ty2) = chkExp (loc, depth, e2)
	      in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		  then error(loc, ["arguments of andalso must have type bool"])
		  else ();
		  (AST.IfExp(e1', e2', AST.ConstExp(AST.DConst(Basis.boolFalse, [])), Basis.boolTy), Basis.boolTy)
	      end
	    | PT.BinaryExp(e1, bop, e2) => let
		val (e1', ty1) = chkExp (loc, depth, e1)
		val (e2', ty2) = chkExp (loc, depth, e2)
		fun mkApp (arg, resTy) = (AST.ApplyExp(arg, AST.TupleExp[e1', e2'], resTy), resTy)
		fun chkApp tyScheme = let
		      val (argTys, instTy as AST.FunTy(argTy, resTy)) = TU.instantiate (depth, tyScheme)
		      in
			if not(U.unify(argTy, AST.TupleTy[ty1, ty2]))
			  then error(loc, [
			      "type mismatch for operator ", PPT.Var.nameOf bop, "\n",
			      "operator expects ", TypeUtil.toString argTy, "\n",
			      "argument has type ", TypeUtil.toString(AST.TupleTy[ty1, ty2])
			    ])
			  else ();
			(argTys, resTy, instTy)
		      end
		in
		  case Env.getValBind bop
		   of SOME(Env.Con dc) => let
			val (argTys, resTy, _) = chkApp (DataCon.typeOf dc)
			in
			  mkApp (AST.ConstExp(AST.DConst(dc, argTys)), resTy)
			end
		    | SOME(Env.Var x) => let
			val (argTys, resTy, _) = chkApp (Var.typeOf x)
			in
			  mkApp (AST.VarExp(x, argTys), resTy)
			end
		    | SOME(Env.Overload(tysch, vars)) => let
			val (argTys, resTy, instTy) = chkApp tysch
			val ovar = ref (AST.Unknown(instTy, vars))
			in
			  Overload.addVar ovar;
			  mkApp (AST.OverloadExp ovar, resTy)
			end
		    | SOME(Env.EqOp eqOp) => let
			val ([ty], resTy, _) = chkApp (Var.typeOf eqOp)
			in
			  Overload.addEqTy ty;
			  mkApp (AST.VarExp(eqOp, [ty]), resTy)
			end
		    | NONE => raise Fail "unknown operator"
		  (* end case *)
		end
	    | PT.ApplyExp(e1, e2) => let
		val (e1', ty1) = chkExp (loc, depth, e1)
		val (e2', ty2) = chkExp (loc, depth, e2)
		val resTy = AST.MetaTy(MetaVar.new depth)
		in
		  if (not(U.unify(ty1, AST.FunTy(ty2, resTy))) handle Fail _ => true)
		    then error(loc, ["type mismatch in application\n",
				     "* expected ", TypeUtil.toString ty1, "\n",
				     "* found    ", TypeUtil.toString (Ty.FunTy(ty2, resTy))])
		    else ();
		  (AST.ApplyExp(e1', e2', resTy), resTy)
		end
	    | PT.ConstExp const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstExp const', ty)
		end
	    | PT.TupleExp es => let
		fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, e)
		      in
			(e'::es, ty::tys)
		      end
		val (es', tys) = List.foldr chk ([], []) es
		in
		  (AST.TupleExp es', TU.tupleTy tys)
		end
	    | PT.ListExp es => let
		val elemTy = AST.MetaTy(MetaVar.new depth)
		val listTy = Ty.ConTy([elemTy], Basis.listTyc)
		fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, e)
		      in
			if not(U.unify(elemTy, ty))
			  then error(loc, [
			      "type mismatch in list expression\n",
			      "* expected ", TypeUtil.toString elemTy, "\n",
			      "* found    ", TypeUtil.toString ty])
			  else ();
			(e'::es, ty::tys)
		      end
		val (es', tys) = List.foldr chk ([], []) es
		fun cons (e1, e2) = AST.ApplyExp(
			AST.ConstExp(AST.DConst(Basis.listCons, [elemTy])),
			AST.TupleExp[e1, e2],
			listTy)
		val exp = List.foldr cons
		      (AST.ConstExp(AST.DConst(Basis.listNil, [elemTy])))
			es'
		in
		  (exp, listTy)
		end
	    | PT.RangeExp (e1, e2, eo) => let
		  val (e1', ty1) = chkExp (loc, depth, e1)
		  val (e2', ty2) = chkExp (loc, depth, e2)
		  val _ = if not(U.unify (ty1, ty2))
			  then error (loc, ["type mismatch in range"])
			  else ()
		  val eo' = (case eo of
				 (SOME exp) => let
				     val (exp', ty) = chkExp (loc, depth, exp)
				 in
				     if not(U.unify (ty, ty1))
				     then error (loc, ["type mismatch in range"])
				     else ();
				     SOME exp'
				 end
			       | NONE => NONE
			    (* end case *))
	      in
		  if not(U.unify (ty1, TypeClass.new Types.Num))
		  then error (loc, ["range elements must have numeric type"])
		  else ();
		  (AST.RangeExp(e1', e2', eo', ty1), Basis.parrayTy ty1)
	      end
	    | PT.PTupleExp es => let
		  fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, e)
		  in
		      (e'::es, ty::tys)
		  end
		  val (es', tys) = List.foldr chk ([], []) es
	      in
		  (AST.PTupleExp es', TU.tupleTy tys)
	      end
	    | PT.PArrayExp es => let
		fun chk (e, (es, ty)) =
		    let
			val (e', ty') = chkExp(loc, depth, e)
		    in
			if not(U.unify (ty, ty'))
			then error(loc, ["type mismatch in parray"])
			else ();
			(e'::es, ty')
		    end
		val (es', ty) = List.foldr chk ([], Ty.MetaTy (MetaVar.new depth)) es
	      in
		  (AST.PArrayExp(es', ty), Basis.parrayTy ty)
	      end
	    | PT.PCompExp (e, pbs, eo) => let
		val pes = chkPBinds (loc, depth, pbs)
		val (e', resTy) = chkExp (loc, depth, e)
		val eo' = (case eo
			 of (SOME exp) => let
			      val (exp', ty) = chkExp (loc, depth, exp)
			      in
				if not(U.unify (ty, Basis.boolTy))
				  then error (loc, ["type mismatch in parray comprehension 'where' clause"])
				  else ();
				SOME exp'
			      end
			  | NONE => NONE
			(* end case *))
		in
		  (AST.PCompExp (e', pes, eo'), Basis.parrayTy resTy)
		end
	    | PT.SpawnExp e => let
		val (e', ty) = chkExp (loc, depth, e)
		in
		  if not(U.unify (ty, Basis.unitTy))
		    then error(loc, ["type mismatch in spawn"])
		    else ();
		  (AST.SpawnExp e', Basis.threadIdTy)
		end
	    | PT.SeqExp es => let
		fun chk [e] = chkExp(loc, depth, e)
		  | chk (e::r) = let
		      val (e', _) = chkExp (loc, depth, e)
		      val (e'', ty) = chk r
		      in
			(AST.SeqExp(e', e''), ty)
		      end
		  | chk _ = raise Fail "empty sequence"
		in
		  chk es
		end
	    | PT.IdExp x => (case Env.getValBind x
		 of SOME(Env.Con dc) => let
		      val (argTys, ty) = TU.instantiate (depth, DataCon.typeOf dc)
		      in
			(AST.ConstExp(AST.DConst(dc, argTys)), ty)
		      end
		  | SOME(Env.Var x') => let
		      val (argTys, ty) = TU.instantiate (depth, Var.typeOf x')
		      in
			(AST.VarExp(x', argTys), ty)
		      end
		  | SOME(Env.Overload(tysch, vars)) => let
		      val (_, instTy) = TU.instantiate (depth, tysch)
		      val ovar = ref (AST.Unknown (instTy, vars))
		      in
			Overload.addVar ovar;
			(AST.OverloadExp ovar, instTy)
		      end
		  | SOME(Env.EqOp _) => raise Fail "compiler bug"
		  | NONE => (
		      error(loc, ["undefined identifier \"", idToString x, "\""]);
		      bogusExp)
		(* end case *))
	    | PT.ConstraintExp(e, ty) => let
		val (_, constraintTy) = ChkTy.checkTy (loc, [], ty)
		val constraintTy = TU.openTy(depth, constraintTy) 
		val (e', ty') = chkExp (loc, depth, e)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (e', ty')
		end
	    | PT.FnExp clauses => let
		val v = PPT.Var.new("anon", ())
		val [l as AST.FB (f, _, _)] = chkFunBinds(loc, depth, [PT.Funct (List.map (fn (pat, e) => (v, [pat], NONE, e)) clauses)])
		val (argTys, ty) = TU.instantiate (depth, Var.typeOf f)
		in
		  (AST.LetExp(AST.FunBind [l], AST.VarExp(f, argTys)), ty)
	        end
	  (* end case *))

    and chkMatch (loc, depth, argTy, resTy, match) = (case match
	   of PT.MarkMatch{span, tree} => chkMatch(span, depth, argTy, resTy, tree)
	    | PT.Match(pat, exp) => let
		val (pat', argTy') = chkPat(loc, depth, pat)
		val (exp', resTy') = chkExp(loc, depth, exp)
		in
		  if not(U.unify(argTy, argTy'))
		    then error(loc, ["type mismatch in case pattern"])
		    else ();
		  if not(U.unify(resTy, resTy'))
		    then error(loc, ["type mismatch in case"])
		    else ();
		  AST.PatMatch(pat', exp')
		end
	  (* end case *))

    and chkPMatch (loc, depth, argTys, resTy, pmatch) = (case pmatch
          of PT.MarkPMatch{span, tree} => chkPMatch(span, depth, argTys, resTy, tree)
	   | PT.PMatch (ps, e) => let
	       fun chkPPats ([], ps', argTys) = (List.rev ps', List.rev argTys)
		 | chkPPats (p::ps, ps', argTys) = let
                     val (p', t') = chkPPat(loc, depth, p)
                     in
                       chkPPats(ps, p'::ps', t'::argTys)
		     end
               val (ps', argTys') = chkPPats(ps, [], [])
               val (e', resTy') = chkExp(loc, depth, e)
               fun u (argTy, argTy') = if not(U.unify(argTy, argTy'))
                                         then error(loc, ["type mismatch in pcase pattern"])
                                         else ();
               in
		 ListPair.app u (argTys, argTys');
                 if not(U.unify(resTy, resTy'))
                   then error(loc, ["type mismatch in pcase"])
                   else ();
                 AST.PMatch (ps', e')
               end
	   | PT.Otherwise e => let
               val (e', resTy') = chkExp(loc, depth, e)
               in
                 if not(U.unify(resTy, resTy'))
                   then error(loc, ["type mismatch in pcase"])
                   else ();
                 AST.Otherwise e'
               end
         (* end case *))

    and chkPBinds (loc, depth, pbs) : (AST.pat * AST.exp) list = 
      List.map (fn pb => chkPBind (loc, depth, pb)) pbs

(* AMS: not sure what the following commented-out code is about. *)
(* chkPBind/s don't return environments, so I've replaced this with a simple map. *)

(* raise Fail "FIXME: union looks wrong" *)
(* let
		val (pe, env1 as Env.ModEnv{varEnv=ve1, ...}) = chkPBind (loc, depth, pb)		
                val (pes, env2  as Env.ModEnv{varEnv=ve2, ...}) = chkPBinds (loc, depth, pbs)
(* FIXME: the following code doesn't work when "pb" contains a shadowing definition *)
		val newve1 = List.filter (fn x => not (Env.inDomainVar (x))) (AtomMap.listKeys ve1)
		in
		  if List.exists (fn x => Env.inDomainVar(env2, x)) newve1
		    then error (loc, ["conflicting pattern bindings in parray comprehension"])
		    else ();
		  (pe::pes, Env.union(env1, env2))
*)		

    and chkPBind (loc, depth, pb) : AST.pat * AST.exp = 
         (case pb
	    of PT.MarkPBind {span, tree} => chkPBind (span, depth, tree)
	     | PT.PBind (pat, exp) => let
                 val (exp', resTy) = chkExp(loc, depth, exp)
		 val (pat', resTy') = chkPat (loc, depth, pat)
		 in
 		   if not (U.unify (resTy, Basis.parrayTy resTy'))
		     then error (loc, ["type mismatch in pattern binding"])
		     else ();
		   (pat', exp')
		 end
	(* end case *))

    and chkPPat (loc, depth, p) : (AST.ppat * AST.ty) = (case p
          of PT.MarkPPat{span, tree} => chkPPat(span, depth, tree)
	   | PT.NDWildPat => let
               val ty = AST.MetaTy(MetaVar.new depth)
               in
                 (AST.NDWildPat ty, ty)
	       end
	   | PT.HandlePat p => raise Fail "todo: chkPPat HandlePat" (* FIXME *)
	   | PT.Pat p => let
               val (p', ty') = chkPat (loc, depth, p)
               in
                 (AST.Pat p', ty')
               end            
        (* end case *))
 
    and chkPat (loc, depth, pat) = (case pat
	   of PT.MarkPat{span, tree} => chkPat(span, depth, tree)
	    | PT.ConPat(conid, pat) => let
		val (pat, ty) = chkPat (loc, depth, pat)
		in
		  case Env.getValBind conid
		   of SOME(Env.Con dc) => (case TU.instantiate (depth, DataCon.typeOf dc)
			 of (tyArgs, AST.FunTy(argTy, resTy)) => (
			      if not(U.unify(argTy, ty))
				then error(loc, ["type mismatch in constructor pattern"])
				else ();
			      (AST.ConPat(dc, tyArgs, pat), resTy))
			  | _ => (
			      error(loc, [
				  "application of nullary constructor ",
				  idToString conid
				]);
			      (bogusPat, bogusTy))
			(* end case *))
		    | _ => (
			error(loc, ["unbound data constructor ", idToString conid]);
			(bogusPat, bogusTy))
		  (* end case *)
		end
	    | PT.BinaryPat(p1, conid, p2) => chkPat (loc, depth, PT.ConPat (conid, PT.TuplePat [p1, p2]))
	    | PT.TuplePat pats => let
		val (pats, ty) = chkPats (loc, depth, pats)
		in
		  (AST.TuplePat pats, ty)
		end
	    | PT.ConstPat const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstPat const', ty)
		end
	    | PT.WildPat => let
		val ty = AST.MetaTy(MetaVar.new depth)
		in
		  (AST.WildPat ty, ty)
		end
	    | PT.IdPat x => (case Env.getValBind x
		 of SOME(Env.Con dc) => (case DataCon.argTypeOf dc
		       of NONE => let
			    val (tyArgs, ty) = TU.instantiate (depth, DataCon.typeOf dc)
			    in
			      (AST.ConstPat(AST.DConst(dc, tyArgs)), ty)
			    end
			| _ => (
			    error(loc, [
				"data constructor ", PPT.Var.nameOf x, " in variable pattern"
			      ]);
			    (bogusPat, bogusTy))
		      (* end case *))
		  | _ => let
		      val ty = AST.MetaTy(MetaVar.new depth)
		      val x' = Var.new(PPT.Var.nameOf x, ty)
		      in
			Env.bindVal(x, Env.Var x');
			(AST.VarPat x', ty)
		      end
		(* end case *))
	    | PT.ConstraintPat(p, ty) => let
		val (_, constraintTy) = ChkTy.checkTy (loc, [], ty)
		val constraintTy = TU.openTy(depth, constraintTy)
		val (p', ty') = chkPat (loc, depth, p)
		in
		   if not(U.unify(ty', constraintTy))
		     then error(loc, ["type mismatch in constraint pattern"])
		     else ();
		  (p', ty')
		end
	  (* end case *))

    and chkPats' (loc, depth, pats : PT.pat list) = let
          fun chk (pat, (ps, tys)) = let
		val (pat', ty) = chkPat (loc, depth, pat)
		in
		  (pat'::ps, ty::tys)
		end
	  val (pats', tys) = List.foldr chk ([], []) pats
          in
             (pats', tys)
          end

    and chkPats (loc, depth, pats : PT.pat list) = let
	  val (pats', tys) = chkPats' (loc, depth, pats)
	  in
	    case tys
	     of [ty] => (pats', ty)
	      | _ => (pats', TU.tupleTy tys)
	    (* end case *)
	  end

    fun checkExp (loc, exp) = chkExp(loc, 0, exp)

    fun checkValDecl (loc, valDecl) = chkValDcl(loc, 0, valDecl)

  end
