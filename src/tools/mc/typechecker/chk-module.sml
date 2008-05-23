(* chk-module.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type check a module.
 *)

structure ChkModule :> sig 

    val checkTopDecls : Error.err_stream -> (Error.span * ProgramParseTree.PML2.decl list * ModuleEnv.env * ModuleEnv.module_map) 
			   -> (ModuleEnv.env * ModuleEnv.module_map * AST.top_dec list)

  end = struct

    structure PPT = ProgramParseTree
    structure PT = PPT.PML2
    structure U = Unify
    structure TU = TypeUtil
    structure Ty = Types
    structure Env = ModuleEnv

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

    val idToString = ProgramParseTree.Var.toString

    val idToAtom = Atom.atom o PPT.Var.nameOf

  (* returns the variables bound in a pattern *)
    fun boundVarsOfPat (pat, bvs) = (case pat
           of PT.MarkPat {tree, ...} => boundVarsOfPat(tree, bvs)
	    | PT.BinaryPat (p1, id, p2) => id :: boundVarsOfPat(p1, boundVarsOfPat(p2, bvs))
	    | PT.ConPat (id, p) => id :: boundVarsOfPat(p, bvs)
	    | PT.TuplePat ps => List.foldl boundVarsOfPat bvs ps
	    | PT.ConstPat _ => bvs
	    | PT.WildPat => bvs
	    | PT.IdPat id => id :: bvs
	    | PT.ConstraintPat (p, _) => boundVarsOfPat(p, bvs)
          (* end case *))

  (* returns the variables bound to functions *)
    fun boundVarsOfFunct (PT.MarkFunct {tree, ...}, bvs) = boundVarsOfFunct(tree, bvs)
      | boundVarsOfFunct (PT.Funct(f, _, _), bvs) = f :: bvs

  (* returns the variables bound in a value declaration *)
    fun boundVarsOfValDcl (vd, bvs) = (case vd
           of PT.MarkVDecl {tree, ...} => boundVarsOfValDcl(tree, bvs)
	    | PT.ValVDecl (p, _) => boundVarsOfPat(p, bvs)
	    | PT.PValVDecl (p, _) => boundVarsOfPat(p, bvs)
	    | PT.FunVDecl fs => List.foldl boundVarsOfFunct bvs fs
           (* end case *))

  (* check type declarations *)
    fun chkTyDcl loc (ptTyDecl, env) = (case ptTyDecl
           of PT.MarkTyDecl {span, tree} => chkTyDcl loc (tree, env)
	    | PT.TypeTyDecl(tvs, id, ty) => let
		val (tvs', ty') = ChkTy.checkTy (!errStrm) (loc, tvs, ty)
		in
		  Env.insertTy(env, id, Env.TyDef(AST.TyScheme(tvs', ty')))
		end
	    | PT.AbsTyDecl (tvs, id) => let
                val tvs' = ChkTy.checkTyVars (!errStrm) (loc, tvs)
		val tyc = TyCon.newAbsTyc(idToAtom id, List.length tvs', false)
		val env' = Env.insertTy(env, id, Env.TyCon tyc)
                in
		  env'
                end
	    | PT.DataTyDecl(tvs, id, cons) => let
		val tvs' = ChkTy.checkTyVars (!errStrm) (loc, tvs)
		val tyc = TyCon.newDataTyc(idToAtom id, tvs')
	      (* update the type environment before checking the constructors so that
	       * recursive types work.
	       *)
		val env = Env.insertTy(env, id, Env.TyCon tyc)
		val newCon = DataCon.new tyc
		fun chkCons (_, ids, [], env, cons) = (env, List.rev cons)
		  | chkCons (loc', ids, con::rest, env, cons) = (case con
		       of PT.MarkConDecl{span, tree} =>
			    chkCons (span, ids, tree::rest, env, cons)
			| PT.ConDecl(conid, optTy) =>
			    if PPT.Var.Set.member(ids, conid)
			      then (
				error (loc', [
				    "duplicate constructor ", PPT.Var.nameOf conid,
				    " in datatype ", PPT.Var.nameOf id
				  ]);
				chkCons (loc, ids, rest, env, cons))
			      else let
				val optTy' = Option.map
                    (* FIXME: handle type variables properly *)
				      (fn ty => #2(ChkTy.checkTy (!errStrm) (loc, [], ty))) optTy
				val con' = newCon(idToAtom conid, optTy')
				in
				  chkCons (loc,
				    PPT.Var.Set.add(ids, conid), rest,
				    Env.insertVar (env, conid, Env.Con con'), con'::cons)
				end
		      (* end case *))
		val (env', cons') = chkCons (loc, PPT.Var.Set.empty, cons, env, [])
	       (* datatypes that have only nullary constructors are equality types *)
		val isEqTy = List.all (fn (Ty.DCon{argTy=NONE, ...}) => true | _ => false) cons'
		in
		  if isEqTy then TyCon.markEqTyc tyc else ();
		  env'
		end
          (* end case *))

    fun chkSpec loc (spec, env) = (case spec
        of PT.MarkSpec {tree, span} => chkSpec span (tree, env)
	 | PT.TypeSpec tyDecl => chkTyDcl loc (tyDecl, env)
	 | PT.ValSpec (x, tvs, ty) => let
           val (tvs', ty) = ChkTy.checkTy (!errStrm) (loc, tvs, ty)
	   val x' = Var.newPoly(PPT.Var.nameOf x, Ty.TyScheme(tvs', ty))
           in
	       Env.insertVar(env, x, Env.Var x')
           end
        (* end case *))

    fun chkSpecs loc (specs, env) = List.foldl (chkSpec loc) env specs

  (* check that a signature is well formed and return the resulting environment *)
    fun chkSignature loc (id, sign, env) = (case sign
            of PT.MarkSig {tree, span} => chkSignature span (id, tree, env)
	     | PT.ExpSig specs => let
	       val modRef = AST.MOD{name=Option.getOpt(id, Atom.atom "sign"), id=Stamp.new(), formals=NONE}
               val sigEnv = Env.fresh(modRef, SOME env)
               in
		  chkSpecs loc (specs, sigEnv)
               end
             | PT.NameSig (id, tyRevls) => (case Env.findSig(env, id)
               of NONE => (error (loc, ["cannot find signature ", PPT.Var.nameOf id]);
			   env)
		| SOME sigEnv => let
	           val modRef = AST.MOD{name=idToAtom id, id=Stamp.new(), formals=NONE}
		   val env' = Env.fresh(modRef, SOME env)
                   val env' as Env.ModEnv{tyEnv, ...} = List.foldl (chkTyDcl loc) env' tyRevls
                   in
		      MatchSig.reveal (sigEnv, tyEnv)
		   end
               (* end case *))
            (* end case *))

    fun pairEnvs (modEnv, residualEnv) = let
	fun f (id, xr, ps) = (case Env.VarMap.find(modEnv, id)
            of NONE => ps
	     | SOME xm => (xm, xr) :: ps
            (* end case *))
        in
	   Env.VarMap.foldli f [] residualEnv
        end

    fun getVars ((Env.Var x1, Env.Var x2), ps) = (x1, x2) :: ps
      | getVars (_, ps) = ps

  (* build a mapping from variable definitions in the module's var environment to
   * fresh variable definitions in the residual signature.
   *)
    fun buildVarSubst (modVarEnv, residualVarEnv) = let
	val valBindSubstPairs = pairEnvs (modVarEnv, residualVarEnv)
	val varBindSubstPairs = List.foldl getVars [] valBindSubstPairs
	val vMp = List.foldl SubstVar.add SubstVar.id varBindSubstPairs
        in 
	  fn v => (case SubstVar.find(vMp, v)
		    of NONE => v
		     | SOME v' => v')
        end

    fun chkModule loc (id, sign, module, (env, moduleEnv, astDecls)) = (case module
        of PT.MarkMod {span, tree} => chkModule span (id, sign, tree, (env, moduleEnv, astDecls))
	 | PT.DeclsMod decls => let
		val modRef = AST.MOD {name=idToAtom id, id=Stamp.new(), formals=NONE}
		val (modEnv, moduleEnv, modAstDecls) = chkTopDcls(loc, decls, Env.fresh(modRef, SOME env), moduleEnv)
		val (modEnv', modAstDecls') = (case sign
                    of SOME sign => let
                       val sigEnv = chkSignature loc (NONE, sign, env)
		       (* NOTE: env contains fresh tycons *)
		       val env = MatchSig.match{err=(!errStrm), loc=loc, modEnv=modEnv, sigEnv=sigEnv}
		       (* replace stale tycons with fresh ones *)
		       val modAstDecls' = 
			     SubstVar.topDecs (buildVarSubst (Env.varEnv modEnv, Env.varEnv env)) modAstDecls
                       in
			   (env, modAstDecls')
                       end
		     | NONE => (modEnv, modAstDecls)
                   (* end case *))
		val body = AST.M_Body(loc, modAstDecls')
                in
		  (Env.insertMod(env, id, modEnv'), 
		   Env.ModuleMap.insert(moduleEnv, modRef, (modEnv, modEnv', body)),
		   AST.TD_Module(loc, modRef, NONE, body) :: astDecls)
	        end
	 | PT.NamedMod modName => (case Env.findMod(env, modName)
               of NONE => (error(loc, ["cannot find module ", idToString modName]);
			   (env, moduleEnv, astDecls))
		| SOME modEnv => let
		      val modRefN = AST.MOD {name=idToAtom id, id=Stamp.new(), formals=NONE}
		      val signEnv = (case sign
                          of SOME sign => let
				 val sigEnv = chkSignature loc (NONE, sign, env)
			     in
				MatchSig.match{err=(!errStrm), loc=loc, modEnv=modEnv, sigEnv=sigEnv}
			     end
			   | NONE => modEnv
                         (* end case *))
		      val body = AST.M_Id(loc, modRefN)
		      in
	                 (Env.insertMod(env, id, signEnv),
			  Env.ModuleMap.insert(moduleEnv, modRefN, (modEnv, signEnv, body)),
			  AST.TD_Module(loc, modRefN, NONE, body) :: astDecls)
                      end 
                 (* end case *))
       (* end case *))

    and chkTopDcl loc (ptDecl, (env, moduleEnv, astDecls)) = (case ptDecl
           of PT.MarkDecl{span, tree} => chkTopDcl span (tree, (env, moduleEnv, astDecls))
	    | PT.TyDecl tyDecl => (chkTyDcl loc (tyDecl, env), moduleEnv, astDecls)
	    | PT.ExnDecl(id, optTy) => let
		val optTy' = Option.map (fn ty => #2(ChkTy.checkTy (!errStrm) (loc, [], ty))) optTy
		val exnCon = Exn.new (idToAtom id, optTy')
		in
		  (Env.insertVar (env, id, Env.Con exnCon), moduleEnv, astDecls)
		end
	    | PT.ValueDecl valDcl => let
		val bind = ChkExp.checkValDecl (!errStrm) (loc, valDcl)
		fun addToEnv (v, env) = 
		      Env.insertVar(env, v, Option.valOf(Env.getValBind v))
		val env = List.foldl addToEnv env (boundVarsOfValDcl (valDcl, []))
		in
		  (env, moduleEnv, AST.TD_Binding bind :: astDecls)
		end
	    | PT.ModuleDecl (id, sign, module) => chkModule loc (id, sign, module, (env, moduleEnv, astDecls))
	    | PT.LocalDecl (localDcls, dcls) => raise Fail "LocalDecl"
	    | PT.SignDecl (id, sign) => let
              val sigEnv = chkSignature loc (SOME (idToAtom id), sign, env)
              in
                 (Env.insertSig(env, id, sigEnv), moduleEnv, astDecls)
              end
	  (* end case *))

  (* refresh types for decls *)
    and freshenDecls decls = let
	  val s = {tyFn=CopySig.substTy, tySFn=CopySig.substTyScheme, dcFn=CopySig.substDCon}
          in
	     SubstTy.topDecs s decls
	  end

    and chkTopDcls (loc, ptDecls, env, moduleEnv) = let
        val (env', moduleEnv, astDecls) = List.foldl (chkTopDcl loc) (env, moduleEnv, []) ptDecls
	val astDecls = freshenDecls astDecls
        in
	   (env', moduleEnv, List.rev astDecls)
        end

    fun checkTopDecls err (loc, ptDecls, env, moduleEnv) = let
	    val _ = errStrm := err
	    in
	       chkTopDcls (loc, ptDecls, env, moduleEnv)
	    end

  end (* ChkModule *)

