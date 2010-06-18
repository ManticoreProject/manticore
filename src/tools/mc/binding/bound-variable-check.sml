(* bound-variable-check.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check the program for unbound variables.
 *)

structure BoundVariableCheck :> sig

  (* check for unbound variables *)
    val check : (Error.err_stream * ProgramParseTree.PML1.program * BindingEnv.env) 
		    -> (ProgramParseTree.PML2.program * BindingEnv.env)

  end = struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure BOMPT2 = ProgramParseTree.PML2.BOMParseTree
    structure Var = ProgramParseTree.Var
    structure BEnv = BindingEnv

    val atos = Atom.toString
    fun qidToString path = QualifiedId.toString (Atom.toString, path)

    val error = ErrorStream.error

  (* attempt to find the binding site of a qualified identifier, reporting an error if none exists *)
    fun findQid (find, kind, dummy) (loc, env, qId) = (case find(env, qId)
           of NONE => (
		error(loc, ["unbound ", kind, " ", qidToString qId]);
		dummy)
	    | SOME x => x
	  (* end case *))

    val dummyVar = Var.new("dummyVar", ())
    val dummyTy = Var.new("dummyTy", ())
    val dummyMod = Var.new("dummyMod", ())

    val findTyQid = findQid (QualifiedId.findTy, "type", BEnv.TypeExp dummyTy)
    fun findDataTyQid (loc, env, qId) = (case findTyQid (loc, env, qId)
            of BEnv.DataTyc id => SOME id
	     | _ => NONE
            (* end case *))
    val findTyQid = BEnv.tyId o findTyQid

    val findValQid = findQid (QualifiedId.findVal, "variable", BEnv.Var dummyVar)
    val findModQid = findQid (QualifiedId.findMod, "module", dummyMod)
    val findModEnv = findQid (QualifiedId.findModEnv, "module", BEnv.empty (Atom.atom ""))

    fun freshVar v = Var.new(Atom.toString v, ())

    fun chkList loc (chkX, xs, env) = let
	  fun f (x, (xs, env)) = let
		 val (x, env) = chkX loc (x, env)
		 in
		   (x :: xs, env)
		 end
	  val (xs', env) = List.foldl f ([], env) xs
	  in
	    (List.rev xs', env)
	  end

    fun chkConst (PT1.IntLit x) = PT2.IntLit x
      | chkConst (PT1.FltLit x) = PT2.FltLit x
      | chkConst (PT1.StrLit x) = PT2.StrLit x

    fun chkTy loc (ty, env) = (case ty
           of PT1.MarkTy {span, tree} => let
		val tree = chkTy span (tree, env)
		in
		  PT2.MarkTy {span=span, tree=tree}
		end
	    | PT1.NamedTy (tys, id) => let
		val tys = chkTys loc (tys, env)
		in
		  PT2.NamedTy(tys, findTyQid(loc, env, id))
		end
	    | PT1.VarTy tv => PT2.VarTy tv
	    | PT1.TupleTy tys => let
		val tys = chkTys loc (tys, env)
		in
		  PT2.TupleTy tys
		end
	    | PT1.FunTy (ty1, ty2) => let
		val ty1 = chkTy loc (ty1, env)
		val ty2 = chkTy loc (ty2, env)
		in
		  PT2.FunTy (ty1, ty2)
		end
           (* end case *))

    and chkTys loc (tys, env) = List.map (fn ty => chkTy loc (ty, env)) tys

    fun chkPat loc (pat, env) = let
	  fun ins (v, patternBoundVars) = 
	        if AtomSet.member(patternBoundVars, v)
		   then (
		    error(loc, ["duplicate pattern-bound variable ", Atom.toString v]);
		    patternBoundVars)
		else AtomSet.add(patternBoundVars, v)
	(* check for duplicate bindings in patterns *)
	  val _ = List.foldl ins AtomSet.empty (Binding.varsOfPat(pat, env))
          in
	    chkPatWithoutDuplicateCheck loc (pat, env)
	  end

    and chkPatWithoutDuplicateCheck loc (pat, env) = (case pat
           of PT1.MarkPat {span, tree} => let
		  val (tree, env) = chkPatWithoutDuplicateCheck span (tree, env)
	          in
		     (PT2.MarkPat{span=span, tree=tree}, env)
		  end
	    | PT1.BinaryPat (pat1, cid, pat2) => let
		  val (pat1, env) = chkPatWithoutDuplicateCheck loc (pat1, env)
		  val (pat2, env) = chkPatWithoutDuplicateCheck loc (pat2, env)
	          in
		     case findValQid(loc, env, cid)
		      of BEnv.Con cid' => (PT2.BinaryPat(pat1, cid', pat2), env)
		       | _ => (PT2.BinaryPat(pat1, dummyVar, pat2), env)
		  end
	    | PT1.ConPat (cid, pat) => let
		  val (pat, env) = chkPatWithoutDuplicateCheck loc (pat, env)
	          in
		     case findValQid(loc, env, cid)
		      of BEnv.Con cid' => (PT2.ConPat(cid', pat), env)
		       | _ => (
			 error(loc, ["invalid constructor ", qidToString cid, " in pattern"]);
			 (PT2.ConPat(dummyVar, pat), env))
		  end
	    | PT1.TuplePat pats => let
		  val (pats, env) = chkPatsWithoutDuplicateCheck loc (pats, env)
	          in
		     (PT2.TuplePat pats, env)
		  end
	    | PT1.ConstPat const => 
	          (PT2.ConstPat (chkConst const), env)
	    | PT1.WildPat => (PT2.WildPat, env)
	    | PT1.IdPat vb => (
	      case QualifiedId.findVal(env, vb)
               of SOME (BEnv.Con c) => 
		  (* this pattern matches a nullary constructor *)
		  (PT2.IdPat c, env)
		| _ => (
		  case QualifiedId.unqualId vb
		   of NONE => (
		        error(loc, ["unbound pattern variable ", qidToString vb]);
			(PT2.IdPat dummyVar, env))
		    | SOME vb => let
			  (* this pattern binds a local variable *)
			  val vb' = freshVar vb
			  val env = BEnv.insertVal(env, vb, BEnv.Var vb')
		          in
			      (PT2.IdPat vb', env)
		          end
		  (* end case *))
              (* end case *))
	    | PT1.ConstraintPat (pat, ty) => let
		  val (pat, env) = chkPatWithoutDuplicateCheck loc (pat, env)
		  val ty = chkTy loc (ty, env)
	          in
		     (PT2.ConstraintPat(pat, ty), env)
		  end
           (* end case *))

    and chkPatsWithoutDuplicateCheck loc (pats, env) = chkList loc (chkPatWithoutDuplicateCheck, pats, env)

    and chkPats loc (pats, env) = chkList loc (chkPat, pats, env)

    fun chkPPat loc (pat, env) = (case pat
           of PT1.MarkPPat {span, tree} => let
		  val (tree, env) = chkPPat span (tree, env)
	          in
		     (PT2.MarkPPat{span=span, tree=tree}, env)
		  end
	    | PT1.NDWildPat => (PT2.NDWildPat, env)
	    | PT1.Pat p => let
                val (p, env) = chkPat loc (p, env)
	        in
		  (PT2.Pat p, env)
	        end			     
	    | PT1.HandlePat p => let
                val (p, env) = chkPat loc (p, env)
                in
                  (PT2.HandlePat p, env)
	        end
           (* end case *))

    and chkPPats loc (ppats, env) = chkList loc (chkPPat, ppats, env)

    and chkValDecl loc (valDecl, env) = (case valDecl
           of PT1.MarkVDecl {span, tree} => let
		  val (tree, env) = chkValDecl span (tree, env)
	          in
		     (PT2.MarkVDecl{span=span, tree=tree}, env)
		  end
	    | PT1.ValVDecl (pat, exp) => let
		  val exp = chkExp loc (exp, env)
		  val (pat, env) = chkPat loc (pat, env)
	          in
		     (PT2.ValVDecl(pat, exp), env)
		  end
	    | PT1.PValVDecl (pat, exp) => let
		  val exp = chkExp loc (exp, env)
		  val (pat, env) = chkPat loc (pat, env)
	          in
		     (PT2.PValVDecl(pat, exp), env)
		  end
	    | PT1.FunVDecl functs => let
		(* add function bindings to the environment *)
		  fun add (PT1.MarkFunct {span, tree}, env) = 
		        add (tree, env)
		    | add (PT1.Funct clauses, env) = let
                        fun add ((f, _, _, _), env) = let
                              val f' = freshVar f
                              in
                                 BEnv.insertVal(env, f, BEnv.Var f')
                              end
                        in
                           List.foldl add env clauses
                        end
		  val env = List.foldl add env functs  
                (* check the function definitions, taking care to keep the environments separate *)
		  fun chk loc (PT1.MarkFunct {span, tree}) = 
		          PT2.MarkFunct{span=span, tree=chk span tree} 
		    | chk loc (PT1.Funct clauses) = let
                          fun chk loc (f, pats, ty, exp) = let
                                  val (pat, env') = chkPats loc (pats, env)
			          val exp = chkExp loc (exp, env')
			          val BEnv.Var f' = Option.valOf(BEnv.findVal(env, f))
			          val ty = (
			                case ty
				         of NONE => NONE
				          | SOME ty => SOME (chkTy loc (ty, env))
			                (* end case *))
			          in
			             (f', pat, ty, exp)
			          end
                          val clauses = List.map (chk loc) clauses
                          in
                             PT2.Funct clauses
                          end
		  val functs = List.map (chk loc) functs
	          in
		     (PT2.FunVDecl functs, env)
		  end
	    | PT1.PrimVDecl(pat, prim) => let
		val prim = BOMBoundVariableCheck.chkPrimValRhs loc (prim, env)
		val (pat, env) = chkPat loc (pat, env)
		in
		  (PT2.PrimVDecl(pat, prim), env)
		end
           (* end case *))

    and chkValDecls loc (valDecls, env) = chkList loc (chkValDecl, valDecls, env)

    and chkMatch loc (match, env) = (case match
           of PT1.MarkMatch {span, tree} => let
		val tree = chkMatch span (tree, env)
		in
		  PT2.MarkMatch{span=span, tree=tree}
		end
	    | PT1.Match (pat, exp) => let
		val (pat, env') = chkPat loc (pat, env)
		val exp = chkExp loc (exp, env')
		in
		  PT2.Match(pat, exp)
		end
           (* end case *))

    and chkPBind loc (PT1.MarkPBind {span, tree}, env) = let
	    val (tree, env) = chkPBind loc (tree, env)
            in
	       (PT2.MarkPBind {span=span, tree=tree}, env)
	    end
      | chkPBind loc (PT1.PBind (pat, exp), env) = let
	    val exp = chkExp loc (exp, env)
	    val (pat, env) = chkPat loc (pat, env)
	    in
	      (PT2.PBind (pat, exp), env)
	    end

    and chkPBinds loc (pbinds, env) = chkList loc (chkPBind, pbinds, env)

    and chkMatches loc (matches, env) = List.map (fn m => chkMatch loc (m, env)) matches

    and chkPMatch loc (pmatch, env) = (case pmatch
	 of PT1.MarkPMatch{span, tree} => let
		val tree = chkPMatch span (tree, env)
		in
		  PT2.MarkPMatch{span=span, tree=tree}
		end
	    | PT1.PMatch(ppats, exp) => let
		val (ppats, env') = chkPPats loc (ppats, env)
		val exp = chkExp loc (exp, env')
		in
		  PT2.PMatch(ppats, exp)
		end
	    | PT1.Otherwise exp => let
		val exp = chkExp loc (exp, env)
		in
		  PT2.Otherwise exp
		end
           (* end case *))

    and chkPMatches loc (pmatches, env) = List.map (fn m => chkPMatch loc (m, env)) pmatches

    and chkExp loc (exp, env) = (case exp
           of PT1.MarkExp{span, tree} => let
		val tree = chkExp span (tree, env)
		in
		  PT2.MarkExp{span=span, tree=tree}
		end
	    | PT1.LetExp(valDecls, exp) => let
		val (valDecls', env') = chkValDecls loc (valDecls, env)
		val exp = chkExp loc (exp, env')
		in
		   PT2.LetExp(valDecls', exp)
		end
	    | PT1.IfExp(e1, e2, e3) => let
		val e1 = chkExp loc (e1, env)
		val e2 = chkExp loc (e2, env)
		val e3 = chkExp loc (e3, env)
		in
		  PT2.IfExp(e1, e2, e3)
		end
	    | PT1.CaseExp(exp, matches) => let
		val exp = chkExp loc (exp, env)
		val matches = chkMatches loc (matches, env)
		in
		  PT2.CaseExp(exp, matches)
		end
	    | PT1.PCaseExp(exps, pmatches) => let
		val exps = chkExps loc (exps, env)
		val pmatches = chkPMatches loc (pmatches, env)
		in
		  PT2.PCaseExp(exps, pmatches)
		end
	    | PT1.HandleExp(exp, matches) => let
		val exp = chkExp loc (exp, env)
		val matches = chkMatches loc (matches, env)
		in
		  PT2.HandleExp(exp, matches)
		end
	    | PT1.RaiseExp exp => let
		val exp = chkExp loc (exp, env)
		in
		  PT2.RaiseExp exp
		end
	    | PT1.AndAlsoExp (exp1, exp2) => let
		val exp1 = chkExp loc (exp1, env)
		val exp2 = chkExp loc (exp2, env)
		in
		  PT2.AndAlsoExp (exp1, exp2)
		end
	    | PT1.OrElseExp(exp1, exp2) => let
		val exp1 = chkExp loc (exp1, env)
		val exp2 = chkExp loc (exp2, env)
		in
		  PT2.OrElseExp(exp1, exp2)
		end
	    | PT1.BinaryExp(exp1, id, exp2) => let
		val exp1 = chkExp loc (exp1, env)
		val exp2 = chkExp loc (exp2, env)
		val id' = (case BEnv.findVal (env, id)
		       of SOME(BEnv.Var id') => id'
			| SOME(BEnv.Con id') => id'
			| NONE => raise Fail(concat["unknown operator \"", Atom.toString id, "\""])
		      (* end case *))
		in
		  PT2.BinaryExp(exp1, id', exp2)
		end
	    | PT1.PChoiceExp exps => let
		val exps = chkExps loc (exps, env)
		in
		  PT2.PChoiceExp exps
		end
	    | PT1.ApplyExp(exp1, exp2) => let
		val exp1 = chkExp loc (exp1, env)
		val exp2 = chkExp loc (exp2, env)
		in
		  PT2.ApplyExp(exp1, exp2)
		end
	    | PT1.IdExp qId => (case findValQid(loc, env, qId)
		of BEnv.Var var => PT2.IdExp var
		 | BEnv.Con c => PT2.IdExp c
	       (* end case *))
	    | PT1.ConstExp const => PT2.ConstExp(chkConst const)
	    | PT1.TupleExp exps => let
		val exps = chkExps loc (exps, env)
		in
		  PT2.TupleExp exps
		end
	    | PT1.ListExp exps => let
		val exps = chkExps loc (exps, env)
		in
		  PT2.ListExp exps
		end
	    | PT1.RangeExp(exp1, exp2, expOpt) => let
		val exp1 = chkExp loc (exp1, env)
		val exp2 = chkExp loc (exp2, env)
		val expOpt = (case expOpt
		       of NONE => NONE
			| SOME exp => let
			    val exp = chkExp loc (exp, env)
			    in
			      SOME exp
			    end
		      (* end case *))
		in
		  PT2.RangeExp(exp1, exp2, expOpt)
		end
	    | PT1.SeqExp exps => let
		val exps = chkExps loc (exps, env)
		in
		  PT2.SeqExp exps
		end
	    | PT1.ConstraintExp (exp, ty) => let
		val exp = chkExp loc (exp, env)
		val ty = chkTy loc (ty, env)
		in
		  PT2.ConstraintExp(exp, ty)
		end
	    | PT1.FnExp (pats, exp) => let
	        val (pats, env) = chkPat loc (pats, env)
		val exp = chkExp loc (exp, env)
	        in
		  PT2.FnExp (pats, exp)
	        end
	    | PT1.SpawnExp exp => let
		val exp = chkExp loc (exp, env)
	        in
		  PT2.SpawnExp exp
	        end
	    | PT1.PArrayExp exps => let
		  val exps = chkExps loc (exps, env)
	          in
		     PT2.PArrayExp exps
		  end
	    | PT1.PTupleExp exps => let
		  val exps = chkExps loc (exps, env)
	          in
		     PT2.PTupleExp exps
		  end
	    | PT1.PCompExp (exp, pbinds, expOpt) => let
		  val (pbinds, env) = chkPBinds loc (pbinds, env)
		  val exp = chkExp loc (exp, env)
		  val expOpt = (
		      case expOpt
		       of NONE => NONE
			| SOME exp => SOME(chkExp loc (exp, env))
		      (* end case *))
	          in
		    PT2.PCompExp (exp, pbinds, expOpt)
		  end
            (* end case *))

    and chkExps loc (exps, env) = List.map (fn e => chkExp loc (e, env)) exps

    fun chkTyDecl loc (tyDecl, env) = (case tyDecl
           of PT1.MarkTyDecl {span, tree} => let
		  val (tree, env) = chkTyDecl span (tree, env)
	          in
                    (PT2.MarkTyDecl{span=span, tree=tree}, env)
	          end
	    | PT1.TypeTyDecl (tvs, id, ty) => let
		  val ty = chkTy loc (ty, env)
		  val id' = freshVar id
		  val env = BEnv.insertTy(env, id, id')
	          in
		     (PT2.TypeTyDecl (tvs, id', ty), env)
		  end
	    | PT1.DataTyDecl decls => let
		  (* insert data type ids first to make recursive types work *)
		  fun ins ((tvs, id, conDecls), env) = BEnv.insertDataTyc(env, id, freshVar id)
		  val env = List.foldl ins env decls
		  (* check the bodies of datatypes *)
		  fun f ((tvs, id, conDecls), (decls, env)) = let
		      val SOME (BEnv.DataTyc dataTy) = BEnv.findTy(env, id)
		      val (conDecls, env) = chkConDecls dataTy loc (conDecls, env)
		      in
		         ((tvs, dataTy, conDecls) :: decls, env)
		      end
		  val (decls, env) = List.foldl f ([], env) decls
	          in
		     (PT2.DataTyDecl (List.rev decls), env)
		  end
	    | PT1.DataTyReplDecl (dtBind, dtRef) => let
		  val dtRef' = (
		        case findDataTyQid(loc, env, dtRef)
			 of NONE => (
			    error(loc, ["unbound datatype ", qidToString dtRef]);
			    dummyTy)
			  | SOME dtRef => dtRef
  		        (* end case *))
		  fun ins ((id, x), env) = BEnv.insertDataCon(env, id, x, dtRef')
		  val env = List.foldl ins env (BEnv.getDataCons dtRef')
		  val env = BEnv.insertDataTyc(env, dtBind, dtRef')
		  in
		    (PT2.DataTyReplDecl (dtRef', dtRef'), env)
		  end
	    | PT1.AbsTyDecl (tvs, id) => let
		  val id' = freshVar id
		  val env = BEnv.insertAbsTyc(env, id, id')
	          in
		     (PT2.AbsTyDecl (tvs, id'), env)
		  end
	    | PT1.PrimTyDecl (tvs, id, bty) => let
		val bty = BOMBoundVariableCheck.chkTy loc (bty, env)
		val id' = freshVar id
		val env = BEnv.insertAbsTyc(env, id, id')
		in
		  (PT2.PrimTyDecl(tvs, id', bty), env)
		end
           (* end case *))

    and chkTyDecls loc (tyDecls, env) = chkList loc (chkTyDecl, tyDecls, env)

    and chkConDecl dataTy loc (conDecl, env) = (case conDecl
           of PT1.MarkConDecl {span, tree} => let
		  val (tree, env) = chkConDecl dataTy span (tree, env)
	          in
                    (PT2.MarkConDecl{span=span, tree=tree}, env)
	          end
	    | PT1.ConDecl (id, tyOpt) => let
		  val id' = freshVar id
		  val env = BEnv.insertDataCon(env, id, id', dataTy)
		  val (tyOpt, env) = (case tyOpt
                        of NONE => (NONE, env)
			 | SOME ty => let
			       val ty = chkTy loc (ty, env)
			       in
			          (SOME ty, env)
			       end
                        (* end case *))
	          in
		     (PT2.ConDecl(id', tyOpt), env)
		  end
           (* end case *))

    and chkConDecls dataTy loc (conDecls, env) = chkList loc (chkConDecl dataTy, conDecls, env)

    fun chkSpec loc (spec, env) = (case spec
           of PT1.MarkSpec {span, tree} => let
		  val (tree, env) = chkSpec span (tree, env)
	          in
		     (PT2.MarkSpec {span=span, tree=tree}, env)
		  end
	    | PT1.IncludeSpec sign => let
		  val (sign, env) = chkSign loc (sign, env)
	          in
		      (PT2.IncludeSpec sign, env)
		  end
	    | PT1.ModuleSpec (mb, sign) => let
		  val (sign, env) = chkSign loc (sign, env)
		  val mb' = freshVar mb
		  val env = BEnv.insertMod(env, mb, (mb', env))
	          in
		      (PT2.ModuleSpec (mb', sign), env)
		  end
	    | PT1.TypeSpec tyDecl => let
		  val (tyDecl, env) = chkTyDecl loc (tyDecl, env)
	          in
		     (PT2.TypeSpec tyDecl, env)
		  end
	    | PT1.ConstSpec (cb, tvs) => let
		  val cb' = freshVar cb
		  val env = BEnv.insertVal(env, cb, BEnv.Var cb')
	          in
		     (PT2.ConstSpec (cb', tvs), env)
		  end
	    | PT1.ValSpec (vb, tvs, ty) => let
		  val ty = chkTy loc (ty, env)
		  val vb' = freshVar vb
		  val env = BEnv.insertVal(env, vb, BEnv.Var vb')
		  in
		     (PT2.ValSpec (vb', tvs, ty), env)
		  end
          (* end case *))

    and chkSpecs loc (specs, env) = chkList loc (chkSpec, specs, env)

    and chkSign loc (sign, env) = (case sign
           of PT1.MarkSig {span, tree} => let
		  val (tree, env) = chkSign span (tree, env)
	          in
		     (PT2.MarkSig {span=span, tree=tree}, env)
		  end
	    | PT1.NameSig (id, tyDecls) => let
		  val (tyDecls, env) = chkTyDecls loc (tyDecls, env)
                  in
		     case BEnv.findSig(env, id)
                      of NONE => (error(loc, ["unbound signature ", Atom.toString id]);
				  (PT2.NameSig(Var.new("dummy", ()), tyDecls), env))
		       | SOME (id', sigEnv) => (PT2.NameSig(id', tyDecls), sigEnv)
	          end
	    | PT1.ExpSig specs => let
		  val sigEnv = BEnv.freshEnv(BEnv.nameOf env, SOME env)
		  val (specs, sigEnv) = chkSpecs loc (specs, sigEnv)
	          in
		     (PT2.ExpSig specs, sigEnv)
		  end
          (* end case *))

  (* constrain a module by a signature, e.g., if we have
   *
   *  signature S = sig
   *    type t
   *    val x : t
   *  end
   *
   *  structure F : S = struct
   *    type t = int
   *    val x : t = 3
   *  end
   *
   * then constrainMod(S, sigOf(F)) is
   *
   * structure F = struct
   *   local
   *     type t' = int
   *     val x' : t' = 3
   *   in
   *     abstype t (* = t' *)
   *     val x : t  = x'
   *   end
   * 
   *)
				
    fun freshVal (id, BEnv.Var _, BEnv.Var _) = BEnv.Var(freshVar id)
      | freshVal (id, BEnv.Var _, BEnv.Con _) = BEnv.Var(freshVar id)
      | freshVal (id, BEnv.Con _, BEnv.Con _) = BEnv.Con(freshVar id)
      | freshVal (id, _, _) = raise Fail "unmatched value kind in constraining signature"

    fun constrainVals (sEnv, mEnv, cEnv) = let
	  fun ins ((id, cV, mV), cEnv) = BEnv.insertVal(cEnv, id, freshVal(id, cV, mV))
          in
	    List.foldl ins cEnv (BEnv.matchValsByName(sEnv, mEnv))
	  end

    fun freshTy (id, BEnv.AbsTyc _, _) = BEnv.AbsTyc(freshVar id)
      | freshTy (id, BEnv.DataTyc _, BEnv.DataTyc _) = BEnv.DataTyc(freshVar id)
      | freshTy (id, BEnv.TypeExp _, _) = BEnv.TypeExp(freshVar id)
      | freshTy _ = raise Fail "unmatched type in constraining signature"

    fun constrainTypes (sEnv, mEnv, cEnv) = let
	  fun ins ((id, cTy, mTy), cEnv) = BEnv.insertType(cEnv, id, freshTy(id, cTy, mTy))
          in
	    List.foldl ins cEnv (BEnv.matchTysByName(sEnv, mEnv))
	  end

    fun constrainMods (sEnv, mEnv, cEnv) = let
	  fun ins ((id, cMod, mMod), cEnv) = raise Fail "todo"
          in
	    List.foldl ins cEnv (BEnv.matchModsByName(sEnv, mEnv))
	  end	  

  (* construct an environment for constraining a module to a signature *)
    fun constrainSig (sEnv, mEnv) = let
	  val cEnv = BEnv.freshEnv'(BEnv.nameOf mEnv, BEnv.outerEnv mEnv, BEnv.bomEnv mEnv, BEnv.modEnv mEnv, BEnv.sigEnv mEnv)
	  val cEnv = constrainVals(sEnv, mEnv, cEnv)
	  val cEnv = constrainTypes(sEnv, mEnv, cEnv)
	  val cEnv = constrainMods(sEnv, mEnv, cEnv)
	  in
	    cEnv
	  end

    fun addValAlias ((id, BEnv.Var cV, BEnv.Var mV), aliases) = PT2.ValVDecl(PT2.IdPat cV, PT2.IdExp mV) :: aliases
      | addValAlias ((id, BEnv.Var cV, BEnv.Con mV), aliases) = PT2.ValVDecl(PT2.IdPat cV, PT2.IdExp mV) :: aliases
      | addValAlias ((id, _, _), aliases) = aliases

    fun mkValConstraintAliases (cEnv, mEnv) = List.foldl addValAlias [] (BEnv.matchValsByName(cEnv, mEnv))

  (* constrain a module by a signature environment. takes a signature and module environment, and returns a fresh
   * constraining environment for the module, a list of value bindings and a list of type bindings. the bindings
   * are aliases for the constrained module's external interface.
   *)
    fun constrainMod (sEnv, mEnv) = let
	  val cEnv = constrainSig(sEnv, mEnv)
          in
	    {cEnv=cEnv, valConstraintAliases=mkValConstraintAliases(cEnv, mEnv), tyConstraintAliases=[]}
	  end

  (* check the constraining signature. we rebind to get scoping right. *)
    fun chkConstraint loc (signOpt, modEnv, env) = (case signOpt
                of NONE => (NONE, [], modEnv)
		 | SOME sign => let
		       val (sign, signEnv) = chkSign loc (sign, env)
		       val {cEnv, valConstraintAliases, tyConstraintAliases} = constrainMod(signEnv, modEnv)
		       in
		           (SOME sign, valConstraintAliases, cEnv)
		       end
                (* end case *))

    fun chkModule loc (mb, module, sign, env) = (case module
            of PT1.MarkMod {span, tree} => let
		   val (tree, sign, rebinds, env) = chkModule span (mb, tree, sign, env)
	           in
		      (PT2.MarkMod {span=span, tree=tree}, sign, rebinds, env)
	           end
	     | PT1.DeclsMod decls => let
		   val (decls, modEnv) = chkDecls loc (decls, BEnv.freshEnv (mb, SOME env))
		   val (sign, rebinds, signEnv) = chkConstraint loc (sign, modEnv, env)
	           in
		       (PT2.DeclsMod decls, sign, rebinds, signEnv)
	           end
	     | PT1.NamedMod qId => let
		   val modId = findModQid(loc, env, qId)
		   val modEnv = findModEnv(loc, env, qId)
		   val (sign, rebinds, signEnv) = chkConstraint loc (sign, modEnv, env)
	           in
                      (PT2.NamedMod modId, sign, rebinds, signEnv)
	           end
	     | PT1.ApplyMod (m, args) => raise Fail "todo"
            (* end case *))
 
    and chkDecl loc (decl, env) = (case decl
           of PT1.MarkDecl {span, tree} => let
		  val (trees, env) = chkDecl span (tree, env)
	          in
	             (List.map (fn tree => PT2.MarkDecl{span=span, tree=tree}) trees, env)
	          end
	    | PT1.ModuleDecl (mb, sign, module) => let	          
		  val mb' = freshVar mb
		  val (module, sign, rebindVals, modEnv) = chkModule loc (mb, module, sign, env)
		  val rebindDecls = List.map PT2.ValueDecl rebindVals
		  val env = BEnv.insertMod(env, mb, (mb', modEnv))
	          in
	             (PT2.ModuleDecl(mb', sign, module) :: rebindDecls, env)
	          end
	    | PT1.TyDecl tyDecl => let
		  val (tyDecl, env) = chkTyDecl loc (tyDecl, env)
	          in
		     ([PT2.TyDecl tyDecl], env)
		  end
	    | PT1.ExnDecl (con, tyOpt) => let
		  val con' = freshVar con
		  val env = BEnv.insertVal(env, con, BEnv.Con con')
		  val (tyOpt, env) = (case tyOpt
                        of NONE => (NONE, env)
			 | SOME ty => let
			       val ty = chkTy loc (ty, env)
			       in
			         (SOME ty, env)
			       end
                        (* end case *))
	          in
		     ([PT2.ExnDecl (con', tyOpt)], env)
		  end
	    | PT1.ValueDecl valDecl => let
		  val (valDecl, env) = chkValDecl loc (valDecl, env)
	          in
		    ([PT2.ValueDecl valDecl], env)
		  end
	    | PT1.LocalDecl (locals, decls) => let
		  val localBinds = AtomSet.fromList (Binding.bindsOfDecls(locals, env))
		  val globalBinds = AtomSet.fromList (Binding.bindsOfDecls(decls, env))
		  fun isLocal v = not(AtomSet.member(globalBinds, v)) andalso AtomSet.member(localBinds, v)
		  val (locals, env) = chkDecls loc (locals, env)
		  val (decls, env) = chkDecls loc (decls, env)
		  val env = BEnv.filterEnv(env, not o isLocal)
	          in
		     ([PT2.LocalDecl (locals, decls)], env)
		  end
	    | PT1.SignDecl (id, sign) => let
		  val id' = freshVar id
		  val (sign, sigEnv) = chkSign loc (sign, BEnv.freshEnv (id, SOME env))
		  val env = BEnv.insertSig(env, id, (id', sigEnv))
		  in
		     ([PT2.SignDecl (id', sign)], env)
		  end
	    | PT1.PrimCodeDecl code => let
		  val (code, env) = BOMBoundVariableCheck.chkCode loc (code, env)
	          in
		     ([PT2.PrimCodeDecl code], env)
	          end
	    | PT1.ExpansionOptsDecl (opts, decls) => let
		  val (decls, env) = chkDecls loc (decls, env)
	          in
		      ([PT2.ExpansionOptsDecl (opts, decls)], env)
		  end
           (* end case *))

    and chkDecls loc (decls, env) = let
	    val (decls, env) = chkList loc (chkDecl, decls, env)
            in
	        (List.concat decls, env)
	    end

    fun check (es, {span, tree}, env) = let
	val _ = ErrorStream.setErrStrm es
	val (tree', env') = chkDecls span (tree, env)
        in		  
	   ({span=span, tree=tree'}, env')
        end

  end (* BoundVariableCheck *)
