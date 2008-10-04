(* used-vars.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Get the used variables of a parse tree.
 *)

structure UsedVars =
  struct

    structure PT = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var

    val var = Var.Set.singleton
    val empty = Var.Set.empty
    fun union vars = List.foldl Var.Set.union empty vars

    fun usedOfList f ls = union(List.map f ls)

    fun usedOfTy ty = (case ty
           of PT.MarkTy {span, tree} => usedOfTy tree
	    | PT.NamedTy (tys, id) => union[var id, usedOfTys tys]
	    | PT.VarTy tv => empty
	    | PT.TupleTy tys => usedOfTys tys
	    | PT.FunTy (ty1, ty2) => usedOfTys[ty1, ty2]
           (* end case *))

    and usedOfTys tys = usedOfList usedOfTy tys

    fun usedOfPat pat = (case pat
           of PT.MarkPat {span, tree} => usedOfPat tree
	    | PT.BinaryPat (pat1, cid, pat2) => union[var cid, usedOfPats[pat1, pat2]]
	    | PT.ConPat (cid, pat) => union[var cid, usedOfPat pat]
	    | PT.TuplePat pats => usedOfPats pats
	    | PT.ConstPat const => empty
	    | PT.WildPat => empty
	    | PT.IdPat vb => var vb
	    | PT.ConstraintPat (pat, ty) => union[usedOfPat pat, usedOfTy ty]
           (* end case *))

    and usedOfPats pats = usedOfList usedOfPat pats

    fun usedOfPPat pat = (case pat
           of PT.MarkPPat {span, tree} => usedOfPPat tree
	    | PT.NDWildPat => empty
	    | PT.Pat p => usedOfPat p 
	    | PT.HandlePat p => usedOfPat p 
           (* end case *))

    and usedOfPPats pats = usedOfList usedOfPPat pats
			   
    and usedOfValDecl valDecl = (case valDecl
           of PT.MarkVDecl {span, tree} => usedOfValDecl tree
	    | PT.ValVDecl (pat, exp) => union[usedOfPat pat, usedOfExp exp]
	    | PT.PValVDecl (pat, exp) => union[usedOfPat pat, usedOfExp exp]
	    | PT.FunVDecl functs => let
		  fun funct (PT.MarkFunct {span, tree}) = funct tree
		    | funct (PT.Funct (f, pats, exp)) = union[var f, usedOfPats pats, usedOfExp exp]
	          in
		     usedOfList funct functs
		  end
	    | PT.PrimVDecl(pat, prim) => union[usedOfPat pat, BOMUsedVars.usedOfPrimValRhs prim]
           (* end case *))

    and usedOfValDecls valDecls = usedOfList usedOfValDecl valDecls

    and usedOfMatch match = (case match
           of PT.MarkMatch {span, tree} => usedOfMatch tree
	    | PT.Match (pat, exp) => union[usedOfPat pat, usedOfExp exp]
           (* end case *))

    and usedOfMatches matches = usedOfList usedOfMatch matches

    and usedOfPMatch pmatch = (case pmatch
	 of PT.MarkPMatch{span, tree} => usedOfPMatch tree
	  | PT.PMatch(ppats, exp) => union[usedOfPPats ppats, usedOfExp exp]
	  | PT.Otherwise exp => usedOfExp exp
           (* end case *))

    and usedOfPMatches pmatches = usedOfList usedOfPMatch pmatches

    and usedOfExp exp = (case exp
           of PT.MarkExp{span, tree} => usedOfExp tree
	    | PT.LetExp(valDecls, exp) => union[usedOfValDecls valDecls, usedOfExp exp]
	    | PT.IfExp(e1, e2, e3) => union[usedOfExp e1, usedOfExp e2, usedOfExp e3]
	    | PT.CaseExp(exp, matches) => union[usedOfExp exp, usedOfMatches matches]
	    | PT.PCaseExp(exps, pmatches) => union[usedOfExp exp, usedOfPMatches pmatches]
	    | PT.HandleExp(exp, matches) => union[usedOfExp exp, usedOfMatches matches]
	    | PT.RaiseExp exp => usedOfExp exp
	    | PT.AndAlsoExp (exp1, exp2) => usedOfExps[exp1, exp2]
	    | PT.OrElseExp(exp1, exp2) => usedOfExps[exp1, exp2]
	    | PT.BinaryExp(exp1, id, exp2) => union[var id, usedOfExps[exp1, exp2]]
	    | PT.PChoiceExp exps => usedOfExps exps
	    | PT.ApplyExp(exp1, exp2) => usedOfExps[exp1, exp2]
	    | PT.ConstExp const => empty
	    | PT.TupleExp exps => usedOfExps exps
	    | PT.ListExp exps => usedOfExps exps
	    | PT.RangeExp(exp1, exp2, NONE) => union[usedOfExp exp1, usedOfExp exp2]
	    | PT.RangeExp(exp1, exp2, SOME exp3) => union[usedOfExp exp1, usedOfExp exp2, usedOfExp exp3]
	    | PT.PTupleExp exps =>  usedOfExps exps
	    | PT.PArrayExp exps =>  usedOfExps exps
	    | PT.PCompExp(exp, pbinds, NONE) => raise Fail "usedOfExp: PCompExp" (* FIXME *)
	    | PT.PCompExp(exp, pbinds, SOME exp') => raise Fail "usedOfExp: PCompExp" (* FIXME *)
	    | PT.SpawnExp exp => usedOfExp exp
	    | PT.IdExp v => var v
	    | PT.SeqExp exps => usedOfExps exps
	    | PT.ConstraintExp (exp, ty) => union[usedOfExp exp, usedOfTy ty]
           (* end case *))

    and usedOfExps exps = usedOfList usedOfExp exps

    fun usedOfTyDecl tyDecl = (case tyDecl
           of PT.MarkTyDecl{span, tree} => usedOfTyDecl tree
	    | PT.TypeTyDecl(tvs, id, ty) => usedOfTy ty
	    | PT.DataTyDecl decls => usedOfConDecls(List.concat(List.map #3 decls))
	    | PT.DataTyReplDecl(lhs, rhs) => var rhs
	    | PT.AbsTyDecl(tvs, id) => empty
	    | PT.PrimTyDecl(tvs, id, bty) => BOMUsedVars.usedOfTy bty
           (* end case *))

    and usedOfTyDecls tyDecls = usedOfList usedOfTyDecl tyDecls

    and usedOfConDecl conDecl = (case conDecl
           of PT.MarkConDecl {span, tree} => usedOfConDecl tree
	    | PT.ConDecl (id, NONE) => empty
	    | PT.ConDecl (id, SOME ty) => usedOfTy ty
           (* end case *))

    and usedOfConDecls conDecls = usedOfList usedOfConDecl conDecls

    fun usedOfSpec spec = (case spec
           of PT.MarkSpec {span, tree} => usedOfSpec tree
	    | PT.IncludeSpec sign => usedOfSign sign
	    | PT.ModuleSpec (mb, sign) => usedOfSign sign
	    | PT.TypeSpec tyDecl => usedOfTyDecl tyDecl
	    | PT.ConstSpec (cb, tvs) => empty
	    | PT.ValSpec (vb, tvs, ty) => usedOfTy ty
          (* end case *))

    and usedOfSpecs specs = usedOfList usedOfSpec specs

    and usedOfSign sign  = (case sign
           of PT.MarkSig {span, tree} => usedOfSign tree
	    | PT.NameSig (id, tyDecls) => usedOfTyDecls tyDecls
	    | PT.ExpSig specs => usedOfSpecs specs
          (* end case *))

    fun usedOfModule module = (case module
	   of PT.MarkMod{span, tree} => usedOfModule tree
	    | PT.DeclsMod decls => usedOfDecls decls
	    | PT.NamedMod m => var m
	    | PT.ApplyMod(m, args) => union(var m :: List.map usedOfModule args)
            (* end case *))

    and usedOfDecl decl = (case decl
	   of PT.MarkDecl{span, tree} => usedOfDecl tree
	    | PT.ModuleDecl(mb, NONE, module) => usedOfModule module
	    | PT.ModuleDecl(mb, SOME sign, module) => union[usedOfSign sign, usedOfModule module]
	    | PT.TyDecl tyDecl => usedOfTyDecl tyDecl
	    | PT.ExnDecl(con, NONE) => empty
	    | PT.ExnDecl(con, SOME ty) => usedOfTy ty
	    | PT.ValueDecl valDecl => usedOfValDecl valDecl
	    | PT.LocalDecl(locals, decls) => union[usedOfDecls locals, usedOfDecls decls]
	    | PT.SignDecl(id, sign) => usedOfSign sign
	    | PT.PrimCodeDecl code => BOMUsedVars.usedOfCode code
	    | PT.ExpansionOptsDecl(_, decls) => usedOfDecls decls
           (* end case *))

    and usedOfDecls decls = usedOfList usedOfDecl decls

  end
