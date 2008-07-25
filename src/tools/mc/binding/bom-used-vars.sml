(* bom-used-vars.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Get the used variables of a BOM parse tree.
 *)

structure BOMUsedVars =
  struct

    structure PT = ProgramParseTree.PML2.BOMParseTree
    structure Var = ProgramParseTree.Var

    val var = Var.Set.singleton
    val empty = Var.Set.empty
    fun union vars = List.foldl Var.Set.union empty vars

    fun usedOfList f ls = union(List.map f ls)

    fun usedOfOption f NONE = empty
      | usedOfOption f (SOME x) = f x

    fun usedOfTy ty = (case ty
           of PT.T_Mark {tree, span} => usedOfTy tree
	    | PT.T_Any => empty
	    | PT.T_Enum x => empty
	    | PT.T_Raw rt => empty
	    | PT.T_Tuple (b, tys) => usedOfTys tys
	    | PT.T_Addr ty => usedOfTy ty
	    | PT.T_Fun (params, exns, rets) => union[usedOfTys params, usedOfTys exns, usedOfTys rets]
	    | PT.T_Cont params => usedOfTys params
	    | PT.T_CFun cp => empty
	    | PT.T_VProc => empty
	    | PT.T_TyCon tc => var tc
	  (* end case *))

    and usedOfTys tys = usedOfList usedOfTy tys

    and usedOfRhs rhs = (case rhs
            of PT.RHS_Mark {tree, span} => usedOfRhs tree
	     | PT.RHS_Exp exp => usedOfExp exp
	     | PT.RHS_SimpleExp sexp => usedOfSexp sexp
	     | PT.RHS_Update (i, sexp1, sexp2) => union[usedOfSexp sexp1, usedOfSexp sexp2]
	     | PT.RHS_Promote sexp => usedOfSexp sexp
	     | PT.RHS_CCall (f, sexps) => usedOfSexps sexps
	     | PT.RHS_VPStore (off, sexp1, sexp2) => union[usedOfSexp sexp1, usedOfSexp sexp2]
	     | PT.RHS_PMLVar v => var v
            (* end case *))

  (* check the lambda where the variable v is already bound *)
    and usedOfLambda (v, params, exns, returnTys, exp) = 
	  union[var v, usedOfVarPats params, usedOfVarPats exns, usedOfTys returnTys, usedOfExp exp]

    and usedOfLambdas lambdas = usedOfList usedOfLambda lambdas

    and usedOfExp exp = (case exp
            of PT.E_Mark {tree, span} => usedOfExp tree
	     | PT.E_Let (vps, rhs, exp) => union[usedOfVarPats vps, usedOfRhs rhs, usedOfExp exp]
	     | PT.E_Fun (lambdas, exp) => union[usedOfLambdas lambdas, usedOfExp exp]
	     | PT.E_Cont (lambda, exp) => union[usedOfLambda lambda, usedOfExp exp]
	     | PT.E_If (sexp, exp1, exp2) => union[usedOfSexp sexp, usedOfExp exp1, usedOfExp exp2]
	     | PT.E_Case (sexp, cases, NONE) => union[usedOfSexp sexp, usedOfCases cases]
	     | PT.E_Case (sexp, cases, SOME (vp, exp)) => union[usedOfSexp sexp, usedOfCases cases, usedOfExp exp, usedOfVarPat vp]
	     | PT.E_Apply (f, args, exns) => union[var f, usedOfSexps args, usedOfSexps exns]
	     | PT.E_Throw (f, args) => union[var f, usedOfSexps args]
	     | PT.E_Return args => usedOfSexps args
	     | PT.E_HLOpApply (hlop, args, exns) => union[var hlop, usedOfSexps args, usedOfSexps exns]
            (* end case *))

    and usedOfSexp sexp = (case sexp
            of PT.SE_Mark {tree, span} => usedOfSexp tree
	     | PT.SE_Var v => var v
	     | PT.SE_Alloc sexps => usedOfSexps sexps
	     | PT.SE_Unwrap sexp => usedOfSexp sexp
	     | PT.SE_Wrap sexp => usedOfSexp sexp
	     | PT.SE_Select (i, sexp) => usedOfSexp sexp
	     | PT.SE_AddrOf (i, sexp) => usedOfSexp sexp
	     | PT.SE_Const (lit, ty) => usedOfTy ty
	     | PT.SE_MLString s => empty
	     | PT.SE_Cast (ty, sexp) => union[usedOfSexp sexp, usedOfTy ty]
	     | PT.SE_Prim (prim, sexps) => union[var prim, usedOfSexps sexps]
	     | PT.SE_HostVProc => empty
	     | PT.SE_VPLoad (off, sexp) => usedOfSexp sexp
            (* end case *))

    and usedOfSexps sexps = usedOfList usedOfSexp sexps

    and usedOfCase (pat, exp) = union[usedOfPat pat, usedOfExp exp]

    and usedOfCases cases = usedOfList usedOfCase cases

    and usedOfPat pat = (case pat
            of PT.P_PMark {tree, span} => usedOfPat tree
	     | PT.P_DCon (dcon, vps) => usedOfVarPats vps
	     | PT.P_Const (lit, ty) => usedOfTy ty
            (* end case *))

    and usedOfVarPat vpat = (case vpat
            of PT.P_VPMark {tree, span} => usedOfVarPat tree
	     | PT.P_Wild NONE => empty
	     | PT.P_Wild (SOME ty) => usedOfTy ty
	     | PT.P_Var (vb, ty) => union[var vb, usedOfTy ty]
            (* end case *))

    and usedOfVarPats vpats = usedOfList usedOfVarPat vpats

    fun usedOfDefn defn = (case defn
	    of PT.D_Mark {tree, span} => usedOfDefn tree
	     | PT.D_Define (b, v, params, exns, returnTys, exp) => 
	         union[var v, usedOfVarPats params, usedOfVarPats exns, usedOfOption usedOfTys returnTys, usedOfOption usedOfExp exp]
	     | PT.D_Extern (CFunctions.CFun{var, name, retTy, argTys, varArg, attrs}) => 
	         empty
	     | PT.D_TypeDef (td, ty) => 
	         usedOfTy ty
             (* end case *))

    fun usedOfCode defns = usedOfList usedOfDefn defns

    fun usedOfPrimValRhs rhs = (case rhs
           of PT.VarPrimVal v => var v
	    | PT.HLOpPrimVal h => var h
	    | PT.LambdaPrimVal lambda => usedOfLambda lambda
           (* end case *))

  end
