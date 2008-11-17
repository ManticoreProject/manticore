(* binding.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Get binding occurrences for parse trees.
 *)

structure Binding =
  struct

    structure PT = ProgramParseTree.PML1
    structure BPT = PT.BOMParseTree
    structure Var = ProgramParseTree.Var

    fun concatMap f ls = List.concat(List.map f ls)

    fun varsOfQid v = (
	  case QualifiedId.unqualId v
	   of SOME v => [v]
	    | NONE => []
          (* end case *))

    fun varsOfPat pat = (
	  case pat
	   of PT.MarkPat {tree, ...} => varsOfPat tree
	    | PT.BinaryPat (p1, v, p2) => varsOfPat p1 @ varsOfPat p2
	    | PT.ConPat (c, p) => varsOfPat p
	    | PT.TuplePat ps => concatMap varsOfPat ps
	    | PT.ConstraintPat (p, t) => varsOfPat p
	    | PT.IdPat qid => varsOfQid qid
	    | PT.WildPat => []
	    | _ => []
          (* end case *))

    fun bindsOfFunct (PT.MarkFunct {tree, ...}) = bindsOfFunct tree
      | bindsOfFunct (PT.Funct (v, _, _)) = v

  (* get the bound variables of a value declaration *)
    fun bindsOfValDecl vd = (
	  case vd
	   of PT.MarkVDecl {tree, ...} => bindsOfValDecl tree
	    | ( PT.ValVDecl (p, _) |
		PT.PValVDecl  (p, _) |
		PT.PrimVDecl (p, _) ) => varsOfPat p
	    | PT.FunVDecl funs => List.map bindsOfFunct funs
          (* end case *))

    fun bindsOfPrimCode defn = (
	  case defn
	   of BPT.D_Mark {tree, ...} => bindsOfPrimCode tree
	    | BPT.D_Define (_, f, _, _, _, _) => [f]
	    | _ => []
          (* end case *))

    fun bindsOfTyDecl tyDecl = (
          case tyDecl
	   of PT.MarkTyDecl {tree, ...} => bindsOfTyDecl tree
	    | PT.TypeTyDecl (_, ty, _) => [ty]
	    | PT.DataTyDecl dts => List.map #2 dts
	    | PT.DataTyReplDecl (ty, _) => [ty]
	    | PT.AbsTyDecl (_, ty) => [ty]
	    | PT.PrimTyDecl (_, ty, _) => [ty]
          (* end case *))

    fun bindsOfModule module = (
	  case module
	   of PT.MarkMod {tree, ...} => bindsOfModule tree
	    | PT.DeclsMod decls => bindsOfDecls decls
	    | PT.ApplyMod (_, modules) => concatMap bindsOfModule modules
	    | _ => []
          (* end case *))

    and bindsOfDecl decl = (
	  case decl
	   of PT.MarkDecl {tree, ...} => bindsOfDecl tree
	    | PT.ModuleDecl (_, _, module) => bindsOfModule module
	    | PT.ValueDecl vd => bindsOfValDecl vd
	    | PT.LocalDecl (ds1, ds2) => bindsOfDecls ds2
	    | PT.PrimCodeDecl defns => concatMap bindsOfPrimCode defns
	    | PT.TyDecl tyDecl => bindsOfTyDecl tyDecl
	    | PT.ExnDecl (exn, _) => [exn]
	    | _ => []
          (* end case *))

  (* bound variables of a declaration *)
    and bindsOfDecls decls = List.concat(List.map bindsOfDecl decls)

  end
