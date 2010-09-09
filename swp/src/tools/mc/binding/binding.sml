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

  (* returns the variables bound by a pattern. note that the environment is necessary
   * to disambiguate between nullary constructors and pattern-bound variables.
   *)
    fun varsOfPat (pat, env) = let
	  fun varsOfPat' pat = (
	        case pat
	          of PT.MarkPat {tree, ...} => varsOfPat' tree
		   | PT.BinaryPat (p1, v, p2) => varsOfPat' p1 @ varsOfPat' p2
		   | PT.ConPat (c, p) => varsOfPat' p
		   | PT.TuplePat ps => concatMap varsOfPat' ps
		   | PT.ConstraintPat (p, t) => varsOfPat' p
		   | PT.IdPat qid => (
		       case QualifiedId.findVal(env, qid)
			of SOME(BindingEnv.Con _) => []        (* nullary constructor, not a pattern-bound variable *)
			 | _ => (
			   case QualifiedId.unqualId qid
			    of SOME v => [v]                   (* pattern-bound variable *)
			     | NONE => []                      (* bogus qualified id in a pattern *)
			   (* end case *))
		       (* end case *))
		   | PT.WildPat => []
		   | _ => []
		 (* end case *))
          in
	    varsOfPat' pat
	  end

    fun bindsOfFunct (PT.MarkFunct {tree, ...}) = bindsOfFunct tree
      | bindsOfFunct (PT.Funct clauses) = List.map (fn (v, _, _, _) => v) clauses

  (* get the bound variables of a value declaration *)
    fun bindsOfValDecl (vd, env) = (
	  case vd
	   of PT.MarkVDecl {tree, ...} => bindsOfValDecl(tree, env)
	    | ( PT.ValVDecl (p, _) |
		PT.PValVDecl  (p, _) |
		PT.PrimVDecl (p, _) ) => varsOfPat(p, env)
	    | PT.FunVDecl funs => concatMap bindsOfFunct funs
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

    fun bindsOfModule (module, env) = (
	  case module
	   of PT.MarkMod {tree, ...} => bindsOfModule (tree, env)
	    | PT.DeclsMod decls => bindsOfDecls (decls, env)
	    | PT.ApplyMod (_, modules) => bindsOfModules(modules, env)
	    | _ => []
          (* end case *))

    and bindsOfModules ([], env) = []
      | bindsOfModules (m :: ms, env) = bindsOfModule(m, env) @ bindsOfModules(ms, env)

    and bindsOfDecl (decl, env) = (
	  case decl
	   of PT.MarkDecl {tree, ...} => bindsOfDecl(tree, env)
	    | PT.ModuleDecl (_, _, module) => bindsOfModule(module, env)
	    | PT.ValueDecl vd => bindsOfValDecl(vd, env)
	    | PT.LocalDecl (ds1, ds2) => bindsOfDecls(ds2, env)
	    | PT.PrimCodeDecl defns => concatMap bindsOfPrimCode defns
	    | PT.TyDecl tyDecl => bindsOfTyDecl tyDecl
	    | PT.ExnDecl (exn, _) => [exn]
	    | _ => []
          (* end case *))

  (* bound variables of a declaration *)
    and bindsOfDecls ([], env) = []
      | bindsOfDecls (d :: ds, env) = bindsOfDecl(d, env) @ bindsOfDecls(ds, env)

  end
