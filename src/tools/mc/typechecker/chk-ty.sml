(* chk-ty.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check that types are well formed.
 *)

structure ChkTy :> sig

  (* check a type for well formedness *)
    val checkTy : Error.err_stream -> (Error.span * ProgramParseTree.PML2.tyvar list * ProgramParseTree.PML2.ty) 
		      -> (AST.tyvar list * AST.ty)

  (* check a type for well formedness *)
    val checkTyVars : Error.err_stream -> (Error.span * ProgramParseTree.PML2.tyvar list)
		      -> AST.tyvar list

  end = struct

    structure PT = ProgramParseTree.PML2
    structure TU = TypeUtil
    structure Ty = Types
    structure Env = ModuleEnv

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

  (* a type expression for when there is an error *)
    val bogusTy = AST.ErrorTy

    val idToString = ProgramParseTree.Var.toString

  (* returns the tyvars used in a type *)
    fun tvsOfTy (ty, tvs) = (case ty
           of PT.MarkTy {tree, ...} => tvsOfTy (tree, tvs)
	    | PT.NamedTy (tys, _) => List.foldl tvsOfTy tvs tys
	    | PT.VarTy tv => tv :: tvs
	    | PT.TupleTy tys => List.foldl tvsOfTy tvs tys
	    | PT.FunTy (ty1, ty2) => tvsOfTy(ty1, tvsOfTy(ty2, tvs))
           (* end case *))

  (* typecheck type expressions as described in Section 6.4 *)
    fun chkTy (loc, tve, ty) = (case ty
	   of PT.MarkTy{span, tree} => chkTy(span, tve, tree)
	    | PT.NamedTy(tyArgs, id) => let
		val tyArgs' = List.map (fn ty => chkTy(loc, tve, ty)) tyArgs
		in
		  case Env.getTyDef id
		   of SOME(Env.TyDef(AST.TyScheme(tvs, ty))) =>
			if (List.length tvs <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", idToString id]);
			    bogusTy)
			  else TU.substitute (ty, ListPair.zip(tvs, tyArgs'))
		    | SOME(Env.TyCon tyc') =>
			if (TyCon.arityOf tyc' <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", idToString id]);
			    bogusTy)
			  else AST.ConTy(tyArgs', tyc')
		    | NONE => (
			error(loc, ["undefined type constructor ", idToString id]);
			bogusTy)
		  (* end case *)
		end
	    | PT.VarTy tv => (case Env.TyVarMap.find(tve, tv)
		 of SOME tv' => AST.VarTy tv'
		  | NONE => (error(loc, ["unbound type variable ", Atom.toString tv]); bogusTy)
		(* end case *))
	    | PT.TupleTy tys =>
		TU.tupleTy(List.map (fn ty => chkTy(loc, tve, ty)) tys)
	    | PT.FunTy(ty1, ty2) =>
		AST.FunTy(chkTy(loc, tve, ty1), chkTy(loc, tve, ty2))
	  (* end case *))

  (* check a list of type variables *)
    fun chkTyVars (loc, tvs) = let
	  fun chk ([], tve, tvs) = (tve, List.rev tvs)
	    | chk (tv::rest, tve, tvs) = let
		val tv' = TyVar.new tv
		in
		  if AtomMap.inDomain(tve, tv)
		    then (
		      error (loc, ["duplicate type variable ", Atom.toString tv]);
		      chk (rest, tve, tv'::tvs))
		    else chk (rest, AtomMap.insert(tve, tv, tv'), tv'::tvs)
		end
	  in
	    chk (tvs, AtomMap.empty, [])
	  end

    fun checkTyVars err (loc, tvs) = #2 (chkTyVars(loc, tvs))

    fun checkTy err (loc, tvs, ty) = let
          val tvUses = tvsOfTy(ty, [])
	  val tvs = if List.length tvs > 0
                       then tvs
                       else tvUses
          val (tve, tvs) = chkTyVars(loc, tvs)
	  val ty' = chkTy(loc, tve, ty)
          in
             (tvs, ty')
          end

  end (* ChkTy *)
