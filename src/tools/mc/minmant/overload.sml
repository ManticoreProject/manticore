(* overload.sml
 *
 * Utility functions for dealing with overload resolution.
 *)

structure Overload : sig

    val initialize : unit -> unit
    val add_var : AST.overload_var ref -> unit
    val add_lit : (Types.ty * Types.ty list) ref -> unit

    val resolve : unit -> unit

  end = struct

    structure U = Unify

    val vars : AST.overload_var ref list ref = ref []
    val lits : (Types.ty * Types.ty list) ref list ref = ref []

    fun initialize () = (vars := []; lits := [])
    fun add_var ov = vars := (ov :: !vars)
    fun add_lit lt = lits := (lt :: !lits)

    fun resolve () = let
	    fun typeOf x = let val Types.TyScheme([], ty) = Var.typeOf x in ty end
	    fun try_var (rc as (ref ov)) =
		(case ov of
		     AST.Unknown (ty, vl) =>
		     let
			 fun try (v::vs) =
			     if U.unifiable (ty, typeOf v)
			     then try vs
			     else ((case vs of
					[] => raise Fail "type mismatch"
				      | [x] => (U.unify (ty, typeOf x); rc := AST.Instance x)
				      | _ => rc := AST.Unknown (ty, vs)
				   (* end case *));
				   try vs; true)
			   | try [] = false
		     in
			 try vl
		     end
		   | AST.Instance _ => false
		(* end case *))

	    fun try_lit (rc as (ref (ty, tl))) =
		 let
		     fun try (t::ts) =
			 if U.unifiable (ty, t)
			 then try ts
			 else ((case ts of
				    [] => raise Fail "type mismatch"
				  | [x] => (U.unify (ty, x); rc := (ty, []))
				  | _ => rc := (ty, ts)
			       (* end case *));
			       try ts; true)
		       | try [] = false
		 in
		     try tl
		 end

	    fun default_type Types.Int = Basis.intTy
	      | default_type Types.Float = Basis.floatTy
	      | default_type Types.Num = Basis.intTy
	      | default_type Types.Order = Basis.intTy
	      | default_type Types.Eq = raise Fail "failed to resolve overloaded variable"

	    fun set_def_ty Types.ErrorTy = ()
	      | set_def_ty (Types.MetaTy _) = ()
	      | set_def_ty (Types.VarTy _) = ()
	      | set_def_ty (ty as Types.ClassTy (Types.Class (ref info))) =
		(case info of
		     Types.CLASS cl => (if not(U.unify (ty, default_type cl))
					then raise Fail "this should never happen"
					else ())
		   | _ => ()
		(* end case *))
	      | set_def_ty (Types.ConTy (tys, _)) = List.app set_def_ty tys
	      | set_def_ty (Types.FunTy (ty1, ty2)) = (set_def_ty ty1; set_def_ty ty2)
	      | set_def_ty (Types.TupleTy tys) = List.app set_def_ty tys

	    fun set_var_defaults () =
		let
		    fun set_def (ref ov) =
			(case ov of
			     AST.Unknown (ty, _) => set_def_ty (TypeUtil.prune ty)
			   | AST.Instance _ => ()
			(* end case *))
		in
		    List.app set_def (!vars)
		end

	    fun set_lit_defaults () = List.app (fn (ref (ty, _)) => set_def_ty ty) (!lits)

	    fun resolve_lists (lits_done, vars_done) =
		if (List.exists try_var (!vars))
		   orelse (List.exists try_lit (!lits))
		then resolve_lists (lits_done, vars_done)
		else (case (lits_done, vars_done) of
			  (false, false) => (set_lit_defaults ();
					     resolve_lists (true, false))
			| (true, false) => (set_var_defaults ();
					    resolve_lists (true, true))
			| _ => raise Fail "failed to resolve overloaded variable"
		     (* end case *))
	in
	    resolve_lists (false, false)
	end

  end
