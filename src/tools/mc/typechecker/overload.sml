(* overload.sml
 *
 * Utility functions for dealing with overload resolution.
 *
 * TODO: proper error reporting and location information
 *)

structure Overload : sig
    
    val initialize : unit -> unit
    val addVar : AST.overload_var ref -> unit
    val addLit : (Types.ty * Types.ty list) -> unit
    val addEqTy : Types.ty -> unit
						    
    val resolve : unit -> unit
			  
  end = struct

    structure Ty = Types
    structure U = Unify

    val debugFlg = ref false

    val () = List.app (fn ctl => ControlRegistry.register TCControls.registry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	      envName = NONE
	    }) [
	      Controls.control {
		  ctl = debugFlg,
		  name = "overload-debug",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "debug overload resolution"
		}
	    ]

    val vars : AST.overload_var ref list ref = ref []
    val lits : (Ty.ty * Ty.ty list) ref list ref = ref []
    val eqTys : Ty.ty list ref = ref[]
							 
    fun initialize () = (vars := []; lits := [])
    fun addVar ov = vars := (ov :: !vars)
    fun addLit lt = lits := (ref lt :: !lits)
    fun addEqTy ty = (eqTys := ty :: !eqTys)
		     
    fun resolve () = let
	val change = ref false

	fun typeOf x = let val Ty.TyScheme([], ty) = Var.typeOf x in ty end

	fun try_var rc = (case !rc
	       of AST.Unknown(ty, vl) => let
		    fun isOK v = if U.unifiable(ty, typeOf v)
			  then true
			  else (
			    if (!debugFlg)
			      then print(concat["    reject ", Var.toString v, "\n"])
			      else ();
			    change := true; false)
		    in
		      if (!debugFlg)
			then print(concat[
			    "  tr_var {", String.concatWith "," (List.map Var.toString vl),
			    "}\n"
			  ])
			else ();
		      case List.filter isOK vl
		       of [] => raise Fail "type mismatch for variable"
			| [x] => (change := true;
				  U.unify (ty, typeOf x);
				  rc := AST.Instance x;
				  false)
			| vl' => (rc := AST.Unknown (ty, vl');
				  true)
		     (* end case *)
		    end
		| _ => false
	      (* end case *))

	fun try_lit (rc as (ref (_, []))) = false
	  | try_lit (rc as (ref (ty, tl))) = let
	      fun isOK t = if U.unifiable(ty, t)
		    then true
		    else (
		      if (!debugFlg)
			then print(concat["    reject ", TypeUtil.toString t, "\n"])
			else ();
		      change := true; false)
	      in
		if (!debugFlg)
		  then print(concat[
		      "  tr_lit (", TypeUtil.fmt {long=true} ty, ", {",
		      String.concatWith "," (List.map TypeUtil.toString tl), "}\n"
		    ])
		  else ();
		case List.filter isOK tl
		 of [] => raise Fail "type mismatch for literal"
		  | [t] => (U.unify (ty, t); rc := (ty, [t]); false)
		  | tl' => (rc := (ty, tl); true)
		(* end case *)
	      end

	fun default_type Ty.Int = Basis.intTy
	  | default_type Ty.Float = Basis.floatTy
	  | default_type Ty.Num = Basis.intTy
	  | default_type Ty.Order = Basis.intTy
	  | default_type Ty.Eq = raise Fail "failed to resolve overloaded variable"

	fun set_def_ty Ty.ErrorTy = ()
	  | set_def_ty (ty as Ty.MetaTy(Ty.MVar{info, ...})) = (case !info
	       of Ty.UNIV _ => ()
		| Ty.CLASS cl => if not(U.unify (ty, default_type cl))
		    then raise Fail "this should never happen"
		    else ()
		| Ty.INSTANCE ty => set_def_ty ty
	      (* end case *))
	  | set_def_ty (Ty.VarTy _) = ()
	  | set_def_ty (Ty.ConTy(tys, _)) = List.app set_def_ty tys
	  | set_def_ty (Ty.FunTy(ty1, ty2)) = (set_def_ty ty1; set_def_ty ty2)
	  | set_def_ty (Ty.TupleTy tys) = List.app set_def_ty tys

	fun set_var_defaults () = let
	      fun set_def ov = (case !ov
		     of AST.Unknown(ty, _) => set_def_ty (TypeUtil.prune ty)
		      | AST.Instance _ => ()
		    (* end case *))
	      in
		if (!debugFlg)
		  then print "set_var_defaults\n"
		  else ();
		List.app set_def (!vars)
	      end

	fun set_lit_defaults () = (
	      if (!debugFlg)
		then print "set_lit_defaults\n"
		else ();
	      List.app (fn (ref (ty, _)) => set_def_ty ty) (!lits))

      (* check that the instances of equality operations have been resolved to equality
       * Ty.
       *)
	fun checkEqOps () = let
	      fun chk ty = if TypeUtil.eqType ty
		    then ()
		    else raise Fail(TypeUtil.toString ty ^ " is not an equalty type")
	      in
		List.app chk (!eqTys)
	      end

	fun resolve_lists (lits_done, vars_done) = (
	      change := false;
	      if (!debugFlg)
		then print "resolve_lists\n"
		else ();
	      vars := (List.filter try_var (!vars));
	      lits := (List.filter try_lit (!lits));
	      if !change
		then resolve_lists (lits_done, vars_done)
		else if (null (!vars)) andalso (null (!lits))
		       then ()
		       else (case (lits_done, vars_done) of
				 (false, false) => (set_lit_defaults ();
						    resolve_lists (true, false))
			       | (true, false) => (set_var_defaults ();
						   resolve_lists (true, true))
			       | _ => raise Fail "failed to resolve overloaded variable"
			    (* end case *)))
	in
	  resolve_lists (false, false);
	  checkEqOps ()
	end

    val resolve = BasicControl.mkTracePassSimple {passName = "resolve", pass = resolve}

  end
