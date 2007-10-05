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

    val debugFlg = ref false

    val () = List.app (fn ctl => ControlRegistry.register MinmantControls.registry {
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
    val lits : (Types.ty * Types.ty list) ref list ref = ref []
							 
    fun initialize () = (vars := []; lits := [])
    fun add_var ov = vars := (ov :: !vars)
    fun add_lit lt = lits := (lt :: !lits)
		     
    fun resolve () = let
	val change = ref false
	    
	fun typeOf x = let val Types.TyScheme([], ty) = Var.typeOf x in ty end
    
	fun try_var rc = (case !rc
	       of AST.Unknown(ty, vl) => let
		    fun isOK v = if U.unifiable(ty, typeOf v)
			  then true
			  else (
			    if (!debugFlg)
			      then print(concat["  reject ", Var.toString v, "\n"])
			      else ();
			    change := true; false)
		    in
		      if (!debugFlg)
			then print(concat[
			    "tr_var {", String.concatWith "," (List.map Var.toString vl),
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
	  | try_lit (rc as (ref (ty, tl))) =
	    (case List.filter (fn t => if U.unifiable (ty, t)
				       then true
				       else (change := true; false))
			      tl of
		 [] => raise Fail "type mismatch for literal"
	       | [t] => (U.unify (ty, t); rc := (ty, [t]); false)
	       | tl' => (rc := (ty, tl); true)
	    (* end case *))
					     
	fun default_type Types.Int = Basis.intTy
	  | default_type Types.Float = Basis.floatTy
	  | default_type Types.Num = Basis.intTy
	  | default_type Types.Order = Basis.intTy
	  | default_type Types.Eq = raise Fail "failed to resolve overloaded variable"
					  
	fun set_def_ty Types.ErrorTy = ()
	  | set_def_ty (Types.MetaTy _) = ()
	  | set_def_ty (Types.VarTy _) = ()
	  | set_def_ty (ty as Types.ClassTy(Types.Class (ref info))) = (
	      case info
	       of Types.CLASS cl => if not(U.unify (ty, default_type cl))
		    then raise Fail "this should never happen"
		    else ()
		| _ => ()
	      (* end case *))
	  | set_def_ty (Types.ConTy(tys, _)) = List.app set_def_ty tys
	  | set_def_ty (Types.FunTy(ty1, ty2)) = (set_def_ty ty1; set_def_ty ty2)
	  | set_def_ty (Types.TupleTy tys) = List.app set_def_ty tys
					     
	fun set_var_defaults () = let
	      fun set_def ov = (case !ov
		     of AST.Unknown(ty, _) => set_def_ty (TypeUtil.prune ty)
		      | AST.Instance _ => ()
		    (* end case *))
	      in
		List.app set_def (!vars)
	      end
	    
	fun set_lit_defaults () = List.app (fn (ref (ty, _)) => set_def_ty ty) (!lits)
	    
	fun resolve_lists (lits_done, vars_done) = (
	      change := false;
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
	  resolve_lists (false, false)
	end

    val resolve = BasicControl.mkTracePassSimple {passName = "resolve", pass = resolve}

  end
