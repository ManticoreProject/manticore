(* match-comp-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Simple environments for compiling matches.
 *)

structure MatchCompEnv : sig

    type env

    val new : Error.err_stream -> env

  (* if the variable is in the domain of the environment, then return
   * what it is renamed as, otherwise return the variable itself.
   *)
    val apply : (env * Var.var) -> Var.var

    val insert : (env * Var.var * Var.var) -> env

    val errStrm : env -> Error.err_stream

  (* return a fresh variable with the same type as the argument and
   * add a renaming to the environment.
   *)
    val rename : (env * Var.var) -> (Var.var * env)
    val renameList : (env * Var.var list) -> (Var.var list * env)

  end = struct

    structure VMap = Var.Map

    datatype env = E of {
	subst : Var.var VMap.map,
	errStrm : Error.err_stream
      }

    fun new errStrm = E{subst = VMap.empty, errStrm = errStrm}

    fun errStrm (E{errStrm, ...}) = errStrm

    fun apply (E{subst, ...}, v) = (case VMap.find(subst, v)
	   of NONE => v
	    | SOME v' => v'
	  (* end case *))

    fun insert (E{errStrm, subst}, v, v') =
	  E{errStrm = errStrm, subst = VMap.insert(subst, v, v')}

    fun rename (env, v) = let
	  val v' = Var.copy v
	  in
	    (v', insert (env, v, v'))
	  end

    fun renameList (E{errStrm, subst}, vs) = let
	  fun f (x, (vs, env)) = let
		val x' = Var.copy x
		in
		  (x'::vs, VMap.insert(env, x, x'))
		end
	  val (vs', subst') = List.foldr f ([], subst) vs
	  in
	    (vs', E{errStrm = errStrm, subst = subst'})
	  end

  end
