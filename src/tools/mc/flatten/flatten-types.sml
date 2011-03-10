(* flatten-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flatten AST types.
 *
 *)

structure FlattenTypes : sig

  val flattenTy       : FlattenEnv.env -> Types.ty -> Types.ty
  val flattenTyScheme : FlattenEnv.env -> Types.ty_scheme -> Types.ty_scheme

end = struct

  structure T = Types 
  structure B = Basis
  structure U = TypeUtil

  structure FEnv = FlattenEnv

  fun assert msg fact = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  fun basisTyc c = List.exists (fn d => TyCon.same (c, d)) B.primTycs

(* nullary basis tycons are considered ground types *)
  fun isGround (T.ConTy ([], c)) = basisTyc c
    | isGround _ = false

  fun isPArrayTyc c = TyCon.same (B.parrayTyc, c)

  fun flattenTy (env : FEnv.env) (t : T.ty) : T.ty = let
    fun ty T.ErrorTy = T.ErrorTy
      | ty (T.MetaTy m) = raise Fail "TODO"
      | ty (T.VarTy a) = T.VarTy a
      | ty (T.FunTy (t1, t2)) = T.FunTy (ty t1, ty t2)
      | ty (T.TupleTy ts) = T.TupleTy (List.map ty ts)
      | ty (T.FArrayTy (t, n)) = raise Fail "TODO" (* not sure this circ should arise *)
      | ty (T.ConTy (ts, c)) = conTy (ts, c)
    and conTy (ts, c) = 
	 (if isPArrayTyc c then (case ts 
            of [] => raise Fail "conTy: parray tyc has no type args"
	     | [t] => if isGround t then 
		        T.FArrayTy (t, T.LfTy)
		      else (case t
		        of T.FunTy (dom, rng) => let
			     val t' = T.FunTy (ty dom, ty rng)
			     in
			       T.FArrayTy (t', T.LfTy)
			     end
			 | T.TupleTy [] (* unit *) =>
			     T.FArrayTy (T.TupleTy [], T.LfTy)
			 | T.TupleTy ts => let
                             val ts' = List.map (ty o B.parrayTy) ts
			     in
			       T.TupleTy ts'
		             end
			 | T.ConTy (ts', c') =>
			    (if isPArrayTyc c' 
			     then operN (ty t) 
			     else raise Fail ("todo: parray of " ^ TyCon.toString c'))
			 | _ => raise Fail ("?: parray of " ^ U.toString t)
		       (* end case *))
	     | ts => raise Fail "conTy: parray tyc has too many type args"    
	   (* end case *))
	  else (* not a parray *) (case FEnv.findTyc (env, c)
            of NONE => 
                 if U.isDataTyc c then let (* FIXME what if no dcons have args? *)
                   val c' = newFlatTyc env c
                   in
		     T.ConTy (List.map ty ts, c')
	           end
		 else
		   T.ConTy (List.map ty ts, c)
	     | SOME c' => T.ConTy (List.map ty ts, c')
           (* end case *))
	 (* end if *))
    and operN t =
         (case t
	   of T.FArrayTy (t, n) => T.FArrayTy (t, T.NdTy n)
	    | T.TupleTy ts => T.TupleTy (List.map operN ts)
	    | _ => raise Fail ("operN: " ^ U.toString t)
	  (* end case *)) 
    in
      ty t
    end
(* FIXME should nullary tycs map to themselves? *)
(* side effect: the new tyc is inserted into the env *)
  and newFlatTyc (env : FEnv.env) (tyc : T.tycon) : T.tycon = let
	val _ = assert "data tyc" (U.isDataTyc tyc)
        fun underscore a = Atom.atom (Atom.toString a ^ "_")
        val (T.Tyc {name, params, def, ...}) = tyc
        val cons = 
         (case def
	   of T.DataTyc {cons, ...} => !cons
	    | _ => raise Fail "newFlatTyc")
	val name' = underscore name     
        val tyc' = TyCon.newDataTyc (name', params) (* FIXME OK to reuse params? *)
        fun dcon d = let
          val name = DataCon.nameOf d
	  val name' = Atom.atom (name ^ "_")
	  val argTy = DataCon.argTypeOf d
	  val argTy' = Option.map (flattenTy env) argTy
	  val d' = DataCon.new tyc' (name', argTy') 
            (* note: this registers d' with tyc' as a side effect *)
          in
            FEnv.insertDCon (env, d, d')
	  end
        in
          FEnv.insertTyc (env, tyc, tyc'); (* insert new tyc into env *)
          List.app dcon cons; (* insert new dcons into env *)
	  tyc'
        end

  fun flattenTyScheme (env : FEnv.env) (T.TyScheme (vs, t)) = 
    T.TyScheme (vs, flattenTy env t)

end
