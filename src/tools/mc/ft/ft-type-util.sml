(* ft-type-util.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type utilities for flattening transformation types.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTTypeUtil = struct

  structure F = FLAST
  structure A = Types (* AST types *)
  structure T = FTTypes
  structure R = RepresentationTypes
  structure N = NestingTreeTypes

(* isGround : A.ty -> bool *)
(* FIXME too permissive? *)
  fun isGround (A.ConTy ([], c)) = true
    | isGround _ = false

(* AST to R conversions *)
  fun tycon_AtoR (A.Tyc {stamp, name, arity, params, props, def}) =
   (case def
      of A.AbsTyc => R.Tyc {stamp=stamp, name=name, arity=arity, 
			      params=params, props=props, def=R.AbsTyc}
       | A.DataTyc {nCons, cons} => let
	   val newDef = R.DataTyc {nCons=ref(!nCons), cons=ref []}
	   val newTyc = R.Tyc {stamp=stamp, name=name, arity=arity, 
			       params=params, props=props, def=newDef}
	   fun lp ([], acc) = List.rev acc
	     | lp (c::cs, acc) = let
	         val A.DCon {id, name, owner, argTy} = c
		 val c' = R.DCon {id=id, name=name, owner=newTyc,
				  argTy=Option.map ty_AtoR argTy}
	         in
		   lp (cs, c'::acc)
	         end
	   val cons' = lp (!cons, [])
	   val _ = setCons (newDef, cons')
           in
	     R.Tyc {stamp=stamp, name=name, arity=arity, 
		    params=params, props=props, def=newDef}
	   end)
  and setCons (R.DataTyc {cons, ...}, c) = (cons := c)
    | setCons (R.AbsTyc, _) = raise Fail "setCons (unreachable, supposedly)"
  and ty_AtoR (A.ErrorTy) = raise Fail "ty_AtoR: ErrorTy"
    | ty_AtoR (A.MetaTy _) = raise Fail "ty_AtoR: MetaTy"
    | ty_AtoR (A.VarTy a) = R.VarTy a
    | ty_AtoR (A.ConTy (ts, c)) = R.ConTy (List.map ty_AtoR ts, tycon_AtoR c)
    | ty_AtoR (A.FunTy (t, u)) = R.FunTy (ty_AtoR t, ty_AtoR u)
    | ty_AtoR (A.TupleTy ts) = R.TupleTy (List.map ty_AtoR ts)

(* ground : I.ty -> R.ty *)
(* convert a I ground type to an R ground type *)
  fun ground (A.ConTy ([], c)) = R.ConTy ([], tycon_AtoR c)
    | ground t = raise Fail ("not a ground type: " ^ TypeUtil.toString t)

(* notTuple : R.ty -> bool *)
  fun notTuple (R.TupleTy []) = true (* this is unit, which doesn't count *)
    | notTuple (R.TupleTy rs) = false
    | notTuple _ = true

  local
    val parrayStamp = TyCon.stampOf (Basis.parrayTyc)
    fun eq s = Stamp.same (parrayStamp, s)
  in
    fun isParrTycR (c as R.Tyc {stamp, ...}) = eq stamp
    val parrTycR = tycon_AtoR Basis.parrayTyc
  end

(* notArray : R.ty -> bool *)
  fun notArray r =
    (case r
       of R.ConTy (ts, c) => not (isParrTycR c)
	| _ => true)


(* isLf : N.ty -> bool *)
  val isLf = (fn N.Lf => true | _ => false)

(* isFlat : R.ty -> bool *)
(* see Definition 5.1 in supporting docs *)
  fun isFlat r =
    (case r
       of R.ConTy ([], _) => true (* nullary constructors are flat *)
	| R.ConTy (ts, c) => raise Fail "todo: account for non-nullary constructors" 
            (* is int option flat? we don't flatten datatypes yet. *)
	| R.FunTy (r1, r2) => isFlat r1 andalso isFlat r2
	| R.TupleTy rs => List.all isFlat rs
	| R.FlatArrayTy (r, n) =>
            isFlat r andalso
	    notTuple r andalso
	    notArray r
	| R.VarTy a => raise Fail "todo: account for tyvar")

(* schemeToString : F.ty_scheme -> string *)
  fun schemeToString (s : F.ty_scheme) : string =
   (case s
      of F.TyScheme ([], t) => T.toString t
       | F.TyScheme (vs, t) => let
           val ss = List.map TypeUtil.tyvarToString vs
	   val ss' = String.concatWith "," ss
	   val t' = T.toString t
           in
	     String.concat ["[", ss', "]", t']
	   end
     (* end case *))

(* rangeType : F.ty -> F.ty *)
(*  fun rangeType (T.FunTy (_, r)) = r
    | rangeType _ = raise Fail "rangeType"
  *)          

  fun rangeType _ = raise Fail "rangeType todo"

end

