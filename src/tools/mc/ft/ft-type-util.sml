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

  structure I = InterfaceTypes
  structure N = NestingTreeTypes
  structure R = RepresentationTypes

(* isGround : I.ty -> bool *)
  fun isGround (I.ConTy ([], c)) = true
    | isGround (I.ConTy (ts, _)) = false
    | isGround (I.FunTy _) = false
    | isGround (I.TupleTy _) = false

(* ground : I.ty -> R.ty *)
(* convert a I ground type to an R ground type *)
  local 
    fun tyc (I.Tyc {stamp, name, arity, params, props, def}) =
          R.Tyc {stamp=stamp, name=name, arity=arity, 
		 params=params, props=props, def=tycon_def def}
    and tycon_def (I.AbsTyc) = R.AbsTyc
      | tycon_def (I.DataTyc {nCons, cons}) = 
          R.DataTyc {nCons = ref(!nCons), cons=ref(List.map dcon (!cons))}
    and dcon (I.DCon {id, name, owner, argTy}) =
     (case argTy
        of SOME t => raise Fail "todo: argTy not NONE"
	 | NONE => R.DCon {id=id, name=name, owner=tyc owner, argTy=NONE})
  in
    fun ground (I.ConTy ([], c)) = R.ConTy ([], tyc c)
      | ground t = raise Fail ("not a ground type: " ^ I.toString t)
  end

(* notTuple : R.ty -> bool *)
  fun notTuple (R.TupleTy []) = true (* this is unit, which doesn't count *)
    | notTuple (R.TupleTy rs) = false
    | notTuple _ = true

(* isArrayTycon : R.tycon -> bool *)
  local
    val parrayStamp = TyCon.stampOf (Basis.parrayTyc)
    fun eq s = Stamp.same (parrayStamp, s)
  in
    fun isParrTycI (c as I.Tyc {stamp, ...}) = eq stamp
    fun isParrTycR (c as R.Tyc {stamp, ...}) = eq stamp
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
	| R.ConTy (ts, c) => raise Fail "todo" (* is int option flat? *)
	| R.FunTy (r1, r2) => isFlat r1 andalso isFlat r2
	| R.TupleTy rs => List.all isFlat rs
	| R.FlatArrayTy (r, n) =>
            isFlat r andalso
	    notTuple r andalso
	    notArray r andalso
	    isLf n)
            
end

