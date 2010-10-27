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

(* notTuple : R.ty -> bool *)
  fun notTuple (R.TupleTy []) = true (* this is unit, which doesn't count *)
    | notTuple (R.TupleTy rs) = false
    | notTuple _ = true

(* notArray : R.ty -> bool *)
  fun notArray r =
    (case r
       of R.ConTy _ => (* TODO *) raise Fail "todo"
	| _ => true)

(* isLf : N.ty -> bool *)
  val isLf = (fn N.Lf => true | _ => false)

(* isFlat : R.ty -> bool *)
(* see Definition 5.1 in supporting docs *)
  fun isFlat r =
    (case r
       of R.ConTy ([], _) => true (* nullary constructors are flat *)
	| R.ConTy _ => raise Fail "todo" (* TODO *)
	| R.FunTy (r1, r2) => isFlat r1 andalso isFlat r2
	| R.TupleTy rs => List.all isFlat rs
	| R.FlatArrayTy (r, n) =>
            isFlat r andalso
	    notTuple r andalso
	    notArray r andalso
	    isLf n)
            
end

