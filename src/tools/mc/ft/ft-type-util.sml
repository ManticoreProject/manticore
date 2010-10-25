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

  structure T = FTTypes

(* notTuple : T.repr_ty -> bool *)
  fun notTuple (T.R_TupleTy []) = true (* this is unit, which doesn't count *)
    | notTuple (T.R_TupleTy rs) = false
    | notTuple _ = true

(* notArray : T.repr_ty -> bool *)
  fun notArray r =
    (case r
       of T.R_ConTy _ => (* TODO *) raise Fail "todo"
	| _ => true)

(* isLf : T.nt_ty -> bool *)
  val isLf = (fn T.Lf => true | _ => false)

(* isFlat : T.repr_ty -> bool *)
(* see Definition 5.1 in supporting docs *)
  fun isFlat r =
    (case r
       of T.R_ConTy ([], _) => true (* nullary constructors are flat *)
	| T.R_ConTy _ => raise Fail "todo" (* TODO *)
	| T.R_FunTy (r1, r2) => isFlat r1 andalso isFlat r2
	| T.R_TupleTy rs => List.all isFlat rs
	| T.R_FlatArrayTy (r, n) =>
            isFlat r andalso
	    notTuple r andalso
	    notArray r andalso
	    isLf n)
            
end

