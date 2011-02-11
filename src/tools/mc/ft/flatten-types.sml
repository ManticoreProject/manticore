(* flatten-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flatten FT types. 
 * See figure 6 in the docs.
 * docs in /path/to/manti-papers/papers/notes/amsft/
 *)

structure FlattenTypes = struct

  structure I = InterfaceTypes
  structure N = NestingTreeTypes
  structure R = RepresentationTypes
  structure F = FTTypes
  structure U = FTTypeUtil
  
(* isParr : I.ty -> bool *)
  fun isParr (I.ConTy (_, c)) = U.isParrTycI c
    | isParr _ = false

(* isParrG : I.ty -> bool *)
(* tests whether argument is an array of a ground type *)
  fun isParrG (I.ConTy ([t], c)) = 
        U.isParrTycI c andalso U.isGround t
    | isParrG _ = false

(* flatten : I.ty -> R.ty *)
  fun flatten (t : I.ty) : R.ty =
   (case t
      of I.ConTy (ts, c) =>
	   if U.isGround t then 
	     U.ground t
	   else if isParrG t then let
             val g = 
              (case t 
		 of I.ConTy ([u], _) => u
		  | _ => raise Fail "bug - should be unreachable")
	     in
               R.FlatArrayTy (U.ground g, N.Lf)
             end
	   else if U.isParrTycI c then
            (case ts
               of [I.FunTy (td, tr)] =>
		    R.FlatArrayTy (R.FunTy (flatten td, flatten tr), N.Lf)
		| [I.TupleTy ys] => let
		    fun parr t = I.ConTy ([t], U.parrTycI)
                    val ys' = List.map (flatten o parr) ys
                    in
		      R.TupleTy ys'
		    end
		| [I.ConTy (ts', c')] =>
                    if U.isParrTycI c' then let
                      val rho = flatten (I.ConTy (ts', c'))
                      in
                        flattenN rho
	              end
		    else 
                      raise Fail "todo"
		| _ => raise Fail "todo")
	   else raise Fail "todo"
       | I.FunTy (t, u) => R.FunTy (flatten t, flatten u)
       | I.TupleTy ts => R.TupleTy (List.map flatten ts)
       | I.VarTy a => raise Fail "todo: account for tyvar in flattening")
(* flattenN is the N operator from Fig. 5 *)
  and flattenN (R.FlatArrayTy (r, n)) = R.FlatArrayTy (r, N.Nd n)
    | flattenN (R.TupleTy rs) = R.TupleTy (List.map flattenN rs)
    | flattenN t = raise Fail ("bug: trying to apply N to " ^ R.toString t)

end
