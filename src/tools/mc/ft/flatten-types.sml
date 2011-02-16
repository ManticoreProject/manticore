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

  structure T = Types (* AST types *)
  structure N = NestingTreeTypes
  structure R = RepresentationTypes
  structure F = FTTypes
  structure U = FTTypeUtil
  
(* isParrTyc : T.tycon -> bool *)
  fun isParrTyc c = TyCon.same (c, Basis.parrayTyc)

(* isParr : T.ty -> bool *)
  fun isParr (T.ConTy (_, c)) = isParrTyc c
    | isParr _ = false

(* isParrG : T.ty -> bool *)
(* tests whether argument is an array of a ground type *)
  fun isParrG (T.ConTy ([t], c)) = 
        isParrTyc c andalso U.isGround t
    | isParrG _ = false

(* flatten : T.ty -> R.ty *)
  fun flatten (t : T.ty) : R.ty =
   (case t
      of T.ConTy (ts, c) =>
	   if U.isGround t then 
	     U.ground t
	   else if isParrG t then let
             val g = 
              (case t 
		 of T.ConTy ([u], _) => u
		  | _ => raise Fail "bug - should be unreachable")
	     in
               R.FlatArrayTy (U.ground g, N.Lf)
             end
	   else if isParrTyc c then
            (case ts
               of [T.FunTy (td, tr)] =>
		    R.FlatArrayTy (R.FunTy (flatten td, flatten tr), N.Lf)
		| [T.TupleTy ys] => let
		    fun parr t = Basis.parrayTy t
                    val ys' = List.map (flatten o parr) ys
                    in
		      R.TupleTy ys'
		    end
		| [T.ConTy (ts', c')] =>
                    if isParrTyc c' then let
                      val rho = flatten (T.ConTy (ts', c'))
                      in
                        operN rho
	              end
		    else 
                      raise Fail "todo"
		| _ => raise Fail "todo")
	   else raise Fail "todo"
       | T.FunTy (t, u) => R.FunTy (flatten t, flatten u)
       | T.TupleTy ts => R.TupleTy (List.map flatten ts)
       | T.VarTy a => R.VarTy a
       | T.ErrorTy => raise Fail "ErrorTy"
       | T.MetaTy _ => raise Fail "MetaTy"
     (* end case *))
(* operN is the N operator from Fig. 5 *)
  and operN (R.FlatArrayTy (r, n)) = R.FlatArrayTy (r, N.Nd n)
    | operN (R.TupleTy rs) = R.TupleTy (List.map operN rs)
    | operN t = raise Fail ("bug: trying to apply N to " ^ R.toString t)

end
