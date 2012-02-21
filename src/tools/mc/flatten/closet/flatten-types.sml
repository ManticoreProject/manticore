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

  structure F = FTTypes (* flattening trx types *)
  structure N = NestingTreeTypes
  structure F = FTTypes
  structure U = FTTypeUtil
   
(* isParrTyc : F.tycon -> bool *)
  fun isParrTyc c = U.isParrTycR c				 

(* isParr : F.ty -> bool *)
  fun isParr (F.ConTy (_, c)) = isParrTyc c
    | isParr _ = false

(* isParrG : F.ty -> bool *)
(* tests whether argument is an array of a ground type *)
  fun isParrG (F.ConTy ([t], c)) = 
        isParrTyc c andalso U.isGround t
    | isParrG _ = false

(* repr : F.repr_ty -> F.repr_ty *)
  fun repr r = 
    if U.isGround r then r
    else
     (case r
        of F.ConTy (ts, c) =>
	     if isParrG r then let
               val g = (case r of F.ConTy ([g], _) => g
				| _ => raise Fail "g")
	       in
                 F.FlatArrayTy (g, N.Lf)
	       end
	     else if isParrTyc c then
              (case ts
		 of [F.FunTy (td, tr)] => let
	              val td' = repr td
		      val tr' = repr tr
		      in
		        F.FlatArrayTy (F.FunTy (td', tr'), N.Lf)
		      end
		  | [F.TupleTy ys] => let
	              val ys' = List.map (repr o U.parrayTy) ys
		      in
		        F.TupleTy ys'
		      end
		  | [ct as F.ConTy (ts', c')] =>
		      if isParrTyc c' then operN ct
		      else raise Fail "todo"
		  | _ => raise Fail "todo"
	        (* end case *))
	     else 
		 raise Fail "todo"
	 | F.FunTy (t, u) => F.FunTy (repr t, repr u)
	 | F.TupleTy ts => F.TupleTy (List.map repr ts)
	 | F.VarTy a => F.VarTy a
	 | F.FlatArrayTy (t, N.Lf) =>
            (case t
	       of F.VarTy _ => raise Fail "todo"
		| F.ConTy ([], g) => F.FlatArrayTy (t, N.Lf)
		| F.ConTy _ => raise Fail "todo"
		| F.FunTy (r1, r2) => F.FlatArrayTy (F.FunTy (repr r1, repr r2), N.Lf)
		| F.TupleTy rs => 
                    F.TupleTy (List.map (repr o (fn r => F.FlatArrayTy (r, N.Lf))) rs)
		| F.FlatArrayTy (t', n') => operN (repr (F.FlatArrayTy (t', n')))
	      (* end case *))
	 | F.FlatArrayTy (t, N.Nd n) => raise Fail "repr"
       (* end case *))

(* operN is the N operator from Fig. 5 *)
  and operN (r : F.repr_ty) : F.repr_ty =
   (case r 
      of F.FlatArrayTy (t, n) => F.FlatArrayTy (t, N.Nd n)
       | F.TupleTy rs => F.TupleTy (List.map operN rs)
       | _ => raise Fail "operN"
     (* end case *))

(* flatten : F.ty -> F.ty *)
  fun flatten (t as F.IR (i, r)) : F.ty = F.IR (i, repr r)

end