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
  structure T = Types (* AST types *)
  structure R = FTReprTypes
  structure N = NestingTreeTypes
		
(* apply a type variable to type substitution to a type *)
  fun applySubst (subst : R.ty TyVar.Map.map, ty0 : R.ty) : R.ty = let
    fun inst ty = 
     (case ty of 
          R.VarTy tv => (case TyVar.Map.find (subst, tv)
			   of NONE => ty
			    | SOME ty => ty
			  (* end case *))
	| R.ConTy (args, tyc) => R.ConTy (List.map inst args, tyc)
	| R.FunTy (ty1, ty2) => R.FunTy (inst ty1, inst ty2)
	| R.TupleTy tys => R.TupleTy (List.map inst tys)
	| R.FlatArrayTy (t, n) => R.FlatArrayTy (inst t, n)
      (* end case *))
    in
      inst ty0
    end

(* apply a type-variable-to-type substitution to a type.  The substitution
 * is represented as a list of type variable/type pairs.
 *)
  fun substitute (ty : R.ty, s : (T.tyvar * R.ty) list) : R.ty = 
   (case s of
	[] => ty
      | _ => applySubst (List.foldl TyVar.Map.insert' TyVar.Map.empty s, ty)
    (* end case *))

(* instantiate a type scheme to a specific list of types.  We assume that
 * the types have the correct kinds for the corresponding type variables.
 *)
  fun apply (F.TyScheme ([], ty), []) = ty
    | apply (sigma as F.TyScheme (tvs, ty), tys) = let
        fun ins (tv, ty, s) = TyVar.Map.insert (s, tv, ty)
        in
	  applySubst (ListPair.foldlEq ins TyVar.Map.empty (tvs, tys), ty)
        end
handle ex => (print(concat["apply(", schemeToString(F.TyScheme(tvs, ty)), ", [",
String.concatWith "," (List.map R.toString tys), "])\n"]); raise ex)

  and schemeToString _ = raise Fail "todo: copy from TypeUtil"

(* (\* *)
(*   val trTy = TranslateTypesFT.trTy *)
(*   val trTycon = TranslateTypesFT.trTycon  *)
(* *\) *)

(* (\* isGround : F.repr_ty -> bool *\) *)
(* (\* FIXME too permissive *\) *)
(*   fun isGround (T.ConTy ([], c)) = true *)
(*     | isGround _ = false *)

(* (\* *)
(* (\* ground : A.ty -> T.ty *\) *)
(* (\* convert an AST ground type to an R ground type *\) *)
(*   fun ground (A.ConTy ([], c)) = T.ConTy ([], trTycon c) *)
(*     | ground t = raise Fail ("not a ground type: " ^ TypeUtil.toString t) *)
(* *\) *)

(* (\* notTuple : T.repr_ty -> bool *\) *)
(*   fun notTuple r = *)
(*    (case r  *)
(*       of (T.TupleTy []) => true (\* this is unit, which doesn't count *\) *)
(*        | (T.TupleTy _) => false *)
(*        | _ => true *)
(*      (\* end case *\)) *)

(* (\* *)
(*   local *)
(*     val parrayStamp = TyCon.stampOf (Basis.parrayTyc) *)
(*     fun eq s = Stamp.same (parrayStamp, s) *)
(*   in *)
(*     fun isParrTycR (c as T.Tyc {stamp, ...}) = eq stamp *)
(*     val parrTycR = trTycon Basis.parrayTyc *)
(*     fun parrayTy (r : T.repr_ty) : T.repr_ty = T.ConTy ([r], parrTycR) *)
(*   end *)

(* (\* notArray : T.ty -> bool *\) *)
(*   fun notArray r = *)
(*     (case r *)
(*        of T.ConTy (ts, c) => not (isParrTycR c) *)
(* 	| _ => true *)
(*       (\* end case *\)) *)
(* *\) *)

(* (\* *)
(* (\* isLf : N.ty -> bool *\) *)
(*   val isLf = (fn N.Lf => true | _ => false) *)

(* (\* isFlat : T.ty -> bool *\) *)
(* (\* see Definition 5.1 in supporting docs *\) *)
(*   fun isFlat (T.IR (t, r)) = let *)
(*     fun repr r = *)
(*      (case r *)
(*         of T.ConTy ([], _) => true (\* nullary constructors are flat *\) *)
(* 	 | T.ConTy (ts, c) => raise Fail "todo: account for non-nullary constructors"  *)
(*              (\* is int option flat? we don't flatten datatypes yet. *\) *)
(* 	 | T.FunTy (r1, r2) => repr r1 andalso repr r2 *)
(* 	 | T.TupleTy rs => List.all repr rs *)
(* 	 | T.FlatArrayTy (r, n) => *)
(*              repr r andalso *)
(* 	     notTuple r andalso *)
(* 	     notArray r *)
(* 	 | T.VarTy a => raise Fail "todo: account for tyvar") *)
(*     in  *)
(*       repr r *)
(*     end *)
(* *\) *)

(* (\* schemeToString : F.ty_scheme -> string *\) *)
(*   fun schemeToString (s : F.ty_scheme) : string = *)
(*    (case s *)
(*       of F.TyScheme ([], t) => T.toString t *)
(*        | F.TyScheme (vs, t) => let *)
(*            val ss = List.map TypeUtil.tyvarToString vs *)
(* 	   val ss' = String.concatWith "," ss *)
(* 	   val t' = T.toString t *)
(*            in *)
(* 	     String.concat ["[", ss', "]", t'] *)
(* 	   end *)
(*      (\* end case *\)) *)

(* (\* domainOf : F.repr_ty -> F.repr_ty *\) *)
(*   fun domainOf (T.FunTy (d, _)) = d *)
(*     | domainOf _ = raise Fail "domainOf" *)

(* (\* rangeOf : F.repr_ty -> F.repr_ty *\) *)
(*   fun rangeOf (T.FunTy (_, r)) = r *)
(*     | rangeOf _ = raise Fail "rangeOf" *)

(* (\* unzipF : F.repr_ty list -> F.repr_ty list * F.repr_ty list *\) *)
(* (\* unzip function types into domains and ranges *\) *)
(* (\* raises and exception if a non-function type is found *\) *)
(*   fun unzipF ts = let *)
(*     fun lp ([], doms, rngs) = (doms, rngs) *)
(*       | lp (h::t, doms, rngs) =  *)
(*          (case h of *)
(* 	      T.FunTy (d, r) => lp (t, d::doms, r::rngs) *)
(* 	    | _ => raise Fail "unzipF") *)
(*     in *)
(*       lp (List.rev ts, [], []) *)
(*     end *)

end

