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
  structure T = FTTypes
  structure N = NestingTreeTypes

  structure FEnv = FlattenEnv
		
  val toString : T.ty -> string = T.toString

  fun tyvarToString (Types.TVar{name, stamp, ...}) = Atom.toString name

  fun fmtScheme {long} (T.TyScheme ([], ty)) = toString ty
    | fmtScheme {long} (T.TyScheme (tvs, ty)) = concat[
        "[", String.concatWith "," (List.map tyvarToString tvs), "]", toString ty
      ]  

  val schemeToString : T.ty_scheme -> string = fmtScheme {long=false}

(* apply a type variable to type substitution to a type *)
  fun applySubst (subst : T.ty TyVar.Map.map, ty0 : T.ty) : T.ty = let
    fun inst ty = 
     (case ty of 
          T.VarTy tv => (case TyVar.Map.find (subst, tv)
			   of NONE => ty
			    | SOME ty => ty
			  (* end case *))
	| T.ConTy (args, tyc) => T.ConTy (List.map inst args, tyc)
	| T.FunTy (ty1, ty2) => T.FunTy (inst ty1, inst ty2)
	| T.TupleTy (i, tys) => T.TupleTy (i, List.map inst tys)
	| T.FlatArrayTy (t, n) => T.FlatArrayTy (inst t, n)
      (* end case *))
    in
      inst ty0
    end

(* apply a type-variable-to-type substitution to a type.  The substitution
 * is represented as a list of type variable/type pairs.
 *)
  fun substitute (ty : T.ty, s : (AST.tyvar * T.ty) list) : T.ty = 
   (case s of
	[] => ty
      | _ => applySubst (List.foldl TyVar.Map.insert' TyVar.Map.empty s, ty)
    (* end case *))

(* instantiate a type scheme to a specific list of types.  We assume that
 * the types have the correct kinds for the corresponding type variables.
 *)
  fun apply (T.TyScheme ([], ty), []) = ty
    | apply (sigma as T.TyScheme (tvs, ty), tys) = let
        fun ins (tv, ty, s) = TyVar.Map.insert (s, tv, ty)
        in
	  applySubst (ListPair.foldlEq ins TyVar.Map.empty (tvs, tys), ty)
        end
handle ex => (print(concat["apply(", schemeToString(F.TyScheme(tvs, ty)), ", [",
String.concatWith "," (List.map T.toString tys), "])\n"]); raise ex)

  val same : T.ty * T.ty -> bool = T.same

  fun parrayTy (r : T.ty) : T.ty = raise Fail "todo"

  fun interfaceTy (t : T.ty) : AST.ty = 
   (case t 
      of T.VarTy a => AST.VarTy a
       | T.ConTy (ts, c) => let
           val T.Tyc {interface=i, ...} = c
           in
             AST.ConTy (List.map interfaceTy ts, i)
	   end
       | T.FunTy (t, u) => AST.FunTy (interfaceTy t, interfaceTy u)
       | T.TupleTy (i, _) => i
       | T.FlatArrayTy (t, n) => farr (t, n)
     (* end case *))
(* we compute the interface ty of a flat array "backwards" 
 * from its representation ty. Note: if the flat array is inside
 * a tuple, its interface type will never be calculated, since 
 * tuple interface types do not consult the types of their 
 * components. *)
  and farr (t : T.ty, n : N.ty) : AST.ty = let
     val i = interfaceTy t  
     fun lp (N.Lf, acc) = Basis.parrayTy acc
       | lp (N.Nd n, acc) = lp (n, Basis.parrayTy acc)
     in
       lp (n, i)
     end

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

