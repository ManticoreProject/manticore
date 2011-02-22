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
  structure N = NestingTreeTypes

  val trTy = TranslateTypesFT.trTy
  val trTycon = TranslateTypesFT.trTycon 

(* isGround : F.repr_ty -> bool *)
(* FIXME too permissive *)
  fun isGround (T.ConTy ([], c)) = true
    | isGround _ = false

(* ground : A.ty -> T.ty *)
(* convert an AST ground type to an R ground type *)
  fun ground (A.ConTy ([], c)) = T.ConTy ([], trTycon c)
    | ground t = raise Fail ("not a ground type: " ^ TypeUtil.toString t)

(* notTuple : T.repr_ty -> bool *)
  fun notTuple r =
   (case r 
      of (T.TupleTy []) => true (* this is unit, which doesn't count *)
       | (T.TupleTy _) => false
       | _ => true
     (* end case *))

  local
    val parrayStamp = TyCon.stampOf (Basis.parrayTyc)
    fun eq s = Stamp.same (parrayStamp, s)
  in
    fun isParrTycR (c as T.Tyc {stamp, ...}) = eq stamp
    val parrTycR = trTycon Basis.parrayTyc
    fun parrayTy (r : T.repr_ty) : T.repr_ty = T.ConTy ([r], parrTycR)
  end

(* notArray : T.ty -> bool *)
  fun notArray r =
    (case r
       of T.ConTy (ts, c) => not (isParrTycR c)
	| _ => true
      (* end case *))

(* isLf : N.ty -> bool *)
  val isLf = (fn N.Lf => true | _ => false)

(* isFlat : T.ty -> bool *)
(* see Definition 5.1 in supporting docs *)
  fun isFlat (T.IR (t, r)) = let
    fun repr r =
     (case r
        of T.ConTy ([], _) => true (* nullary constructors are flat *)
	 | T.ConTy (ts, c) => raise Fail "todo: account for non-nullary constructors" 
             (* is int option flat? we don't flatten datatypes yet. *)
	 | T.FunTy (r1, r2) => repr r1 andalso repr r2
	 | T.TupleTy rs => List.all repr rs
	 | T.FlatArrayTy (r, n) =>
             repr r andalso
	     notTuple r andalso
	     notArray r
	 | T.VarTy a => raise Fail "todo: account for tyvar")
    in 
      repr r
    end

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

(* domainOf : F.repr_ty -> F.repr_ty *)
  fun domainOf (T.FunTy (d, _)) = d
    | domainOf _ = raise Fail "domainOf"

(* rangeOf : F.repr_ty -> F.repr_ty *)
  fun rangeOf (T.FunTy (_, r)) = r
    | rangeOf _ = raise Fail "rangeOf"

(* unzipF : F.repr_ty list -> F.repr_ty list * F.repr_ty list *)
(* unzip function types into domains and ranges *)
(* raises and exception if a non-function type is found *)
  fun unzipF ts = let
    fun lp ([], doms, rngs) = (doms, rngs)
      | lp (h::t, doms, rngs) = 
         (case h of
	      T.FunTy (d, r) => lp (t, d::doms, r::rngs)
	    | _ => raise Fail "unzipF")
    in
      lp (List.rev ts, [], [])
    end

end

