(* ft-synth-ops.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Our flattening transformation includes type-indexed families of operators.
 * All these operators may be statically synthesized out of simple components.
 * This module performs the synthesis.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTSynthOps = struct

  structure F = FLAST
  structure T = FTTypes
  structure N = NestingTreeTypes
  structure U = FTTypeUtil

(* explicitly-typed operators *)
  datatype oper
    = ID of T.repr_ty
    | Cat of T.repr_ty
    | Map of oper * T.repr_ty
    | Unzip of T.repr_ty
    | Compose of oper * oper
    | CrossCompose of oper list

(* Two forms of composition: 
 * Compose represents standard sequential composition:
 *   fn (f, g) => (fn x => f (g x))
 * CrossCompose represents what I believe is called "pointwise" composition:
 *   fn (f, g) => (fn (x, y) => (f x, g, y))
 * Note Compose is sensitive to the types of the arguments given to it: 
 *   the domain of its 2nd arg must match the range of the first.
 * CrossCompose, by contrast, produces a meaningful result unconditionally.
 *)

(* lift : repr_ty -> repr_ty *)
  fun lift t = T.FlatArrayTy (t, N.Lf)
		
(* catTy : repr_ty * N.ty -> repr_ty *)
  fun catTy (r, n) = let
    val dom = lift (T.FlatArrayTy (r, n))
    val rng = T.FlatArrayTy (r, N.Nd n)
    in
      T.FunTy (dom, rng)
    end

(* mapTy : repr_ty * repr_ty * N.ty -> repr_ty *)
  fun mapTy (r, r', n) = 
    T.FunTy (T.FunTy (r, r'), 
	     T.FunTy (T.FlatArrayTy (r, n), 
		      T.FlatArrayTy (r', n)))

(* fl : repr_ty -> oper *)
(* Note that the flattener with argument r must be interpreted as
 * the flattener of arrays of type {r; lf}.
 *)
  fun fl (r : T.repr_ty) : oper = let
    val repr = FlattenTypes.repr
    val arr  = lift r
    val farr = repr arr
    val f =
     (case r
        of T.FunTy (r1, r2) => ID (T.FunTy (farr, farr))
	 | T.TupleTy ts => let
	     val unzipDom = arr
	     val unzipRng = T.TupleTy (List.map (repr o lift) ts)
             val unzip = Unzip (T.FunTy (unzipDom, unzipRng))
             in
	       Compose (CrossCompose (List.map fl ts), unzip)
	     end
	 | T.FlatArrayTy (t', n) => let
             val cat = Cat (catTy (t', n))
	     val map = Map (fl t', mapTy (arr, farr, n))
             in
	       Compose (cat, map)
	     end
	 | T.ConTy (ts, c) => 
	     if U.isGround r then 
               ID (T.FunTy (farr, farr))
	     else 
	       raise Fail "todo"
	 | T.VarTy a => raise Fail "todo")
    in
      f
    end

(* typeOfOper : oper -> T.repr_ty *)
  fun typeOfOper (q : oper) : T.repr_ty = 
   (case q of 
	ID t => t
      | Cat t => t
      | Map (_, t) => t
      | Unzip t => t
      | Compose (q1, q2) => let
	  val t1 = typeOfOper q1
	  val t2 = typeOfOper q2
	  in
	    (* throw in a check here... *)
	    if (T.reprSame (U.domainOf t1, U.rangeOf t2)) then
              T.FunTy (U.domainOf t2, U.rangeOf t1)
	    else
              raise Fail "typeOfOper: compose mismatch"
          end
      | CrossCompose qs => let
	  (* no checking needed in this kind of composition *)
	  val (ds, rs) = (U.unzipF o List.map typeOfOper) qs
          in
	    T.FunTy (T.TupleTy ds, T.TupleTy rs)
	  end
    (* end case *))
   
(* operToCode : oper * T.repr_ty -> F.exp *)
  fun operToCode (x : oper, t : T.repr_ty) : F.exp = raise Fail "todo"

end
