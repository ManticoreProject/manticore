(* flatten-ops.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Our flattening transformation includes type-indexed families of operators.
 * All these operators may be statically synthesized out of simple components.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FlattenOps = struct

  structure T = Types

(* explicitly-typed operators *)
  datatype oper
    = ID of T.ty
    | Cat of T.ty
    | Unzip of T.ty
    | Map of oper * T.ty
    | Compose of oper * oper
    | CrossCompose of oper list

(* Two forms of composition: 
 * Compose represents standard sequential composition:
 *   fn (f, g) => (fn x => f (g x))
 * CrossCompose represents what I believe is called "pointwise" composition:
 *   fn (f, g) => (fn (x, y) => (f x, g, y))
 * Note Compose is sensitive to the types of the arguments given to it: 
 *   the range of its second arg must match the domain of the first.
 * CrossCompose, by contrast, produces a meaningful result unconditionally.
 *)

(*
(* lift : ty -> ty *)
  fun lift t = T.FArrayTy (t, T.LfTy)
		
(* catTy : ty * nt_ty -> ty *)
  fun catTy (r, n) = let
    val dom = lift (T.FArrayTy (r, n))
    val rng = T.FArrayTy (r, T.NdTy n)
    in
      T.FunTy (dom, rng)
    end

(* mapTy : ty * ty * nt_ty -> ty *)
  fun mapTy (r, r', n) = 
    T.FunTy (T.FunTy (r, r'), 
	     T.FunTy (T.FArrayTy (r, n), 
		      T.FArrayTy (r', n)))

(* fl : ty -> oper *)
(* Note that the flattener with argument r must be interpreted as
 * the flattener of arrays of type {r; lf}.
 *)
  fun fl (r : T.ty) : oper = let
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
*)

end
