(* int-farray-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basis functions on pairs of int_farrays.
 *)

structure IntFArrayPair = struct

  val fail = Fail.fail "IntFArrayPair"

  structure S = ShapeTree
  structure F = IntFArray
  structure R = IntRope

(* flatMapEq_int : ((int * int) -> int) -> int_farray * int_farray -> int_farray *)
(* pre: the input ropes have the exact same shape *)
(*   (stronger condition than same length) *)
  fun flatMapEq_int f (F.FArray (data1, shape1), F.FArray (data2, shape2)) = 
    if not (S.same (shape1, shape2)) then
      fail "flatMapEq_int" "shapes differ"
    else let
      val data' = IntRopePair.fastMapP_int f (data1, data2)
      in
        F.FArray (data', shape1)
      end

(* mapEq_intPair : (int * int -> int * int) -> int f * int f -> int f * inf f *)
(* pre: the input ropes have the exact same shape *)
  fun mapEq_intPair f (F.FArray (data1, shape1), F.FArray (data2, shape2)) = 
    if not (S.same (shape1, shape2)) then
      fail "mapEq_intPair" "shapes differ"
    else let
      val (data1', data2') = IntRopePair.mapEq_intPair f (data1, data2)
      in
        (F.FArray (data1', shape1), F.FArray (data2', shape1))
      end
   
(* tabulate : int * (int -> ('a * 'b)) -> 'a f_array * 'b f_array *)
  fun tabulate (n, f) = let
    val (data1, data2) = IntRopePair.tabP (n, f)
    val shape = S.Lf (0, n)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

(* tabFromToStep : int * int * int * (int -> int * int) -> int_farray * int_farray *)
(* lo incl, hi incl *)
  fun tabFromToStep (from, to_, step, f) = let
    val (data1, data2) = IntRopePair.tabFromToStepP (from, to_, step, f)
    val len = R.length data1
    val _ = if (len = R.length data2) then () else fail "tabFromToStep" "len"
    val shape = S.Lf (0, len)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

(* (\* groundReduce : (('a * 'b) * ('a * 'b) -> ('a * 'b)) -> 'a * 'b -> 'a f_array * 'b f_array -> 'a * 'b *\)  *)
(*     fun groundReduce (assocOp : 'a * 'a -> 'a)  *)
(* 		     (zero : 'a)  *)
(* 		     (F.FArray (dataA, shapeA), F.FArray (dataB, shapeB)) = let *)
(*       val _ = if S.same (shapeA, shapeB) then () else fail "groundReduce" "shapes"  *)
(*       in (case shapeA *)
(*         of S.Lf (lo, hi) => let  *)
(*              val F.FArray (dataA', shapeA') = F.clean (F.FArray (dataA, shapeA)) *)
(* 	     val F.FArray (dataB', shapeB') = F.clean (F.FArray (dataB, shapeB)) *)
(*              in  *)
(*                RopePair.reduceP (assocOp, zero, (dataA', dataB')) *)
(* 	     end *)
(* 	 | S.Nd _ => fail "groundReduce" "flat array of ground types expected" *)
(*         (\* end case *\)) *)
(*       end *)

end
