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

(* let ip stand for int * int: *)
(* flatReduce : (ip * ip -> ip) -> ip -> (int_farray * int_farray) -> ip * ip *)
(* pre: the arg arrays are both flat (not nested) *)
  fun flatReduce assocOp zero (arr1, arr2) = (case (F.clean arr1, F.clean arr2)
    of (F.FArray (data1, shape1), F.FArray (data2, shape2)) => let
         val _ = if S.same (shape1, shape2) then () else fail "reduce" "shapes"
         in (case shape1
           of S.Lf (lo, hi) => IntRopePair.reduceP (assocOp, zero, (data1, data2))
	    | S.Nd _ => fail "reduce" "nd"
           (* end case *))
         end
    (* end case *))

end
