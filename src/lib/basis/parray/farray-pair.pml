(* farray-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basis functions on pairs of f_arrays.
 *)

structure FArrayPair = struct

  structure F = FArray

(* flatMapEq : (('a * 'b) -> 'c) -> 'a f_array * 'b f_array -> 'c f_array *)
(* pre: 'a and 'b are ground types (int, float, etc.) *)
(* pre: the input maps have the exact same shape *)
(*   (stronger condition than same length) *)
(* post: the output will not necessarily be flat; *)
(*   an appropriate flattener should be applied to it *)
(*   (the compiler will insert one) *)
  fun flatMapEq f (F.FArray (data1, shape1), F.FArray (data2, shape2)) = 
    if not (F.sameNT (shape1, shape2)) then
      raise Fail "flatMapEq"
    else let
      val data' = RopePair.fastMapP (f, data1, data2)
      in
        F.FArray (data', shape1)
      end

(* tabulate : int * (int -> ('a * 'b)) -> 'a f_array * 'b f_array *)
  fun tabulate (n, f) = let
    val (data1, data2) = RopePair.tabP (n, f)
    val shape = F.Lf (0, n)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

(* tabFromToStep : int * int * int * (int -> 'a * b) -> 'a f_array * 'b f_array *)
(* lo incl, hi incl *)
  fun tabFromToStep (from, to_, step, f) = let
    val (data1, data2) = RopePair.tabFromToStepP (from, to_, step, f)
    val shape = F.Lf (0, Rope.length data1)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

end
