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

end
