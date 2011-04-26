(* farray-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basis functions on pairs of f_arrays.
 *)

structure FArrayPair = struct

  structure S = ShapeTree
  structure F = FArray
  structure R = Rope

(* flatMapEq : (('a * 'b) -> 'c) -> 'a f_array * 'b f_array -> 'c f_array *)
(* pre: 'a and 'b are ground types (int, float, etc.) *)
(* pre: the input maps have the exact same shape *)
(*   (stronger condition than same length) *)
(* post: the output will not necessarily be flat; *)
(*   an appropriate flattener should be applied to it *)
(*   (the compiler will insert one) *)
  fun flatMapEq f (F.FArray (data1, shape1), F.FArray (data2, shape2)) = 
    if not (S.same (shape1, shape2)) then
      raise Fail "flatMapEq"
    else let
      val data' = RopePair.fastMapP (f, data1, data2)
      in
        F.FArray (data', shape1)
      end

(* tabulate : int * (int -> ('a * 'b)) -> 'a f_array * 'b f_array *)
  fun tabulate (n, f) = let
    val (data1, data2) = RopePair.tabP (n, f)
    val shape = S.Lf (0, n)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

(* tabFromToStep : int * int * int * (int -> 'a * b) -> 'a f_array * 'b f_array *)
(* lo incl, hi incl *)
  fun tabFromToStep (from, to_, step, f) = let
    val (data1, data2) = RopePair.tabFromToStepP (from, to_, step, f)
    val len = R.length data1
    val _ = if (len = R.length (data2)) then () else (raise Fail "len")
    val shape = S.Lf (0, len)
    in
      (F.FArray (data1, shape), F.FArray (data2, shape))
    end

(* (\* groundReduce : (('a * 'b) * ('a * 'b) -> ('a * 'b)) -> 'a * 'b -> 'a f_array * 'b f_array -> 'a * 'b *\)  *)
(*     fun groundReduce (assocOp : 'a * 'a -> 'a)  *)
(* 		     (zero : 'a)  *)
(* 		     (F.FArray (dataA, shapeA), F.FArray (dataB, shapeB)) = let *)
(*       val _ = if S.same (shapeA, shapeB) then () else (raise Fail "FArrayPair.groundReduce")  *)
(*       in (case shapeA *)
(*         of S.Lf (lo, hi) => let  *)
(*              val F.FArray (dataA', shapeA') = F.clean (F.FArray (dataA, shapeA)) *)
(* 	     val F.FArray (dataB', shapeB') = F.clean (F.FArray (dataB, shapeB)) *)
(*              in  *)
(*                RopePair.reduceP (assocOp, zero, (dataA', dataB')) *)
(* 	     end *)
(* 	 | S.Nd _ => raise Fail "groundReduce: flat array of ground types expected" *)
(*         (\* end case *\)) *)
(*       end *)

end
