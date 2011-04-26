(* farray.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A implementation of nesting trees and flattened arrays in Manticore.
 *)

structure FArray = struct

  structure S = ShapeTree
  structure R = Rope

  (* ***** FLATTENED ARRAYS ***** *)  

  datatype 'a f_array = FArray of 'a R.rope * S.shape_tree
  
  (* empty : 'a f_array *)
    val empty = FArray (R.empty, S.Lf (0, 1))

  (* dataOf : 'a f_array -> 'a rope *)
    fun dataOf (FArray (r, _)) = r

  (* concatRope : 'a f_array rope -> 'a f_array *)
    fun concatRope fs = let
      val len = R.length fs
      fun lp (curr, i, data, ts) =
        if curr >= len then
          FArray (data, S.Nd (List.rev ts))
	else let
          val FArray (d, t) = R.sub (fs, curr)
          val data' = R.concat (data, d)
          val t' = S.incrBy i t
          val ts' = t'::ts
          val i' = S.maxIdx t'
          in
            lp (curr+1, i', data', ts')
	  end
      in
	lp (0, 0, R.empty, nil)
      end

  (* flatten : 'a f_array f_array -> 'a f_array *)
    fun flatten f = concatRope (dataOf f)

  (* length : 'a f_array -> int *)
    fun length (FArray (_, t)) = (case t
      of S.Lf (lo, hi) => hi-lo (* note: hi is excl upper bound, lo is incl lower bound *)
       | S.Nd ts => List.length ts
      (* end case *))

  (* flatSub : 'a f_array * int -> 'a *)
    fun flatSub (FArray (data, shape), i) = (case shape
      of S.Lf (lo, hi) => R.sub (data, lo+i)
       | S.Nd ts => raise Fail "flatSub"
      (* end case *))

  (* nestedSub : 'a f_array * int -> 'a f_array *)
  (* the f_array returned is one level less deep than the arg *)
    fun nestedSub (FArray (data, shape), i) = (case shape
      of S.Nd ts => FArray (data, List.nth (ts, i))
       | S.Lf _ => raise Fail "nestedSub"
      (* end case *))

  (* tab : int * (int -> 'a) -> 'a f_array *)
    fun tab (n, f) =
      if n <= 0 then 
        empty
      else let
        val data = R.tabP (n, f)
	val shape = S.Lf (0, n)
        in
          FArray (data, shape)
	end

  (* tabFromToStep : int * int * int * (int -> 'a) -> 'a f_array *)
    fun tabFromToStep (from, to_, step, f) = let
      val data = R.tabFromToStepP (from, to_, step, f)
      val shape = S.Lf (0, R.length data)
      in
	FArray (data, shape)
      end

  (* flatMap : ('a -> 'b) -> 'a f_array -> 'b f_array *)
  (* pre: 'a is a ground type (int, float, etc.) *)
  (* post: the output will not necessarily be flat; *)
  (*   an appropriate flattener should be applied to it *)
  (*   (the compiler will insert one) *)
    fun flatMap f (FArray (data, shape)) = let
      val data' = R.mapP (f, data)
      in
        FArray (data', shape)
      end

  (* nestedMap : ('a -> 'b) -> 'a f_array -> 'b f_array *)
    fun nestedMap f (FArray (data, shape)) = raise Fail "todo"

  (* clean : 'a f_array -> 'a f_array *)
    fun clean (FArray (data, shape)) = (case shape
      of S.Lf (lo, hi) =>
           if lo = 0 andalso hi = R.length data then 
             FArray (data, shape)
	   else let
             (* FIXME this is a slow implementation *)
             val data' = R.fromSeq (R.partialSeq (data, lo, hi))
             in
               FArray (data', S.Lf (0, hi-lo))
             end
       | S.Nd ts => let
           val (lo, hi) = S.span shape
           (* FIXME this is a slow implementation *)
	   val data' = R.fromSeq (R.partialSeq (data, lo, hi))
           in
             if lo = 0 then
               FArray (data', shape)
	     else if lo > 0 then
	       FArray (data', S.incrBy (~lo) shape)
	     else
	       raise Fail "clean: this should never happen"
	   end
      (* end case *))

  (* groundReduce : ('a * 'a -> 'a) -> 'a -> 'a f_array -> 'a *) 
    fun groundReduce (assocOp : 'a * 'a -> 'a) (zero : 'a) (FArray (data, shape)) = 
     (case shape
        of S.Lf (lo, hi) => let 
             val FArray (data', shape') = clean (FArray (data, shape))
             in 
               R.reduceP (assocOp, zero, data')
	     end
	 | S.Nd _ => raise Fail "groundReduce: flat array of ground types expected"
        (* end case *))

  (* intRange : int * int * int -> int f_array *)
    fun intRange (from, to_, step) = let
      val data = R.rangeP (from, to_, step)
      val shape = S.Lf (0, R.length data)
      in
	FArray (data, shape)
      end     

  (* flatApp : ('a -> unit) -> 'a f_array -> unit *)
    fun flatApp f (FArray (data, shape)) = (case shape
      of S.Lf (lo, hi) =>
           (* remember, farrays might carry ballast *)
           if lo = 0 andalso hi = R.length data then 
             R.app (f, data)
	   else
             R.app (f, R.fromSeq (R.partialSeq (data, lo, hi)))
       | S.Nd _ => raise Fail "flatApp"
      (* end case *))

end
