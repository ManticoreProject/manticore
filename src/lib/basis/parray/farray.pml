(* farray.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A implementation of nesting trees and flattened arrays in Manticore.
 *)

structure FArray = struct

  val fail = Fail.fail "FArray"

  structure S = Shape
  structure R = Rope

  (* ***** FLATTENED ARRAYS ***** *)  

  datatype 'a farray = FArray of 'a R.rope * S.shape
  
  (* empty : 'a farray *)
    val empty = FArray (R.empty (), S.Lf (0, 1))

  (* dataOf : 'a farray -> 'a rope *)
    fun dataOf (FArray (r, _)) = r

  (* concatRope : 'a farray rope -> 'a farray *)
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
	lp (0, 0, R.empty (), nil)
      end

  (* flatten : 'a farray farray -> 'a farray *)
    fun flatten f = concatRope (dataOf f)

  (* length : 'a farray -> int *)
    fun length (FArray (_, t)) = (case t
      of S.Lf (lo, hi) => hi-lo (* note: hi is excl upper bound, lo is incl lower bound *)
       | S.Nd ts => List.length ts
      (* end case *))

  (* flatSub : 'a farray * int -> 'a *)
    fun flatSub (FArray (data, shape), i) = (case shape
      of S.Lf (lo, hi) => R.sub (data, lo+i)
       | S.Nd ts => fail "flatSub" "Nd"
      (* end case *))

  (* nestedSub : 'a farray * int -> 'a farray *)
  (* the farray returned is one level less deep than the arg *)
    fun nestedSub (FArray (data, shape), i) = (case shape
      of S.Nd ts => FArray (data, List.nth (ts, i))
       | S.Lf _ => fail "nestedSub" "Lf"
      (* end case *))

  (* tab : int * (int -> 'a) -> 'a farray *)
    fun tab (n, f) =
      if n <= 0 then 
        empty
      else let
        val data = R.tab (n, f)
	val shape = S.Lf (0, n)
        in
          FArray (data, shape)
	end

  (* tabFromToStep : int * int * int * (int -> 'a) -> 'a farray *)
    fun tabFromToStep (from, to_, step, f) = let
      val data = R.tabFromToStep (from, to_, step, f)
      val shape = S.Lf (0, R.length data)
      in
	FArray (data, shape)
      end

  (* flatMap : ('a -> 'b) -> 'a farray -> 'b farray *)
  (* pre: 'a is a ground type (int, float, etc.) *)
  (* post: the output will not necessarily be flat; *)
  (*   an appropriate flattener should be applied to it *)
  (*   (the compiler will insert one) *)
    fun flatMap f (FArray (data, shape)) = let
      val data' = R.map f data
      in
        FArray (data', shape)
      end

  (* nestedMap : ('a -> 'b) -> 'a farray -> 'b farray *)
    fun nestedMap f (FArray (data, shape)) = fail "nestedMap" "todo"

  (* fromList : 'a list -> 'a farray *)
    fun fromList xs = (case xs
      of nil => empty
       | _ => let
           val data = R.fromList xs
	   val shape = S.Lf (0, R.length data)
           in
             FArray (data, shape)
           end
      (* end case *))

  (* clean : 'a farray -> 'a farray *)
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
	       fail "clean" "this should never happen"
	   end
      (* end case *))

  (* groundReduce : ('a * 'a -> 'a) -> 'a -> 'a farray -> 'a *) 
    fun groundReduce (assocOp : 'a * 'a -> 'a) (zero : 'a) (FArray (data, shape)) = 
     (case shape
        of S.Lf (lo, hi) => let 
             val FArray (data', shape') = clean (FArray (data, shape))
             in 
               R.reduce assocOp zero data'
	     end
	 | S.Nd _ => fail "groundReduce" "flat array of ground types expected"
        (* end case *))

  (* intRange : int * int * int -> int farray *)
    fun intRange (from, to_, step) = let
      val data = R.range (from, to_, step)
      val shape = S.Lf (0, R.length data)
      in
	FArray (data, shape)
      end     

  (* flatApp : ('a -> unit) -> 'a farray -> unit *)
    fun flatApp f (FArray (data, shape)) = (case shape
      of S.Lf (lo, hi) =>
           (* remember, farrays might carry ballast *)
           if lo = 0 andalso hi = R.length data then 
             R.app f data
	   else
             R.app f (R.fromSeq (R.partialSeq (data, lo, hi)))
       | S.Nd _ => fail "flatApp" "Nd"
      (* end case *))

end
