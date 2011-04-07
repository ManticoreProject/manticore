(* farray.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A implementation of nesting trees and flattened arrays in Manticore.
 *)

structure FArray = struct

  (* ***** NESTING TREES ***** *)

  (* The nesting tree datatype and some basic operations. *)
    datatype nesting_tree
      = Lf of int * int (* lower bound inclusive, upper bound exclusive *)
      | Nd of nesting_tree list

  (* sameNT : nesting_tree * nesting_tree -> bool *)
    fun sameNT (t1, t2) = (case (t1, t2)
      of (Lf (lo1, hi1), Lf (lo2, hi2)) => (lo1 = lo2) andalso (hi1 = hi2)
       | (Nd ts1, Nd ts2) => ListPair.allEq sameNT (ts1, ts2)
       | _ => false
      (* end case *))

  (* minIdx : nesting_tree -> int *)
    fun minIdx nt = (case nt 
      of Lf (i, _) => i
       | Nd ts => minIdx (List.hd ts)
      (* end case *))

  (* maxIdx : nesting_tree -> int *)
    fun maxIdx nt = (case nt
      of Lf (_, i) => i
       | Nd ts => let
           fun lp ts = (case ts
             of t::nil => maxIdx t
	      | _::tl  => lp tl
	      | nil => raise Fail "maxIdx"
             (* end case *))
	   in
	     lp ts
	   end
     (* end case *))

  (* span : nesting_tree -> int * int *)
  (* returns lower bound incl, upper bound excl *)
    fun span t = (minIdx t, maxIdx t)
                
  (* incrBy : int -> nesting_tree -> nesting_tree *)
    fun incrBy i = let
      fun incr nt = (case nt
        of Lf (lo, hi) => Lf (lo+i, hi+i)
	 | Nd ts => Nd (List.map (incrBy i) ts)
        (* end case *))
      in
	incr
      end

  (* toString : nesting_tree -> string *)
    fun toString t = 
     (case t
       of Lf (lo, hi) => 
	    String.concat ("Lf("::Int.toString(lo)::","::Int.toString(hi)::")"::nil)
	| Nd (ts) => let
            val s = String.concatWith "," (List.map toString ts)
            in
              String.concat ("Nd["::s::"]"::nil)
            end
       (* end case *))

  (* ***** FLATTENED ARRAYS ***** *)  

    datatype 'a f_array 
      = FArray of 'a Rope.rope * nesting_tree
  
  (* empty : 'a f_array *)
    val empty = FArray (Rope.empty, Lf (0, 1))

  (* dataOf : 'a f_array -> 'a rope *)
    fun dataOf (FArray (r, _)) = r

  (* concatRope : 'a f_array rope -> 'a f_array *)
    fun concatRope fs = let
      val len = Rope.length fs
      fun lp (curr, i, data, ts) =
        if curr >= len then
          FArray (data, Nd (List.rev ts))
	else let
          val FArray (d, t) = Rope.sub (fs, curr)
          val data' = Rope.concat (data, d)
          val t' = incrBy i t
          val ts' = t'::ts
          val i' = maxIdx t'
          in
            lp (curr+1, i', data', ts')
	  end
      in
	lp (0, 0, Rope.empty, nil)
      end

  (* flatten : 'a f_array f_array -> 'a f_array *)
    fun flatten f = concatRope (dataOf f)

  (* length : 'a f_array -> int *)
    fun length (FArray (_, t)) = (case t
      of Lf (lo, hi) => hi-lo (* note: hi is excl upper bound, lo is incl lower bound *)
       | Nd ts => List.length ts
      (* end case *))

  (* flatSub : 'a f_array * int -> 'a *)
    fun flatSub (FArray (data, shape), i) = (case shape
      of Lf (lo, hi) => Rope.sub (data, lo+i)
       | Nd ts => raise Fail "flatSub"
      (* end case *))

  (* nestedSub : 'a f_array * int -> 'a f_array *)
  (* the f_array returned is one level less deep than the arg *)
    fun nestedSub (FArray (data, shape), i) = (case shape
      of Nd ts => FArray (data, List.nth (ts, i))
       | Lf _ => raise Fail "nestedSub"
      (* end case *))

  (* tab : int * (int -> 'a) -> 'a f_array *)
    fun tab (n, f) =
      if n <= 0 then 
        empty
      else let
        val data = Rope.tabP (n, f)
	val shape = Lf (0, n)
        in
          FArray (data, shape)
	end

  (* flatMap : ('a -> 'b) -> 'a f_array -> 'b f_array *)
  (* pre: 'a is a ground type (int, float, etc.) *)
  (* post: the output will not necessarily be flat; *)
  (*   an appropriate flattener should be applied to it *)
  (*   (the compiler will insert one) *)
    fun flatMap f (FArray (data, shape)) = let
      val data' = Rope.mapP (f, data)
      in
        FArray (data', shape)
      end

  (* nestedMap : ('a -> 'b) -> 'a f_array -> 'b f_array *)
    fun nestedMap f (FArray (data, shape)) = raise Fail "todo"

  (* clean : 'a f_array -> 'a f_array *)
    fun clean (FArray (data, shape)) = (case shape
      of Lf (lo, hi) =>
           if lo = 0 andalso hi = Rope.length data then 
             FArray (data, shape)
	   else let
             (* FIXME this is a slow implementation *)
             val data' = Rope.fromSeq (Rope.partialSeq (data, lo, hi))
             in
               FArray (data', Lf (0, hi-lo))
             end
       | Nd ts => let
           val (lo, hi) = span shape
           (* FIXME this is a slow implementation *)
	   val data' = Rope.fromSeq (Rope.partialSeq (data, lo, hi))
           in
             if lo = 0 then
               FArray (data', shape)
	     else if lo > 0 then
	       FArray (data', incrBy (~lo) shape)
	     else
	       raise Fail "clean: this should never happen"
	   end
      (* end case *))

  (* groundReduce : ('a * 'a -> 'a) -> 'a -> 'a f_array -> 'a *) 
    fun groundReduce (assocOp : 'a * 'a -> 'a) (zero : 'a) (FArray (data, shape)) = 
     (case shape
        of Lf (lo, hi) => let 
             val FArray (data', shape') = clean (FArray (data, shape))
             in 
               Rope.reduceP (assocOp, zero, data')
	     end
	 | Nd _ => raise Fail "groundReduce: flat array of ground types expected"
        (* end case *))

end
