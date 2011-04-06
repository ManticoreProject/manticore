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

  (* maxIdx : nesting_tree -> int*)
    fun maxIdx nt = (case nt
      of Lf (_, i) => i
       | Nd ts => let
           fun lp ts = 
            (case ts
              of t::nil => maxIdx t
	       | _::tl  => lp tl
	       | nil => raise Fail "maxIdx"
              (* end case *))
	   in
	     lp ts
	   end
     (* end case *))

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
      of Lf (lo, hi) => hi-lo
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

end
