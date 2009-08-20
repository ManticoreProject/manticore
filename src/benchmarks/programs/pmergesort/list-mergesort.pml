(* list-mergesort.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mergesort over a list.
 *)

structure ListMergesort =
  struct

    fun merge (x,y) = (
	  case (x,y)
	   of (nil,y) => y
	    | (x,nil) => x
	    | ((hx::tx),
	       (hy::ty)) => 
	      if hx < hy then hx :: merge(tx,(hy::ty))
	      else hy :: merge((hx::tx),ty)
          (* end case *))

    fun split xs = (
	  case xs
	   of (nil) => (nil,nil)
	    | (a::nil) => (a::nil,nil)
	    | (a::b::t) => let val (x,y) = split(t) in (a::x,b::y) end
          (* end case *))

    fun mergesort xs = (
	  case xs
	   of (nil) => nil
	    | (x::nil) => x::nil
	    | (l) =>
	      let
		  val (l1,l2) = split(l)
	      in
		  merge(mergesort(l1),mergesort(l2))
	      end
          (* end case *))

  end

structure Main =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))

    fun benchMergesort (seqSz, n, sort) = let
	  val r = randomList n
	  val b = Time.now ()
          val r = sort r
	  val e = Time.now ()
          in
	    Print.printLn(Time.toString (e-b));
	    ()
	  end

    val _ = benchMergesort(PrimIO.readInt(), PrimIO.readInt(), ListMergesort.mergesort)

  end
