structure BenchPMergesort =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
    fun randomRope n = Ropes.fromList(randomList n)

    fun benchQuicksort (seqSz, n) = let
	  val r = randomList n
          val (r, t) = Time.timeToEval(fn () => ListQuicksort.quicksort r)
          in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    ()
	  end

    fun benchMergesort (seqSz, n, sort) = let
	  val r = randomRope n
          val (r, t) = Time.timeToEval(fn () => sort r)
          in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    (*Print.printLn(Ropes.toString Int.toString r);*)
	    ()
	  end

    val () = benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesortWithSeqBc.pMergesort)
(*    val () = benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesort.pMergesort)*)
(*    val () = benchQuicksort(PrimIO.readInt(), PrimIO.readInt())*)

  end
