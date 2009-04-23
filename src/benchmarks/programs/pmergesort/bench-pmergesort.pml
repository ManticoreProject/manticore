structure BenchPMergesort =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
    fun randomRope n = Ropes.tabFromToP(0, n, fn _ => Rand.inRangeInt (0, 10000))

    fun benchQuicksort (seqSz, n) = let
	  val r = randomList n
          val (r, t) = Time.timeToEval(fn () => ListQuicksort.quicksort r)
          in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    ()
	  end

    fun lessThan (x, y) = x < y

    fun benchMergesort (seqSz, n, sort) = let
	  val r = randomRope n
          val (r, t) = Time.timeToEval(fn () => sort lessThan r)
          in
	    Print.printLn(Long.toString t);
	    (*Print.printLn(Ropes.toString Int.toString r);*)
	    ()
	  end

    val () = ImplicitThread.runWithGroup(MultiprogrammedWorkStealing.workGroup(), fn () => (
(*     benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesortWithSeqBc.pMergesort)*)
   benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesort.pMergesort)
(*    benchQuicksort(PrimIO.readInt(), PrimIO.readInt())*)))

  end
