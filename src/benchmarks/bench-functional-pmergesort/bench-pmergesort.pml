structure BenchPMergesort =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
    fun randomRope n = Ropes.fromList(randomList n)

    fun bench (seqSz, n) = let
	  val r = randomRope n
          val (r, t) = Time.timeToEval(fn () => PMergesort.pMergesort r)
          in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    (*Print.printLn(Ropes.toString Int.toString r);*)
	    ()
	  end

    val () = bench(PrimIO.readInt(), PrimIO.readInt())

  end
