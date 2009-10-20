structure BenchPMergesort =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
    fun randomRope n = Rope.tabP(n, fn _ => Rand.inRangeInt (0, 10000))

    fun benchQuicksort (seqSz, n) = let
	  val r = randomList n
          val (r, t) = Time.timeToEval(fn () => ListQuicksort.quicksort r)
          in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    ()
	  end

    fun benchMergesort (seqSz, n, sort) = let
	  val r = randomRope n
	  val b = Time.now ()
	  val r = sort Int.compare r
	  val t = Time.now () - b
          in
	    Print.printLn(Time.toString t);
	    (*Print.printLn(Ropes.toString Int.toString r);*)
	    ()
	  end

    val () = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), fn () => ( 
(*     benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesortWithSeqBc.pMergesort) *)
   benchMergesort(PrimIO.readInt(), PrimIO.readInt(), PMergesort.pMergesort)
(*    benchQuicksort(PrimIO.readInt(), PrimIO.readInt())*)))

  end

