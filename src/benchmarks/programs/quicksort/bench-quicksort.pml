fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
fun randomParray n = tabP(n, fn _ => Rand.inRangeInt (0, 10000))

fun benchSeq (seqSz, n) = 
    let
	val r = randomList n
	val (r, t) = Time.timeToEval(fn () => ListQuicksort.quicksort r)
    in
	Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	()
    end

fun bench (seqSz, n, sort) = 
    let
	val r = randomParray n
        val (r, t) = Time.timeToEval(fn () => sort r)
    in
	Print.printLn(Long.toString t);
	()
    end

val () = ImplicitThread.runWithGroup(MultiprogrammedWorkStealing.workGroup(), fn () => (
			 bench(PrimIO.readInt(), PrimIO.readInt(), Quicksort.quicksort)))
