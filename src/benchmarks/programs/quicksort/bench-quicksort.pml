fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))
fun randomRope n = tabP (n, fn _ => Rand.inRangeInt (0, 10000))

fun bench (seqSz, n, sort) = 
    let
	val r = randomRope n
	val b = Time.now ()
	val r' = sort r
	val e = Time.now ()
    in
	Print.printLn(Time.toString (e-b));
	()
    end

val () = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), fn () => (
			 bench(PrimIO.readInt(), PrimIO.readInt(), Quicksort.quicksort)))
