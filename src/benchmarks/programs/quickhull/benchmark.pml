fun isbelow ((a, b) : point, line) = 
    let
	val l = filterP (fn (x, y) =>  a < x andalso b < y, line)
    in
	lengthP l <> 0
    end

fun bench (seqSz, n) = 
    let
	val pts = tabP (n, fn _ => (Rand.randFloat (~100000.0, 100000.0), Rand.randFloat (~100000.0, 100000.0)))
	val b = Time.now ()
        val hull = quickhull pts
	val e = Time.now ()
    in
	Print.printLn(Long.toString (e - b));
	()
    end

val () = (*ImplicitThread.runWithGroup(MultiprogrammedWorkStealing.workGroup(), fn () => ( *)
			 bench(PrimIO.readInt(), PrimIO.readInt())


