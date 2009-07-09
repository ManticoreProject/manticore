fun isbelow ((a, b) : point, line) = 
    let
	val l = filterP (fn (x, y) =>  a < x andalso b < y, line)
    in
	lengthP l <> 0
    end

fun bench (seqSz, n) = 
    let
	val pts = tabP (n, fn _ => (Rand.randFloat (~100000.0, 100000.0), Rand.randFloat (~100000.0, 100000.0)))
	val p0 = subP (pts, 0)
	val x0 = reduceP (fn ((x1, y1), (x2, y2)) => if x1 < x2 andalso y1 < y2 then (x1, y1) else (x2, y2), p0, pts)
	val y0 = reduceP (fn ((x1, y1), (x2, y2)) => if x1 > x2 andalso y1 > y2 then (x1, y1) else (x2, y2), p0, pts)
	val b = Time.now ()
        val hull = quickhull (x0, y0, pts)
	val e = Time.now ()
    in
	Print.printLn(Long.toString (e - b));
	()
    end

val () = (*ImplicitThread.runWithGroup(MultiprogrammedWorkStealing.workGroup(), fn () => ( *)
			 bench(PrimIO.readInt(), PrimIO.readInt())


