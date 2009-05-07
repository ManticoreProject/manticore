structure FR = FloatRope

val time = Time.timeToEval
val println = Print.printLn

val sz = (PrimIO.readInt ())

fun gen n = Float.fromInt n * 0.000001

val xsP = Ropes.tabP (sz, gen)

val _ = println "built xsP"

val xsF = FR.tabP (sz, gen)

val _ = println "xsF"
val _ = println ("xsF " ^ (if FR.isBalanced xsF then "is" else "is not") ^ " balanced")

val (resP, tP) = time (fn _ => Scan.plusScan_float xsP)

val (resF, tF) = time (fn _ => FloatScan.plusScan xsF)

val _ = println ("tP\t" ^ Long.toString tP)

val _ = println ("tF\t" ^ Long.toString tF)

val _ = println "Done."
