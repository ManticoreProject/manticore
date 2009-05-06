structure R = FloatRope
structure S = FloatScan

val _ = PrimIO.readInt ()
val n = PrimIO.readInt ()
fun doit () =
    let
	val a = R.tabP (n, fn _ => Float.fromInt (Rand.inRangeInt (0, 100)))
val _ = Print.printLn "hi"
	val (s, t) = Time.timeToEval (fn () => S.plusScan a)
	val _ = Print.printLn (Long.toString t)
    in () end

val _ = ImplicitThread.runWithGroup (MultiprogrammedWorkStealing.workGroup (), doit)
