structure R = FloatRope
structure S = FloatScan

val _ = PrimIO.readInt ()
val a = R.tabP (PrimIO.readInt (), fn _ => Float.fromInt (Rand.inRangeInt (0, 100)))
fun doit () =
    let
	val (s, t) = Time.timeToEval (fn () => S.plusScan a)
	val _ = Print.printLn (Long.toString t)
    in () end

val _ = ImplicitThread.runWithGroup (SwpWorkStealing.workGroup (), doit)
