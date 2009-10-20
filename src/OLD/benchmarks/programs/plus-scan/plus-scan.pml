structure R = Rope
structure S = Scan

val _ = PrimIO.readInt ()
val a = R.tabP (PrimIO.readInt (), fn _ => (Rand.inRangeInt (0, 100)))
fun doit () =
    let
	val b = Time.now ()
	val s = S.plusScan a
	val t = Time.now () - b
	val _ = Print.printLn (Time.toString t)
    in () end

val _ = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), doit)
