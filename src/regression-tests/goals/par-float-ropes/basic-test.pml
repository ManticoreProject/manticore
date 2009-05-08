structure FR = FloatRope

val time = Time.timeToEval
val println = Print.printLn

val sz = (PrimIO.readInt ())

fun gen n = Float.fromInt n * 0.000001

val xsP = Ropes.tabP (sz, gen)

(*
val _ = println "built xsP"
*)

val xsF = FR.tabP (sz, gen)

(*
val _ = println "xsF"
val _ = println ("xsF " ^ (if FR.isBalanced xsF then "is" else "is not") ^ " balanced")
*)

fun sched susp = ImplicitThread.runWithGroup (MultiprogrammedWorkStealing.workGroup (), susp)

(* wrap : (unit -> 'a) -> unit -> 'a *)
fun sched thunk = (fn () =>
 (ImplicitThread.runWithGroup (MultiprogrammedWorkStealing.workGroup (), thunk)))

val (resP, tP) = time (sched (fn _ => Scan.plusScan_float xsP))

(*
val _ = println "ran plusScan on polymorphic rope"
*)

val (resF, tF) = time (sched (fn _ => FloatScan.plusScan xsF))

(*
val _ = println "ran plusScan on float rope"
*)

val msg = (Int.toString sz) ^ "," ^ (Long.toString tP) ^ "," ^ (Long.toString tF)

val _ = println msg

