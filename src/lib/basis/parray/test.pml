structure R = Rope

val workStealing = WorkStealing.workGroup ()
val r' = ImplicitThread.runOnWorkGroup (workStealing, fn () => let
  val r = R.fromList (List.tabulate (10, fn i => i))
  val r' = R.map (fn x => x + 1) r
  in
    r'
  end)
val l = R.toList r'
val _ = List.app (fn x => Print.printLn (Int.toString x)) l
