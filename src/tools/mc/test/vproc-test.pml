val vps = VProcExtras.vprocs()

fun thd () = let
      val vp = VProcExtras.host()
      in
	Print.print("running on " ^ Int.toString(VProcExtras.id vp) ^ "\n")
      end

fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

val _ = List.app (VProcExtras.spawnOn thd) vps

val _ = (Threads.yield(); delay 26; Print.print "done\n")
