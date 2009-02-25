val vps = VProcExtras.vprocs()

fun thd vp = let
      val cv = CVar.new()
      fun thd' () = let
	    val vp = VProcExtras.host()
	    in
	      Print.print("running on " ^ Int.toString(VProcExtras.id vp) ^ "\n");
	      CVar.signal cv
	    end
      in
	Print.print("spawn thread on " ^ Int.toString(VProcExtras.id vp) ^ "\n");
	VProcExtras.spawnOn thd' vp;
	cv
      end

fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

val cvs = List.map thd vps

val _ = (
      Threads.yield();
      Print.print "threads created\n";
(* NOTE: without the delay, the scheduler shuts down the system *)
      delay 20;
      List.app CVar.wait cvs;
      Print.print "done\n")

