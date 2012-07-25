val host :: neighbor :: _ = VProcExtras.vprocs()

(* wake the neighbor vproc *)
fun thd0 () = Print.printLn "thd0"
val _ = VProcExtras.spawnOn thd0 neighbor

fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

fun thd1 () = (
    Print.printLn "begin thd1";
    delay 18;
    Print.printLn "end thd1")
val _ = VProcExtras.spawnOn thd1 host

val () = delay 21

val () = Print.printLn "finished"
