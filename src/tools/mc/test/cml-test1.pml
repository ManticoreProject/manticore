fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

val n = 29;
val iters = 20;

fun wasteTime (j : int, i : long) =
    if i < iters
       then let 
        val x : long = fib n
        in 
            print ("worker num " ^ itos j ^ "\n");
	    wasteTime (j, i+1)
        end
       else fib n;

val ch = channel ();

fun worker i = let
    val _ = print "worker0\n"
   val x = wasteTime (i, 0)
   in
      print ("worker " ^ itos i ^ "\n");
      send (ch, i)
   end
;

val nThreads = 10;

fun server () = let
    fun spawnLoop i = if i = nThreads
        then ()
        else (spawn (worker i); spawnLoop (i+1))
    fun recvLoop i = if i = nThreads
        then ()
        else ( print("received " ^ itos (recv ch) ^ "\n");
               recvLoop (i+1) )
    in
        spawnLoop 0;
	recvLoop 0;
        print "All threads done\n"
    end
;

server ()
