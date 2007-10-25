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
      send (ch, x)
   end
;

fun server () = (
    spawn (worker 1);
    spawn (worker 2);
    let val x1 = recv ch
        val x2 = recv ch
    in
       print("received " ^ ltos x1 ^ " " ^ ltos x2 ^ "\n")
    end )
;

server ()
