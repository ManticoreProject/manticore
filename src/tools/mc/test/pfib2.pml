fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun wasteTime (x : long) = let
    fun loop (i, acc) = if i < x
        then let
	   val f = fib 8
           in loop (i+1, acc+f) end
        else acc
    in
(*    print ("wasting time\n");*)
       loop (0, 0)
    end
;

let val (x, y) = (| wasteTime 4000000, wasteTime 1000000 |) in
   print ((ltos x)^"\n");
   print ((ltos y)^"\n")
end
