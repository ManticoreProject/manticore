fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun wasteTime (i : long) = (case i
    of 0 => 0
     | _ =>  let val _ = fib 8
             in wasteTime (i-1) end
     (* end case *));

(| wasteTime 3000000, wasteTime 3000001 |)
