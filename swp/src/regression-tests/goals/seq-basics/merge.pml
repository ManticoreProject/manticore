    structure K = Int
		  
    fun lessThan (x, y) = (
	  case K.compare(x, y)
	   of LESS => true
	    | _ => false
          (* end case *))

  (* merge two sorted lists into one sorted list *)
    fun sMerge (xs, ys) = (
	  case (xs, ys)
	   of (nil, ys) => ys
	    | (xs, nil) => xs
	    | (x :: xs, y :: ys) => 
	      if lessThan(x, y) then x :: sMerge(xs, y :: ys) else y :: sMerge(x :: xs, ys)
          (* end case *))

val xs = sMerge(1::3::5::nil,2::4::nil)
val () = List.app Print.print (List.map Int.toString xs)
val () = Print.printLn ""
