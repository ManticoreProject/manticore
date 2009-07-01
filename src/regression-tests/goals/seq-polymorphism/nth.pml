fun nth (l, n) = let
      fun loop (es, n) = (case (es, n)
	     of (e :: es, 0) => e
	      | (nil, _) => (raise Fail "subscript")
	      | (e :: t, n) => loop(t, n-1)
	    (* end case *))
      in
	if n >= 0
	  then loop (l, n)
	  else raise Fail "subscript"
      end

val x = Print.printLn(Int.toString(nth(1::2::nil, 1)))
