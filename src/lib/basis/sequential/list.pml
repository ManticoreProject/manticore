structure List =
  struct

    datatype 'a list =
	     NIL
	   | CONS of ('a * 'a list)

    structure PT = PrimTypes

    fun foldl f id xs = let
	    fun lp (xs, acc) = (
		case xs
                 of NIL => acc
		  | CONS(x, xs) => lp(xs, f(x, acc))
                 (* end case *))
            in
	       lp(xs, id)
	    end
    fun foldr f id xs = let
	    fun lp (xs, acc) = (
		  case xs
		   of NIL => acc
		    | CONS(x, xs) => f(x, lp(xs, acc))
                  (* end case *))
            in
	       lp(xs, id)
	    end

(*    fun rev xs = foldl CONS NIL xs*)

    val xs = CONS(1,CONS(2,NIL))

    fun l2s f ls = (
	  case ls
	   of NIL => ""
	    | CONS(x, xs) => f x ^ l2s f xs
          (* end case *))

    fun app f ls = let
	  fun lp xs = (
	        case xs 
		 of NIL => ()
		  | CONS(x, xs) => (
		      f x;
		      lp xs)
                (* end case *))
          in
	     lp ls
	  end

    val length = let
	  fun f (x, len) = len + 1
          in
	    foldl f 0
	  end

  end
