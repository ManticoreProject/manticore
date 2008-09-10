(* list.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


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

    fun nth (i, ls) = (
	  case ls
	   of NIL => Option.NONE
	    | CONS(x, xs) =>
	      if (i = 0)
		 then Option.SOME x
	      else nth(i-1, xs)
          (* end case *))

    fun rev ls = let
	fun lp (ls, acc) = (
	    case ls
	     of NIL => acc
	      | CONS(x, ls) => lp(ls, CONS(x, acc))
            (* end case *))
        in
	  lp(ls, NIL)
	end

    fun map (f, ls) = let
	  fun lp (ls, acc) = (
	      case ls
	       of NIL => rev acc
		| CONS(x, ls) => lp(ls, CONS(f x, acc))
              (* end case *))
          in
	    lp(ls, NIL)
          end

    fun concat (ls1, ls2) = let
	  fun lp ls = (
	      case ls
	       of NIL => ls2
		| CONS(x, ls) => CONS(x, lp ls)
 	      (* end case *))
          in
	     lp ls1
	  end

  end
