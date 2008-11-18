(* list.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure List =
  struct

    datatype list = datatype list

    structure PT = PrimTypes

    fun null xs = (
	  case xs
	   of nil => true
	    | _ => false
          (* end case *))

    fun foldl f id xs = let
	    fun lp (xs, acc) = (
		case xs
                 of nil => acc
		  | x :: xs => lp(xs, f(x, acc))
                 (* end case *))
            in
	       lp(xs, id)
	    end

    fun foldr f id xs = let
	    fun lp (xs, acc) = (
		  case xs
		   of nil => acc
		    | CONS(x, xs) => f(x, lp(xs, acc))
                  (* end case *))
            in
	       lp(xs, id)
	    end

    fun rev xs = foldl CONS nil xs

(*    val xs = CONS(1,CONS(2,nil)) *)

    fun l2s f ls = (
	  case ls
	   of nil => ""
	    | CONS(x, xs) => f x ^ l2s f xs
          (* end case *))

    fun app f ls = let
	  fun lp xs = (
	        case xs 
		 of nil => ()
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

    fun nth (i, xs) = raise Fail "todo (some problem with Option.NONE here)"
(*let
      fun lp args =
       (case args
          of (_, nil)   => Option.NONE
	   | (0, x::_)  => Option.SOME x
	   | (n, x::xs) => lp (n-1, xs)
          (* end case *))
      in
        if i < 0
	then Option.NONE
	else lp (i, xs)
      end
*)

    fun rev ls = let
	fun lp (ls, acc) = (
	    case ls
	     of nil => acc
	      | CONS(x, ls) => lp(ls, CONS(x, acc))
            (* end case *))
        in
	  lp(ls, nil)
	end

    fun map f ls = let
	  fun lp (ls, acc) = (
	      case ls
	       of nil => rev acc
		| CONS(x, ls) => lp(ls, CONS(f x, acc))
              (* end case *))
          in
	    lp(ls, nil)
          end

    fun append (ls1, ls2) = let
	  fun lp ls = (
	      case ls
	       of nil => ls2
		| CONS(x, ls) => CONS(x, lp ls)
 	      (* end case *))
          in
	     lp ls1
	  end

    fun concat xss = foldr append nil xss

    fun zip (xs, ys) = let
	fun loop (xs, ys, zs) = (case (xs, ys)
	    of (nil, _) => rev(zs)
	     | (_, nil) => rev(zs)
	     | (x :: xs, y :: ys) => loop(xs, ys, (x, y) :: zs)
	    (* end case *))
	 in
	    loop(xs, ys, nil)
	 end


    fun unzip xs = let
	fun loop (xs, (zs1, zs2)) = (case xs
	    of nil => (rev(zs1), rev(zs2))
	     | (x1, x2) :: xs => loop(xs, (x1 :: zs1, x2 :: zs2))
	    (* end case *))
	 in
	    loop(xs, (nil, nil))
	 end


    fun unzip3 xs = let
	fun loop (xs, (zs1, zs2, zs3)) = (case xs
	    of nil => (rev(zs1), rev(zs2), rev(zs3))
	     | (x1, x2, x3) :: xs => loop(xs, (x1 :: zs1, x2 :: zs2, x3 :: zs3))
	    (* end case *))
	 in
	    loop(xs, (nil, nil, nil))
	 end

    fun filter f ls = let
	fun loop arg = (case arg
	    of (nil, res) => rev res
	     | (x :: xs, res) => loop(xs, if f x then x :: res else res)
	    (* end case *))
	in
	   loop(ls, nil)
	end

    fun zipWith (oper, xs, ys) = map oper (zip(xs, ys))

    fun take (xs, n) = raise Fail "todo"
    fun drop (xs, n) = raise Fail "todo"

  end
