fun cons (x, xs) = x :: xs;

fun foldl (f, acc, ls) = (case ls
    of nil => acc
     | x::xs => foldl (f, f (x,acc), xs)
    (* end case *))
;

fun rev xs = foldl (cons, nil, xs);   

fun filter (g, ls) = let
    fun f (x, xs) = if g x then x :: xs else xs
    in
       rev (foldl (f, nil, ls))
    end
;

(*fun filter (g, ls) = (case ls
    of nil => nil
     | x :: xs => if g x then x :: filter (g, xs)
                         else filter (g, xs)
    (* end case *))
;
*)

fun intListToString ls = let
    fun loop (ls, acc) =  (case ls
        of nil => acc
	 | x :: y :: nil => acc ^ (itos x) ^ ", " ^ (itos y)
	 | x :: xs => loop (xs, acc ^ (itos x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

fun floatListToString ls =  let
    fun loop (ls, acc) =  (case ls
        of nil => acc
	 | x :: nil => acc ^ (ftos x)
	 | x :: xs => loop (xs, acc ^ (ftos x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

fun append (xs, ys) = (case xs
    of nil => ys
     | x::xs => x :: append (xs, ys)
   (* end case *));

fun qs xs = (case xs 
    of nil => xs
     | p::xs => let
       fun lte x = x <= p
       fun gt x = x > p
       val xs1 = filter (lte, xs)
       val xs2 = filter (gt, xs)
       in	
          append (qs xs1, p :: qs xs2)
       end 
    (* end case *));

val xs = 4::3::2::1::nil;
val ys = 4.0 :: 3.12 :: 5.0 :: 0.01 :: 3.3 :: 4.120001 :: 12312321.1 :: 1023.234 :: nil;

val _ = print ( (floatListToString (qs ys)) ^ "\n")
