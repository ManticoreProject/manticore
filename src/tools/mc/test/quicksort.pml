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

fun intListToString ls = let
    fun loop (ls, acc) =  (case ls
        of nil => acc
(* FIXME: bug / missing feature in match compiler fails on this case *)
(*	 | x :: y :: nil => acc ^ (itos x) ^ ", " ^ (itos y)*)
	 | x :: xs => loop (xs, acc ^ (itos x) ^ ", ")
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

print ( (intListToString (qs xs)) ^ "\n")
