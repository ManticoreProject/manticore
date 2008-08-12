fun pow1Neg n = powf (~1.0, itof n);

fun pi n = let
    fun loop (i, acc) = if i >= n
        then acc
        else let
           val a = pow1Neg i
           val k = itof i
           val b = k * 2.0 + 1.0
           in
	       loop (i+1, acc + (a / b))
           end
     in
        loop (0, 0.0) * 4.0
     end
;

val iterations = 1000;

val _ = print ("pi approximation: "^ftos (pi iterations)^" for "^itos iterations^" iterations \n")
