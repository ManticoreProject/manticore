fun pow1Neg n = Float.pow (~1.0, Float.fromInt n);

fun pi n = let
    fun loop (i, acc) = if i >= n
        then acc
        else let
           val a = pow1Neg i
           val k = Float.fromInt i
           val b = k * 2.0 + 1.0
           in
	       loop (i+1, acc + (a / b))
           end
     in
        loop (0, 0.0) * 4.0
     end

val iterations = 1000

val _ = Print.printLn ("pi approximation: "^Float.toString (pi iterations)^" for "^Int.toString iterations^" iterations")
