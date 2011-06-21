fun id (n : int) = n;

val a = Array.tabulate (2000, id);
fun sum a =
    let fun lp (i, s) = 
	    if i = Array.length a then
		s
	    else
		lp(i+1, Array.sub(a, i) + s)
    in
	lp (0, 0)
    end

val _ = Print.printLn (Int.toString (sum a))
