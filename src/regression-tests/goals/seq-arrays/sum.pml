fun id (n : int) = n;

val a = Array64.tabulate (2000, id);
fun sum a =
    let fun lp (i, s) = 
	    if i = Array64.length a then
		s
	    else
		lp(i+1, Array64.sub(a, i) + s)
    in
	lp (0, 0)
    end

val _ = Print.printLn (Int.toString (sum a))
