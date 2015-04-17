




val x = ReadSet.new()

fun lpABCDE(i, rs, l) = 
	if i = 0
	then rs
	else let val vs = List.tabulate(10, fn _ => Vector.tabulate(100, fn i => i))
			 val _ = ReadSet.app (fn x => print (Int.toString x ^ ", "), rs)
		 in lpABCDE(i-1, ReadSet.insert(i, rs), vs @ l)
		 end


val x = lpABCDE(100, x, nil) handle Fail s => (print s; raise Fail s)
val _ = ReadSet.app (fn x => print (Int.toString x ^ ", "), x)

