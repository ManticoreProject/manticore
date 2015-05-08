




fun lpABCDE(i, rs, l) = 
	if i = 0
	then rs
	else let val vs = List.tabulate(10, fn _ => Vector.tabulate(1000, fn i => i))
			 val _ = ReadSet2.printRS(rs, fn x => print (Int.toString x ^ ", "))
			 val _ = print "\n"
		 in lpABCDE(i-1, ReadSet2.insert(i, rs), vs @ l)
		 end


val _ = lpABCDE(100, ReadSet2.new(), nil)

