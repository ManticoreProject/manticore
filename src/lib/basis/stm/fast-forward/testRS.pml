




fun lpABCDE(i, rs, l) = 
	if i = 0
	then rs
	else let val vs = List.tabulate(10, fn _ => Vector.tabulate(100, fn i => i))
		 in lpABCDE(i-1, ReadSet.insert(i, rs), vs @ l)
		 end


fun lp i =
	if i = 0
	then nil
	else 
		let val x = ReadSet.new()
			val ch = PrimChan.new()
			val _ = Threads.spawnOn(i-1, fn _ => PrimChan.send(ch, lpABCDE(100, x, nil)))
		in ch::lp (i-1) end

fun join chs =
	case chs
		of ch::chs => 
			let val x = PrimChan.recv(ch)
				val _ = ReadSet.app (fn x => print (Int.toString x ^ ", "), x)
				val _ = print "\n\n"
			in join chs end
		| nil => ()

val _ = join (lp 2)
