

fun getArg f args = 
    case args 
        of arg::arg'::args => 
            if String.same(f, arg) then SOME arg'
            else getArg f (arg'::args)
         |_ => NONE

val args = CommandLine.arguments ()


val K = 
	case getArg "-size" args 
	   of SOME arg => Option.valOf(Int.fromString arg)
		| NONE => 50


fun lpABCDE(i, rs, l) = 
	if i = 0
	then rs
	else let val vs = List.tabulate(K, fn _ => Vector.tabulate(10, fn i => i))
			 val _ = ReadSet2.length rs
			 val _ = print ("About to insert element " ^ Int.toString i ^ "\n")
			 val _ = print "\n"
		 in lpABCDE(i-1, ReadSet2.insert(i, rs), vs @ l)
		 end


val rs = lpABCDE(10000, ReadSet2.new(), nil)

val _ = ReadSet2.printRS(rs, fn x => print(Int.toString x ^ ", "))
val _ = print "\n"