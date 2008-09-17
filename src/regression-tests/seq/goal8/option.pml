datatype 'a option
	 = NONE
	 | SOME of 'a

fun o2s opt = (
      case opt
       of NONE => "none"
	| SOME i => Int.toString i)

val _ = Print.printLn (o2s NONE)
