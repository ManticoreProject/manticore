datatype 'a option
	 = NONE
	 | SOME of 'a

fun o2s opt = (
      case opt
       of NONE => "none"
	| SOME i => itos i)

val _ = print (o2s NONE^"\n")
