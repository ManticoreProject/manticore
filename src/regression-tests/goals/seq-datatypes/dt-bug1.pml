datatype ls = NIL | CONS of (int * ls)

fun f x = case x of CONS(i, ls) => 2 | NIL => 1

val _ = Print.print (Int.toString (f NIL)^"\n")

