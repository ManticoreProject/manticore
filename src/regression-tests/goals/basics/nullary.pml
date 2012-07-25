datatype 'a mvar = MVAR of 'a | MVARFULL | MVAREMPTY

fun f(a) =(
    case a
     of MVAREMPTY => print "empty\n"
      | MVARFULL => print "full\n"
      | MVAR(mv) => print"other\n")

val _ = f(MVAR 1)
val _ = f(MVARFULL)
val _ = f(MVAREMPTY)

