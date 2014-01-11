
val x = IVar.newIVar()

val y = Fork.fork(fn _ => (IVar.getIVar x; ()))

val _ = print "after fork\n"

val _ = IVar.putIVar(x, 12)

val _ = print "done\n"


