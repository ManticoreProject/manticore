val _ = print "started vprocs without deadlock\n"

val x = IVar.newIVar()
val y = IVar.newIVar()

fun fib x = if x <= 2
            then 1
            else fib (x-1) + fib(x-2)

exception E
val (a, b) = SpecPar.spec(fn _ => (IVar.putIVar(x, fib 30); raise E), fn _ => (IVar.putIVar(y, 12); IVar.getIVar x; ()))
    handle e => (1, IVar.putIVar(y, ~1))

val _ = print "About to read from IVar y\n\n"

val c = IVar.getIVar y

val _ = print ("Ans = " ^ Int.toString c ^ "\n")


