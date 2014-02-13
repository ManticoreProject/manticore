val _ = print "Started up vprocs without deadlock\n"

fun fib n = if n <= 2
            then 1
            else fib(n-1) + fib(n-2)

val x = IVar.newIVar()


exception E
val _ = SpecPar.spec(fn _ => (IVar.putIVar(x, fib 30); raise E), fn _ => IVar.putIVar(x, 10)) handle e => ((), ())


val res = IVar.getIVar x
val _ = print ("x = " ^ Int.toString res ^ "\n")




