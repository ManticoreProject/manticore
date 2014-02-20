val _ = print "Started up vprocs without deadlock\n"

fun fib n = if n <= 2
            then 1
            else fib(n-1) + fib(n-2)

val x = IVar.newIVar()
val y = IVar.newIVar()

exception E

val _ = SpecPar.spec(fn _ => SpecPar.spec(fn _ => (fib 35; raise E), fn _ => IVar.putIVar(x, 10)) handle e => ((), ()),
                     fn _ => SpecPar.spec(fn _ => (fib 20; IVar.putIVar(x, 12)), fn _ => IVar.putIVar(y, 10)))

val res = IVar.getIVar x
val _ = print ("x = " ^ Int.toString res ^ "\n")


