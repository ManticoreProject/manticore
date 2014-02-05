val _ = print "Started up vprocs without deadlocking\n"

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x = IVar.newIVar()
val y = IVar.newIVar()
val z = IVar.newIVar()

fun f x = 
    let val _ = print "Entering f x\n"
        val y = IVar.getIVar x
        val _ = print ("read " ^ Int.toString(y) ^ " from ivar x\n")
        val _ = IVar.putIVar(z, y)
    in (print "f(x) is exiting\n")
    end

fun g() = 
    let val _ = print "Entering g()\n"
        val x = IVar.getIVar z
    in print ("read " ^ Int.toString x ^ " from ivar z\n")
    end

exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 40; raise E), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => SpecPar.spec(fn _ => f x, fn _ => g()))


fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val _ = fib 40

val _ = print "exiting...\n"



