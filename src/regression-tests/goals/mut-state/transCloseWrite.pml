val _ = print "Started up vprocs without deadlocking\n"

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x = IVar.newIVar()
val y = IVar.newIVar()
val z = IVar.newIVar()
val x1 = IVar.newIVar()


fun f x = 
    let val y = IVar.getIVar x
        val _ = print ("read " ^ Int.toString(y) ^ " from ivar x\n")
        val _ = IVar.putIVar(z, y)
        val _ = IVar.putIVar(x1, 2)
    in ()
    end

fun g() = 
    let val x = IVar.getIVar z
        val _ = print ("read " ^ Int.toString x ^ " from ivar z\n")
        val _ = IVar.putIVar(y, x)
    in ()
    end

exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 35; raise E), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => SpecPar.spec(fn _ => f x, fn _ => g()))


val res = IVar.getIVar y
val _ = print("read " ^ Int.toString res ^ " from ivar y\n")











