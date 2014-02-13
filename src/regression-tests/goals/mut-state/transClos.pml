val _ = SpecPar.printVP()

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x : int IVar.ivar = IVar.newIVar()
val y : int IVar.ivar = IVar.newIVar()

fun f x =
    let val res = IVar.getIVar x
        val _ = print ("read " ^ Int.toString res ^ " from ivar x\n")
        val _ = IVar.putIVar(y, res)
    in ()
    end

exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec(fn _ => (fib 35; raise E), fn _ => (IVar.putIVar(x, 10))) handle e => ((), IVar.putIVar(x, 12)),
                     fn _ => f x)


val res = IVar.getIVar y


val _ = print ("Read " ^ Int.toString res ^ " from ivar y\n")



