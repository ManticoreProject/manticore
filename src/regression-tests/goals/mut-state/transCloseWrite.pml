val _ = print "Started up vprocs without deadlocking\n"

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x = IVar.newIVar()
val y = IVar.newIVar()
val z = IVar.newIVar()

fun f x = 
    let val y = IVar.getIVar x
        val _ = print ("read " ^ Int.toString(y) ^ " from ivar x\n")
        val _ = IVar.putIVar(z, y)
    in (print "f(x) is exiting\n")
    end

fun g() = 
    let val _ = print "test g()\n"
       val x = IVar.getIVar z
    in print ("read " ^ Int.toString x ^ " from ivar z\n")
    end

exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 40; print "done with fib\n"; raise E), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => SpecPar.spec(fn _ => f x, fn _ => g()))




(*
fun f x = (print "calling fib\n"; fib x; print "done calling fib\n")

val _ = SpecPar.spec(fn _ => SpecPar.spec(fn _ => f 40, fn _ => f 40), 
                     fn _ => SpecPar.spec(fn _ => f 40, fn _ => f 40))*)
