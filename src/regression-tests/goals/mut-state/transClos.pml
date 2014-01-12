val _ = print "Started up vprocs without deadlocking\n"

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x = IVar.newIVar()
val y = IVar.newIVar()
val z = IVar.newIVar()

fun f x = 
    let val _ = print "entering f x\n"
        val y = IVar.getIVar x
    in print ("read " ^ Int.toString(y) ^ " from ivar x\n")
    end
(*
exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 35; raise E), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => f x)
*)

exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 35; ()), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => SpecPar.spec(fn _ => f x, fn _ => f x))

val _ = print "program exiting...\n"
