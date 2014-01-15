val _ = SpecPar.printVP()

fun fib x = if x <= 2
            then 1
            else fib(x-2) + fib(x-1)

val x = IVar.newIVar()
val y = IVar.newIVar()
val z = IVar.newIVar()

fun f x i = 
    let val _ = print ("entering f x (" ^ Int.toString(i) ^ ")\n")
        val y = IVar.getIVar x
    in print ("read " ^ Int.toString(y) ^ " from ivar x (" ^ Int.toString i ^ ")\n")
    end
(*
exception E
val _ = SpecPar.spec(fn _ => SpecPar.spec( fn _ => (fib 35; raise E), fn _ => IVar.putIVar(x, 10)) handle e => (IVar.putIVar(x, 12), ()),
                     fn _ => f x)
*)

(*if the read inside of "f x" goes through before the write to x, then this deadlocks.*)
val _ = SpecPar.spec(fn _ => SpecPar.spec(fn _ => (fib 35; ()), fn _ => IVar.putIVar(x, 10)),
                     fn _ => SpecPar.spec(fn _ => f x 1, fn _ => f x 2))



val _ = print "program exiting...\n"



