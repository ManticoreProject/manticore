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
If we have something like (SpecPar.spec(fn _ => 1, fn _ => 1)), if the speculative thread finishes
first, it will go into the finish loop and preempt itself, adding itself to the deque.  If the commit
thread then comes through, it will try and pop something off the deque, if it gets the second thread's
preempted state, then we will reexecute the speculative thread, which is wrong.
*)

val _ = SpecPar.spec(fn _ => (SpecPar.spec(fn _ => ("computing fib\n"; fib 35; print "done computing fib\n"; SpecPar.runningOn()), fn _ => (print "Writing to ivar\n"; IVar.putIVar(x, 10); print "done writing to ivar\n")); print "done with first spec\n"; SpecPar.runningOn()),
                     fn _ => SpecPar.spec(fn _ => f x 1, fn _ => f x 2))

val _ = print "program exiting...\n"



