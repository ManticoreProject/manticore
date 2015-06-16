(* atomic-counter.pml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Make a counter that gets updated atomically
 *)

structure AtomicCounter =
struct

    _primcode (

        typedef counter = [fun(/ ->long), fun(/ -> long)];

        define @new(x:unit / exh:exh) : counter = 
            let counter : ![long] = alloc(0:long)
            let counter : ![long] = promote(counter)
            fun inc() : long = 
                let old : long = I64FetchAndAdd(&0(counter), 1:long)
                return(old)
            fun get() : long = return(#0(counter))
            let funs : counter = alloc(inc, get)
            return(funs);

        define @bump(atomicCounter : counter) : long = 
            let incFun : fun(/ -> long) = #0(atomicCounter)
            let old : long = apply incFun()
            return(old);

        define @get(atomicCounter : counter) : long = 
            let getFun : fun(/ -> long) = #1(atomicCounter)
            let current : long = apply getFun()
            return(current)
        ;
    )

    type 'a counter = _prim(counter)
    val new : unit -> 'a counter = _prim(@new)

end
