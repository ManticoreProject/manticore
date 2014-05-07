(*Block on an ivar that was speculatively read from until it becomes committed*)

#include "spin-lock.def"

structure BlockOnIVar = 
struct

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v) do ccall M_Print_Int(msg, v) 
#else
#define PDebug(msg) 
#define PDebugInt(msg, v) 
#endif /* !NDEBUG */

    _primcode(
        define @block(x:unit / exh:exh) : unit = 
            let reads : any = FLS.@get-key(alloc(READS_KEY) / exh)
            let reads : [List.list] = ([List.list]) reads
            let rList : List.list = #0(reads)
            fun lp(l : List.list) : () = 
                case l 
                    of CONS(hd:IVar.ivar, tl:List.list) => 
                        let id : int = #8(hd)
                        SPIN_LOCK(hd, 0)
                        if(#1(hd))
                        then cont resume(x:unit) = apply lp(tl)
                             PDebugInt("IVar %d is still speculative\n", id)
                             let thd : ImplicitThread.thread = ImplicitThread.@capture(resume/exh)
                             let waiters : List.list = #9(hd)
                             let newWaiters : List.list = CONS(thd, waiters)
                             let newWaiters : List.list = promote(newWaiters)
                             do #9(hd) := newWaiters
                             SPIN_UNLOCK(hd, 0)
                             SchedulerAction.@stop()
                        else SPIN_UNLOCK(hd, 0) PDebugInt("IVar %d has now been committed\n", id) apply lp(tl)
                     | nil => return()
                end
            do apply lp(rList)  
            return(UNIT)
        ;

    )

    val block : unit -> unit = _prim(@block)
    
end




