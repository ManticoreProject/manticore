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
            let actions : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
            do IVar.@commit(#0(actions) / exh)
            let spec : ![int] = FLS.@get-key(alloc(SPEC_KEY) / exh)
            do #0(spec) := COMMIT
            return(UNIT)
        ;

    )

    val block : unit -> unit = _prim(@block)
    
end




