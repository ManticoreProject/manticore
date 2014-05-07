(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for fork parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)

structure Fork (*: sig
    val fork : (unit -> unit) -> unit
    end*) = 
struct

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v) do ccall M_Print_Int(msg, v) 
#else
#define PDebug(msg) 
#define PDebugInt(msg, v) 
#endif /* !NDEBUG */

#ifndef SEQUENTIAL
    _primcode(
        define @init(_ : unit / exh : exh) : [int] = 
            let res : ![int] = alloc(3)
            let res : ![int] = promote(res)
            return(res)
        ;

        define @bump(c : [int] / exh : exh) : ml_int = 
            let c : ![int] = (![int])c
            let old : int = I32FetchAndAdd(&0(c), 1)
            let v : [int] = alloc(old)
            return(v)
        ;

    )

    val init : unit -> int = _prim(@init)
    val bump : int -> int = _prim(@bump)

    val tidCount = init()

    fun newTID() = bump(tidCount)

    _primcode(
     
         typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)        

        define @newTID = newTID;
                
        define @fork(a : fun(unit / exh -> unit) / exh : exh) : unit =
            let cbl : Cancelation.cancelable = Cancelation.@new(UNIT / exh)
            let parentTID : tid = FLS.@get-key(alloc(TID_KEY) / exh)
            let parentTID : tid = promote(parentTID)
            let parentSpec : [bool] = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let parentSpec : bool = #0(parentSpec)
            cont t(_ : unit) =
                let n : [int] = @newTID(UNIT / exh)
                let tid : tid = alloc(I32Add(#0(parentTID), 1), CONS((any)alloc(#0(n)), #1(parentTID))) 
                let tid : tid = promote(tid)
                let s : ml_string = alloc("Forking thread with tid", 23)
                let s : ml_string = IVar.@tid-to-string(tid, s / exh)
                PDebug(#0(s))
                do FLS.@set-key(alloc(alloc(TID_KEY), tid) / exh)
                do FLS.@set-key(alloc(alloc(SPEC_KEY), alloc(parentSpec)) / exh)
                let res : unit = apply a(UNIT / exh)
                SchedulerAction.@stop()
            let thread : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread(t, cbl / exh)
            do ImplicitThread.@spawn-thread(thread / exh)
            return(UNIT)
        ;

    )
    val fork : (unit -> unit) -> unit = _prim(@fork)
#endif
end




