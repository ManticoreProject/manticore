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
#include "spin-lock.def"
#include "debug.def"

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v) do ccall M_Print_Int(msg, v) 
#else
#define PDebug(msg) 
#define PDebugInt(msg, v) 
#endif /* !NDEBUG */

#ifndef SEQUENTIAL
    _primcode(
     
         typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)        

        define @incMinor(tid : tid / exh:exh) : tid = 
            case #1(tid)
                of CONS(hd : [int,int], tl : List.list) =>
                    let newTid : tid = alloc(#0(tid), CONS(alloc(#0(hd), I32Add(#1(hd), 1)), tl))
                    return(newTid)
                 |nil => let e : exn = Fail("Empty thread id\n")
                         do ccall M_Print("Empty thread id in fork\n")
                         throw exh(e)
            end
        ;             
                
        define @fork(a : fun(unit / exh -> unit) / exh : exh) : unit =
            let cbl : Cancelation.cancelable = Cancelation.@new(UNIT / exh)
            let parentTID : tid = FLS.@get-key(alloc(TID_KEY) / exh)
            let parentTID : tid = promote(parentTID)
            let parentSpec : [int] = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let actions : ![List.list] = alloc(nil)
            let actions : ![List.list] = promote(actions)
            let specKey : [int] = alloc(#0(parentSpec))
            let specKey : [int] = promote(specKey)
            cont t(_ : unit) =
                let ite : FLS.ite = FLS.@get-ite(/exh)
                let fls : FLS.fls = FLS.@new(UNIT/exh)
                let fls : FLS.fls = promote(fls)
                do FLS.@set(fls)
                do FLS.@set-ite(ite / exh)
                do FLS.@set-key(alloc(alloc(ACTIONS_KEY), actions) / exh)
                do FLS.@set-key(alloc(alloc(SPEC_KEY), specKey) / exh)
                let tid : tid = alloc(I32Add(#0(parentTID), 1), CONS((any)alloc(1, 1), #1(parentTID))) 
                let tid : tid = promote(tid)
                let s : ml_string = IVar.@tid-to-string(tid, @"Forking thread with tid and specval = %d" / exh)
                PDebugInt(#0(s), #0(specKey))
                do FLS.@set-key(alloc(alloc(TID_KEY), tid) / exh)
                do FLS.@set-key(alloc(alloc(SPEC_KEY), specKey) / exh)
                let res : unit = apply a(UNIT / exh)
                fun waitToCommit() : () = 
                    if I32Eq(#0(parentSpec), COMMIT)
                    then do Pause() apply waitToCommit()
                    else IVar.@commit(#0(actions) / exh)    
                do IVar.@commit(#0(actions) / exh) (*apply waitToCommit()    *)                           
                let s : ml_string = alloc("Forked thread terminating", 25)
                let s : ml_string = IVar.@tid-to-string(tid, s / exh)
                PDebug(#0(s))
                SchedulerAction.@stop()
            let thread : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread(t, cbl / exh)
            let thread : ImplicitThread.thread = promote(thread)
            do ImplicitThread.@spawn-thread(thread / exh)
            let newTid : tid = @incMinor(parentTID / exh)
            do FLS.@set-key(alloc(alloc(TID_KEY), newTid) / exh)
            let actions : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
            let newActions : List.list = CONS(alloc(FACT, alloc(cbl, actions, specKey)), #0(actions))
            let newActions : List.list = promote(newActions)
            do #0(actions) := newActions
            return(UNIT)
        ;

    )
    val fork : (unit -> unit) -> unit = _prim(@fork)
#endif
end




