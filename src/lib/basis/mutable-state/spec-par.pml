(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)

#include "spin-lock.def"

structure SpecPar (*: sig
    val spec : (unit -> 'a * unit -> 'b) -> ('a, 'b)
    end*) = struct

    val pLock = 0

    fun printLock() = pLock

    _primcode(

        typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)        

        define @getKey = FLS.getKey;
        define @find = FLS.find;

        define @getPrintLock = printLock;
        
        define @printTID(x : unit / exh : exh) : unit = 
            let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
            let tid : tid = (tid) tid
            let pLock : [int] = @getPrintLock(UNIT/exh)
            let pLock : ![int] = (![int])pLock
            SPIN_LOCK(pLock, 0)
            do ccall M_Print("TID: ")
            fun helper(tid : List.list) : () = 
                case tid
                    of CONS(hd : [int], tail : List.list) => 
                        do ccall M_PrintInt(#0(hd))
                        do ccall M_Print(", ")
                        apply helper(tail)
                    | nil => return()
                end
            do apply helper(#1(tid))
            do ccall M_Print("\n")
            SPIN_UNLOCK(pLock, 0)
            return(UNIT)
        ;

        define @pSpec(arg : [fun(unit / exh -> any), fun(unit / exh -> any)] / exh : exh):[any,any] = 
            let a : fun(unit / exh -> any) = #0(arg)
            let b : fun(unit / exh -> any) = #1(arg)
            let dummy : any = enum(0) : any
            let res : ![any,any] = alloc(dummy, dummy)
            let count : ![int] = alloc(0)
            let count : ![int] = promote(count)
            let cbl : Cancelation.cancelable = Cancelation.@new(UNIT/exh)
            let writeList : ![List.list] = alloc(nil)
            let writeList : ![List.list] = promote(writeList)
            let parentTID : any = FLS.@get-key(alloc(TID_KEY) / exh)
            cont slowClone(_ : unit) = (*work that can potentially be stolen*)  
                do ccall M_Print("Spawning new thread\n")
                do FLS.@set-key(alloc(alloc(WRITES_KEY), writeList) / exh)
                let parentTID : tid = promote((tid)parentTID)
                let myTID : tid = alloc(I32Add(#0(parentTID), 1), CONS((any)alloc(1), #1(parentTID)))
                do FLS.@set-key(alloc(alloc(TID_KEY), myTID) / exh)
                let keyValPair : [[int], bool] = alloc(alloc(SPEC_KEY), true)
                do FLS.@set-key(keyValPair / exh)  (*Put in spec mode*)
                let res : ![any,any] = promote(res)
                let v_1 : any = apply b(UNIT / exh)
                let v'_1 : any = promote(v_1)
                do #1(res) := v'_1
                fun finish() : [any,any] = 
                    if I32Eq(#0(count), 1)
                    then do IVar.@commit(#0(writeList) / exh)
                         return(res)
                    else do Pause()
                         apply finish()
                apply finish()  (*wait until main thread is done and then commit*)
            let thd : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread(slowClone, cbl / exh)
            do ImplicitThread.@spawn-thread(thd / exh)
            cont newExh(e : exn) = 
                let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
                if NotEqual(removed, UNIT)(*not stolen*)
                then do ccall M_Print("Exception raised, but speculative thread was not stolen\n")
                     throw exh(e)  (*simply propogate exception*)
                else do ccall M_Print("Exception raised and speculative thread was stolen\n")
                     let _ : unit = Cancelation.@cancel(cbl / exh)
                     let updated : int = I32FetchAndAdd(&0(count), 1)
                     let writes : List.list = #0(writeList)
                     do ccall M_Print("Entering rollback\n")
                     do IVar.@rollback(writes / exh)
                     throw exh(e)
            let v_0 : any = apply a(UNIT/newExh)
            let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
            fun finish(t : Option.option) : [any, any] = case t
                of Option.SOME(t : ImplicitThread.thread) => 
                    if Equal(t, thd)
                    then do ccall M_Print("Speculative computation was not stolen\n")
                         let k : PrimTypes.fiber = #0(t)
                         do ccall M_Print("Throwing to fiber\n")
                         let _ : unit = throw k(UNIT)
                         let v_1 : any = apply b(UNIT/exh)
                         let res : [any, any] = alloc(v_0, v_1)
                         return(res)
                    else 
                         do ImplicitThread.@spawn-thread(thd/exh)
                         let t' : Option.option = ImplicitThread.@remove-thread(thd/exh)
                         apply finish(t')
                | Option.NONE => do ccall M_Print("Speculative computation was stolen\n")
                                 let res : ![any, any] = promote(res)
                                 let v'_0 : any = promote(v_0)
                                 do #0(res) := v'_0
                                 let updated : int = I32FetchAndAdd(&0(count), 1)
                                 SchedulerAction.@stop()
               end
            apply finish(removed)                  
    (*        case removed 
                of Option.SOME(t : ImplicitThread.thread) => 
                    do ccall M_Print("Speculative computation was not stolen\n")
                    let v_1 : any = apply b(UNIT/exh)
                    let res : [any, any] = alloc(v_0, v_1)
                    return(res)
                  |Option.NONE => let res : ![any, any] = promote(res)   
                                  let v'_0 : any = promote(v_0)
                                  do #0(res) := v'_0
                                  let updated : int = I32FetchAndAdd(&0(count), 1)
                                  SchedulerAction.@stop()           
                 (*Stolen thread should always return the tuple after committing writes*)
           end *)
        ;
        
    )

    val spec : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b) = _prim(@pSpec)
    val printTID : unit -> unit = _prim(@printTID)
    

    
end

