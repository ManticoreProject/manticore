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

        define @printVP(x:unit/exh:exh) : unit = 
            let vp : vproc = host_vproc
            let vp : int = VProc.@vproc-id(vp)
            do ccall M_Print_Int("Executing on vproc: %d\n", vp)
            return(UNIT)
        ;

        define @getKey = FLS.getKey;
        define @find = FLS.find;

        define @getPrintLock = printLock;

        define @runningOn(x:unit/exh:exh) : unit = 
            let vp : vproc = host_vproc
            let vp : int = VProc.@vproc-id(vp)
            do ccall M_Print_Int("Executing on vproc: %d\n", vp)
            return(UNIT)
        ;
        
        define @printTID(x : unit / exh : exh) : unit = 
            let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
            let tid : tid = (tid) tid
            do ccall M_Print("TID: ")
            fun helper(tid : List.list) : () = 
                case tid
                    of CONS(hd : [int], tail : List.list) => 
                        do apply helper(tail)
                        do ccall M_Print_Int("%d, ", #0(hd))
                        return()
                    | nil => return()
                end
            do apply helper(#1(tid))
            do ccall M_Print("\n")
            return(UNIT)
        ;

        define @tidToString(/exh : exh) : PrimTypes.ml_string = 
            let tid : any = FLS.@get-key(alloc(TID_KEY)/exh)
            let tid : tid = (tid) tid
            fun helper(tid : List.list) : PrimTypes.ml_string = 
                case tid 
                    of CONS(hd : [int], tail : List.list) => 
                        let s : PrimTypes.ml_string = Int.@to-string(hd/exh)
                        let l : PrimTypes.ml_string = apply helper(tail)
                        let l : List.list = CONS(s, CONS(", ", CONS(l, nil)))
                        let s : PrimTypes.ml_string = String.@string-concat-list(l/exh)
                        return(s)
                     | nil => return("")
                end
            apply helper(#1(tid))
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
            let stolen : ![int] = alloc(0)
            let stolen : ![int] = promote(stolen)
            cont slowClone(_ : unit) = (*work that can potentially be stolen*)
                let updated : int = I32FetchAndAdd(&0(stolen), 1)
                do if I32Eq(updated, 1)
                   then SchedulerAction.@stop()
                   else return()
                let vp : vproc = host_vproc
                let vp : int = VProc.@vproc-id(vp)
                do ccall M_Print_Int("Starting up on vproc %d\n", vp)
                do FLS.@set-key(alloc(alloc(WRITES_KEY), writeList) / exh)
                let parentTID : tid = promote((tid)parentTID)
                let myTID : tid = alloc(I32Add(#0(parentTID), 1), CONS((any)alloc(2), #1(parentTID)))
                do FLS.@set-key(alloc(alloc(TID_KEY), myTID) / exh)
                let keyValPair : [[int], bool] = alloc(alloc(SPEC_KEY), true)
                do FLS.@set-key(keyValPair / exh)  (*Put in spec mode*)
                let res : ![any,any] = promote(res)
                let v_1 : any = apply b(UNIT / exh)
                let v_1' : any = promote(v_1)
                do #1(res) := v_1'
                do ccall M_Print_Int("Entering finish loop on vp %d\n", vp)
                fun finish() : [any,any] = 
                    if I32Eq(#0(count), 1)
                    then do IVar.@commit(#0(writeList) / exh)
                         return(res)
                    else (*do Pause()*) (*Should we change this to simply check its cancel flag? *)
                      (*   let c : bool = Cancelation.@check-canceled(/exh)
                         if (c)
                         then do ccall M_Print("Canceled\n") SchedulerAction.@stop()
                         else do ccall M_Print_Int("Not Canceled on vp %d\n", vp) apply finish()*) do ccall M_Print_Int("Finishing on vp %d\n", vp) apply finish()
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
                     let writes : List.list = #0(writeList)
                     do IVar.@rollback(writes / exh)
                     throw exh(e)
            let parentTID : tid = (tid) parentTID
            let newTID : tid = alloc(I32Add(#0(parentTID), 1), CONS((any) alloc(1), #1(parentTID)))
            do FLS.@set-key(alloc(alloc(TID_KEY), newTID) / exh)    (*Now executing as "left child"*)
            do ccall M_Print("Executing a\n")
            let v_0 : any = apply a(UNIT/newExh)
            let vp : vproc = host_vproc let vp : int = VProc.@vproc-id(vp)
            do ccall M_Print_Int("Done executing a on vp: %d\n", vp)
            do FLS.@set-key(alloc(alloc(TID_KEY), parentTID) / exh) (*change tid back*)
            let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
            case removed 
                of Option.SOME(t : ImplicitThread.thread) => 
                    let stolen : int = I32FetchAndAdd(&0(stolen), 1)
                    if I32Eq(stolen, 1)
                    then do ImplicitThread.@spawn-thread(t/exh)
                         let res : ![any, any] = promote(res)
                         let v_0' : any = promote(v_0)
                         do if Equal(t, thd)
                            then do ccall M_Print("Popped equal thread\n\n\n\n") return()
                            else do ccall M_Print("Popped thread that was not equal\n\n\n\n") return()

                         let vp : vproc = host_vproc let vp : int = VProc.@vproc-id(vp)
                         let t : PrimTypes.fiber = #0(t)
                         do ccall M_Print_Int("Throwing to fiber on vp %d\n", vp)
                         do throw t(UNIT)
                            
                         do #0(res) := v_0'
                         let updated : int = I32FetchAndAdd(&0(count), 1)
                         SchedulerAction.@stop()  
                    else let vp : vproc = host_vproc
                         let vp : int = VProc.@vproc-id(vp)
                         do ccall M_Print_Int("Speculative computation was not stolen (vp = %d)\n", vp)
                         let t : PrimTypes.fiber = #0(t)
                         let v_1 : any = apply b(UNIT/exh)
                         let res : [any, any] = alloc(v_0, v_1)
                         return(res)
                  |Option.NONE => let res : ![any, any] = promote(res) 
                                  let vp : vproc = host_vproc let vp : int = VProc.@vproc-id(vp)
                                  do ccall M_Print_Int("Speculative computation was stolen on vp %d\n", vp)  
                                  let v_0' : any = promote(v_0)
                                  do #0(res) := v_0'
                                  let updated : int = I32FetchAndAdd(&0(count), 1)
                                  SchedulerAction.@stop()
                 (*Stolen thread should always return the tuple after committing writes*)
           end 
        ;
        
    )

    val runningOn : unit -> unit = _prim(@runningOn)
    val spec : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b) = _prim(@pSpec)
    val printTID : unit -> unit = _prim(@printTID)
    val printVP : unit -> unit = _prim(@printVP)

    
end

