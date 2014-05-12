(* spec-par.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)

#include "spin-lock.def"

structure SpecPar (*: sig
    val spec : (unit -> 'a * unit -> 'b) -> ('a, 'b)
    end*) = struct


    _primcode(
#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v) do ccall M_Print_Int(msg, v) 
#else
#define PDebug(msg) 
#define PDebugInt(msg, v) 
#endif /* !NDEBUG */

#ifndef SEQUENTIAL       
        typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)

        define @printVP(x:unit/exh:exh) : unit = 
            let vp : vproc = host_vproc
            let vp : int = VProc.@vproc-id(vp)
            PDebugInt("Executing on vproc: %d\n", vp)
            return(UNIT)
        ;

        define @getKey = FLS.getKey;
        define @find = FLS.find;
        
        define @runningOn(x:unit/exh:exh) : unit = 
            let vp : vproc = host_vproc
            let vp : int = VProc.@vproc-id(vp)
            PDebugInt("Executing on vproc: %d\n", vp)
            return(UNIT)
        ;
        
        define @pSpec(arg : [fun(unit / exh -> any), fun(unit / exh -> any)] / exh : exh):[any,any] = 
            let a : fun(unit / exh -> any) = #0(arg)
            let b : fun(unit / exh -> any) = #1(arg)
            let res : ![any,any] = alloc(UNIT, UNIT)
            let res : ![any,any] = promote(res)
            let count : ![int] = alloc(0)  (*used to determine who continues after completed*)
            let count : ![int] = promote(count)
            let cbl : Cancelation.cancelable = Cancelation.@new(UNIT/exh)
            let actionList : ![List.list] = alloc(nil) (*action list of speculative thread*)
            let actionList : ![List.list] = promote(actionList)
            let parentTID : any = FLS.@get-key(alloc(TID_KEY) / exh)
            let parentTID : tid = (tid) parentTID
            let specTID : tid = alloc(I32Add(#0(parentTID), 1), CONS((any) alloc(2, 1), #1(parentTID)))
            let specTID : tid = promote(specTID)
            let parentSpecKey : ![bool] = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let parentSpecKey : ![bool] = promote(parentSpecKey)
            let specVal : ![int] = alloc(CREATED_SPEC)
            let specVal : ![int] = promote(specVal)
            let parentActionList : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
            let parentActionList : ![List.list] = promote(parentActionList)
            cont execContinuation(s : ![any, any]) = 
                do FLS.@set-key(alloc(alloc(TID_KEY), parentTID) / exh)
                do FLS.@set-key(alloc(alloc(ACTIONS_KEY), parentActionList) / exh)
                return(s)  
            cont slowClone(_ : unit) = (*work that can potentially be stolen*)
                let a : ml_string = alloc("Spawning slow clone on vp %d with tid", 37)
                let s : ml_string = IVar.@tid-to-string(specTID, a / exh)
                let vp : vproc = host_vproc
                let vp : int = VProc.@vproc-id(vp)
                PDebugInt(#0(s), vp)
                do FLS.@set-key(alloc(alloc(ACTIONS_KEY), actionList) / exh)
                do FLS.@set-key(alloc(alloc(TID_KEY), specTID) / exh)
                do FLS.@set-key(alloc(alloc(SPEC_KEY), specVal) / exh)  (*Put in spec mode*)
                let v_1 : any = apply b(UNIT / exh)
                let v_1' : any = promote(v_1)
                do #1(res) := v_1' 
                let updated : int = I32FetchAndAdd(&0(count), 1)
                if I32Eq(updated, 0)
                then PDebug("Speculative thread exiting...\n") SchedulerAction.@stop()
                else do IVar.@commit(#0(actionList)/exh)
                     PDebug("Speculative thread finished second, done committing writes\n")
                     do #0(specVal) := COMMIT
                     throw execContinuation(res)
            let thd : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread(slowClone, cbl / exh)
            do ImplicitThread.@spawn-thread(thd / exh)
            cont newExh(e : exn) = 
                let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
                let newTID : tid = FLS.@get-key(alloc(TID_KEY) / exh)
                case removed
                    of Option.NONE =>
                        let s : ml_string = alloc("Exception raised (stolen)", 25)
                        let s : ml_string = IVar.@tid-to-string(newTID, s / exh) 
                        PDebug(#0(s))
                        let _ : unit = Cancelation.@cancel(cbl / exh)
                        do IVar.@rollback(#0(actionList) / exh)
                        do FLS.@set-key(alloc(alloc(TID_KEY), parentTID) / exh)
                        do FLS.@set-key(alloc(alloc(ACTIONS_KEY), parentActionList) / exh)
                        throw exh(e)
                     | Option.SOME(t:ImplicitThread.thread) => 
                        let s : ml_string = alloc("Exception raised (not stolen)", 29)
                        let s : ml_string = IVar.@tid-to-string(newTID, s / exh)
                        PDebug(#0(s))
                        do FLS.@set-key(alloc(alloc(TID_KEY), parentTID) / exh)
                        do FLS.@set-key(alloc(alloc(ACTIONS_KEY), parentActionList) / exh)
                        throw exh(e)  (*simply propogate exception*)
                end
            let newActions : List.list = CONS(alloc(FACT, alloc(cbl, actionList, specVal)), #0(parentActionList))
            let newActions : List.list = promote(newActions)
            do #0(parentActionList) := newActions
            let parentTID : tid = (tid) parentTID
            let newTID : tid = parentTID (*alloc(I32Add(#0(parentTID), 1), CONS(alloc(1, 1), #1(parentTID)))  *)
            do FLS.@set-key(alloc(alloc(TID_KEY), newTID) / exh)    (*Now executing as "left child"*)
            let s : ml_string = IVar.@tid-to-string(newTID, @"Before executing left child" / exh)
            PDebug(#0(s))
            let v_0 : any = apply a(UNIT/newExh)
            let s : ml_string = IVar.@tid-to-string(newTID, @"After executing left child" / exh)
            PDebug(#0(s))
            do #0(specVal) := COMMIT
            let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
            fun waitToCommit() : () = 
                if (#0(parentSpecKey))
                then do Pause() apply waitToCommit()
                else return()
            case removed 
                of Option.SOME(t : ImplicitThread.thread) => 
                         PDebug("Speculative computation was not stolen\n")
                         let v_1 : any = apply b(UNIT/exh)
                         let res : ![any, any] = alloc(v_0, v_1)
                         (*do apply waitToCommit()*)
                         do IVar.@commit(#0(parentActionList) / exh)
                         throw execContinuation(res)
                  |Option.NONE => PDebug("Speculative computation was stolen\n")  
                                  let v_0' : any = promote(v_0)
                                  do #0(res) := v_0'
                                  let updated : int = I32FetchAndAdd(&0(count), 1)
                                  if I32Eq(updated, 0)
                                  then PDebug("Commit thread finished first\n") SchedulerAction.@stop()
                                  else PDebug("Commit thread finished Last\n")
                                   (*    do apply waitToCommit()*)
                                       do IVar.@commit(#0(parentActionList) / exh)
                                       do IVar.@commit(#0(actionList) / exh)
                                       throw execContinuation(res)
           end
        ;

#else
        define @pSpec(arg : [fun(unit / exh -> any), fun(unit / exh -> any)] / exh : exh):[any,any] = 
            let a : fun(unit / exh -> any) = #0(arg)
            let b : fun(unit / exh -> any) = #0(arg)
            let r1 : any = apply a (UNIT / exh)
            let r2 : any = apply b (UNIT / exh)
            let res : [any, any] = alloc(r1, r2)
            return(res)
        ;
#endif
    )


    val spec : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b) = _prim(@pSpec)
    
end




