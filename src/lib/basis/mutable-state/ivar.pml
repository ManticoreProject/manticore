(* ivar.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * I-Structures
 *)

#include "spin-lock.def"
#include "debug.def"

structure IVar = (*
sig
    type 'a ivar
    val new : () -> 'a ivar
    val put : ('a ivar * 'a) -> ()
    val get : 'a ivar -> 'a
end*)
struct

    val ivarCount = 0

    fun getIVarCount() = ivarCount
    _primcode(

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v) do ccall M_Print_Int(msg, v) 
#else
#define PDebug(msg) 
#define PDebugInt(msg, v) 
#endif /* !NDEBUG */


#ifndef SEQUENTIAL    
        extern void *M_Print_Int(void *, int);
        extern void *M_Print_Int2(void *, int, int);

        typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id (list of pairs first element is major number, second is minor*)  
            
        typedef waiter = ![
            ![List.list],               (*0: write list of this thread*)
            ImplicitThread.thread,      (*1: thread*)
            tid,                        (*2: thread id*)
            Cancelation.cancelable,     (*3: cancelable object*)
            ![Option.option]];          (*4: waiting for ivar to commit*)
    
        typedef ivar = ![
            int,           (*0: spin lock*)
            bool,          (*1: speculative?*)
            any,           (*2: value*)
            bool,          (*3: full?*)
            List.list,     (*4: list of waiters if this is empty*)
            List.list,     (*5: dependent readers*)
            [![List.list], Option.option], (*6: write list and cancelable of writer (cancelable
                                                will be NONE if the ivar is unwritten*)
            List.list,     (*7: list of writers if this is spec full*)
            int,           (*8: Unique number assigned to each ivar*)
            List.list];    (*9: List of threads who speculatively read and are now waiting for it to be committed*)
            
        define @getCount = getIVarCount;

        define @bump(/exh : exh) : int = 
            let iVarCount : [int] = @getCount(UNIT/exh)
            let iVarCount : ![int] = (![int]) iVarCount
            let old : int = I32FetchAndAdd(&0(iVarCount), 1)
            return(old)
        ;

        define @tid-to-string2(l : List.list, msg:ml_string / exh:exh) : ml_string = 
            fun lp(l : List.list) : ml_string = 
            case l
                of CONS(hd:[int, int], tl:List.list) => 
                    let maj : ml_string = Int.@raw-int-to-string(#0(hd) / exh)
                    let min : ml_string = Int.@raw-int-to-string(#1(hd) / exh)
                    let rest : ml_string = apply lp(tl)
                    let l : List.list = CONS(rest, CONS(alloc("(", 1), CONS(maj, CONS(alloc(", ", 2), CONS(min, CONS(alloc(") -> ", 5), nil))))))
                    let s : ml_string = String.@string-concat-list(l / exh)
                    return(s)
                 | nil => let s : ml_string = alloc(": ROOT -> ", 10)
                          let l : List.list = CONS(msg, CONS(s, nil))
                          let s : ml_string = String.@string-concat-list(l / exh)
                          return(s)
            end
            apply lp(l)
        ;              

        define @tid-to-string(tid:tid, msg:ml_string / exh:exh) : ml_string = 
            let l : List.list = #1(tid)
            let s : ml_string = @tid-to-string2(l, msg / exh)
            let newline : ml_string = alloc("\n", 1)
            let s' : ml_string = String.@string-concat-list(CONS(s, CONS(newline, nil)) / exh)
            return(s')
        ;

        define @pmlInd(w:waiter, ws : List.list / exh : exh) : List.list = 
            let ws' : List.list = CONS(w, ws)
            return(ws')
            (*
            let s : ml_string = @tid-to-string(#2(w), @"Adding thread to working set" / exh)
            PDebug(#0(s))
            (*majors should be equal, tid1's minor should be less than or equal to tid2's*)
            fun prefixEq(tid1 : List.list, tid2 : List.list) : bool = case tid1 
                of CONS(hd1 : [int, int], tl1 : List.list) => case tid2
                    of CONS(hd2 : [int, int], tl2 : List.list) => 
                            if I32Eq(#0(hd1), #0(hd2)) 
                            then if I32Lte(#1(hd1), #1(hd2))
                                 then apply prefixEq(tl1, tl2) 
                                 else return(false)
                            else return(false)
                     | nil => let e : exn = Fail(@"Impossible") throw exh(e)
                    end
                  |nil => return(true)
                end
            fun prefixNeq(
            (*Is tid1 a prefix of tid2?*)
            fun prefix(tid1 : tid, tid2 : tid) : bool = 
                let n1 : int = #0(tid1)
                let n2 : int = #0(tid2)
                let l1 : List.list = #1(tid1)
                let l2 : List.list = #1(tid2)
                if I32Eq(n1, n2)
                then apply prefixEq(l1, l2)
                else if I32Gt(n1, n2) 
                     then case l1
                        of CONS(hd1 : [int], tl1 : List.list) => let tid1' : ![int, List.list] = alloc(I32Sub(n1, 1), tl1)
                                apply prefix(tid1', tid2)
                          |nil => return(false)
                          end
                     else case l2
                        of CONS(hd2 : [int], tl2 : List.list) => let tid2' : ![int, List.list] = alloc(I32Sub(n2, 1), tl2)
                            apply prefix(tid1, tid2')
                          |nil => return(false)
                        end
            fun ind(ws : List.list) : List.list = case ws 
                of CONS(w1 : waiter, tl : List.list) => 
                    let prefixRes : bool = apply prefix(#2(w1), #2(w))
                    if (prefixRes)
                    then PDebug("Element in WS is a prefix of one we are trying to add\n") return(ws)
                    else 
                         let prefixRes2 : bool = apply prefix(#2(w), #2(w1))
                         if (prefixRes2)
                         then PDebug("Element we are adding is a prefix of something in the WS\n") apply ind(tl)
                         else PDebug("Element is pairwise incompatible with element in WS\n") let res : List.list = apply ind(tl)
                              return(CONS(w1, res))
                |nil => return(CONS(w, nil))
                end
           apply ind(ws)*)
        ;
(*
        define @pmlInd(w : waiter, ws : List.list / exh : exh) : List.list = 
            let s : ml_string = @tid-to-string(#2(w), @"Adding thread to working set" / exh)
            PDebug(#0(s))
            (*majors should be equal, tid1's minor should be less than or equal to tid2's*)
            fun prefixEq(tid1 : List.list, tid2 : List.list) : bool = case tid1 
                of CONS(hd1 : [int, int], tl1 : List.list) => case tid2
                    of CONS(hd2 : [int, int], tl2 : List.list) => 
                            if I32Eq(#0(hd1), #0(hd2)) 
                            then if I32Lte(#1(hd1), #1(hd2))
                                 then apply prefixEq(tl1, tl2) 
                                 else return(false)
                            else return(false)
                     | nil => let e : exn = Fail(@"Impossible") throw exh(e)
                    end
                  |nil => return(true)
                end
            (*Is tid1 a prefix of tid2?*)
            fun prefix(tid1 : tid, tid2 : tid) : bool = 
                let n1 : int = #0(tid1)
                let n2 : int = #0(tid2)
                let l1 : List.list = #1(tid1)
                let l2 : List.list = #1(tid2)
                if I32Eq(n1, n2)
                then apply prefixEq(l1, l2)
                else if I32Gt(n1, n2) 
                     then return(false)
                     else case l2
                        of CONS(hd2 : [int], tl2 : List.list) => let tid2' : ![int, List.list] = alloc(I32Sub(n2, 1), tl2)
                            apply prefix(tid1, tid2')
                          |nil => return(false)
                        end
            fun ind(ws : List.list) : List.list = case ws 
                of CONS(w1 : waiter, tl : List.list) => 
                    let prefixRes : bool = apply prefix(#2(w1), #2(w))
                    if (prefixRes)
                    then PDebug("Element in WS is a prefix of one we are trying to add\n") return(ws)
                    else 
                         let prefixRes2 : bool = apply prefix(#2(w), #2(w1))
                         if (prefixRes2)
                         then PDebug("Element we are adding is a prefix of something in the WS\n") apply ind(tl)
                         else PDebug("Element is pairwise incompatible with element in WS\n") let res : List.list = apply ind(tl)
                              return(CONS(w1, res))
                |nil => return(CONS(w, nil))
                end
           apply ind(ws)
        ;*)

        define @iNew( x : unit / exh : exh) : ivar =
            let unique : int = @bump(/exh)
            let c : [![List.list], Option.option] = alloc((![List.list])alloc(nil), Option.NONE)
            let x : ivar = alloc(0, false, $0, false, nil, nil, c, nil, unique, nil)
            let x : ivar = promote(x)
            return (x);

        define @getCancelable(/exh : exh) : Cancelation.cancelable = 
            let ite : FLS.ite = FLS.@get-ite(/ exh)
            case #1(ite)
                of Option.SOME(c:Cancelation.cancelable) => return(c)
                 | Option.NONE => let e : exn = Fail(@"No cancelable object\n")
                                  do ccall M_Print("No cancelable object (getCancelable)\n")
                                  throw exh(e) 
            end                                    
        ;

       define @rollback(actions : List.list / exh : exh) : () = 
            do ccall M_Print("Performing rollback\n")
            fun findWriter(l : List.list) : [Option.option, List.list] = case l
                    of CONS(hd : ![Option.option], tl : List.list) => case #0(hd)
                            of Option.SOME(w : [any, cont(unit)]) => return(alloc(#0(hd), tl))
                             | Option.NONE => apply findWriter(tl)
                            end
                     | nil => return(alloc(Option.NONE, nil))
                    end
            fun helper(actions : List.list, workingSet : List.list) : List.list = case actions
                of nil => return(workingSet)
                 | CONS(hd : [int, any], tl : List.list) => 
                    case #0(hd)
                        of WACT => 
                            let i : ivar = (ivar) #1(hd)
                            SPIN_LOCK(i, 0)
                            let id : int = #8(i)
                            PDebugInt("Rolling back write action on IVar: %d\n", id)
                            let nextWrite : [Option.option, List.list] = apply findWriter(#7(i))
                            let nextWrite : [Option.option, List.list] = promote(nextWrite)
                            do #7(i) := #1(nextWrite)
                            do case #0(nextWrite)
                                  of Option.SOME(w : [any, ImplicitThread.thread]) => 
                                        do #2(i) := #0(w)
                                        PDebug("Resuming next writer in line\n")
                                        do ImplicitThread.@resume-thread(#1(w) / exh)
                                        return()
                                   | Option.NONE => do #3(i) := false (*set to empty*)
                                                    return ()
                               end
                            let dependents : List.list = #5(i)
                            do #5(i) := nil
                            SPIN_UNLOCK(i, 0)
                            let newWS : List.list = apply procDependents(dependents, tl, workingSet, i)
                            return(newWS)
                         | SWACT =>
                            let o : ![Option.option] = (![Option.option]) #1(hd)
                            PDebug("Rolling back speculative write action\n")
                            do #0(o) := Option.NONE
                            apply helper(tl, workingSet)
                         | RACT => 
                            let i : ivar = (ivar) #1(hd)
                            PDebugInt("Rolling back read action for ivar %d\n", #8(i))
                            apply helper(tl, workingSet) 
                         | FACT => 
                            PDebug("Rolling back fork\n")
                            let info : [Cancelation.cancelable, ![List.list], ![int]] = 
                                    ([Cancelation.cancelable, ![List.list], ![int]]) #1(hd)
                            let _ : unit = Cancelation.@cancelSingleThread(#0(info) / exh)
                            let childActions : ![List.list] = #1(info)
                            let l : int = PrimList.@length(#0(childActions) / exh)
                            PDebugInt("Child action list has %d elements\n", l)
                            let actions' : List.list = PrimList.@append(#0(childActions), tl / exh)
                            apply helper(actions', workingSet)
                         | _ => 
                            let e : exn = Fail(@"Unrecognized action")
                            do ccall M_Print("Unrecognized action in rollback\n")
                            throw exh(e)
                    end  
                 end
            and getReleventActions(actions : List.list, i:ivar) : List.list = 
                case actions
                    of CONS(hd : [int, any], tl : List.list) =>
                        case #0(hd)
                            of RACT => 
                                let i' : ivar = (ivar) #1(hd)
                                if Equal(i, i')
                                then return(nil)
                                else let acts : List.list = apply getReleventActions(tl, i)
                                     return(CONS(hd, acts))
                             | _ => let acts : List.list = apply getReleventActions(tl, i)
                                    return(CONS(hd, acts))
                        end
                     | nil => return(nil)
                end               
            and procDependents(deps : List.list, actions : List.list, workingSet : List.list, i : ivar) : List.list = case deps
                of nil => apply helper(actions, workingSet)
                 | CONS(hd : waiter, tl : List.list) => 
                    let waitingToCommit : ![Option.option] = #4(hd)
                    do #0(waitingToCommit) := Option.NONE
                    let workingSet' : List.list = @pmlInd(hd, workingSet / exh)
                    let tid : tid = #2(hd)
                    let s : ml_string = alloc("dependent reader", 16)
                    let s : ml_string = @tid-to-string(tid, s / exh)
                    PDebug(#0(s))
                    let ws : ![List.list] = #0(hd)
                    let relevent : List.list = apply getReleventActions(#0(ws), i)
                    let actions' : List.list = PrimList.@append(actions, relevent / exh)
                    apply procDependents(tl, actions', workingSet', i)
                end
            let restarts : List.list = apply helper(actions, nil)
            let restarts : List.list = promote(restarts)
            let l : int = PrimList.@length(restarts / exh)
            fun restart (waiters : List.list) : () = case waiters
                    of nil => return ()
                     | CONS(waiter : waiter, tl : List.list) => 
                          let s : ml_string = alloc("Rolling back dependent reader", 29)
                          let s : ml_string = @tid-to-string(#2(waiter), s / exh)
                          PDebug(#0(s))
                          let c : Cancelation.cancelable = #3(waiter)
                          let _ : unit = Cancelation.@cancelSingleThread(c / exh)
                          (*FIXME: this is a gross hack*)
                          let newC : Cancelation.cancelable = Cancelation.@new(UNIT/exh)
                          let cOpt : Option.option = Option.SOME(newC)
                          let cOpt : Option.option = promote(cOpt)
                          let fiber : cont(unit) = #0(#1(waiter))
                          let fiber : cont(unit) = Cancelation.@wrap-fiber(newC, fiber / exh)
                          let ite : FLS.ite = #1(#1(waiter))
                          let ite : ![List.list, Option.option] = (![List.list, Option.option]) ite
                          do #1(ite) := cOpt
                          let thd : ImplicitThread.thread = alloc(fiber, ite)
                          let thd : ImplicitThread.thread = promote(thd)
                          do ImplicitThread.@resume-thread(thd/exh)
                          apply restart(tl)
                     end
            let l : List.list = PrimList.@rev(restarts / exh)
            apply restart(l)
       ;  

        define @commit(actions : List.list / exh:exh) : () = 
            let tid : tid = FLS.@get-key(alloc(TID_KEY) / exh)
            let s : ml_string = @tid-to-string(tid, @"Entering commit" / exh)
            PDebug(#0(s))
            fun helper(acts : List.list) : () = case acts
                of nil => return()
                 | CONS(hd : [int, any], tl : List.list) =>
                    do apply helper(tl)
                    case #0(hd)
                        of RACT =>
                         let i : ivar = (ivar) #1(hd)
                         SPIN_LOCK(i, 0)
                         if (#1(i))
                         then let waiters : List.list = #9(i)
                              cont k(x:unit) = 
                                let s : ml_string = @tid-to-string(tid, @"Thread restarted from blocking on waiting for ivar to turn commit" / exh)
                                PDebug(#0(s))
                                return()
                              let waitingToCommit : ![Option.option] = FLS.@get-key(alloc(BLOCKED_ON_KEY) / exh)
                              let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
                              let t : Option.option = Option.SOME(thd)
                              let t : Option.option = promote(t)
                              do #0(waitingToCommit) := t
                              let newWaiters : List.list = CONS(waitingToCommit, waiters)
                              let newWaiters : List.list = promote(newWaiters)
                              do #9(i) := newWaiters
                              let s : ml_string = @tid-to-string(tid, @"Blocking on trying to commit read that is still speculative (ivar %d)" /exh)
                              PDebugInt(#0(s), #8(i))
                              SPIN_UNLOCK(i, 0)
                              SchedulerAction.@stop()
                         else SPIN_UNLOCK(i, 0) 
                              let s : ml_string = @tid-to-string(tid, @"Previously spec ivar (%d) is now committed" / exh)
                              PDebugInt(#0(s), #8(i))
                              return()
                        | WACT =>
                              let i : ivar = (ivar)#1(hd)
                              SPIN_LOCK(i, 0)
                              PDebugInt("Committing ivar %d\n", #8(i))
                              if (#1(i))
                              then do #1(i) := false
                                    let waiters : List.list = #9(i)
                                    fun resume (item : [Option.option] / exh : exh) : () =
                                        case #0(item)
                                            of Option.SOME(t:ImplicitThread.thread) =>
                                                do ImplicitThread.@resume-thread(t / exh) return()
                                             | Option.NONE => return()
                                        end
	                                do PrimList.@app (resume, waiters / exh) 
	                                SPIN_UNLOCK(i, 0)
	                                return()
                               else (*let e : exn = Fail(@"Committing ivar that is already commit full\n")
                                    do ccall M_Print("Committing ivar that is already commit full!\n")
                                    SPIN_UNLOCK(i, 0)
                                    throw exh(e)        *)
                                    SPIN_UNLOCK(i, 0) return()
                        |FACT => 
                            let info : [Cancelation.cancelable, ![List.list], ![int]] = ([Cancelation.cancelable, ![List.list], ![int]]) #1(hd)
                            return()
                            
                        |_ => apply helper(tl) 
                    end
                  end
             apply helper(actions)
       ;
        
        define @iGet(i : ivar / exh : exh) : any = 
        fun restart() : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            let fls : FLS.fls = FLS.@get-in-atomic(self)
            let tid : tid = FLS.@get-key(alloc(TID_KEY) / exh)
            cont readSpec(vp : vproc) = 
                let actionList : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
                let aList : List.list = #0(actionList)
                let aList : List.list = promote(aList)
                cont getK(x : unit) = apply restart()
                let c : Cancelation.cancelable = @getCancelable(/exh)
            (*    let getK : cont(unit) = Cancelation.@wrap-fiber(getK)*)
                let thd : ImplicitThread.thread = ImplicitThread.@capture(getK/exh)
                let s : ml_string = alloc("Reading spec full ivar %d", 25)
                let s : ml_string = @tid-to-string(tid, s / exh)
                PDebugInt(#0(s), #8(i))
                let tid : any = FLS.@get-key(alloc(TID_KEY)/exh)
                let waitingToCommit : ![Option.option] = FLS.@get-key(alloc(BLOCKED_ON_KEY) / exh)
                let item : waiter = alloc(actionList, thd, (tid) tid, c, waitingToCommit)
                let l : List.list = CONS(item, #5(i))
                let l : List.list = promote(l)
                do #5(i) := l
                (*record a speculative read action*)
                let newActions : List.list = CONS(alloc(RACT, i), #0(actionList))
                let newActions : List.list = promote(newActions)
                do #0(actionList) := newActions
                let spec : any = FLS.@get-key(alloc(SPEC_KEY) / exh)
                let spec : ![int] = (![int]) spec
                do if I32Eq(#0(spec), CREATED_SPEC)
                   then return()
                   else let s : ml_string = @tid-to-string(tid, @"Turning speculative from ivar %d" / exh)
                        PDebugInt(#0(s), #8(i)) do #0(spec) := TURNED_SPEC return ()
                SPIN_UNLOCK(i, 0)
                do SchedulerAction.@atomic-end(vp)
                return(#2(i))
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*full*)
            then if (#1(i))  (*spec full*)
                 then throw readSpec(self)
                 else SPIN_UNLOCK(i, 0)   (*commit full*)
                      do SchedulerAction.@atomic-end(self)
                      PDebugInt("Reading commit full ivar %d\n", #8(i))
                      return (#2(i))
            else cont getK'(x : unit) = 
                    if Equal(#1(i), true)
                    then let self : vproc = SchedulerAction.@atomic-begin()
                         SPIN_LOCK(i, 0)
                         throw readSpec(self)
                    else PDebug("Restarting blocked thread with commit full ivar\n") return(#2(i))
                 let id : int = #8(i)
                 let s : ml_string = alloc("Reading from empty ivar %d", 26)
                 let s : ml_string = @tid-to-string(tid, s / exh)
                 PDebugInt(#0(s), id)
                 let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                 let c : Cancelation.cancelable = @getCancelable(/exh)
                 let getK' : cont(unit) = Cancelation.@wrap-fiber(c, getK' / exh)
                 let thd : ImplicitThread.thread = ImplicitThread.@capture(getK'/exh)
                 let actionList : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
                 let item : waiter = alloc(actionList, thd, (tid) tid, c, (![Option.option])alloc(Option.NONE))
                 let l : list = CONS(item, #4(i))
                 let l : list = promote(l)
                 do #4(i) := l
                 SPIN_UNLOCK(i, 0)
                 SchedulerAction.@stop-from-atomic(self)
        apply restart()         
        ;

        define @iPut(arg : [ivar, any] / exh : exh) : unit = 
            fun enter() : unit = 
                let i : ivar = #0(arg)
                let tid:tid = FLS.@get-key(alloc(TID_KEY) / exh)
                let v : any = #1(arg)
                let v : any = promote(v)
                let b : [bool] = ([bool])v
                let spec : ![int]= FLS.@get-key(alloc(SPEC_KEY) / exh)
                let id : int = #8(i)
                fun restart (waiters : List.list) : unit = case waiters
                    of nil => return (UNIT)
                     | CONS(hd : waiter, tl : List.list) => 
                          PDebug("Restarting blocked reader\n")
                          do ImplicitThread.@resume-thread(#1(hd) / exh)
                          apply restart(tl)
                     end
                let self : vproc = SchedulerAction.@atomic-begin()
                SPIN_LOCK(i, 0)
                if Equal(#3(i), true) (*already full*)
                then if I32Gt(#0(spec), COMMIT) (*speculative*)
                     then let s : ml_string = @tid-to-string(tid, @"Speculatively writing to full ivar %d" / exh)
                          PDebugInt(#0(s), #8(i))
                          if I32Eq(#0(spec), TURNED_SPEC)
                          then let actions : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
                               PDebug("Calling commit\n")
                               SPIN_UNLOCK(i, 0)
                               do @commit(#0(actions) / exh)   (*now we are running in commit mode*)
                               PDebug("Done committing\n")
                               do #0(spec) := COMMIT
                               do #0(actions) := nil
                               apply enter()
                          else let actions : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
                               let actionList : List.list = #0(actions)
                               cont k(x : unit) = 
                                 let tid:tid = FLS.@get-key(alloc(TID_KEY) / exh)
                                 let s : ml_string = alloc("Spec writer resuming", 20)
                                 let s : ml_string = @tid-to-string(tid, s / exh)
                                 let newList : List.list = CONS(alloc(WACT, i), actionList)
                                 do #0(actions) := newList
                                 return(UNIT)
                               let c : Cancelation.cancelable = @getCancelable(/exh)
                               let k : cont(unit) = Cancelation.@wrap-fiber(c, k / exh)
                               let thd : ImplicitThread.thread = ImplicitThread.@capture(k/exh)
                               let item : [any, ImplicitThread.thread] = alloc(v, thd)
                               let item : ![Option.option] = alloc(Option.SOME(item))
                               let newList : List.list = CONS(alloc(SWACT, item), actionList)
                               let newList : List.list = promote(newList)
                               do #0(actions) := newList
                               let l : List.list = CONS(item, #7(i))
                               let l : List.list = promote(l)
                               do #7(i) := l
                               SPIN_UNLOCK(i, 0)
                               SchedulerAction.@stop-from-atomic(self)
                     else 
                          if(#1(i))
                          then let s : ml_string = alloc("Commit writing to spec full ivar %d", 35)
                               let s : ml_string = @tid-to-string(tid, s / exh)
                               PDebugInt(#0(s), #8(i))
                               fun removeWrite(l : List.list, i : ivar) : List.list = case l
                                    of CONS(hd : [int, any], tl : List.list) => 
                                        if I32Eq(#0(hd), WACT)
                                        then if Equal(#1(hd), i)
                                             then return(tl)
                                             else let rest : List.list = apply removeWrite(tl, i)
                                                  return(CONS(hd, rest))
                                        else let rest : List.list = apply removeWrite(tl, i)
                                             return(CONS(hd, rest))
                                     | nil => do ccall M_Print("Could not find write in action list\n")
                                              let e : exn = Fail(@"Impossible") throw exh(e)
                                   end
                               fun restartBlockedWriters(l : List.list) : () = case l
                                of CONS(hd : ![Option.option], tl:List.list) => 
                                    case #0(hd)
                                        of Option.SOME(arg : [any, ImplicitThread.thread]) => 
                                            do ccall M_Print("restarting blocked writer\n")
                                            do ImplicitThread.@resume-thread(#1(arg) / exh)
                                            apply restartBlockedWriters(tl)
                                         | Option.NONE => apply restartBlockedWriters(tl)
                                    end
                                  |nil => return()
                                end
                               let specWriterInfo : [![List.list], Option.option] = #6(i)  (*action list and cancelable of writer*)
                               case #1(specWriterInfo) 
                                of Option.SOME(c : Cancelation.cancelable) => 
                                    let _ : unit = Cancelation.@cancel(c / exh)
                                    let blockedWriters : List.list = #7(i)
                                    do apply restartBlockedWriters(blockedWriters)
                                    do #7(i) := nil
                                    SPIN_UNLOCK(i, 0)
                                    do @rollback(CONS(alloc(WACT, i), nil) / exh)
                                    SPIN_LOCK(i, 0)
                                    do #2(i) := v
                                    do #1(i) := false
                                    (*threads who read when this was spec and are now waiting for it to commit*)
                                    let waiters : List.list = #9(i)  
                                    fun resume (item : [Option.option] / exh : exh) : () =
                                        case #0(item)
                                            of Option.SOME(t:ImplicitThread.thread) => 
                                                do ImplicitThread.@resume-thread(t/exh) return()
                                             | Option.NONE => return()
                                        end
	                                do PrimList.@app (resume, waiters / exh)
	                                fun resume'(item:waiter / exh:exh):() = 
	                                    ImplicitThread.@resume-thread(#1(item) / exh)
	                                do PrimList.@app(resume', #4(i) / exh)
                                    do #3(i) := true
                                    let newActionList : List.list = apply removeWrite(#0(#0(specWriterInfo)), i)
                                    let newActionList : List.list = promote(newActionList)
                                    do #0(#0(specWriterInfo)) := newActionList
                                    SPIN_UNLOCK(i, 0)
                                    do SchedulerAction.@atomic-end(self)
                                    return(UNIT)
                                 | Option.NONE => SPIN_UNLOCK(i, 0)
                                                  PDebug("No cancelable object\n")
                                                  let e : exn = Fail(@"no cancelable object\n")
                                                  throw exh(e)
                               end
                          else SPIN_UNLOCK(i, 0)
                               do SchedulerAction.@atomic-end(self)
                               PDebugInt("Writing to commit full ivar %d\n", #8(i))
                               let e : exn = Fail(@"Attempt to write to full IVar")
                               throw exh(e)
                else let blocked : List.list = #4(i)
                     do #4(i) := nil
                     do #2(i) := v    (*value field*)
                     do if I32Eq(#0(spec), COMMIT)  (*spec field*)
                        then do #1(i) := false return ()
                        else do #1(i) := true  return()
                     do #3(i) := true (*full field*)
                     do if I32Gt(#0(spec), 0)
                        then let actions : ![List.list] = FLS.@get-key(alloc(ACTIONS_KEY) / exh)
                            let s : ml_string = @tid-to-string(tid, @"Spec writing to empty ivar %d" / exh)
                             PDebugInt(#0(s), #8(i))
                             let c : Cancelation.cancelable = @getCancelable(/exh)
                             let c : [![List.list], Option.option] = alloc(actions, Option.SOME(c))
                             let c : [![List.list], Option.option] = promote(c)
                             do #6(i) := c
                             let newActions : List.list = promote(CONS(alloc(WACT, i), #0(actions)))
                             do #0(actions) := newActions
                             return()
                        else PDebugInt("Commit writing to empty ivar %d\n", #8(i)) return()
                     SPIN_UNLOCK(i, 0)
                     apply restart (blocked)
            apply enter()      
        ;
#else

        typedef ivar = ![
            any,           (*0: value*)
            bool,          (*1: full?*)
            List.list      (*2: list of waiters if this is empty*)
        ];

        typedef waiter = ![
            vproc,
            FLS.fls,
            cont(unit)];      


        define @iNew( x : unit / exh : exh) : ivar =
            let x : ivar = alloc($0, false, nil)
            let x : ivar = promote(x)
            return (x)
        ;

        define @iGet(i : ivar / exh : exh) : any = 
            do ccall M_Print("Reading ivar\n")
            if (#1(i))
            then return(#0(i))
            else cont k(x : unit) = return(#0(i))
                 let vp : vproc = host_vproc
                 let fls : FLS.fls = FLS.@get()
                 let item : waiter = alloc(vp, fls, k)
                 let l : List.list = CONS(item, #2(i))
                 let l : List.list = promote(l)
                 do #2(i) := l
                 SchedulerAction.@stop()
        ;

        define @iPut(arg : [ivar, any] / exh : exh) : unit = 
            do ccall M_Print("Writing ivar\n")
            let i : ivar = #0(arg)
            let v : any = #1(arg)
            let v : any = promote(v)
            if (#1(i))
            then let e : exn = Fail(@"Putting to full ivar\n")
                 throw exh(e)
            else do #0(i) := v
                 return(UNIT)
        ;
        define @commit(a : List.list / exh: exh) : () = return();
#endif

    )
    
    type 'a ivar = _prim(ivar)
    val newIVar : unit -> 'a ivar = _prim(@iNew)
    val getIVar : 'a ivar -> 'a = _prim(@iGet)
    val putIVar : ('a ivar * 'a) -> unit = _prim(@iPut)
    
end




