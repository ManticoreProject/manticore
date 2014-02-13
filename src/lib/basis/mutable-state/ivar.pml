(* ivar.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
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
        extern void *M_Print_Int(void *, int);
        extern void *M_Print_Int2(void *, int, int);

        typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)  
            
        typedef waiter = ![
            ![List.list],               (*0: write list of this thread*)
            ImplicitThread.thread,      (*1: thread*)
            tid,                        (*2: thread id*)
            Cancelation.cancelable];    (*3: cancelable object*)
    
        typedef ivar = ![
            int,           (*0: spin lock*)
            bool,          (*1: speculative?*)
            any,           (*2: value*)
            bool,          (*3: full?*)
            List.list,     (*4: list of waiters if this is empty*)
            List.list,     (*5: restart info*)
            tid,           (*6: thread id*)
            List.list,     (*7: list of writers if this is spec full*)
            int];

        #define PDebug(msg) do ccall M_Print(msg)   

        #define PDebugInt(msg, v) do ccall M_Print_Int(msg, v)  

        define @getCount = getIVarCount;

        define @bump(/exh : exh) : int = 
            let iVarCount : [int] = @getCount(UNIT/exh)
            let iVarCount : ![int] = (![int]) iVarCount
            let old : int = I32FetchAndAdd(&0(iVarCount), 1)
            return(old)
        ;

        define @printTID(tid : tid) : () = 
            fun lp (l : List.list) : () = case l
                of CONS(hd : [int], tl : List.list) => 
                    do apply lp(tl)
                    PDebugInt("%d, ", #0(hd))
                    return()
                | nil => return()
                 end
            PDebug("TID: ")
            do apply lp (#1(tid))
            PDebug("\n")
            return()
        ;
        
        define @pmlInd(w : waiter, ws : List.list / exh : exh) : List.list = 
            fun prefixEq(tid1 : List.list, tid2 : List.list) : bool = case tid1 
                of CONS(hd1 : [int], tl1 : List.list) => case tid2
                    of CONS(hd2 : [int], tl2 : List.list) => 
                            if I32Eq(#0(hd1), #0(hd2)) then apply prefixEq(tl1, tl2) else return(false)
                     | nil => let e : exn = Fail(@"Impossible") throw exh(e)
                    end
                  |nil => case tid2
                    of CONS(_ : [int], _ : List.list) => let e : exn = Fail(@"Impossible") throw exh(e)
                     | nil => return(true)
                  end
                end
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
                    then return(ws)
                    else 
                         let prefixRes2 : bool = apply prefix(#2(w), #2(w1))
                         if (prefixRes2)
                         then apply ind(tl)
                         else let res : List.list = apply ind(tl)
                              return(CONS(w1, res))
                |nil => return(CONS(w, nil))
                end
           apply ind(ws)
        ;

        define @iNew( x : unit / exh : exh) : ivar =
            let unique : int = @bump(/exh)
            let t : tid = alloc(0, nil)
            let x : ivar = alloc(0, false, $0, false, nil, nil, t, nil, unique)
            let x : ivar = promote(x)
            return (x);

        define @getCancelable(/exh : exh) : Cancelation.cancelable = 
            let ite : FLS.ite = FLS.@get-ite(/exh)
            let c : Option.option = #1(ite)
            case c 
                of Option.SOME(c' : Cancelation.cancelable) => return(c')
                 | Option.NONE => PDebug("Error in ivar.pml: no cancelation!\n")
                                  let e : exn = Fail(@"Error: no cancelation in ivar.pml\n")
                                  throw exh(e)
            end
        ;
        
        define @iGet(i : ivar / exh : exh) : any = 
        fun restart() : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            let fls : FLS.fls = FLS.@get-in-atomic(self)
            let fls : FLS.fls = promote(fls)
            cont readSpec(vp : vproc) = 
                cont getK(x : unit) = PDebug("dependent reader restarting\n") apply restart()
                let thd : ImplicitThread.thread = ImplicitThread.@capture(getK/exh)
                PDebug("Reading speculatively full ivar\n")
                let tid : any = FLS.@get-key(alloc(TID_KEY)/exh)
                let c : Cancelation.cancelable = @getCancelable(/exh)
                let writeList : any = FLS.@get-key(alloc(WRITES_KEY) / exh)
                let item : waiter = alloc((![List.list]) writeList, thd, (tid) tid, c)
                let l : List.list = CONS(item, #5(i))
                let l : List.list = promote(l)
                do #5(i) := l
                let id : int = #8(i)
                SPIN_UNLOCK(i, 0)
                do SchedulerAction.@atomic-end(vp)
                return(#2(i))
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*full*)
            then if Equal(#1(i), true)  (*spec full*)
                 then throw readSpec(self)
                 else SPIN_UNLOCK(i, 0)   (*commit full*)
                      do SchedulerAction.@atomic-end(self)
                      PDebug("Reading commit full ivar\n")
                      return (#2(i))
            else cont getK'(x : unit) = 
                    if Equal(#1(i), true)
                    then let self : vproc = SchedulerAction.@atomic-begin()
                         SPIN_LOCK(i, 0)
                         throw readSpec(self)
                    else return(#2(i))
                 PDebug("Reading from empty ivar\n")
                 let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                 let c : Cancelation.cancelable = @getCancelable(/exh)
                 let thd : ImplicitThread.thread = ImplicitThread.@capture(getK'/exh)
                 let writeList : any = FLS.@get-key(alloc(WRITES_KEY) / exh)
                 let item : waiter = alloc((![List.list]) writeList, thd, (tid) tid, c)
                 let l : list = CONS(item, #4(i))
                 let l : list = promote(l)
                 do #4(i) := l
                 SPIN_UNLOCK(i, 0)
                 SchedulerAction.@stop-from-atomic(self)
        apply restart()         
        ;

        define @iPut(arg : [ivar, any] / exh : exh) : unit = 
            let i : ivar = #0(arg)
            let v : any = #1(arg)
            let v : any = promote(v)
            let spec : any = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let spec : [bool] = ([bool]) spec
            let spec : bool = (bool)#0(spec)
            let id : int = #8(i)
            let tid : any = FLS.@get-key(alloc(TID_KEY)/exh)    
            let tid : tid = promote((tid)tid)        
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
            then if Equal(spec, true)
                 then PDebug("*****Writing to ivar that is already speculatively full\n")
                      cont getK(x : unit) = 
                        do #2(i) := v    (*value*)
                        do #1(i) := true (*still spec full until committed*)
                        do #3(i) := true (*full*)      
                        let readers : List.list = #4(i)
                        do #4(i) := nil
                        let _ : unit = apply restart(#4(i))
                        return(UNIT)                  
                      let fls : FLS.fls = FLS.@get-in-atomic(self)
                      let fls : FLS.fls = promote(fls)
                      let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                      let c : Cancelation.cancelable = @getCancelable(/exh)
                      let thd : ImplicitThread.thread = ImplicitThread.@capture(getK/exh)
                      let writeList : any = FLS.@get-key(alloc(WRITES_KEY) / exh)
                      let item : waiter = alloc((![List.list]) writeList, thd, (tid) tid, c)
                      let item : waiter = promote(item)
                      let l : List.list = CONS(item, #7(i))
                      let l : List.list = promote(l)
                      do #7(i) := l
                      SPIN_UNLOCK(i, 0)
                      SchedulerAction.@stop-from-atomic(self)
                 else SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      PDebug("Attempt to write to full IVar, exiting...\n")
                      let e : exn = Fail(@"Attempt to write to full IVar")
                      throw exh(e)
            else let blocked : List.list = #4(i)
                 do #4(i) := nil
                 do #2(i) := v    (*value field*)
                 do #1(i) := spec (*spec field*)
                 do #3(i) := true (*full field*)
                 do #6(i) := tid
                 let writes : ![List.list] = FLS.@get-key(alloc(WRITES_KEY) / exh)
                 let newWrites : List.list = promote(CONS(i, #0(writes)))
                 do #0(writes) := newWrites
                 let id : int = #8(i)
                 SPIN_UNLOCK(i, 0)
                 apply restart (blocked)
        ;

        (*cases:
            1) ivar is already commit full -> raise error
            2) ivar is spec full -> replace the value with this one and restart dependants
            3) ivar is empty -> not possible
        *)
        define @commit(writes : List.list/ exh:exh) : () = 
            fun helper(ws : List.list) : () = case ws
                of nil => return()
                 | CONS(hd : ivar, tl : List.list) => 
                    SPIN_LOCK(hd, 0)
                    do #1(hd) := false
                    SPIN_UNLOCK(hd, 0)
                    apply helper(tl)
                end
            apply helper(writes)
       ;

       (*Takes a list of ivars that need to be rolled back, and a working set
         of computations to be restarted (initially nil)*)
       define @rollback(writes : List.list / exh : exh) : () = 
            let l : int = PrimList.@length(writes / exh)
            fun helper(writes : List.list, workingSet : List.list) : List.list = case writes
                of nil => return(workingSet)
                 | CONS(hd : ivar, tl : List.list) => 
                    let id : int = #8(hd)
                    PDebugInt("Rolling back IVar: %d\n", id)
                    do #3(hd) := false (*set to empty*)
                    let dependents : List.list = #5(hd)
                    do #5(hd) := nil
                    do #7(hd) := nil
                    let newWS : List.list = apply procDependents(dependents, tl, workingSet)
                    return(newWS)
                 end
            and procDependents(deps : List.list, ivars : List.list, workingSet : List.list) : List.list = case deps
                of nil => apply helper(ivars, workingSet)
                 | CONS(hd : waiter, tl : List.list) => 
                    PDebug("getting dependent readers\n")
                    let workingSet' : List.list = @pmlInd(hd, workingSet / exh)
                    let ws : ![List.list] = #0(hd)
                    let ivars' : List.list = PrimList.@append(#0(ws), ivars / exh)
                    apply procDependents(tl, ivars', workingSet')
                end
            let restarts : List.list = apply helper(writes, nil)
            let restarts : List.list = promote(restarts)
            let l : int = PrimList.@length(restarts / exh)
            fun restart (waiters : List.list) : () = case waiters
                    of nil => return ()
                     | CONS(waiter : waiter, tl : List.list) => 
                          PDebug("Restarting dependent reader\n")
                          let c : Cancelation.cancelable = #3(waiter)
                          let _ : unit = Cancelation.@cancel(c / exh)
                          (*this seems weird, but the thread we are resuming needs to be associated
                          **with this cancelable object, and by setting the cancel flag, the resumed
                          **thread will immediately exit unless we "uncancel" it.*)
                          do Cancelation.@uncancel(c / exh)
                          do ImplicitThread.@resume-thread(#1(waiter)/exh)
                          apply restart(tl)
                     end
            apply restart(restarts)
       ;    
       
    )
    
    type 'a ivar = _prim(ivar)
    val newIVar : unit -> 'a ivar = _prim(@iNew)
    val getIVar : 'a ivar -> 'a = _prim(@iGet)
    val putIVar : ('a ivar * 'a) -> unit = _prim(@iPut)
    
end




