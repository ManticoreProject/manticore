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

    fun tidToString (l, msg) =
    case l
        of x::xs => (tidToString(xs, msg)) ^ " -> " ^ Int.toString x
         | nil => msg ^ ": ROOT"


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

        define @tid-to-string' = tidToString;

        define @tid-to-string(tid:tid, msg:ml_string / exh:exh) : ml_string = 
            let l : List.list = #1(tid)
            let arg :[List.list, ml_string] = alloc(l, msg)
            let s : ml_string = @tid-to-string'(arg / exh)
            let newline : ml_string = alloc("\n", 1)
            let s' : ml_string = String.@string-concat-list(CONS(s, CONS(newline, nil)) / exh)
            return(s')
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
            let c : [![List.list], Option.option] = alloc((![List.list])alloc(nil), Option.NONE)
            let x : ivar = alloc(0, false, $0, false, nil, nil, c, nil, unique, nil)
            let x : ivar = promote(x)
            return (x);

        define @getCancelable(/exh : exh) : Cancelation.cancelable = 
            let ite : FLS.ite = FLS.@get-ite(/exh)
            let c : Option.option = #1(ite)
            case c 
                of Option.SOME(c' : Cancelation.cancelable) => return(c')
                 | Option.NONE => let c : Cancelation.cancelable = Cancelation.@new(UNIT/exh)
                                  cont k(x:unit) = return(c)
                                  let k' : cont(unit) = Cancelation.@wrap-fiber(c, k / exh)
                                  let x : unit = UNIT
                                  throw k'(x)
            end
        ;

       define @rollback(writes : List.list, specWrites : List.list / exh : exh) : () = 
            let l : int = PrimList.@length(writes / exh)
            fun findWriter(l : List.list) : [Option.option, List.list] = case l
                    of CONS(hd : ![Option.option], tl : List.list) => case #0(hd)
                            of Option.SOME(w : [any, cont(unit)]) => return(alloc(#0(hd), tl))
                             | Option.NONE => apply findWriter(tl)
                            end
                     | nil => return(alloc(Option.NONE, nil))
                    end
            fun helper(writes : List.list, specWrites : List.list, workingSet : List.list) : List.list = case writes
                of nil => case specWrites 
                            of CONS(hd : ![Option.option], tl : List.list) => 
                                do #0(hd) := Option.NONE
                                apply helper(writes, tl, workingSet)
                             | nil => return(workingSet)
                          end
                 | CONS(hd : ivar, tl : List.list) => 
                    let id : int = #8(hd)
                    PDebugInt("Rolling back IVar: %d\n", id)
                    let nextWrite : [Option.option, List.list] = apply findWriter(#7(hd))
                    let nextWrite : [Option.option, List.list] = promote(nextWrite)
                    do #7(hd) := #1(nextWrite)
                    do case #0(nextWrite)
                          of Option.SOME(w : [any, ImplicitThread.thread]) => 
                                do #2(hd) := #0(w)
                                do ImplicitThread.@resume-thread(#1(w) / exh)
                                return()
                           | Option.NONE => do #3(hd) := false (*set to empty*)
                                            return ()
                       end
                    let dependents : List.list = #5(hd)
                    do #5(hd) := nil
                    let newWS : List.list = apply procDependents(dependents, tl, specWrites, workingSet)
                    return(newWS)
                 end
            and procDependents(deps : List.list, ivars : List.list, specWrites : List.list, workingSet : List.list) : List.list = case deps
                of nil => apply helper(ivars, specWrites, workingSet)
                 | CONS(hd : waiter, tl : List.list) => 
                    PDebug("getting dependent readers\n")
                    let workingSet' : List.list = @pmlInd(hd, workingSet / exh)
                    let ws : ![List.list] = #0(hd)
                    let ivars' : List.list = PrimList.@append(#0(ws), ivars / exh)
                    apply procDependents(tl, ivars', specWrites, workingSet')
                end
            let restarts : List.list = apply helper(writes, specWrites, nil)
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
        
        define @iGet(i : ivar / exh : exh) : any = 
        fun restart() : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            let fls : FLS.fls = FLS.@get-in-atomic(self)
            let fls : FLS.fls = promote(fls)
            cont readSpec(vp : vproc) = 
                cont getK(x : unit) = PDebug("dependent reader restarting\n") apply restart()
                let thd : ImplicitThread.thread = ImplicitThread.@capture(getK/exh)
                PDebugInt("Reading speculatively full ivar %d\n", #8(i))
                let tid : any = FLS.@get-key(alloc(TID_KEY)/exh)
                let c : Cancelation.cancelable = @getCancelable(/exh)
                let writeList : any = FLS.@get-key(alloc(WRITES_KEY) / exh)
                let item : waiter = alloc((![List.list]) writeList, thd, (tid) tid, c)
                let l : List.list = CONS(item, #5(i))
                let l : List.list = promote(l)
                do #5(i) := l
                let reads : any = FLS.@get-key(alloc(READS_KEY) / exh)
                let reads : ![List.list] = (![List.list]) reads
                let temp : List.list = #0(reads)
                let newReads : List.list = CONS(i, temp)
                let newReads : List.list = promote(newReads)
                do #0(reads) := newReads
           (*     let spec : any = FLS.@get-key(alloc(SPEC_KEY) / exh)
                let spec : ![bool] = (![bool]) spec
                do #0(spec) := true  *)
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
                 let id : int = #8(i)
                 PDebugInt("Reading from empty ivar %d\n", id)
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
            let tid:tid = FLS.@get-key(alloc(TID_KEY) / exh)
            let s : ml_string = alloc("Writing to ivar %d, with thread id", 34)
            let s : ml_string = @tid-to-string(tid, s / exh)
            PDebugInt(#0(s), #8(i))
            let v : any = #1(arg)
            let v : any = promote(v)
            let b : [bool] = ([bool])v
            do if(#0(b)) then PDebug("Writing true\n") return () else PDebug("Writing false\n") return()
            let spec : any = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let spec : [bool] = ([bool]) spec
            let spec : bool = (bool)#0(spec)
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
            then if Equal(spec, true)
                 then PDebugInt("*****Speculatively writing to full ivar %d\n", #8(i))
                      let specWriteListPtr : ![List.list] = FLS.@get-key(alloc(SPEC_WRITES_KEY)/exh)
                      let specWriteList : List.list = #0(specWriteListPtr)
                      let writeListPtr : ![List.list] = FLS.@get-key(alloc(WRITES_KEY)/exh)
                      let writeList : List.list = #0(writeListPtr)
                      cont k(x : unit) = 
                        let specWriteList : List.list = promote(specWriteList)
                        do #0(specWriteListPtr) := specWriteList
                        let newList : List.list = CONS(i, writeList)
                        let newList : List.list = promote(newList)
                        do #0(writeListPtr) := newList
                        return(UNIT)
                      let thd : ImplicitThread.thread = ImplicitThread.@capture(k/exh)
                      let item : [any, ImplicitThread.thread] = alloc(v, thd)
                      let item : [any, ImplicitThread.thread] = promote(item)
                      let item : ![Option.option] = alloc(Option.SOME(item))
                      let newSpecWriteList : List.list = CONS(item, specWriteList)
                      let newSpecWriteList : List.list = promote(newSpecWriteList)
                      do #0(specWriteListPtr) := newSpecWriteList
                      let l : List.list = CONS(item, #7(i))
                      let l : List.list = promote(l)
                      do #7(i) := l
                      SPIN_UNLOCK(i, 0)
                      SchedulerAction.@stop-from-atomic(self)
                 else 
                      if(#1(i))
                      then PDebug("Commit writing to speculatively full ivar\n")
                           fun removeWrite(l : List.list, i : ivar) : List.list = case l
                                of CONS(hd : ivar, tl : List.list) => 
                                    if Equal(hd, i)
                                    then return(tl)
                                    else let rest : List.list = apply removeWrite(tl, i)
                                         return(CONS(hd, rest))
                                 | nil => let e : exn = Fail(@"Impossible") throw exh(e)
                               end
                           fun restartBlockedWriters(l : List.list) : () = case l
                            of CONS(hd : ![Option.option], tl:List.list) => 
                                case #0(hd)
                                    of Option.SOME(arg : [any, ImplicitThread.thread]) => 
                                        do ImplicitThread.@resume-thread(#1(arg) / exh)
                                        apply restartBlockedWriters(tl)
                                     | Option.NONE => apply restartBlockedWriters(tl)
                                end
                              |nil => return()
                            end
                           let specWriterInfo : [![List.list], Option.option] = #6(i)  (*write list and cancelable of writer*)
                           case #1(specWriterInfo) 
                            of Option.SOME(c : Cancelation.cancelable) => 
                                let _ : unit = Cancelation.@cancel(c / exh)
                                let blockedWriters : List.list = #7(i)
                                do apply restartBlockedWriters(blockedWriters)
                                let waiters : List.list = #9(i)
                                do apply restartBlockedWriters(waiters)
                                do #7(i) := nil
                                do @rollback(CONS(i, nil), nil / exh)
                                do #2(i) := v
                                do #1(i) := false
                                do #3(i) := true
                                let newWriteList : List.list = apply removeWrite(#0(#0(specWriterInfo)), i)
                                let newWriteList : List.list = promote(newWriteList)
                                do #0(#0(specWriterInfo)) := newWriteList
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
                           let id : int = #8(i)
                           do ccall M_Print_Int("Attempt to write to commit full IVar %d, exiting...\n", id)
                           let e : exn = Fail(@"Attempt to write to full IVar")
                           throw exh(e)
            else let blocked : List.list = #4(i)
                 do #4(i) := nil
                 do #2(i) := v    (*value field*)
                 do #1(i) := spec (*spec field*)
                 do #3(i) := true (*full field*)
                 do if(spec)
                    then let writes : ![List.list] = FLS.@get-key(alloc(WRITES_KEY) / exh)
                         PDebugInt("Speculatively writing to empty ivar %d\n", #8(i))
                         let c : Cancelation.cancelable = @getCancelable(/exh)
                         let c : [![List.list], Option.option] = alloc(writes, Option.SOME(c))
                         let c : [![List.list], Option.option] = promote(c)
                         do #6(i) := c
                         let newWrites : List.list = promote(CONS(i, #0(writes)))
                         do #0(writes) := newWrites
                         return()
                    else PDebugInt("Commit writing to empty ivar %d\n", #8(i)) return()
                 let id : int = #8(i)
                 SPIN_UNLOCK(i, 0)
                 apply restart (blocked)
        ;

        (*cases:
            1) ivar is already commit full -> raise error
            2) ivar is spec full -> replace the value with this one and restart dependants
            3) ivar is empty -> not possible*)
        define @commit(writes : List.list/ exh:exh) : () = 
            PDebug("Calling commit\n")
            fun helper(ws : List.list) : () = case ws
                of nil => return()
                 | CONS(hd : ivar, tl : List.list) => 
                    SPIN_LOCK(hd, 0)
                    PDebugInt("Committing ivar %d\n", #8(hd))
                    do if (#1(hd))
                       then do #1(hd) := false
                            let waiters : List.list = #9(hd)
                            let s : int = PrimList.@length(waiters / exh)
                            PDebugInt("waiters list length = %d\n", s)
                            fun resume (thd : ImplicitThread.thread / exh : exh) : () =
	                             ImplicitThread.@resume-thread (thd / exh)
	                        do PrimList.@app (resume, waiters / exh)
                            return()
                       else let e : exn = Fail(@"Committing ivar that is already commit full\n")
                            do ccall M_Print("Committing ivar that is already commit full!\n")
                            throw exh(e)
                    SPIN_UNLOCK(hd, 0)
                    apply helper(tl)
                end
            apply helper(writes)
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
            else do ccall M_Print("Reading empty ivar1\n")
                 cont k(x : unit) = return(#0(i))
                 let vp : vproc = host_vproc
                 do ccall M_Print("Reading empty ivar2\n")
                 let fls : FLS.fls = FLS.@get()
                 do ccall M_Print("Reading empty ivar3\n")
                 let item : waiter = alloc(vp, fls, k)
                 do ccall M_Print("Reading empty ivar4\n")
                 let l : List.list = CONS(item, #2(i))
                 do ccall M_Print("Reading empty ivar5\n")
                 let l : List.list = promote(l)
                 do ccall M_Print("Reading empty ivar6\n")
                 do #2(i) := l
                 do ccall M_Print("Reading empty ivar7\n")
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
            
#endif

    )
    
    type 'a ivar = _prim(ivar)
    val newIVar : unit -> 'a ivar = _prim(@iNew)
    val getIVar : 'a ivar -> 'a = _prim(@iGet)
    val putIVar : ('a ivar * 'a) -> unit = _prim(@iPut)
    
end




