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

    (*determines if tid1 is a prefix of tid2*)
    fun pmlPrefix((n1 : int, tid1 : int list), (n2 : int, tid2 : int list)) = case (tid1, tid2)
        of (x::xs, y::ys) => if n1 = n2
                             then if x <= y
                                  then pmlPrefix((n1-1, xs), (n2-1, ys))
                                  else false
                             else if n2 > n1
                                  then pmlPrefix((n1, tid1), (n2-1, ys))
                                  else false
         | ([], []) => true
         | ([], y::ys) => true
         | (x::xs, []) => false
                                                                        
    (*Check if an element is independent of a set of other elements.  If it is,
      filter out any elements it is a previx of*)
    fun pmlInd((v,f,c,tid,can), workingSet) = 
        let fun helper workingSet = case workingSet
                of (v',f',c',tid',can')::xs => 
                    if pmlPrefix(tid, tid')
                    then helper xs
                    else if pmlPrefix(tid', tid)
                         then workingSet
                         else (v',f',c',tid',can') :: helper xs
                 | [] => (v,f,c,tid,can) :: []      
        in helper workingSet
        end
        

    _primcode(
        typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)  
            
        typedef waiter = ![
            vproc,         (*0: vproc affinity*)
            FLS.fls,       (*1: fiber local storage*)
            cont(any, any),     (*2: thread's continuation*)
            tid,            (*3: thread id*)
            Cancelation.cancelable];  (*4: cancelable*)        
    
        typedef ivar = ![
            int,           (*0: spin lock*)
            bool,          (*1: speculative?*)
            any,           (*2: value*)
            bool,          (*3: full?*)
            List.list,     (*4: list of waiters if this is empty*)
            List.list,     (*5: restart info*)
            any,           (*6: thread id*)
            List.list      (*7: list of writers if this is spec full*)];

        define @printTID2(tid : tid / exh : exh) : () = 
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
            return()
        ;

        define @printTID(/exh : exh) : () = 
            let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
            let tid : tid = (tid) tid
            @printTID2(tid/exh)
        ;

        define @iNew( x : unit / exh : exh) : ivar =
            let x : ivar = alloc(0, false, $0, false, nil, nil, $0, nil)
            let x : ivar = promote(x)
            return (x);

        define @getCancelable(/exh : exh) : Cancelation.cancelable = 
            let ite : FLS.ite = FLS.@get-ite(/exh)
            let c : Option.option = #1(ite)
            case c 
                of Option.SOME(c' : Cancelation.cancelable) => return(c')
                 | Option.NONE => do ccall M_Print("Error in ivar.pml: no cancelation!\n")
                                  let e : exn = Fail(@"Error: no cancelation in ivar.pml\n")
                                  throw exh(e)
            end
        ;

        define @iGet(i : ivar / exh : exh) : any = 
        fun restart() : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*full*)
            then if Equal(#1(i), true)  (*spec full*)
                 then cont getK (x : unit, x : unit) = do ccall M_Print("Dependent reader executing restart\n") apply restart()
                      do ccall M_Print("Reading speculatively full ivar1\n")
                      let fls : FLS.fls = FLS.@get-in-atomic(self)
                      let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                      let affinity : vproc = host_vproc
                      let c : Cancelation.cancelable = @getCancelable(/exh)
                      let item : waiter = alloc(affinity, fls, getK, (tid) tid, c)
                      let item : waiter = promote(item)
                      let l : List.list = CONS(item, #5(i))
                      let l : List.list = promote(l)
                      do #5(i) := l  
                      SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      return(#2(i))
                 else SPIN_UNLOCK(i, 0)   (*commit full*)
                      do SchedulerAction.@atomic-end(self)
                      do ccall M_Print("Reading commit full ivar\n")
                      return (#2(i)) 
            else cont getK'(x : any, s : bool) = (*empty*)
                    if Equal(s, true) (*previously read empty, check if its now spec full*)
                    then let self : vproc = SchedulerAction.@atomic-begin()
                         SPIN_LOCK(i, 0)
                         do ccall M_Print("Reading speculatively full ivar\n")
                         cont getK''(x : unit, x : unit) = do ccall M_Print("Dependent reader executing restart\n") apply restart()
                         let fls : FLS.fls = FLS.@get-in-atomic(self)
                         let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                         let affinity : vproc = self
                         let c : Cancelation.cancelable = @getCancelable(/exh)
                         let item : waiter = alloc(affinity, fls, getK'', (tid) tid, c)
                         let l : List.list = CONS(item, #5(i))
                         let l : List.list = promote(l)
                         do #5(i) := l
                         SPIN_UNLOCK(i, 0)
                         do SchedulerAction.@atomic-end(self)
                         return(x)
                    else do ccall M_Print("restarting previously blocked thread\n") return(x)
                 do ccall M_Print("Reading from empty ivar\n")
                 let fls : FLS.fls = FLS.@get-in-atomic(self)
                 let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                 let c : Cancelation.cancelable = @getCancelable(/exh)
                 let item : waiter = alloc(self, fls, getK', (tid) tid, c)
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
            let self : vproc = SchedulerAction.@atomic-begin()
            let spec : any = FLS.@get-key(alloc(SPEC_KEY) / exh)
            let spec : bool = (bool)spec
            fun restart (waiters : List.list) : unit = case waiters
                of nil => return (UNIT)
                 | CONS(hd : waiter, tl : List.list) => 
                      do ccall M_Print("Restarting blocked reader\n")
                      let k : cont(any, any) = #2(hd)
                      cont takeK(_ : unit) = throw k (v, spec)
                      let wrapped : cont(unit) = Cancelation.@wrap-fiber(#4(hd), takeK / exh)
                      do VProcQueue.@enqueue-on-vproc(#0(hd), #1(hd), wrapped) 
                      apply restart(tl)
                 end
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*already full*)
            then if Equal(spec, true)
                 then SPIN_LOCK(i, 0)  (*this write is speculative*)
                      cont getK(x : unit, x : unit) = 
                        do #2(i) := v    (*value*)
                        do #1(i) := true (*still spec full until committed*)
                        do #3(i) := true (*full*)      
                        let readers : List.list = #4(i)
                        do #4(i) := nil
                        let _ : unit = apply restart(#4(i))
                        return(UNIT)                  
                      let fls : FLS.fls = FLS.@get-in-atomic(self)
                      let tid : any = FLS.@get-key(alloc(TID_KEY) / exh)
                      let c : Cancelation.cancelable = @getCancelable(/exh)
                      let item : waiter = alloc(self, fls, getK, (tid) tid, c)
                      let item : waiter = promote(item)
                      let l : List.list = CONS(item, #5(i))
                      let l : List.list = promote(l)
                      do #5(i) := l
                      SPIN_UNLOCK(i, 0)
                      SchedulerAction.@stop-from-atomic(self)
                 else SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      do ccall M_Print("Attempt to write to full IVar, exiting...\n")
                      let e : exn = Fail(@"Attempt to write to full IVar")
                      throw exh(e)
            else let blocked : List.list = #4(i)
                 do #4(i) := nil
                 SPIN_UNLOCK(i, 0)
                 do #2(i) := v    (*value field*)
                 do #1(i) := spec (*spec field*)
                 do #3(i) := true (*full field*)
                 let writes : ![List.list] = FLS.@get-key(alloc(WRITES_KEY) / exh)
                 let newWrites : List.list = promote(CONS(i, #0(writes)))
                 do #0(writes) := newWrites
                 do ccall M_Print("Writing to ivar\n")
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
                    if Equal(#1(hd), true) (*speculative?*)
                    then if Equal(#3(hd), true) (*full?*)
                         then return()
                         else return()
                    else return ()
                end
            apply helper(writes)
       ;

       define @prefix = pmlPrefix;
       
       define @independent = pmlInd;

       (*Takes a list of ivars that need to be rolled back, and a working set
         of computations to be restarted (initially nil)*)
       define @rollback(writes : List.list / exh : exh) : () = 
            fun helper(writes : List.list, workingSet : List.list) : List.list = case writes
                of nil => return(workingSet)
                 | CONS(hd : ivar, tl : List.list) => 
                    do ccall M_Print("Rolling back IVar\n")
                    do #3(hd) := false (*set to empty*)
                    let dependents : List.list = #5(hd)
                    let newWS : List.list = PrimList.@append(dependents, workingSet / exh)
                    let newWS' : List.list = apply helper(tl, newWS)
                    return(newWS')
                 end
            and procDependents(deps : List.list, ws : List.list) : List.list = case deps
                of nil => return(ws)
                 | CONS(hd : waiter, tl : List.list) => 
                    let fls : FLS.fls = #1(hd)
                    let fls : FLS.fls = promote(fls)
                    let writes : ![List.list] = FLS.@get-key(alloc(WRITES_KEY) / exh)
                    let ws' : List.list = PrimList.@append(#0(writes), ws / exh)
                    apply procDependents(tl, ws')
                end
            fun reduce(dependents : List.list, workingSet : List.list) : List.list = case dependents
                of CONS(hd : waiter, tl : List.list) => 
                    let arg : [[any,any,any,[[int], List.list], any], List.list] = 
                            ([[any,any,any,[[int], List.list], any], List.list]) alloc(hd, workingSet)
                    let res : List.list = @independent(arg / exh)
                    apply reduce(tl, res)
                | nil => return(workingSet)
                end
            let restarts : List.list = apply helper(writes, nil)
            let restarts : List.list = apply reduce(restarts, nil)
            fun restart (waiters : List.list) : () = case waiters
                    of nil => return ()
                     | CONS(waiter : waiter, tl : List.list) => 
                          do ccall M_Print("Restarting dependent reader\n")
                          let c : Cancelation.cancelable = #4(waiter)
                          do ccall M_Print("Trying to cancel: ")
                          do @printTID2(#3(waiter) / exh)
                          let _ : unit = Cancelation.@cancel(c / exh)
                          do ccall M_Print("Restarting dependent reader\n")
                          let k : cont(any, any) = #2(waiter)
                          cont takeK(_ : unit) = throw k (UNIT, UNIT)
                          do VProcQueue.@enqueue-on-vproc(#0(waiter), #1(waiter), takeK)
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

 


