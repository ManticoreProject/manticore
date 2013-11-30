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
    _primcode(
        typedef waiter = ![
            vproc,         (*0: vproc affinity*)
            FLS.fls,       (*1: fiber local storage*)
            cont(any)];    (*2: thread's continuation*)
    
        typedef ivar = ![
            int,           (*0: spin lock*)
            bool,          (*1: speculative?*)
            any,           (*2: value*)
            bool,          (*3: full?*)
            List.list,     (*4: list of waiters if this is empty*)
            List.list,     (*5: restart info*)
            any,           (*6: thread id*)
            List.list      (*7: list of writers if this is spec full*)];
            
        define @iNew( x : unit / exh : exh) : ivar =
            let x : ivar = alloc(0, false, $0, false, nil, nil, $0, nil)
            return (x);

        define @iGet(i : ivar / exh : exh) : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*full*)
            then if Equal(#1(i), true)  (*spec full*)
                 then cont getK (x : any) = return(x)
                      do ccall M_Print("Reading speculatively full ivar\n")
                      let fls : FLS.fls = FLS.@get-in-atomic(self)
                      let item : waiter = alloc(self, fls, getK)
                      let l : List.list = CONS(item, #5(i))
                      let l : List.list = promote(l)
                      do UPDATE(5, i, l)
                      SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      return(#2(i))
                 else SPIN_UNLOCK(i, 0)   (*commit full*)
                      do SchedulerAction.@atomic-end(self)
                      do ccall M_Print("Reading commit full ivar\n")
                      return (#2(i)) 
            else cont getK(x : any) = return(x)   (*empty*)
                 PRINT_DEBUG("reading from empty ivar\n")
                 do ccall M_Print("Reading from empty ivar\n")
                 let fls : FLS.fls = FLS.@get-in-atomic(self)
                 let item : waiter = alloc(self, fls, getK)
                 let l : list = CONS(item, #4(i))
                 let l : list = promote(l)
                 do UPDATE(4, i, l) 
                 SPIN_UNLOCK(i, 0)
                 SchedulerAction.@stop-from-atomic(self)
        ;

        define @iPut(arg : [ivar, any] / exh : exh) : unit = 
            let i : ivar = #0(arg)
            let v : any = #1(arg)
            let self : vproc = SchedulerAction.@atomic-begin()
            let fls : FLS.fls = FLS.@get-in-atomic(self)
     (*       let spec : bool = SpecPar.@get-key(SPEC_KEY / exh)*)
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*already full*)
            then let e : exn = Fail(@"Attempt to write to full IVar")
                 throw exh(e) 
            else do #3(i) := true
                 do #2(i) := v
                 SPIN_UNLOCK(i, 0)
                 fun restart (waiters : List.list) : unit = case waiters
                    of nil => return (UNIT)
                     | CONS(hd : waiter, tl : List.list) => 
                          PRINT_DEBUG("restarting waiting thread in ivar\n")
                          let k : cont(any) = #2(hd)
                          cont takeK(_ : unit) = throw k (v)
                          do VProcQueue.@enqueue-on-vproc(#0(hd), #1(hd), takeK)
                          apply restart(tl)
                     end
                 apply restart (#4(i))
        ;

        (*cases:
            1) ivar is already commit full -> raise error
            2) ivar is spec full -> replace the value with this one and restart dependants
            3) ivar is empty -> not possible
        *)
        define @commit(writes : List.list/ exh:exh) : unit = 
            fun helper(ws : List.list) : unit = case ws
                of nil => return(UNIT)
                 | CONS(hd : ivar, tl : List.list) => 
                    if Equal(#1(hd), true) (*speculative?*)
                    then if Equal(#3(hd), true) (*full?*)
                         then let e : exn = Fail(@"write to commit full ivar")
                              throw exh(e)
                         else let e : exn = Fail(@"IVar: Impossible - committing unwritten ivar")
                              throw exh(e)
                    else return (UNIT)
                end
                    
            apply helper(writes)
       ;

    )
    
    type 'a ivar = _prim(ivar)
    val newIVar : unit -> 'a ivar = _prim(@iNew)
    val getIVar : 'a ivar -> 'a = _prim(@iGet)
    val putIVar : ('a ivar * 'a) -> unit = _prim(@iPut)


    local 
        val commitHelper : 'a list -> unit = _prim(@commit)
    in 
        fun commit() = 
            let val writes : 'a list = FLS.getKey(1)
            in commitHelper writes
            end
    end
    
end

 


