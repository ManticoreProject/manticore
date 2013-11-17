(* ivar.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * I-Structures
 *)

#include "spin-lock.def"
#include "debug.def"

#define EMPTY_VAL                  $0

structure IVar = (*
    type 'a ivar
    val new : () -> 'a ivar
    val put : ('a ivar * 'a) -> ()
    val get : 'a ivar -> 'a
end*)
struct
    _primcode(
        typedef waiter = ![
            vproc,      (*0: vproc affinity*)
            FLS.fls,    (*1: fiber local storage*)
            cont(any)]; (*2: thread's continuation*)
    
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
            let x : ivar = alloc(0, false, EMPTY_VAL, false, nil, nil, $0, nil)
            return (x);

        (*TODO: Use CAS instead of spin lock?*)
        define @iGet(i : ivar / exh : exh) : any = 
            let self : vproc = SchedulerAction.@atomic-begin()
            SPIN_LOCK(i, 0)
            if Equal(#3(i), true) (*full*)
            then if Equal(#1(i), true)
                 then cont getK (x : any) = return(x)
                      let fls : FLS.fls = FLS.@get-in-atomic(self)
                      let item : waiter = alloc(self, fls, getK)
                      let l : list = CONS(item, #5(i))
                      do UPDATE(5, i, l)
                      SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      return(#2(i))
                 else SPIN_UNLOCK(i, 0)
                      do SchedulerAction.@atomic-end(self)
                      return (#2(i)) 
            else cont getK(x : any) = return(x)
                 let fls : FLS.fls = FLS.@get-in-atomic(self)
                 let item : waiter = alloc(self, fls, getK)
                 let l : list = CONS(item, #4(i))
                 let l : list = promote(l)
                 do UPDATE(4, i, l) 
                 SPIN_UNLOCK(i, 0)
                 SchedulerAction.@stop-from-atomic(self)
        ;

        define @iPut(arg : [ivar, any, bool] / exh : exh) : unit = 
            let i : ivar = #0(arg)
            let v : any = #1(arg)
            let spec : bool = #2(arg)
            let self : vproc = SchedulerAction.@atomic-begin()
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
                          let k : cont(any) = #2(hd)
                          cont takeK(_ : unit) = throw k (v)
                          do VProcQueue.@enqueue-on-vproc(#0(hd), #1(hd), takeK)
                          apply restart(tl) 
                     end
                 apply restart (#4(i))
        ;

        
    )


    type 'a ivar = _prim(ivar)
    val newIVar : unit -> 'a ivar = _prim(@iNew)
    val getIVar : 'a ivar -> 'a = _prim(@iGet)
    val putIVar : ('a ivar * 'a * bool) -> unit = _prim(@iPut)
end

 


