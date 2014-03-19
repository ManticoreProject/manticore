(* lvarSet.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LVar Sets
 *)

#include "spin-lock.def"
#include "debug.def"

structure LVarSet = (*
sig
    type 'a lvar
    val new : () -> 'a ivar
    val put : ('a ivar * 'a) -> ()
    val get : 'a ivar -> 'a
end*)
struct
#ifndef SEQUENTIAL
    _primcode(

        extern void* M_Print_Int(void *, int);
        typedef lvar = ![
            int,           (*0: spin lock*)
            bool,          (*1: speculative?*)
            List.list,     (*2: state*)
            List.list,     (*3: list of waiters*)
            List.list,     (*4: dependent readers*)
            List.list,     (*5: thread ids*)
            List.list,     (*6: list of writers if this is spec full*)
            int,           (*7: size of the set*)
            List.list      (*8: Callbacks*)];

        define @atomicLub(elem : any, set : lvar) : int = 
            fun lp() : int = 
                let s : List.list = #2(set)
                let newSet : List.list = CONS(elem, s)
                let newSet : List.list = promote(newSet)
                let s' : List.list = CAS(&2(set), s, newSet)
                if Equal(s', s)
                then let size : int = I32FetchAndAdd(&7(set), 1)
                     return(I32Add(size, 1))
                else apply lp()
            apply lp()
       ;

        define @new(x : unit / exh : exh) : lvar = 
            let lv : lvar = alloc(0, false, nil, nil, nil, nil, nil, 0, nil)
            let lv : lvar = promote(lv)
            return(lv)
        ;


        define @put(arg : [any, lvar] / exh : exh) : unit = 
            let v : any = #0(arg)
            let lv : lvar = #1(arg)
            SPIN_LOCK(lv, 0)
            let newSize : int = @atomicLub(v, lv)
            do ccall M_Print_Int("LVar size is %d\n", newSize)
            fun restart(l : List.list) : List.list = case l
                of CONS(hd : [int, ImplicitThread.thread], tl : List.list) => 
                    if I32Lt(#0(hd), newSize)
                    then do ImplicitThread.@resume-thread(#1(hd) / exh)
                         apply restart(tl)
                    else let rest : List.list = apply restart(tl)
                         let newList : List.list = CONS(hd, rest)
                         return(newList)
                 |nil => return(nil)
               end
            let newWaiters : List.list = apply restart(#3(lv))
            let newWaiters : List.list = promote(newWaiters)
            do #3(lv) := newWaiters
            fun execCallbacks(l : List.list) : () = case l
                of CONS(f : fun(any / exh -> unit), tl : List.list) => 
                    do ccall M_Print("Executing callback\n")
                    cont cb(x : unit) = let _ : unit = apply f(v / exh) SchedulerAction.@stop()
                    let t : ImplicitThread.thread = ImplicitThread.@new-thread(cb / exh)
                    do ImplicitThread.@spawn-thread(t / exh)
                    apply execCallbacks(tl)
                 | nil => return()
                end
            do apply execCallbacks(#8(lv))
            SPIN_UNLOCK(lv, 0)
            return(UNIT)
        ;

        define @waitSize(arg : [[int], lvar] / exh : exh) : unit = 
            let size : int = #0(#0(arg))
            let lv : lvar = #1(arg)
            if I32Lt(#7(lv), size)
            then cont resume(x : unit) = return(x)
                 do ccall M_Print("Going to sleep\n")
                 let t : ImplicitThread.thread = ImplicitThread.@capture(resume / exh)
                 let item : [int, ImplicitThread.thread] = alloc(size, t)
                 fun addItem() : unit = 
                    let l : List.list = #3(lv)
                    let l' : List.list = CONS(item, l)
                    let l' : List.list = promote(l')
                    let l'' : List.list = CAS(&3(lv), l, l')
                    if Equal(l'', l)
                    then SchedulerAction.@stop()
                    else apply addItem()
                 apply addItem()
            else return(UNIT)
        ;

        define @foreach(arg : [lvar, fun(any / exh -> unit)] / exh : exh) : unit = 
            let lv : lvar = #0(arg)
            let callback : fun(any / exh -> unit) = #1(arg)
            fun add() : () = 
                let callbacks : List.list = #8(lv)
                let callbacks' : List.list = CONS(callback, callbacks)
                let callbacks' : List.list = promote(callbacks')
                let callbacks'' : List.list = CAS(&8(lv), callbacks, callbacks')
                if Equal(callbacks'', callbacks)
                then return()
                else apply add()
            do apply add()
            return(UNIT)
        ;

        define @get(lv : lvar / exh : exh) : List.list = 
            return(#2(lv))
        ;
        
    )

    type 'a lvar = _prim(lvar)
    val new : unit -> 'a lvar = _prim(@new)
    val put : 'a * 'a lvar -> unit = _prim(@put)
    val waitSize : (int * 'a lvar) -> unit = _prim(@waitSize)
    val forEach : ('a lvar * ('a -> unit)) -> unit = _prim(@foreach)
    val get : 'a lvar -> 'a list = _prim(@get)
#endif
end
