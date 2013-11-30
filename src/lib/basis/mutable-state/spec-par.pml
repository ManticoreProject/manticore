(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)

#include "debug.def"

structure SpecPar (*: sig
    val spec : (() -> 'a * () -> 'b) -> ('a, 'b)
    end*) = struct

    _primcode(

        define @get-key(k : any / exh : exh) : any = 
            let k' : [int] = ([int])k
            let key : int = (int)#0(k')
            fun loop(dict : List.list) : any = case dict
                of CONS(hd : [[int], any], tail : List.list) => 
                    if I32Eq(#0(#0(hd)), key)
                    then return(#1(hd))
                    else apply loop(tail)
                |nil => let e : exn = Fail(@"FLS key not found in SpecPar")
                        do ccall M_Print("FLS key not found in SpecPar get-key\n")
                        throw exh(e)
                end
            let dict : List.list = FLS.@get-dict(UNIT / exh)
            apply loop(dict)
        ;

        define @set-key(arg : [[int], any] / exh : exh) : () = 
            let k : [int] = #0(arg)
            let value : any = #1(arg)
            let key : int = #0(k)
            let keyValPair : [[int], any] = ([[int], any])arg
            fun loop(dict : List.list) : List.list = case dict
                of CONS(hd : [[int], any], tail : List.list) => 
                    if I32Eq(#0(#0(hd)), key)
                    then do ccall M_Print("Successfully updated dictionary\n")
                         return(CONS(keyValPair, tail))
                    else let rest : List.list = apply loop(tail)
                         return (CONS(hd, rest))
                | nil => let e : exn = Fail(@"FLS key not found in SpecPar set-key")
                         do ccall M_Print("FLS key not found in SpecPar set-key\n")
                         throw exh(e)
                end
            let dict : List.list = FLS.@get-dict(UNIT / exh)
            let newDict : List.list = apply loop(dict)
            let _ : unit = FLS.@set-dict(newDict / exh)
            return()
        ;
        
        define @pSpec(arg : [fun(unit / exh -> any), fun(unit / exh -> any)] / exh : exh):[any,any] = 
            let a : fun(unit / exh -> any) = #0(arg)
            let b : fun(unit / exh -> any) = #1(arg)
            let dummy : any = enum(0) : any
            let res : ![any,any] = alloc(dummy, dummy)
            let count : ![int] = alloc(0)
            cont slowClone(_ : unit) = (*work that can potentially be stolen*)
                do ccall M_Print("Spawning new thread\n")
                let keyValPair : [[int], bool] = alloc(alloc(SPEC_KEY), true)
                do @set-key(keyValPair / exh)  (*Put in spec mode*)
                let res : ![any,any] = promote(res)
                let v_1 : any = apply b(UNIT / exh)
                let v'_1 : any = promote(v_1)
                do #1(res) := v'_1 (*TODO: wait until #0(res) is filled and then commit*)
                let updated : int = I32FetchAndAdd(&0(count), 1)
                if I32Eq(updated, 1)
                then return(res)
                else SchedulerAction.@stop()
            let thd : ImplicitThread.thread = ImplicitThread.@new-thread(slowClone/exh)
            do ImplicitThread.@spawn-thread(thd/exh)
            let v_0 : any = apply a(UNIT/exh)
            let removed : Option.option = ImplicitThread.@remove-thread(thd/exh)
            if NotEqual(removed, UNIT) (*not stolen*)
            then let v_1 : any = apply b(UNIT/exh)
                 let res : [any,any] = alloc(v_0, v_1)
                 return(res)
            else let res : ![any,any] = promote(res)
                 let v'_0 : any = promote(v_0)
                 do #0(res) := v'_0
                 let updated : int = I32FetchAndAdd(&0(count), 1)
                 if I32Eq(updated, 1)
                 then return(res)
                 else SchedulerAction.@stop()
                 
        ;
        
    )

    val spec : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b) = _prim(@pSpec)


    

    
end

