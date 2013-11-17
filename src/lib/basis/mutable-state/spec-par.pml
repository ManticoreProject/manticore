(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)



structure SpecPar (*: sig
    val spec : (() -> 'a * () -> 'b) -> ('a, 'b)
    end*) = struct

    _primcode(
        define @pSpec(arg : [fun(unit -> any), fun(unit -> any)] / exh:exh):[any,any] = 
            let a : fun(unit -> any) = #0(arg)
            let b : fun(unit -> any) = #1(arg)
            return(apply a(), apply b())
        ;
        
    )

    val spec : (() -> 'a * () -> 'b) -> ('a * 'b) = _prim(pSpec)


end

