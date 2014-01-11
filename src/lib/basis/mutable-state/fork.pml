(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for fork parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)

structure Fork (*: sig
    val fork : (unit -> unit) -> unit
    end*) = 
struct

    _primcode(
        define @init(_ : unit / exh : exh) : [int] = 
            let res : ![int] = alloc(2)
            return(res)
        ;

        define @bump(c : [int] / exh : exh) : unit = 
            let c : ![int] = (![int])c
            let old : int = I32FetchAndAdd(&0(c), 1)
            return(UNIT)
        ;

    )

    val init : unit -> int = _prim(@init)
    val bump : int -> unit = _prim(@bump)

    val tidCount = init()

    fun newTID() = 
        let val _ = bump(tidCount)
        in tidCount
        end
      
        

    _primcode(
         typedef tid = ![
            int,           (*Size of the list*)
            List.list];    (*thread id*)        

        define @newTID = newTID;

        
            
        define @fork(a : fun(unit / exh -> unit) / exh : exh) : unit =
            let cbl : Cancelation.cancelable = Cancelation.@new(UNIT / exh)
            let parentTID : tid = FLS.@get-key(alloc(TID_KEY) / exh)
            let parentSpec : bool = FLS.@get-key(alloc(SPEC_KEY) / exh)
            cont finish(result : any) = return(result)
            cont t(_ : unit) =
                let n : [int] = @newTID(UNIT / exh)
                let tid : tid = alloc(I32Add(#0(parentTID), 1), CONS((any)alloc(#0(n)), #1(parentTID))) 
                do FLS.@set-key(alloc(alloc(TID_KEY), tid) / exh)
                do FLS.@set-key(alloc(alloc(SPEC_KEY), parentSpec) / exh)
                let res : any = apply a(UNIT / exh)
                throw finish(res)
            let thread : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread(t, cbl / exh)
            do ImplicitThread.@spawn-thread(thread / exh)
            return(UNIT)
        ;

    )
    val fork : (unit -> unit) -> unit = _prim(@fork)
end




