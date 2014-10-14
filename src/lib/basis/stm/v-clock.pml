(* v-clock.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A global version clock used for checking STM consistency
 *)


structure VClock = (* :
    sig
	    val bump : unit -> int
	    val get : unit -> int
	end
*)
struct

    _primcode(
        (*makes sure the version counter is in the global heap*)
        define @init-count(_:unit / exh:exh) : ml_int = 
            let c : [int] = alloc(1)
            let c : [int] = promote(c)
            return(c);
    )

    val initCounter : unit -> int = _prim(@init-count)

    val counter = initCounter()
    fun getCount() = counter
    
    _primcode(
        typedef stamp = int;
    
        define @getCount = getCount;

        define @bump(/ exh:exh) : stamp =
            let counter : ml_int = @getCount(UNIT / exh)
            let counter : ![int] = (![int]) counter
            let old : int = I32FetchAndAdd(&0(counter), 1)
            return(old)
        ;

    )

end


 


