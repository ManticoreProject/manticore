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

    val counter = 1
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

        define @get(/ exh:exh) : stamp = 
            let counter : ml_int = @getCount(UNIT / exh)
            return(#0(counter))
        ;
    )

end


 


