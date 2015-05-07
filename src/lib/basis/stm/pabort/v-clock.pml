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
	  end
*)
struct

    _primcode(
        (*makes sure the version counter is in the global heap*)
        define @init-count(_:unit / exh:exh) : ml_long = 
            let c : ![long] = alloc(2:long)
            let c : ![long] = promote(c)
            return(c);
    )

    val initCounter : unit -> long = _prim(@init-count)

    val counter = initCounter()
    fun getCount() = counter
    
    _primcode(
        typedef stamp = long;
    
        define @getCount = getCount;

        define inline @bump(/ exh:exh) : stamp =
            let counter : ![long] = @getCount(UNIT / exh)
            let old : long = I64FetchAndAdd(&0(counter), 1:long)
            return(old)
        ;

        define inline @get(/exh:exh) : long = 
            let counter : ![long] = @getCount(UNIT/exh)
            return(#0(counter))
        ;

        define inline @get-boxed(/exh:exh) : ![long] = 
            let counter : ![long] = @getCount(UNIT/exh)
            return(counter)
        ;

        define inline @set(newCount : long / exh:exh) : () = 
            let counter : ![long] = @getCount(/exh)
            do #0(counter) := newCount
            return()
        ;

    )

end


 


