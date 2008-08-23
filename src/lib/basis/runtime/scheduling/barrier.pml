(* barrier.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic memory barriers. 
 *)

#define NUM_IN_BARRIER_OFF    0
#define BARRIER_COUNT_OFF     1

structure Barrier =
  struct

    structure PT = PrimTypes

    _primcode(
      typedef barrier = ![
                int,         (* number of fibers that are part of the barrier *)
		int          (* count of ready fibers *)
              ];

    (* create a barrier for n fibers *)
      define @new(n : int / exh : PT.exh) : barrier =
	let barrier : barrier = alloc(n, 0)
	let barrier : barrier = promote(barrier)
	return(barrier)
      ;

    (* signal that the fiber is ready *)
      define @ready(b : barrier / exh : PT.exh) : () =
	let x : int = I32FetchAndAdd (&BARRIER_COUNT_OFF(b), 1)
	return()
      ;

    (* wait for the n fibers to become ready *)
      define @barrier(n : int, b : barrier / exh : PT.exh) : () =
	fun lp () : () =
	    if I32Eq(n, SELECT(BARRIER_COUNT_OFF, b))
	       then return()
	    else apply lp()
	apply lp()
      ;
    )

  end
