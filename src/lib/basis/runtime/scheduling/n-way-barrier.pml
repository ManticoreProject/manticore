(* n-way-barrier.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * N-way memory barrier. Fibers spin wait to pass through the barrier.
 *)

structure NWayBarrier =
  struct

    _primcode(

#define NUM_IN_BARRIER_OFF    0
#define BARRIER_COUNT_OFF     1

      typedef barrier = ![
                int,         (* NUM_IN_BARRIER_OFF: number of fibers that are part of the barrier *)
		int          (* BARRIER_COUNT_OFF: count of ready fibers *)
              ];

    (* create a barrier *)
      define @new(n : int    (* number of participants *)
		  / exh : exh) : barrier =
	let barrier : barrier = alloc(n, 0)
	let barrier : barrier = promote(barrier)
	return(barrier)
      ;

    (* one more participant is ready to pass through the barrier *)
      define @ready (b : barrier / exh : exh) : () =
	let x : int = I32FetchAndAdd (&BARRIER_COUNT_OFF(b), 1)
	return()
      ;

    (* wait to pass through the barrier *)
      define @barrier(b : barrier / exh : exh) : () =
        fun isDone (/ exh : exh) : bool =
	    return(I32Eq(SELECT(NUM_IN_BARRIER_OFF, b), SELECT(BARRIER_COUNT_OFF, b)))
        PrimSynch.@spin-wait(isDone / exh)
      ;
    )

  end
