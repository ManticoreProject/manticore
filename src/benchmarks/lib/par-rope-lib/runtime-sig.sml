(* runtime-sig.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Some utilities for accessing the run-time scheduler.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

signature RUNTIME =
  sig

    datatype splitting_strategy = datatype SplittingStrategy.splitting_strategy

  (* determines the splitting strategy used by this functor *)
    val splittingStrategy : splitting_strategy

  (* forkjoin (f, g) *)
  (* forks two tasks to evaluate f () and g () (possibly) in parallel and returns the pair of return values *)
  (* returned by the two calls *)
    val forkjoin : (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  (* parMap f xs *)
  (* maps in parallel the given function over each element of the list *)
    val parMap : ('a -> 'b) -> 'a list -> 'b list

  (* numAvailProcs () *)
  (* returns the number of processors currently available to the computation *)
    val numAvailProcs : unit -> int

  (* otherHungryProcs () *)
  (* returns true if some other processors may be hungry for work *)
    val otherHungryProcs : unit -> bool

  (* returns the unique integer id of the worker or processor on which the calling context is executing *)
    val workerId : unit -> int

  end
