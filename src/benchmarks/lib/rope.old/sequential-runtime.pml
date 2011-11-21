(* sequential-runtime.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Sequential run-time support.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure SequentialRuntime =
  struct
    datatype splitting_strategy = datatype SplittingStrategy.splitting_strategy
    val splittingStrategy = NO_SPLIT
    fun forkjoin (f, g) = (f (), g ())
    val parMap = List.map
    fun numAvailProcs () = 1
    fun otherHungryProcs () = false
  end
