(* sequential-runtime.sml
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

structure SequentialRuntime : RUNTIME =
  struct
    datatype splitting_strategy = datatype SplittingStrategy.splitting_strategy
    val splittingStrategy = SplittingStrategy.default
    fun forkjoin (f, g) = (f (), g ())
    val parMap = List.map
    fun numAvailProcs () = 1
    fun otherHungryProcs () = false
    fun workerId () = 0
  end
