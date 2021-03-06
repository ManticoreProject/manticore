(* runtime.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Runtime = struct

  val numProcs = VProc.numVProcs ()

  val par2 = fn (f, g) => (| f (), g () |)

  (* val parN = fn l => List.map (fn f => f ()) l *)

#ifndef SEQUENTIAL
  val hungryProcs = WorkStealing.isLocalDequeEmpty
#else
  fun hungryProcs () = false
#endif

end
