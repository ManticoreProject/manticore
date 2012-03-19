(* def-implicit-thread-sched.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Initialize the default implicit-thread scheduler.
 *)

val defaultImplicitThreadSched = GlobalBFSScheduler.workGroup ()
val workStealingForThread0 = WorkStealing.workGroup ()
val () = ImplicitThread.defaultWorkGroupBegin defaultImplicitThreadSched 
val () = ImplicitThread.defaultWorkGroupBegin workStealingForThread0
fun getDefaultImplicitThreadSched () = defaultImplicitThreadSched

val () = DEBUG("schedulers: initialized default implicit-thread scheduler")
