(* def-implicit-thread-sched.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Initialize the default implicit-thread scheduler.
 *)

val defaultImplicitThreadSched = GlobalBFSScheduler.workGroup()
val () = ImplicitThread.defaultBegin defaultImplicitThreadSched
fun getDefaultImplicitThreadSched () = defaultImplicitThreadSched

val () = DEBUG("schedulers: initialized default implicit-thread scheduler")

