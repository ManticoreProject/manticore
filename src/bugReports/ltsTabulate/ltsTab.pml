(* ltsTab.pml 3/19/2014
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This simply makes a call to Rope.tabulate.  Sometimes an exception gets raised in the 
 * "rootU" function, and sometimes it simply seg faults.  These two errors seem to be happening
 * nondeterministically.  Perhaps there is a bug in the garbage collector?
**)





val x = [| 1 to 10 |]
