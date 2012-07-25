(* future-sig.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Futures.
 *)

signature FUTURE =
  sig

    type 'a thunk = unit -> 'a
    type 'a future

    val touch : 'a future -> 'a
    val future : 'a thunk -> 'a future
    val cancel : 'a future -> unit

  end
