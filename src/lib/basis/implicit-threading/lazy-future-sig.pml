(* lazy-future-sig.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Components for futures with lazy semantics.
 *)

signature LAZY_FUTURE =
  sig

    type 'a future

  (* create a future. the second argument is a flag to determine whether the future is cancelable. *)
    val delay : ((unit -> 'a) * bool) -> 'a future
  (* place the future on the current work group ready queue. *)
    val run : 'a future -> unit
  (* synchronize on completion of the future. *)
    val force : 'a future -> 'a
  (* cancel the future. if the future is not cancelable, this operation is a no-op. *)
    val cancel : 'a future -> unit

  end
