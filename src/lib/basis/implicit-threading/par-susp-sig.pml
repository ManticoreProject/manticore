(* par-susp-sig.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions.
 *)

signature PAR_SUSP =
  sig

    type 'a suspension

  (* create a suspension. the second argument is a flag to specify whether the suspension is cancelable. *)
    val delay : ((unit -> 'a) * bool) -> 'a suspension

  (* place the suspension on the current work group ready queue. *)
    val run : 'a suspension -> unit

  (* synchronize on completion of the suspension. *)
    val force : 'a suspension -> 'a

  (* cancel the suspension. if the suspension is not cancelable, this operation is a no-op. *)
    val cancel : 'a suspension -> unit

  end
