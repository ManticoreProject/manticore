(* par-susp-sig.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions
 * 
 * A suspension is a value which represents a computation that may or
 * may not have evauated. Below is a substrate for "parallel
 * suspensions", which are suspensions that are allows to evaluate
 * across multiple processors. This substrate consists of a suspension
 * type and a few operations for controling the evaluation of
 * suspensions.
 *
 *)

signature PAR_SUSP =
  sig

    type 'a suspension

  (* delay (f, cancelable) *)
  (* create a suspension for f *)
  (* the flag called cancelable determines whether the suspension is
  cancelable. *)
  (* NOTE: this operation does *not* initiate the evaluation of the
  suspension *)
    val delay : ((unit -> 'a) * bool) -> 'a suspension

  (* run s *)
  (* allow the evaluation of the suspension to be distributed to a
  remote processor (e.g., by placing the suspension on a shared work
  queue) *)
    val run : 'a suspension -> unit

  (* force s *)
  (* returns the result of evaluating the suspension *)
  (* this operation blocks if the suspension is being evaluated by a
  remote processor, evaluates the suspension function if the
  suspension has not yet been evaluated, and otherwise returns the
  result of evaluating the suspension *)
  (* precondition: s has not been canceled *)
    val force : 'a suspension -> 'a

  (* cancel s *)
  (* cancels the evaluation of s *)
  (* if s is being evaluated by a remote processor, the remote
  processor is interrupted and the evaluation of s is aborted. *)
  (* if a processor subsequently tries to evaluate s, the evaluation
  will abort. *)
    val cancel : 'a suspension -> unit

  end
