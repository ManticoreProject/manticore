(* fcms-sig.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * First-class monadic schedules. The implementation is adapted from the one described
 * by Mirani and Hudak (TOPLAS, 2004).
 *)

signature FCMS =
  sig

  (** The scheduling monad **)

    type 'a os_monad
    val unit : 'a -> 'a os_monad
    val bind : 'a os_monad -> ('a -> 'b os_monad) -> 'b os_monad

  (** Primitives for futures **)

    type 'a future
  (* spawn a future *)
    val future : (unit -> 'a) -> 'a future
  (* synchronize on a future *)
    val touch : 'a future -> 'a

  (** Scheduler combinators **)

    type schedule

  (* schedule the future *)
    val d : 'a future -> schedule os_monad
  (* synchronize on the future *)
    val r : 'a future -> schedule os_monad
  (* completely evaluate the future *)
    val e : 'a future -> schedule os_monad
  (* schedule in parallel *)
    val par : schedule os_monad -> schedule os_monad -> schedule os_monad
  (* schedule sequentially (left to right) *)
    val seq : schedule os_monad -> schedule os_monad -> schedule os_monad
  (* get the load on the current scheduler (typically the number of active workers) *)
    val getLoad : int os_monad
  (* schedule in sequential order when load is high, and in parallel otherwise. the first
   * argument is the threshold value that is compared to the load.
   *)
    val parWhenUnloaded : int -> schedule os_monad -> schedule os_monad -> schedule os_monad

  (** The scheduler **)

  (* schedule a future *)
    val sched : 'a future -> schedule os_monad -> 'a

  end
