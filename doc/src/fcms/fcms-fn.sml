(* fcms-fn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * First-class monadic schedules. The implementation is adapted from the one described
 * by Mirani and Hudak (TOPLAS, 2004).
 *)

functor FCMSFn (

  structure F : LAZY_FUTURE
  val primCurrentLoad : unit -> int

  ) :> FCMS = struct

    datatype 'a os_monad = MK_OS of unit -> 'a

    fun unit x = MK_OS(fn () => x)

    fun bind (MK_OS x) f = f(x())

    type 'a future = 'a F.future

    val future = F.delay
    val touch = F.force

    type schedule = unit

  (* schedule the future *)
    fun d fut = MK_OS(fn () => F.run fut)

  (* synchronize on the future *)
    fun r fut = MK_OS(fn () => (F.force fut; ()))

  (* schedule in parallel *)
    fun par (MK_OS s1) (MK_OS s2) = MK_OS(fn () => let
	  val x = future s1
	  val y = future s2
          in
	    F.force x;
	    F.force y;
	    ()
	  end)

  (* schedule sequentially (left to right) *)
    fun seq (MK_OS s1) (MK_OS s2) = MK_OS(fn () => (
	  s1();
	  s2();
          ()))

  (* completely evaluate the future *)
    fun e fut = seq (d fut) (r fut)

  (* get the load on the current scheduler (typically the number of active workers) *)
    val getLoad = MK_OS primCurrentLoad

  (* schedule in sequential order when load is high, and in parallel otherwise. the first
   * argument is the threshold value that is compared to the load.
   *)
    fun parWhenUnloaded thresh s1 s2 = 
	  bind getLoad (fn load => if load < thresh then par s1 s2 else seq s1 s2)

  (* schedule a future *)
    fun sched fut (MK_OS s) = (
	  s();
	  F.run fut;
	  F.force fut)

  end
