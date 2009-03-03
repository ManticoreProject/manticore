(* fcms.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * First-class monadic schedules. The implementation is adapted from the one described
 * by Mirani and Hudak (TOPLAS, 2004).
 *)

structure FCMS (*  : FCMS *) =
  struct

    structure Susp = ParSusp

    datatype 'a os_monad = MK_OS of unit -> 'a

    fun unit x = MK_OS(fn () => x)

    fun bind (MK_OS x) f = f(x())
 
(* FIXME: see test/ty.pml *)
(*    type 'a future = 'a Susp.suspension *)

    fun future f = Susp.delay(f, false)
    val touch = Susp.force
    val cancel = Susp.cancel

    type schedule = unit

    val empty = MK_OS(fn () => ())

  (* schedule the future *)
    fun d susp = MK_OS(fn () => Susp.run susp)

  (* synchronize on the future *)
    fun r susp = MK_OS(fn () => (Susp.force susp; ()))

  (* schedule in parallel *)
    fun par (MK_OS s1) (MK_OS s2) = MK_OS(fn () => let
	  val x = future s1
	  val y = future s2
          in
            Susp.run x;
	    Susp.run y;
	    Susp.force x;
	    Susp.force y;
	    ()
	  end)

  (* schedule sequentially (left to right) *)
    fun seq (MK_OS s1) (MK_OS s2) = MK_OS(fn () => (
	  s1();
	  s2();
          ()))

  (* completely evaluate the future *)
    fun e susp = seq (d susp) (r susp)

    fun primCurrentLoad _ = (raise Fail "todo")

  (* get the load on the current scheduler (typically the number of active workers) *)
    val getLoad = MK_OS primCurrentLoad

  (* schedule in sequential order when load is high, and in parallel otherwise. the first
   * argument is the threshold value that is compared to the load.
   *)
    fun parWhenUnloaded thresh s1 s2 = 
	  bind getLoad (fn load => if load < thresh then par s1 s2 else seq s1 s2)

  (* schedule a future *)
    fun sched susp (MK_OS s) = (
	  s();
	  Susp.run susp;
	  Susp.force susp)

  end
