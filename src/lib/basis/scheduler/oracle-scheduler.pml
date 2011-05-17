(* oracle-scheduler.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *
 *)

structure CED (* :> sig
    type ced
    val initialize   : string -> ced
    val estimate     : ced * int -> float
    val measured     : ced * int * float -> unit

    val addToLog     : ced * int * float -> unit
    val printLog     : ced -> unit
  end *) = struct

  structure CAI = UnsafeCacheAlignedIntArray
  structure CAF = UnsafeCacheAlignedFloatArray

  val nbVProcs = VProc.numVProcs ()
  fun vprocID () = VProc.id (VProc.host ())

  val logSz = (ParseCommandLine.parse1 "-oracle-logging" Int.fromString 0) div nbVProcs
  val logCst = ParseCommandLine.find "-oracle-logging-constant"

  val defaultConstant = 1.0
  val nbGrouped = 20 * nbVProcs
  val nbBegin = nbGrouped div 5
  val alpha = 0.5
  val minChangeFactor = 0.05
  val maxChangeFactor = 1.0

  (** Logging **)
  (* The local log consists of the following parts: *)
  (*   - The memory cell nbA records the number of elements contained in the local *)
  (*     log. After this number exceeds lgSz, we stop adding elements. *)
  (*   - The array estA records the task-execution times estimated by the oracle. *)
  (*   - The array actA records the actual task execution times. *)
  (*   - The array cstA records the current constant associated with the CED data structure *)
  type log_local = (CAI.array * CAF.array * CAF.array * CAF.array)
  (* The log consists of an array of local logs. We maintain one local log for each vproc. *)
  type log = string * log_local Array.array

  type ced_local = (CAI.array * CAI.array * CAF.array)

  type ced = (FloatRef.ref * ced_local Array.array * log)

  fun initializeLog name = let
    fun f _ = (CAI.create 1, CAF.create logSz, CAF.create logSz, CAF.create logSz)
    in
      (name, Array.tabulate (nbVProcs, f))
    end

  fun initialize name = let
    val glob = FloatRef.new defaultConstant
    val loc = Array.tabulate (nbVProcs, fn _ => let
                     val startA = CAI.create 1
		     val nbA = CAI.create 1
		     val sumA = CAF.create 1
		     in
		       CAI.update (startA, 0, 1);
		       CAI.update (nbA, 0, 0);
		       CAF.update (sumA, 0, 0.0);
		       (startA, nbA, sumA)
		     end)
    in
      (glob, loc, initializeLog name)
    end

  fun workOf m = if m < 1 then 1 else m

  fun estimate ((glob, _, _), m) = Float.fromInt (workOf m) * FloatRef.get glob

  fun post (glob, c) = let
    val g = FloatRef.get glob
    val diff = c - g
    in
      if Float.abs diff > g * minChangeFactor then let
	val diff' = 
	      if Float.abs diff > g * maxChangeFactor then 
		Float.fromInt (Float.sign diff) * g * maxChangeFactor
	      else
		diff
	val incr = alpha * diff'
	in
	  FloatRef.set (glob, g + incr)
	end
      else
	()
    end

  fun measured ((glob, loc, log), m, t) = let
    val p = vprocID ()
    val (startA, nbA, sumA) = Array.sub (loc, p)
    val start = CAI.sub (startA, 0) = 1
    val nb = CAI.sub (nbA, 0)
    val sum = CAF.sub (sumA, 0)
    val start' = start andalso (nb < nbBegin)
    val nb' = nb + 1
    val w = workOf m
    val sum' = sum + (t / Float.fromInt w)
    val c = sum' / Float.fromInt nb'
    in
      if start' then 
	post (glob, c)
      else if nb' = nbGrouped then (
	post (glob, c);
	CAI.update (startA, 0, 0);
	CAI.update (nbA, 0, 0);
	CAF.update (sumA, 0, 0.0))
      else (
	CAI.update (startA, 0, if start' then 1 else 0);
	CAI.update (nbA, 0, nb');
	CAF.update (sumA, 0, sum'))
    end

  fun addToLog (r, m, act) = let
    val (glob, _, (cstName, log)) = r
    fun add () = let
      val est = estimate (r, m)
      val p = vprocID ()
      val (nbA, estA, actA, cstA) = Array.sub (log, p)
      val nb = CAI.sub (nbA, 0)
      in
	if nb < logSz then (
	  CAF.update (estA, nb, est);
	  CAF.update (actA, nb, act);
	  CAF.update (cstA, nb, FloatRef.get glob);
	  CAI.update (nbA,  0,  nb + 1);
	  ())
	else
	  ()
      end
    in
      case logCst
	of SOME (cstName'::_) => 
	     if String.same (cstName, cstName') then add () else  ()
	 | _ => add ()
    end

  fun printCAF (n, a) = let
    fun go i =
      if i < n then (
	Print.print (Float.toString (CAF.sub (a, i))^" ");
	go (i + 1))
      else
	()
    in
      go 0
    end

  fun printLog r = let
    val (_, _, (name, log)) = r
    val nbAs = Array.map (fn (nbA, _, _, _) => nbA) log
    fun pr (p, a) = (
      Print.print (Int.toString p^"\n");
      printCAF (CAI.sub (Array.sub (nbAs, p), 0), a);
      Print.print "\n";
      ())
    val estAs = Array.map (fn (_, estA, _, _) => estA) log
    val actAs = Array.map (fn (_, _, actA, _) => actA) log
    val cstAs = Array.map (fn (_, _, _, cstA) => cstA) log
    in
      Print.print (name^"\n");
      Array.appi pr estAs;
      Array.appi pr actAs;
      Array.appi pr cstAs;
      ()
    end

end

structure OracleScheduler (* : sig
    type ('a, 'b) quad = CED.ced * ('a -> int) * ('a -> 'b) * ('a -> 'b)
    val oracle : ('a, 'b) quad * 'a -> 'b
  end *) = struct

  structure CAI = UnsafeCacheAlignedIntArray
  structure CAF = UnsafeCacheAlignedFloatArray

  val nbVProcs = VProc.numVProcs ()
  fun vprocID () = VProc.id (VProc.host ())

  val kappa = Float.fromInt (ParseCommandLine.parse1 "-oracle-kappa" Int.fromString 500000)

  fun measuredRun (r, m, k) = let
    val t1 = CycleCounter.getTicks ()
    val v = k ()
    val t2 = CycleCounter.getTicks ()
    val elapsed = Float.fromLong (CycleCounter.elapsed (t1, t2))
    in
      if CED.logSz > 0 then CED.addToLog (r, m, elapsed) else ();
      CED.measured (r, m, elapsed);
      v
    end

  fun oracle ((r, f_cst, f_seq, f_orc), v) = let
    val m = f_cst v
    val est = CED.estimate (r, m)
    val b = est > kappa
    fun k_seq () = f_seq v
    fun k'_seq () = measuredRun (r, m, k_seq)
    fun k_orc () = f_orc v
    val k = if b then k_orc else k'_seq
    in
      (b, k)
    end

end
