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
    val measured     : ced * int * float * float -> unit
  end *) = struct

  structure CAI = UnsafeCacheAlignedIntArray
  structure CAF = UnsafeCacheAlignedFloatArray

  val nbVProcs = VProc.numVProcs ()
  fun vprocID () = VProc.id (VProc.host ())

  val defaultConstant = 1.0
  val nbBegin = 4
  val nbGrouped = 20 * nbVProcs
  val alpha = 0.1
  val minChange = 0.05
  val maxChange = 1.0

  type info = (CAI.array * CAI.array * CAF.array)

  type ced = (FloatRef.ref * info Array.array)

  fun initialize name = let
    val glob = FloatRef.new defaultConstant
    val loc = Array.tabulate (nbVProcs, fn _ => let
                     val start = CAI.create 1
		     val nb = CAI.create 1
		     val sum = CAF.create 1
		     in
		       (start, nb, sum)
		     end)
    in
      (glob, loc)
    end

  fun estimate ((glob, _), m) = Float.fromInt m * FloatRef.get glob

  fun post (glob, c) = let
    val g = FloatRef.get glob
    val diff = c - g
    in
      if Float.abs diff > g * minChange then let
	val diff' = 
	      if Float.abs diff > g * maxChange then 
		Float.fromInt (Float.sign diff) * g * maxChange
	      else
		diff
	val incr = alpha * diff'
	in
	  FloatRef.set (glob, g + incr)
	end
      else
	()
    end

  fun measured ((glob, loc), m, t, est) = let
    val p = vprocID ()
    val (startA, nbA, sumA) = Array.sub (loc, p)
    val start = CAI.sub (startA, 0) = 1
    val nb = CAI.sub (nbA, 0)
    val sum = CAF.sub (sumA, 0)
    val start' = start andalso (nb < nbBegin)
    val nb' = nb + 1
    val sum' = sum + (t / Float.fromInt m)
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

end

structure OracleScheduler (* : sig
    type ('a, 'b) quad = CED.ced * ('a -> int) * ('a -> 'b) * ('a -> 'b)
    val oracle : ('a, 'b) quad * 'a -> 'b
  end *) = struct

  structure CAI = UnsafeCacheAlignedIntArray
  structure CAF = UnsafeCacheAlignedFloatArray

  val reportPredictionError = ParseCommandLine.exists "-oracle-report-prediction-error"
  val predictionErrorNB = 
    ParseCommandLine.parse1 "-oracle-nb-prediction-errors" Int.fromString 500

  val nbVProcs = VProc.numVProcs ()
  fun vprocID () = VProc.id (VProc.host ())

  (* Prediction error *)
  (* This memory records the current global average error across every oracle *)
  (* prediction. *)

  type prediction_error_info = (CAI.array * CAF.array)

  type prediction_error = prediction_error_info Array.array

  val predictionError = 
    Array.tabulate (nbVProcs, fn _ => (CAI.create 1, CAF.create predictionErrorNB))

  fun recordPredictionError (perr, m, t) = let
    val p = vprocID ()
    val (nbA, errA) = Array.sub (perr, p)
    val nb = CAI.sub (nbA, 0)
    val nb' = nb + 1
    in
      if nb < predictionErrorNB then (
       CAI.update (nbA, 0, nb');
       CAF.update (errA, nb, m / t))
      else
	()
    end

  fun doReportPredictionError perr = let
    fun proc (_, (nbA, errA)) = let
      val nb = CAI.sub (nbA, 0)
      fun go i =
        if i < nb then (
	  Print.print (Float.toString (CAF.sub (errA, i))^" ");
	  go (i + 1))
	else
	  ()
      in
	go 0
      end
    in
      Array.appi proc perr;
      ()
    end

  val kappa = 1.0

  val minWorkForTimerRes = 0.000001  (* 1 microsecond *)

  fun measuredRun (r, m, est, k) = let
    val t1 = CycleCounter.getTicks ()
    val v = k ()
    val t2 = CycleCounter.getTicks ()
    val elapsed = Float.fromLong (CycleCounter.elapsed (t1, t2))
    in
      if reportPredictionError then
	recordPredictionError (predictionError, est, elapsed)
      else 
	();
      CED.measured (r, m, elapsed, est);
      v
    end

  fun oracle ((r, f_cst, f_seq, f_orc), v) = let
    val m = f_cst v
    val est = CED.estimate (r, m)
    val b = est > kappa
    fun k_seq () = f_seq v
    fun k'_seq () = measuredRun (r, m, est, k_seq)
    fun k_orc () = f_orc v
    val k = if b then k_orc else k'_seq
    in
      (b, k)
    end

end
