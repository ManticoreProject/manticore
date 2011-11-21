functor WorkStealingStatsGenFn (
              structure E : WORK_STEALING_EXPERIMENT
            ) : WORK_STEALING_STATS = 
  struct

      open E

      val maxNProcs = List.foldl Int.max 1 (List.map #n_procs E.runs)

      (* returns the average and standard deviation *)
      fun stats (data : real list) = 
	  let
	      val ndata = real(List.length data)
	      val avg = (List.foldl Real.+ 0.0 data) / ndata
	      fun sq x = x*x
	      val std = Math.sqrt(List.foldl (fn (x, s) => s + sq(x - avg)) 0.0 data) / ndata
	  in
	      {avg = avg, std = std}
	  end

      (* returns fraction of overall execution time is spent busy or idle *)
      fun timeBreakdown ({workStealing = {vprocState, ...}, ...} : Common.run) =
	  let
	      val timeBusyOverall = List.foldl (op +) 0.0 (List.map #timeBusy vprocState)
	      val timeIdleOverall = List.foldl (op +) 0.0 (List.map #timeIdle vprocState)
	      val timeOverall = timeBusyOverall + timeIdleOverall
	  in
              timeBusyOverall / timeOverall
	  end

      (* breakdown of work stealing load balance for all n-procssor runs *)
      fun loadBreakdown n : {n_procs : int, avgTimeBusyOverall : real, stdDevTimeOverall : real} =
	  let
	      val nProcRuns = List.filter (fn {n_procs, ...} : Common.run => n_procs = n) E.runs
	      val {avg, std} = stats (List.map timeBreakdown nProcRuns)
	  in
	      {n_procs = n, avgTimeBusyOverall=avg, stdDevTimeOverall=std}
	  end
	  
      val loadBreakdowns : {n_procs : int, avgTimeBusyOverall : real, stdDevTimeOverall : real} list =
	  List.map loadBreakdown E.n_procs

      fun ropeRebalancingTotTime ({time_sec, workStealing = {vprocState, ...}, ...} : Common.run) =
	  let
	      val {avg,std} = stats (List.map #timeRebalancing vprocState)
	  in
	      {avg=avg / time_sec, std=std / time_sec}
	  end

      fun ropeRebalancingNumRebalanced ({time_sec, workStealing = {vprocState, ...}, ...} : Common.run) =
	  stats (List.map (real o #numEltsRebalanced) vprocState)

      fun ropeRebalancing1 n : {n_procs : int, avgTotTimeAllProcs : real, stdTotTimeAllProcs : real,
			       avgNumEltsRebalancedPerProc : real, stdNumEltsRebalancedPerProc : real} =
	  let
	      val nProcRuns = List.filter (fn {n_procs, ...} : Common.run => n_procs = n) E.runs
	      val sts = List.map ropeRebalancingTotTime nProcRuns
	      val {avg, ...} = stats (List.map #avg sts)
	      val {avg=std, ...} = stats (List.map #std sts)

	      val stsNrb = List.map ropeRebalancingNumRebalanced nProcRuns
	      val {avg=avgrb, ...} = stats (List.map #avg stsNrb)
	      val {avg=stdrb, ...} = stats (List.map #std stsNrb)
	  in
	      {n_procs = n, avgTotTimeAllProcs = avg, stdTotTimeAllProcs = std, avgNumEltsRebalancedPerProc = avgrb,
	       stdNumEltsRebalancedPerProc = stdrb}
	  end

      val ropeRebalancing = List.map ropeRebalancing1 E.n_procs

      fun steal n =
	  let 
	      fun get ({workStealing={numSteals, ...}, ...} : Common.run) = List.foldl (op +) 0 numSteals
	      val nProcRuns = List.filter (fn {n_procs, ...} : Common.run => n_procs = n) E.runs
	      val {avg, std} = stats (List.map (real o get) nProcRuns)
	  in
	      {n_procs=n, avg=avg, std=std}
	  end
      val steals = List.map steal E.n_procs

      fun failedSteal n =
	  let 
	      fun get ({workStealing={numFailedStealAttempts, ...}, ...} : Common.run) = List.foldl (op +) 0 numFailedStealAttempts
	      val nProcRuns = List.filter (fn {n_procs, ...} : Common.run => n_procs = n) E.runs
	      val {avg, std} = stats (List.map (real o get) nProcRuns)
	  in
	      {n_procs=n, avg=avg, std=std}
	  end
      val failedSteals = List.map failedSteal E.n_procs

      fun timeToSteal n =
	  let 
	      fun max x = List.foldl Real.max 0.0 x
	      fun getAvg ({workStealing={timeStealing, ...}, ...} : Common.run) = max (List.map #avg timeStealing)
	      fun getMax ({workStealing={timeStealing, ...}, ...} : Common.run) = max (List.map #max timeStealing)
	      fun getTotTime ({time_sec, ...} : Common.run) = time_sec
	      val nProcRuns = List.filter (fn {n_procs, ...} : Common.run => n_procs = n) E.runs
	      val {avg=avgAvg, std=stdAvg} = stats (List.map (getAvg) nProcRuns)
	      val {avg=avgMax, std=stdMax} = stats (List.map ( getMax) nProcRuns)
	      val {avg=avgTotTime, std=stdTotTime} = stats (List.map getTotTime nProcRuns)
	  in
	      {n_procs=n, avgTotTime=avgTotTime, stdTotTime=stdTotTime, avgAvg=avgAvg, stdAvg=stdAvg, avgMax=avgMax, stdMax=stdMax}
	  end
      val timeSpentStealing = List.map timeToSteal E.n_procs

  end
