structure Report = struct

  fun println s = print (s^"\n")

  fun pct r = Real.fmt (StringCvt.FIX (SOME 2)) (r * 100.0)

  fun splitStratToString ss =
      (case List.tl (String.tokens (fn c => c = #" ") ss)
	of ["ebs-sp", n] => "EBS_SP("^n^")"
	 | ["ebs-ap", k, v] => "EBS_AP("^k^","^v^")"
	 | ["lbs", n] =>    "LBS   ("^n^")"
	 | ["lps", n] =>    "LPS   ("^n^")"
	 | ["ns"] =>        "NS           ")

  fun fmtLoadBreakdown (_, args, [{n_procs, avgTimeBusyOverall, stdDevTimeOverall}]) =
      String.concatWith "\t\t" [splitStratToString args, pct avgTimeBusyOverall ^ "%"]

  (* report load breakdowns *)
  val _ = (
      println "Load breakdown";
      println ("Benchmark: "^(#1 (List.hd WorkStealingStats.loadBreakdowns)));
      println (String.concatWith "\t\t"
	     ["Split Strat.", "Time busy per proc"]);
      List.app (println o fmtLoadBreakdown) WorkStealingStats.loadBreakdowns;
      println ""
  )

  fun fmtRopeRebalancing (_, args, [{n_procs, avgTotTimeAllProcs, stdTotTimeAllProcs,
			 avgNumEltsRebalancedPerProc, stdNumEltsRebalancedPerProc}]) =
      String.concatWith "\t\t\t" [splitStratToString args, pct avgTotTimeAllProcs ^ "%", 
				  pct stdTotTimeAllProcs ^ "%",
				  Real.fmt (StringCvt.FIX (SOME 2)) avgNumEltsRebalancedPerProc,
				  Real.fmt (StringCvt.FIX (SOME 2)) stdNumEltsRebalancedPerProc
				 ]

  (* report rope rebalancing stats *)
  val _ = (
      println "Time spent rebalancing ropes";
      println ("Benchmark: "^(#1 (List.hd WorkStealingStats.ropeRebalancing)));
      println (String.concat
	     ["Split Strat.\t\t\t", "Time per proc.\t\t", "Error\t\t\t", "ANRPP\t\t\t\t", "SNRPP"]);
      List.app (println o fmtRopeRebalancing) WorkStealingStats.ropeRebalancing)

end
