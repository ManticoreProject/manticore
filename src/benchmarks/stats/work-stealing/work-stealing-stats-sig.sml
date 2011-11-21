structure Common = struct

(* garbage collection stats *)

  datatype gc 
    = GC of {n_collections : int,       (* number of collections *)
	     alloc_bytes   : Int64.int, (* bytes allocated *)
	     copied_bytes  : Int64.int, (* bytes copied *)
	     time_coll_sec : real}      (* elapsed time *)


  datatype gc_stats 
    = GCS of {processor    : int,
	      minor        : gc,
	      major        : gc,
	      global       : gc,
	      promotion    : {n_promotions       : int,
			      prom_bytes         : Int64.int,
			      mean_prom_time_sec : real}} (* mean time *)
    | GCST of {processor    : int,
	       minor        : gc,
	       major        : gc,
	       global       : gc,
	       time         : real,
	       promotion    : {n_promotions       : int,
			       prom_bytes         : Int64.int,
			       mean_prom_time_sec : real}} (* mean time *)

  type vproc_time_breakdown =
       {timeBusy             : real,      (* total time the vproc was busy work useful work *)
	timeIdle             : real,      (* total time the vproc was idle *) 
	timeSleeping         : real,      (* total time the vproc was sleeping (this is a subset of timeIdle) *)
        timeRebalancing      : real,      (* total time spent rebalancing ropes *)
	numEltsRebalanced    : int,       (* number of rope elements rebalanced *)
        numRebalances        : int}       (* number of times rope rebalancing occurs *)

  type work_stealing_stats =
       { numVProcs                      : int,                (* redudant w.r.t. n_procs below *)
	 clock                          : string,             (* system clock used to obtain timing results *)
	 numSteals                      : int list,           (* number of steals *)
	 numFailedStealAttempts         : int list,           (* number of failed steal attempts *)
	 vprocState                     : vproc_time_breakdown list,
	 timeStealing                   : {avg : real, max : real} list}
                                                            (* per-vproc times average and maximum time
							     * spent attempting steals (includes both
							     * successful and unsuccessful cases) *)
	      
  type run 
    = {n_procs         : int, 
       time_sec        : real,
       workStealing    : work_stealing_stats,
       gc              : gc_stats list,
       cpu_time_sec    : real option,
       max_space_bytes : int option} 

  datatype pl = Manticore | Java | SML | DPH

  fun pltos Manticore = "Manticore"
    | pltos Java = "Java"
    | pltos SML = "SML"
    | pltos DPH = "Data Parallel Haskell"

end

signature WORK_STEALING_EXPERIMENT = sig

(* metadata *)

  val problem_name : string
  val username : string
  val datetime : string

(* Manticore info *)

(* TODO maybe these should all be part of a record, so they're 
 *      all always present together
 *)

(* the following are optional since they apply only to manticore contexts, *)
(* as opposed to contexts for runs in other languages (sml, etc.) *)
  val compiler_src_url : string option (* manticore compiler, of course *)
  val compiler_svn     : int option    (* svn version no. *)
  val script_url       : string option (* the svn url of the run-benchmark script *)
  val script_svn       : int option    (* svn version no. *)
  val seq_compilation  : bool option   (* sequential compilation? yes or no *)
  val max_leaf_size    : int option
  val seq_cutoff       : int option 

(* proposed alternative:

  type manticore_data = {compiler_src_url : string,
                         compiler_svn : int,
                         script_url : string,
                         script_svn : int,
                         seq_compilation : bool,
                         max_leaf_size : int,
                         seq_cutoff : int option} (* seq_cutoff isn't always there? *)

  val manticore = manticore_data option  

 *)

(* Common info *)

(* the following are not options, since they should all be easy to provide *)
(* for all runs *)
  val language : Common.pl
  val compiler : string
  val version  : string

  val description : string

  val bench_url : string
  val bench_svn : int    (* svn version no. *)
  val input     : string
  val machine   : string (* what machine was the trial run on? *)

  val n_procs   : int list (* different values for the number of processors used in the experiment *)
  
  val runs : Common.run list

end

signature WORK_STEALING_STATS = sig

  include WORK_STEALING_EXPERIMENT

  val steals : {n_procs : int, avg : real, std : real} list
  val failedSteals : {n_procs : int, avg : real, std : real} list
  val loadBreakdowns : {n_procs : int, avgTimeBusyOverall : real, stdDevTimeOverall : real} list
  val timeSpentStealing : {n_procs : int, avgTotTime : real, stdTotTime : real, avgAvg : real, stdAvg : real, avgMax : real, stdMax : real} list
  (* avgTotTimeAllProcs = average_{over each proc p}(timeRebalancing_ / timeBusy_p)*)
  val ropeRebalancing : {n_procs : int, avgTotTimeAllProcs : real, stdTotTimeAllProcs : real, avgNumEltsRebalancedPerProc : real, stdNumEltsRebalancedPerProc : real} list

end
