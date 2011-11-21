(* splitting-strategy.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Different strategies a scheduler can use to control the splitting of ropes into "chunks". A chunk
 * is a unit of parallel work. Computations on two different chunks can be executed in parallel, but each
 * element inside the same chunk must be processed serially.
 *
 * This module processes the run-time command-line arguments to determine a default splitting strategy. Use
 * the argument
 *   -splitting-strategy <strategy>
 * where <strategy> is one of
 *   ns                            "no splitting"
 *   ebs-sp <int>                  "eager binary splitting with simple partitioning"
 *   ebs-ap <int>                  "eager binary splitting with auto partitioning"
 *   lbs <int>                     "lazy binary splitting"
 *   lps <int>                     "lazy P-ary splitting where P is the number of available processors"
 * If no command-line argument is supplied, default is lbs 2.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure SplittingStrategy =
  struct

  (* alternative splitting strategies offered by the parallel rope operations *)
    datatype splitting_strategy 
      = NO_SPLIT               (* no splitting: *)
                               (*   - sequential execution *)
      | EBS_SP of int          (* eager binary splitting with simple partitioning (argument called SST): *)
	                       (*   - split at a cat node; stop at a leaf node *)
	                       (*   - chunk size is the same as the leaf size *)
                               (*   - the SST is the "stop-splitting threshold" *)
      | EBS_AP of (int * int)
                               (* eager binary splitting with auto partitioning: *)
                               (*   - split the rope top down *)
		               (* From the paper Optimization via Reflection on Work Stealing in TBB:
				   Automatic grain size selection is specified by using
				   an auto_partitioner, which causes a range that is
				   divisible to not necessarily be divided. It makes the
				   decision as follows. There are two fixed parameters: K
				   and V. Both are four in the current implementation. A
				   variable n is associated with each range or subrange. It
				   is initialized to PxK for the top level range. P is the
				   number of threads controlled by the scheduler. Each
				   time a range is split, each resulting subrange gets an n
				   that is half of the original n. If a range is stolen, its n is
				   forced to be at least V. When n reaches 1, the
				   corresponding range is not split, even if is_divisible
				   says it can be.
				*)
      | LBS of int             (* lazy binary splitting (argument called PPT): *)
	                       (*   - split when it appears likely that other workers are hungry *)
	                       (*   - the PPT is the maximum number of elements that can be processed *)
                               (*     before checking whether a split is needed *)
      | LPS of int             (* lazy P-ary splitting (argument called PPT): *)
	                       (*   - split in P chunks when it appears likely that other workers are hungry, *)
                               (*     where P is the number of currently available processors *)
	                       (*   - the PPT is the maximum number of elements that can be processed *)
                               (*     before checking whether a split is needed *)

    local
	val dflt = LBS 2
	fun stringSame (s1, s2) = String.same (s1, s2)
	fun fromString ss =
	    (case ss
	      of s1 :: s2 :: s3 :: _ =>
		 if stringSame (s1, "ns")          then SOME NO_SPLIT
		 else if stringSame (s1, "ebs-ap") then SOME (EBS_AP (Option.getOpt (Int.fromString s2, 4), 
								      Option.getOpt (Int.fromString s3, 4)))
		 else NONE
	       | s1 :: s2 :: _ =>
		 if stringSame (s1, "ns")          then SOME NO_SPLIT
		 else if stringSame (s1, "ebs-sp") then SOME (EBS_SP (Option.getOpt (Int.fromString s2, 1)))
		 else if stringSame (s1, "lbs")    then SOME (LBS    (Option.getOpt (Int.fromString s2, 1)))
		 else if stringSame (s1, "lps")    then SOME (LPS    (Option.getOpt (Int.fromString s2, 1)))
		 else NONE
	       | s1 :: nil =>
		 if stringSame (s1, "ns") then SOME NO_SPLIT
		 else NONE
	       | nil => NONE
	    (* end case *))
	fun findParam ss =
	    (case ss
	      of nil => dflt
	       | s1 :: ss => 
		 if stringSame (s1, "-splitting-strategy") then
		     (case fromString ss
		       of SOME ss => ss
			| NONE => dflt
		     (* end case *))
		 else findParam ss
	    (* end case *))
    in
    val default = findParam (CommandLine.arguments ())
    end
	

  end
