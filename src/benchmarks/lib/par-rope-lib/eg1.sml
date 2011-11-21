structure Eg1 = struct

  (* NOTE: the comments below are mirrored in rope-impl-fn.sml *)
  (* alternative splitting strategies offered by the parallel rope operations *)
    datatype splitting_strategy 
      = NO_SPLIT               (* no splitting: *)
                               (*   - sequential execution *)
      | EBS_SP                 (* eager binary splitting with simple partitioning: *)
	                       (*   - split at a cat node; stop at a leaf node *)
	                       (*   - chunk size is the same as the leaf size *)
      | EBS_AP of {K : int}    (* eager binary splitting with auto partitioning: *)
                               (*   - split the rope once into K * P chunks of roughly equal size, *)
                               (*     where P is the number of processors and K is the given constant *)
		               (*   - chunk size is determined dynamically *)
      | LBS of {ppt : int}     (* lazy binary splitting: *)
	                       (*   - split when it appears likely that other workers are hungry *)
	                       (*   - the ppt is the maximum number of elements that can be processed *)
                               (*     before checking whether a split is needed *)
      | LPS of {ppt : int}     (* lazy P-ary splitting: *)
	                       (*   - split in P chunks when it appears likely that other workers are hungry, *)
                               (*     where P is the number of currently available processors *)
	                       (*   - the ppt is the maximum number of elements that can be processed *)
                               (*     before checking whether a split is needed *)

  structure R = RopeFn (
		   structure S = VectorSeq
		   val maxLeafSize = 1
		   datatype splitting_strategy = datatype splitting_strategy
		   val splittingStrategy = NO_SPLIT
		   fun forkjoin (f, g) = (f (), g ())
		   val parMap = List.map
		   fun numAvailProcs () = 1
		   fun otherHungryProcessors () = false
		)

  fun r2s r = (R.toString Int.toString r)^"******\n"

  val _ = print (r2s (R.fromList (List.tabulate (23, fn i => i))))

  val r = R.fromList (List.tabulate (1, fn i => i))
  val k = 4
  val _ = List.app (print o r2s)  (R.dice (r, k))
  val _ = if List.length (R.dice (r, k)) <> Int.min (R.length r, k) then raise Fail "bogus k" else ()

end
