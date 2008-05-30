structure VarargCCall =
  struct

    structure SA = StagedAllocation

    fun encodeLoc (_, SA.REG (_, r), CCall.K_GPR) = (VarargCCall.GPR, regToInt r)
      | encodeLoc (_, SA.REG (_, r), CCall.K_FPR) = (VarargCCall.FPR, regToInt r)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, CCall.K_GPR) = (VarargCCall.STK, offB)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, CCall.K_FPR) = (VarargCCall.STK, offB)
      | encodeLoc (_, SA.NARROW (loc, w', k), _) = encodeLoc (w', loc, k)

  (* takes a vararg and a location and returns the vararg triplet *)
    fun varArgTriplet (arg, loc) = let
	   val (k, l) = encodeLoc loc
           in
	     (arg, k, l)
	   end

  (* takes a list of varargs and returns vararg triplets *)
    fun encodeArgs args = let
  	   val step = SA.mkStep CCall.CCs.callStages
	   val slots = List.map (CCall.slotOfCTy o VarargCCall.argToCTy) args
	   val (str', locs) = SA.doStagedAllocation(CCall.CCs.str0, step, slots)
           in
	      ListPair.mapEq varArgTriplet (args, List.rev locs)
	   end

end
