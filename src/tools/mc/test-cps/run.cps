(* run.cps *)

module Run (wi : [int], kRet : cont([int]), exh : cont(any)) =

  let stop : enum(1) = 1

  let retVal : [int] = wrap(1:int)

  (* Tie the recursive knot with mkAct, since kAct is not allowed
   * to contain recursive instances. *)
  fun mkAct (i : int, k : cont(any), exh : cont(any)) =
      (* kAct is a scheduler action that terminates upon a stop
       * signal, and resumes upon a preempt signal. *)
      cont kAct (fiber : cont(enum(1))) =
           if I64Eq (fiber, stop) then throw kRet(retVal)
	   else 
	       cont k' (kAct : cont(cont(enum(1)))) =
	            run kAct fiber
               apply mkAct (i, k', exh)
      throw k(kAct)  (* the voodoo is strong here *)

  apply mkAct (0:int, kRet, exh)