(* run.cps *)

module Run (wi : [int], kRet : cont([int]), exh : cont(any)) =

  let stop : any = 1

  (* Tie the recursive knot with mkAct, since kAct is not allowed
   * to contain recursive instances. *)
  fun mkAct (i : int, k : cont(any), exh : cont(any)) =
      (* kAct is a scheduler action that terminates upon a stop
       * signal, and resumes upon a preempt signal. *)
      cont kAct (fiber : cont(any)) =
           if I64Eq (fiber, stop) then throw kRet(1)
	   else 
	       cont k' (kAct : cont(any)) =
	            run kAct fiber
               apply mkAct (i, k', exh)
      throw k(kAct)  (* the voodoo is strong here *)

  apply mkAct (0:int, kRet, exh)