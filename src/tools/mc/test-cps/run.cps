(* run.cps *)

module Run
  fun init (wi : [int], kRet : cont([int]), exh : cont(any)) =

    let stop : enum(1) = 1
    let limit : long = 100000000000000

    let retVal : [int] = wrap(1:int)

    fun loop (x : long, k : cont(any), exh : cont(any)) =
	  if I64Eq(x, limit)
	    then let vp : vproc = host_vproc
	      forward vp stop
	    else apply loop(I64Sub(x, 1:long), k, exh)

    (* Tie the recursive knot with mkAct, since kAct is not allowed
     * to contain recursive instances. *)
    fun mkAct (i : int, k : cont(any), exh : cont(any)) =
	(* kAct is a scheduler action that terminates upon a stop
	 * signal, and resumes upon a preempt signal. *)
	cont kAct (fiber : cont(enum(0))) =
             if I64Eq (fiber, stop) then throw kRet(retVal)
	     else 
		 cont k' (kAct : cont(cont(enum(0)))) =
		      let vp : vproc = host_vproc
	              run vp kAct fiber
        	 apply mkAct (i, k', exh)
	throw k(kAct)  (* the voodoo is strong here *)

    cont fiber (x : any) = apply loop(0:long, kRet, exh)

    cont mkActRet (act : cont(cont(enum(1)))) =
	let vp : vproc = host_vproc
	run vp act fiber

    apply mkAct (0:int, mkActRet, exh)

