(* vproc-chan.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synchronous channels for vprocs. Channels have the following properties:
 *   - one shot
 *   - short response time (guaranteed by vproc signals)
 *)

structure VProcChan =
  struct

    structure O = Option

    _primcode (

      typedef thunk = fun (unit / exh -> any);

      typedef chan = ![O.option];

      define @new (/ exh : exh) : chan =
	let ch : chan = alloc(O.NONE)
	return(ch)
      ;

    (* send the value over the channel. NOTE: this operation is local to this module. *)
      define @send (ch : chan, x : any / exh : exh) : () =
	let ch : chan = promote(ch)
	case #0(ch)
	 of O.NONE =>
	    let x : any = (any)x
	    let x : O.option = promote(O.SOME(x))
	    do UPDATE(0, ch, x)
	    return()
	  | O.SOME(y : any) =>
	    let e : exn = Fail(@"attempt to send twice over an vproc channel")
	    throw exh(e)
	end
      ;

    (* given a channel, remote vproc, and thunk, we force the thunk on the remote
     * vproc and populate the channel. the thunk evaluates with signals masked.
     *)
      define @messenger (ch : chan, remoteVP : vproc, m : thunk / exh : exh) : () =
	do assert(NotEqual(host_vproc, remoteVP))
	cont k (x : unit) =
	  do vpstore(ATOMIC, host_vproc, true)
	  let x : any = apply m(UNIT / exh)
	  do @send(ch, x / exh)
	  let _ : unit = Control.@stop(/ exh)
	  return()
	VProc.@send-messenger(remoteVP, k / exh)
      ;

    (* receive over a channel; we spin until the matching send completes. to prevent deadlock
     * we perform a yield operation at each time around the spin loop.
     *)
      define @recv-spin (ch : chan / exh : exh) : any =
	let ch : chan = promote(ch)
	let m : bool = vpload(ATOMIC, host_vproc)
	do vpstore(ATOMIC, host_vproc, true)
	fun spin () : O.option =
	      case #0(ch)
	       of O.NONE => 
		  let _ : unit = Control.@atomic-yield(/ exh)
		  apply spin()	    
		| O.SOME (x : any) =>
		  return(x)
	      end
	let x : O.option = apply spin()
	do vpstore(ATOMIC, host_vproc, m)
	return(x)
      ;

    )

  end
