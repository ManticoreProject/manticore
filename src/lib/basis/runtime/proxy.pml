(* proxy.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module contains functions for VProc thread queues. Each VProc owns a single 
 * queue, and access to the queue is restricted to certain patterns. A VProc can
 * only dequeue from its own queue, but can enqueue on either its own queue or a
 * remote queue.
 *
 * VProc queues consist of three linked lists: the primary and secondary lists and the
 * landing pad. We can dequeue locally on the primary list, enqueue locally on
 * the secondary list, and enqueue remotely on the landing pad.
 *)

structure Proxy (* :
  sig

    _prim(

    typedef proxy;
    
    (* create a Proxy *)
    define inline @createProxy (self : vproc, k : PT.fiber) : proxy;
    
    (* is the Fiber associated with the proxy already promoted *)
      define inline @promotedProxy (myProxy : proxy) : bool;
    
    (* check if the vproc is the same then the creator of the proxy*)
      define inline @vprocProxy (self : vproc, myProxy : proxy) : bool;
      
    (* Get the fiber out of the proxy table *)
     define inline @getProxyFiber (myProxy : proxy) : PT.fiber; 
     
     define @thief-from-atomic-proxy (thiefVP : vproc, victimVP : vproc, workGroupId : UID.uid, wid : long, policy : int, fiber : PT.fiber)
								      : PT.fiber;
    
    )

  end *) = struct

    structure PT = PrimTypes
	
    _primcode (
    
     (* linked queue elements *)
      typedef proxy = ![
	  vproc,	(* vproc *)
	  any		(*ID in Table or pointer to promoted continuation*)
      ];
      
       (* hooks into the C runtime system *)
     extern void * createProxy (void *,void *) __attribute__((pure));
     extern void isCont (void *);
     extern void isProxy (void *, int);
     extern void deleteProxy (void *, int);
     extern void M_PrintInt (int);
     extern int isFree (void *) __attribute__((pure));
     extern void * returnCont (void *,int ) __attribute__((pure));
     
     define inline @maxEntry () : int = 
	return (512 : int)
     ;
	
     (* Get the fiber out of the proxy table *)
     define inline @getProxyFiber (myProxy : proxy) : PT.fiber =
          let myFiber : PT.fiber = ccall returnCont(#0(myProxy),#1(myProxy))
	  do ccall deleteProxy (#0(myProxy),#1(myProxy))
	  return (myFiber)
     ;	
     
      (* is the Fiber associated with the proxy already promoted *)
      define inline @promotedProxy (myProxy : proxy) : bool =
	let max : int = @maxEntry()
	if I64Gt(#1(myProxy),max) then return(true)
	else return(false)
      ;
      
      (* check if the vproc is the same then the creator of the proxy *)
      define inline @vprocProxy (self : vproc, myProxy : proxy) : bool =
	if Equal(#0(myProxy),self) then
	return(true)
	else 
	return(false)
      ;
      
       (* send a thief from thiefVP to steal from victimVP. the result is a continuation stored in the proxy table *)
	define @thief-from-atomic-proxy (thiefVP : vproc, myProxy : proxy) : PT.fiber =
	    let victimVP : vproc = #0(myProxy)
	    do assert (NotEqual (thiefVP, victimVP))
	    
	  (* communication channel used to pass the result of the steal from the victim vproc back to the
	   * thief. the channel is initially nil, but once the steal attempt completes the channel is
	   * seeded with the continuation.
	   *)
	    let ch : ![Option.option] = alloc (Option.NONE)
	    let ch : ![Option.option] = promote (ch)
	  (* the thief fiber executes on the victim vproc *)
	    cont thief (_ : unit) =
	      do ccall M_PrintInt(15)
	      let myFiber : PT.fiber = @getProxyFiber(myProxy)		
	      (* successfully stole multiple threads *)
	      let x : Option.option = promote (Option.SOME(myFiber))
	      do #0(ch) := x
	      SchedulerAction.@stop ()
	      
	  (* send the thief fiber to the victim vproc *)
	    let fls : FLS.fls = FLS.@get()
	    do VProc.@send-from-atomic (thiefVP, victimVP, fls, thief)
	    
	    fun wait () : PT.fiber =
		case #0(ch)
		 of Option.NONE =>
		    do Pause()
		    let _ : vproc = SchedulerAction.@yield-in-atomic (thiefVP)
		    apply wait ()
		 | Option.SOME (myFiber : PT.fiber) =>
		    return (myFiber)
		end
	  (* wait for the thief to report its findings *)
	    apply wait ()
	  ;
      
	
    (* create a Proxy *)
      define inline @createProxy (self : vproc, fiber : PT.fiber) : cont() =
	let free : int = ccall isFree(self)
	(* are there free entries in the Proxy Table *)
	if I32Eq(free,0) then
	    (* do ccall M_PrintInt(5) *)
	    (* create the proxy *)
	    let myProxy : proxy = ccall createProxy(self,fiber)
	    (* do ccall isProxy (self,#1(myProxy)) *)
	    (* create the continuation that runs the code *)
	    cont Proxy (x : unit) =
	    (* if promoted then we can just run the fiber stored in the proxy *)
	    let prom : bool = @promotedProxy(myProxy)
	    if Equal(prom,true) then
		(* do ccall M_PrintInt(6) *)
		let myFiber : PT.fiber = #1(myProxy)
		throw myFiber(x)
	    else
		(* check if the vproc is the same than the creator *)
	        let sameVproc : bool = @vprocProxy(host_vproc,myProxy) 
	        if Equal(sameVproc,true) then 
		    (* if yes then get the fiber out of the local proxy table *)
		    (* do ccall M_PrintInt(7) *)
		    let myFiber : PT.fiber = @getProxyFiber(myProxy)
		    throw myFiber(x)
	        else
		    (* if not we have to send a thieve *)
	            (* do ccall M_PrintInt(8) *)
		    (* let myFiber : PT.fiber = @getProxyFiber(myProxy) *)
		    let myFiber : PT.fiber = @thief-from-atomic-proxy (host_vproc,myProxy)
		    throw myFiber(x)
	    return(Proxy)	
	else
	    (*if no free entries return the original fiber *)
	    (* do ccall M_PrintInt(10) *)
	    return (fiber)
      ;
      

    )

  end
  