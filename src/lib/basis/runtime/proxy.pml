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
    define inline @createProxy (self : vproc, k : PT.fiber) : cont();

    )

  end *) = struct

    structure PT = PrimTypes
    
    #define TABLE_ENTRY_OFFB     8 
    #define TABLE_OFFB	         16
    #define TABLE_POS(i)	I32Mul(i,TABLE_OFFB)
	
    _primcode (
    
     (* linked queue elements *)
      typedef proxy = ![
	  vproc,	(* vproc *)
	  any		(*ID in Table or pointer to promoted continuation*)
      ];
      
      
       (* hooks into the C runtime system *)
     extern void *AllocProxy (void *, int, void *,int) __attribute__((alloc,pure));
     
     
      define inline @getFiberFromTable (myProxy : proxy) : PT.fiber =
        (* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,host_vproc)
	let pos : int = AdrLoadI32((addr(int))&1(myProxy))
	let myFiber : PT.fiber = AdrLoad(AdrAddI32(myAddr,I32Add(TABLE_POS(pos),TABLE_ENTRY_OFFB)))
	return(myFiber) 
     ;
     
     define inline @deleteProxy (myProxy : proxy) : () =
	let beginning : int = vpload (PROXYTABLEENTRIES,#0(myProxy))
	(* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,#0(myProxy))
	let pos : int = AdrLoadI32((addr(int))&1(myProxy))
	do AdrStoreI32((addr(int))AdrAddI32(myAddr,TABLE_POS(pos)),beginning)
        do vpstore (PROXYTABLEENTRIES,#0(myProxy),pos)
	return() 
     ;
     
     define inline @addTableEntry (myFiber : PT.fiber) : proxy =
	(* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,host_vproc)
	(* get next free entry in the proxy table *)
	let pos : int = vpload (PROXYTABLEENTRIES,host_vproc)
	(* move free pointer to next object *)
	let nextid : int = AdrLoadI32((addr(int))AdrAddI32(myAddr,TABLE_POS(pos)))
	do vpstore (PROXYTABLEENTRIES,host_vproc,nextid)
	let myProxy : proxy = ccall AllocProxy (host_vproc, 2, host_vproc, pos)
	(* store the proxy and continuation at the offside position *)
	do AdrStore(AdrAddI32(myAddr,TABLE_POS(pos)),myProxy)
	do AdrStore(AdrAddI32(myAddr,I32Add(TABLE_POS(pos),TABLE_ENTRY_OFFB)),myFiber)
	return(myProxy) 
     ;
     
          
     define inline @maxEntry () : int = 
	 let maxProxy : int = vpload (MAXPROXY,host_vproc)
	 return(maxProxy)
     ;
     
     define inline @isFree () : bool = 
	 let id : int = vpload (PROXYTABLEENTRIES,host_vproc)
	 if I32Eq(id,1000) then return(false)
	 else return(true)
     ;
	
     (* Get the fiber out of the proxy table *)
     define inline @getProxyFiber (myProxy : proxy) : PT.fiber =
	  let myFiber : PT.fiber = @getFiberFromTable(myProxy)
	  do @deleteProxy(myProxy)
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
	let free : bool = @isFree()
	(* are there free entries in the Proxy Table *)
	if Equal(free,true) then
	    let myProxy : proxy = @addTableEntry(fiber)
	    (* create the continuation that runs the code *)
	    cont Proxy (x : unit) =
	    (* if promoted then we can just execute the fiber stored in the proxy *)
	    let prom : bool = @promotedProxy(myProxy)
	    if Equal(prom,true) then
		let myFiber : PT.fiber = #1(myProxy)
		throw myFiber(x)
	    else
		(* check if the vproc is the same than the creator *)
	        let sameVproc : bool = @vprocProxy(host_vproc,myProxy) 
	        if Equal(sameVproc,true) then 
		    (* if yes then get the fiber out of the local proxy table *)
		    let myFiber : PT.fiber = @getProxyFiber(myProxy)
		    throw myFiber(x)
	        else
		    (* if not we have to send a thieve *)
		    let myFiber : PT.fiber = @thief-from-atomic-proxy (host_vproc,myProxy)
		    throw myFiber(x)
	    return(Proxy)	
	else
	    (*if no free entries return the original fiber *)
	    return (fiber)
      ;
      

    )

  end
  