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
	  any		(* ID in Table or pointer to promoted continuation *)
      ];
      
      
       (* hooks into the C runtime system *)
     extern void *AllocProxy (void *, int, void *,int) __attribute__((alloc,pure));
     extern void M_PrintInt (int);
     extern void M_PrintLong (long);
     extern void isProxy (void *, int);
     extern void isPrintProxy (int);
     extern void globalCheck(void *);
     
      define inline @getFiberFromTable (myProxy : proxy) : PT.fiber =
        (* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,host_vproc)
(* this does not typecheck!
	let pos : int = (int)#1(myProxy)
*)
	let pos : int = I64ToI32(AdrLoadI64((addr(long))&1(myProxy)))
	let myFiber : PT.fiber = AdrLoad(AdrAddI32(myAddr,I32Add(TABLE_POS(pos),TABLE_ENTRY_OFFB)))
	return(myFiber) 
     ;
     
     define inline @deleteProxy (myProxy : proxy) : () =
	let nextfree : int = vpload (PROXYTABLEENTRIES,#0(myProxy))
	let last : int = I32Sub(nextfree,1)
	(* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,#0(myProxy))
	(* position of the proxy in the table *)
	let pos : int = AdrLoadI32((addr(int))&1(myProxy))
	(* if the proxy isn't the last element *)
	if I32NEq(last,pos) then
	(* move last entry to the empty entry and free it *)
	let lastProxy : proxy = AdrLoad(AdrAddI32(myAddr,TABLE_POS(last)))
	let lastFiber : PT.fiber = AdrLoad(AdrAddI32(myAddr,I32Add(TABLE_POS(last),TABLE_ENTRY_OFFB)))
	(* change position in last proxy *)
	do AdrStoreI64((addr(long))&1(lastProxy),I32ToI64(pos))
	(* save in the new table position *)
	do AdrStore(AdrAddI32(myAddr,TABLE_POS(pos)),lastProxy)
	do AdrStore(AdrAddI32(myAddr,I32Add(TABLE_POS(pos),TABLE_ENTRY_OFFB)),lastFiber)
        do vpstore (PROXYTABLEENTRIES,#0(myProxy),last)
	return() 
	else
	do vpstore (PROXYTABLEENTRIES,#0(myProxy),last)
	return() 
     ;
     
     define inline @addTableEntry (myFiber : PT.fiber) : proxy =
	(* get address of the proxy table *)
	let myAddr : addr(any) = vpload(PROXYTABLE,host_vproc)
	(* get next free entry in the proxy table *)
	let pos : int = vpload (PROXYTABLEENTRIES,host_vproc)

(* do ccall M_PrintInt(1) 
do ccall globalCheck(host_vproc) *)
	
	let myProxy : proxy = ccall AllocProxy (host_vproc,2,host_vproc, pos)
	(* let myProxy : proxy = alloc_special (host_vproc, pos) *)
	
	(* store the proxy and continuation at the offside position *)
	do AdrStore(AdrAddI32(myAddr,TABLE_POS(pos)),myProxy)
	do AdrStore(AdrAddI32(myAddr,I32Add(TABLE_POS(pos),TABLE_ENTRY_OFFB)),myFiber)
	
	(*do ccall isProxy (#0(myProxy),#1(myProxy))*)
	
	(* move free pointer to next object *)
	let nextid : int = I32Add(pos,1)
	do vpstore (PROXYTABLEENTRIES,host_vproc,nextid)

do ccall isProxy(host_vproc, pos)

	return(myProxy) 
     ;
     
     define inline @isFree () : bool = 
	 let id : int = vpload (PROXYTABLEENTRIES,host_vproc)
	 let max : int = vpload (MAXPROXY,host_vproc)
	 if I32Eq(id,max) then return(false)
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
	let max : int = vpload (MAXPROXY,host_vproc)
	let pos : long = AdrLoadI64((addr(long))&1(myProxy))
	if I64Gte(pos,I32ToI64(max)) then return(true)
	else return(false)
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
		(* check if the vproc is the same as the creator *)
		let proxyVProc : vproc = #0(myProxy)
	        if Equal(#0(myProxy),host_vproc) then 
		    (* if yes then get the fiber out of the local proxy table *)
		    let myFiber : PT.fiber = @getProxyFiber(myProxy)
		    throw myFiber(x)
	        else
		    (* if not we have to send a thief *)
		    let myFiber : PT.fiber = @thief-from-atomic-proxy (host_vproc,myProxy)
		    throw myFiber(x)
	    return(Proxy)	
	else
	    (*if no free entries return the original fiber *)
	    return (fiber)
      ;
      

    )

  end
  
