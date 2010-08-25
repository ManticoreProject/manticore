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
    
    (* create the continuation for the Vproc Queue *)
      define inline @execute (myProxy : proxy) : cont();
      
      define inline @test (fiber : PT.fiber) : cont();
    
    )

  end *) = struct

    structure PT = PrimTypes
	
    _primcode (
    
     (* linked queue elements *)
      typedef proxy = ![
	  vproc,	(* vp id *)
	  any		(*ID in Table*)
      ];
      
       (* hooks into the C runtime system *)
     extern void * createProxy (void *,void *) __attribute__((pure));
     extern void isCont (void *);
     extern void isProxy (void *, int);
     extern void M_PrintInt (int);
     extern int isFree (void *) __attribute__((pure));
     extern void * returnCont (void *,int );
     
     define inline @maxEntry () : int = 
	return (512 : int)
     ;
	
	
    (* create a Proxy *)
      define inline @createProxy (self : vproc, fiber : PT.fiber) : cont() =
	let free : int = ccall isFree(self)
	if I32Eq(free,0) then
	    do ccall M_PrintInt(5)
	    let id : proxy = ccall createProxy(self,fiber)
	    do ccall isProxy (self,#1(id))
	    cont Proxy (x : unit) = throw fiber(x)
	   (* cont Proxy (self : vproc) = throw fiber(x)
	        * let test : bool = @vprocProxy(self,id) 
		* if Equal(test,0) then 
		* @getProxyFiber(id)
	        * else throw fiber(x)
		*)
	    return (Proxy)
	else
	    do ccall M_PrintInt(10)
	    return (fiber)
      ;
      
      (* is the Fiber associated with the proxy already promoted *)
      define inline @promotedProxy (myProxy : proxy) : bool =
	let max : int = @maxEntry()
	if I32Gt(#1(myProxy),max) then return(0)
	else return(1)
      ;
      
      (* check if the vproc is the same then the creator of the proxy *)
      define inline @vprocProxy (self : vproc, myProxy : proxy) : bool =
	if Equal(#0(myProxy),self) then return(0)
	else return(1)
      ;
      
      (* create the continuation for the Vproc Queue *)
      define inline @execute (myProxy : proxy) : cont() =
          cont Proxy () = return ()
	  (* in *)
	  return (Proxy)
      ;
      
      (* create the continuation for the Vproc Queue *)
      define inline @test (fiber : PT.fiber) : cont() =
          cont Proxy (x : unit) = throw fiber(x)
	  (* in *)
	  return (Proxy)
      ;
      
       (* create the continuation for the Vproc Queue *)
      define inline @getProxyFiber (myProxy : proxy) : PT.fiber =
          let myFiber : PT.fiber = ccall returnCont(#0(myProxy),#1(myProxy))
	  return (myFiber)
      ;

    )

  end
  