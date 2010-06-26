(* vproc.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Virtual processors.
 *)

structure VProc (* :
  sig

    _prim(

    (** Unique ids **)

    (* unique id of a vproc *)
      define @vproc-id (vp : vproc) : int;
    (* find the vproc with a given unique id *)
      define @vproc-by-id (id : int) : vproc =
    (* total number of vprocs *)
      define @num-vprocs () : int;

    (** Vproc lists and iterators **)

    (* returns the list of all vprocs *)
      define @all-vprocs () : List.list;
    (* returns the list of all vprocs, excluding host vproc. 
     * NOTE: signals must be masked before the call.
     *)
      define @other-vprocs-from-atomic (self : vproc / exh : exh) : List.list;
    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : ();
    (* apply f to each vproc except the host vproc. *)
      define @for-other-vprocs-from-atomic (self : vproc, f : fun(vproc / exh ->) / exh : exh) : ();

    (** Signaling and sleeping **)

    (* place a signal on the landing pad of the remote vproc. 
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : ();
    (* returns threads that have been placed on the given vproc's landing pad *)
      define @recv-from-atomic (self : vproc) : queue_item;
    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : ();
    (* put the vproc to sleep until either a signal arrives on its landing pad or the given amount
     * of time has elapsed
     * the return value depends on how the vproc was brought out of sleeping: true if by
     * a remote vproc and false if by a POSIX signal or timeout
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @nanosleep-from-atomic (vp : vproc, nsec : long) : bool;

    )

    val numVProcs : unit -> int
    
  end *) = struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode (

    (* hooks into the C runtime system (parallel-rt/vproc/vproc.c) *)
      extern void* GetNthVProc (int) __attribute__((pure));
      extern int GetNumVProcs () __attribute__((pure));
      extern void *SleepCont (void *) __attribute__((alloc));
      extern void *ListVProcs (void *) __attribute__((pure,alloc));
      extern void VProcWake (void *);
      extern void VProcExit (void *);

    (* returns the total number of vprocs *)
      define inline @num-vprocs () : int =
	  let n : int = ccall GetNumVProcs()
	  return (n)
	;

      define inline @num-vprocs-w (_ : unit / exh : exh) : ml_int =
	  let n : int = @num-vprocs ()
          return (alloc (n))
	;

    (* returns the unique id of the given vproc *)
      define inline @vproc-id (vp : vproc) : int =
	  let id : int = vpload(VPROC_ID, vp)
	  return (id)
	;

    (* find the vproc with a given unique id *)
      define inline @vproc-by-id (id : int) : vproc =
#ifndef NDEBUG
	  let max : int = @num-vprocs()
	  do assert(I32Lt(id, max))
	  do assert(I32Gte(id, 0:int))
#endif
	  let vp : vproc = ccall GetNthVProc(id)
	  return (vp)
	;

    (** vproc allocation and iterators  **)

    (* returns the list of all vprocs *)
      define @all-vprocs () : List.list =
	  let vps : List.list = ccall ListVProcs(host_vproc)
	  return(vps)
	;

    (* returns the list of all vprocs, excluding the host vproc. 
     * NOTE: signals must be masked before the call.
     *)
      define @other-vprocs-from-atomic (self : vproc / exh : exh) : List.list =
	  fun lp (vps : List.list, others : List.list) : List.list =
	      case vps
	       of nil => return(others)
		| CONS(vp : [vproc], vps : List.list) =>
		    if Equal(#0(vp), self)
		      then apply lp(vps, others)
		      else apply lp(vps, CONS(vp, others))
	      end
	  let vps : List.list = ccall ListVProcs(self)
	  apply lp(vps, nil)
	;

    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : () =
	  fun lp (vps : List.list) : () =
	      case vps
	       of nil => return()
		| CONS(vp : [vproc], vps : List.list) =>
		  do apply f(#0(vp) / exh)
		  apply lp(vps)
	      end
	  let vps : List.list = ccall ListVProcs(host_vproc)
	  apply lp(vps)
	;

    (* apply f to each vproc except the host vproc. 
     * NOTE: the functions are applied in
     *)
      define @for-other-vprocs-from-atomic (self : vproc, f : fun(vproc / exh ->) / exh : exh) : () =
	  fun g (vp : vproc / exh : exh) : () =
		if NotEqual(vp, self) then apply f(vp / exh)
		else return()
	  @for-each-vproc(g / exh)
	;

    (** Signaling and sleeping **)

    (* place a signal on the landing pad of the remote vproc.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
          do assert(NotEqual(self, dst))
	  fun lp () : () =
	      let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, dst)
	      let ldgPadNew : queue_item = alloc(fls, k, ldgPadOrig)
	      let ldgPadNew : queue_item = promote(ldgPadNew)
	      let x : queue_item = CAS((addr(queue_item))vpaddr(VP_LANDING_PAD,dst), ldgPadOrig, ldgPadNew)
	      if NotEqual(x, ldgPadOrig) then
		  do Pause ()
		  apply lp ()
	      else
		  let sleeping : bool = vpload(VP_SLEEPING, dst)
		  do case sleeping
		      of true =>
			 do ccall VProcWake(dst)
		         return()
		       | false => 
			 return()
		     end
                  return()
	  do apply lp()
        (* trigger a preemption on the destination vproc by zeroing out the vproc's limit pointer *)
          fun preempt () : () =
	      let limitPtrOrig : any = vpload(LIMIT_PTR, dst)
              let x : any = CAS((addr(any))vpaddr(LIMIT_PTR, dst), limitPtrOrig, $0)
              if NotEqual (x, limitPtrOrig) then
		  do Pause()
		  apply preempt()
	      else
		  return ()
          do apply preempt()
          let dstId : int = @vproc-id (dst)
          do Logging.@log-PreemptVProc (self, dstId)
	  return()
      ;

    (* returns threads that have been placed on the given vproc's landing pad *)
      define @recv-from-atomic (self : vproc) : queue_item =
          let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, self)
          if Equal (ldgPadOrig, Q_EMPTY) then
	      return (Q_EMPTY)
	  else
	    (* the landing pad will remain empty until the function below succeeds in removing all the
	     * existing threads. this property holds because other vprocs may only add threads to the landing
	     * pad but cannot remove any threads.
	     *) 
	      fun lp () : queue_item =
		  let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, self)
                  let x : queue_item = CAS((addr(queue_item))vpaddr(VP_LANDING_PAD, self), ldgPadOrig, Q_EMPTY)
                  if Equal(ldgPadOrig, x) then
		      return(x)
		  else
		      do Pause()
                      apply lp()
               apply lp()
      ;

    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : () =
	  fun sleep () : () =
              cont wakeupK (b : bool) = return ()
	    (* the C runtime expects the resumption continuation to be in vp->wakeupCont *)
	      do vpstore(VP_WAKEUP_CONT, vp, wakeupK)
	      let sleepK : cont(long) = ccall SleepCont (vp)
	    (* the zero indicates that there is no limit on the duration *)
	      throw sleepK(0:long)
	  do apply sleep()
	  return()
	;

    (* put the vproc to sleep until either a signal arrives on its landing pad or the given time
     * has elapsed. the time is in nanoseconds.
     * the return value depends on how the vproc was brought out of sleeping: true if by
     * a remote vproc and false if by a POSIX signal or timeout
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @nanosleep-from-atomic (vp : vproc, nsec : long) : bool =
          cont wakeupK (b : bool) = return (b)
	  fun sleep () : () =
	    (* the C runtime expects the resumption continuation to be in vp->wakeupCont *)
	      do vpstore(VP_WAKEUP_CONT, vp, wakeupK)
	      let sleepK : cont(long) = ccall SleepCont (vp)
	      throw sleepK(nsec)
	  do apply sleep()
	  return(true)
	;

    )

    val numVProcs : unit -> int = _prim (@num-vprocs-w)

  end
