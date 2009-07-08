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
      define @send-signal-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : ();
    (* place a signal on the landing pad of the remote vproc. the vproc is guaranteed
     * to handle the signal within a constant number of computational steps.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-high-priority-signal-from-atomic (self : vproc, dst : vproc, k : PT.fiber) : ();
    (* receive pending signals from the host vproc's landing pad. 
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @recv-from-atomic (self : vproc) : queue_item;
    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : ();
    (* put the vproc to sleep until either a signal arrives on its landing pad or the given amount
     * of time has elapsed
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @nanosleep-from-atomic (vp : vproc, nsec : long) : ();

    )

    val numVProcs : unit -> int
    
  end *) = struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode (

    (* hooks into the C runtime system (parallel-rt/vproc/vproc.c) *)
      extern void* GetNthVProc (int);
      extern int GetNumVProcs ();
      extern void *SleepCont (void *) __attribute__((alloc));
      extern void *ListVProcs (void *) __attribute__((alloc));
      extern void VProcWake (void *);
      extern void VProcPreempt (void *, void*);

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
		| List.CONS(vp : [vproc], vps : List.list) =>
		    if Equal(#0(vp), self)
		      then apply lp(vps, others)
		      else apply lp(vps, List.CONS(vp, others))
	      end
	  let vps : List.list = ccall ListVProcs(self)
	  apply lp(vps, nil)
	;

    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : () =
	  fun lp (vps : List.list) : () =
	      case vps
	       of nil => return()
		| List.CONS(vp : [vproc], vps : List.list) =>
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
          cont exit () = return()
	  fun lp () : () =
	      let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, dst)
	      let ldgPadNew : queue_item = alloc(fls, k, ldgPadOrig)
	      let ldgPadNew : queue_item = promote(ldgPadNew)
	      let x : queue_item = CAS((addr(queue_item))vpaddr(VP_LANDING_PAD,dst), ldgPadOrig, ldgPadNew)
	      if NotEqual(x, ldgPadOrig)
		then
		  do Pause ()
		  apply lp ()
		else
		    let sleeping : bool = vpload(VP_SLEEPING, dst)
		    do case sleeping
		       of true =>
			    do ccall VProcWake(dst)
			    return()
			| false => return()
		      end
                    throw exit()
	  do apply lp()
	  return()
      ;

    (* place a high-priority signal on the landing pad of the remote vproc. the vproc is guaranteed
     * to handle the signal within a constant number of computational steps.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-high-priority-signal-from-atomic (self : vproc, dst : vproc, k : PT.fiber) : () =
          let fls : FLS.fls = FLS.@get-in-atomic(self)
          do @send-from-atomic(self, dst, fls, k)
          do ccall VProcPreempt(self, dst)
	  return()
      ;

    (* receive pending signals from the host vproc's landing pad.
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @recv-from-atomic (self : vproc) : queue_item =
          cont exit () = return(Q_EMPTY)
          let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, self)
          do if Equal(ldgPadOrig, Q_EMPTY)
	      then throw exit()
	      else return()
          let x : queue_item = CAS((addr(queue_item))vpaddr(VP_LANDING_PAD, self), ldgPadOrig, Q_EMPTY)
          if Equal(x, ldgPadOrig)
	      then return(x)
	      else return(Q_EMPTY)
      ;

    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : () =
	  fun sleep () : () =
              cont wakeupK (x : unit) = return ()
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
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @nanosleep-from-atomic (vp : vproc, nsec : long) : () =
	  fun sleep () : () =
              cont wakeupK (x : unit) = return ()
	    (* the C runtime expects the resumption continuation to be in vp->wakeupCont *)
	      do vpstore(VP_WAKEUP_CONT, vp, wakeupK)
	      let sleepK : cont(long) = ccall SleepCont (vp)
	      throw sleepK(nsec)
	  do apply sleep()
	  return()
	;

    )

    val numVProcs : unit -> int = _prim (@num-vprocs-w)

  end
