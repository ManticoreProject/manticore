(* work-stealing-deque.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler. 
 *
 * Memory management:
 * Since we allocate deques in the C heap, we rely on a reference counting scheme to manage memory
 * associated with deques. We maintain reference counts as follows. The reference count of
 * a newly-created deque is set to 1 (by @new-from-atomic). Reference counts are incremented
 * for each deque returned by the call to @local-deques-from-atomic. Reference counts are decremented
 * by @release-from-atomic and @release-deques-from-atomic. The garbage collector frees deques that
 * are both empty and have a reference count of zero.
 *
 *)

structure WorkStealingDeque (* :
  sig

    _prim (

      typedef deque;

    (* the second argument is the number of elements. the new deque is automatically claimed for the
     * calling process.
     *)
      define inline @new-from-atomic (self : vproc, workGroupId : ImplicitThread.work_group_id, size : int) : deque;

      define inline @is-full-from-atomic (self : vproc, deque : deque) : bool;
      define inline @is-empty-from-atomic (self : vproc, deque : deque) : bool;
      define inline @is-claimed-from-atomic (self : vproc, deque : deque) : bool;
    (* returns the number of elements contained in the deque *)
      define inline @size (deque : deque) : int;

      define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : ();
      define inline @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option;
      define inline @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option;

    (* returns the deques associated with the given vproc and the work group id. all the deques in this list are 
     * automatically claimed for the caller. *)
      define @local-deques-from-atomic (self : vproc, workGroupId : ImplicitThread.work_group_id) : (* [deque] *) List.list;

      define inline @release-from-atomic (self : vproc, deque : deque) : ();
      define @release-deques-from-atomic (self : vproc, deques : List.list) : ();

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : ImplicitThread.work_group_id, deque : deque) : deque;

    (* the list of returned threads is ordered from oldest to youngest *)
      define @to-list-from-atomic (self : vproc, deque : D.deque) : (* ImplicitThread.thread *) List.list;
    (* the list of threads is inserting in order from oldest to youngest *)
      define @add-list-from-atomic (self : vproc, deque : D.deque, thds : List.list) : ();

    )

  end *) = struct

#define DEQUE_NIL_ELT        enum(0):any

    _primcode (

      extern void* GetNthVProc (int);
      extern void* M_DequeAlloc (void*, long, int);
      extern void* M_LocalDeques (void*, long) __attribute__((alloc));
      extern void M_AssertDequeAddr (void*, int, void*);

    (* Deque representation:
     *
     * For compactness, we represent deques as circular buffers. There are two pointers into this buffer:
     * "old" and "new". The old pointer points to the oldest element on the deque and new points to the 
     * newest. Our convention is that old points to the leftmost element and new points to the rightmost
     * element. In other words, elements increase in age going from right to left.
     *
     * To distinguish whether the deque is empty or full, we always keep one deque entry open, which means
     * that we waste a word of memory for each deque. 
     *
     *)

    (* the type deque has the byte layout corresponding to the C struct below *)
      typedef deque = ![any];
    (*

	struct Deque_s {
	    int32_t       old;           // pointer to the oldest element in the deque
	    int32_t       new;           // pointer to the address immediately to the right of the newest element
	    int32_t       maxSz;         // max number of elements
            int32_t       nClaimed;      // the number of processes that hold a reference to the deque
	    Value_t       elts[];        // elements of the deque
	};

    *)

    )

#define DEQUE_OLD_OFFB        0
#define DEQUE_NEW_OFFB        4
#define DEQUE_MAXSZ_OFFB      8
#define DEQUE_NCLAIMED_OFFB   12
#define DEQUE_ELTS_OFFB       16

#define LOAD_DEQUE_OLD(deque)        AdrLoadI32 ((addr(int))&0(deque))
#define LOAD_DEQUE_NEW(deque)        AdrLoadI32 ((addr(int))AdrAddI64 (&0(deque), DEQUE_NEW_OFFB:long))
#define STORE_DEQUE_OLD(deque, i)    AdrStoreI32 ((addr(int))&0(deque), i)
#define STORE_DEQUE_NEW(deque, i)    AdrStoreI32 ((addr(int))AdrAddI64 (&0(deque), DEQUE_NEW_OFFB:long), i)

#define LOAD_DEQUE_MAX_SIZE(deque)   AdrLoadI32 ((addr(int))AdrAddI64 (&0(deque), DEQUE_MAXSZ_OFFB:long))

#define LOAD_DEQUE_NCLAIMED(deque)        AdrLoadI32 ((addr(int))AdrAddI64 (&0(deque), DEQUE_NCLAIMED_OFFB:long))
#define STORE_DEQUE_NCLAIMED(deque, c)    AdrStoreI32 ((addr(int))AdrAddI64 (&0(deque), DEQUE_NCLAIMED_OFFB:long), c)

      _primcode (

	define inline @size (deque : deque) : int =
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		return (I32Sub (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	    else (* wrapped around *)
		return (I32Add (I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 
					LOAD_DEQUE_OLD(deque)), 
				LOAD_DEQUE_NEW(deque)))
	  ;

	define @assert-in-bounds (deque : deque, i : int) : () =
	    do assert (I32Gte (i, 0))
	    do assert (I32Lt (i, LOAD_DEQUE_MAX_SIZE(deque)))
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		do assert (I32Gte (i, LOAD_DEQUE_OLD(deque)))
		do assert (I32Lt (i, LOAD_DEQUE_NEW(deque)))
  	        return ()
	    else
		if I32Gt (i, LOAD_DEQUE_NEW(deque)) then
		    do assert (I32Gte (i, LOAD_DEQUE_OLD(deque)))
		    return ()
		else if I32Eq (i, LOAD_DEQUE_NEW(deque)) then
		    do assert_fail()
		    return ()
		else
		    do assert (I32Lt (i, LOAD_DEQUE_NEW(deque)))
  	            return ()
	  ;

	define inline @assert-ptr (deque : deque, i : int) : () =
#ifndef NDEBUG
	    do ccall M_AssertDequeAddr (deque, i, AdrAddI64 (&0(deque), 
				   I64Add (DEQUE_ELTS_OFFB:long,         (* the byte offset of elts *)
					 I32ToI64X (I32LSh (i, 3)))))
#endif
	    return ()
	  ;

	define inline @update (deque : deque, i : int, elt : any) : () =
	    do @assert-in-bounds (deque, i)
            do @assert-ptr (deque, i)
	    do AdrStore (AdrAddI64 (&0(deque), 
				   I64Add (DEQUE_ELTS_OFFB:long,         (* the byte offset of elts *)
				   I32ToI64X (I32LSh (i, 3)))), 
			  elt)
	    return ()
	  ;

	define inline @sub (deque : deque, i : int) : any =
	    do @assert-in-bounds (deque, i)
            do @assert-ptr (deque, i)
	    let elt : any = AdrLoad (AdrAddI64 (&0(deque),         (* the byte offset of elts *)
					       I64Add (DEQUE_ELTS_OFFB:long,
					       I32ToI64X (I32LSh (i, 3)))))
	    return (elt)
	  ;

	(* check the deque for consistency *)
	define @check-deque (deque : deque) : () =
	    do assert(NotEqual(deque, DEQUE_NIL_ELT))
	    do assert (I32Gte (LOAD_DEQUE_NEW(deque), 0))
	    do assert (I32Gte (LOAD_DEQUE_OLD(deque), 0))
	    do assert (I32Lt (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque)))
	    do assert (I32Lt (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_MAX_SIZE(deque)))
	    return ()
	  ;

      (* move the index i one position left w.r.t. the deque size sz *)
	define inline @move-left (i : int, sz : int) : int =
	    if I32Lte (i, 0) then
		return (I32Sub (sz, 1))
	    else
		return (I32Sub (i, 1))
	  ;

      (* move the index i one position right w.r.t. the deque size sz *)
	define inline @move-right (i : int, sz : int) : int =
	    if I32Gte (i, I32Sub (sz, 1)) then
		return (0)
	    else
		return (I32Add (i, 1))
	  ;


      define inline @is-empty (deque : deque) : bool =
	  if I32Eq (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)) then
	      return (true)
	  else
	      return (false)
	;

      define inline @is-full (deque : deque) : bool =
	  let size : int = @size (deque)
        (* leave one space open *)
	  if I32Gte (size, I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 1)) then
	      return (true)
	  else
	      return (false)
	;

      define inline @new-from-atomic (self : vproc, workerId : UID.uid, size : int) : deque =
	  let deque : deque = ccall M_DequeAlloc (self, workerId, size)
          return (deque)
	;

       define inline @is-full-from-atomic (self : vproc, deque : deque) : bool =
           do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	   @is-full (deque)
	 ;

       define inline @is-empty-from-atomic (self : vproc, deque : deque) : bool =
           do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	   @is-empty (deque)
	 ;
      
      define inline @is-claimed-from-atomic (self : vproc, deque : deque) : bool =
          if I32Eq (LOAD_DEQUE_NCLAIMED(deque), 0) then
	      return (false)
	  else
	      return (true)
        ;
       
    (* precondition: the deque is not full *)
      define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : () =
	  do assert (NotEqual (deque, enum(0):any))
	  do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	  do @check-deque (deque)
	  do assert(NotEqual(elt, DEQUE_NIL_ELT))
	  let isFull : bool = @is-full (deque)
(*           do assert (BNot (isFull))*)
	  let new : int = LOAD_DEQUE_NEW(deque)
	  let newR : int = @move-right (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	  do STORE_DEQUE_NEW(deque, newR)
	  do @update (deque, new, elt)
	  do @check-deque (deque)
	  return ()
	;

      define inline @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option =
	  do assert (NotEqual (deque, enum(0):any))
	  do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	  do @check-deque (deque)
	  let isEmpty : bool = @is-empty (deque)
	  case isEmpty
	   of true =>
	      return (Option.NONE)
	    | false =>
	      let newL : int = @move-left (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	      let elt : any = @sub (deque, newL)
	      do @update (deque, newL, DEQUE_NIL_ELT)
	      do STORE_DEQUE_NEW(deque, newL)
	      do @check-deque (deque)
	      do assert (NotEqual(elt, DEQUE_NIL_ELT))
	      return (Option.SOME (elt))
	  end
	;

      define inline @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option =
	  do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	  do @check-deque (deque)
	  let isEmpty : bool = @is-empty (deque)
	  case isEmpty
	   of true =>
	      return (Option.NONE)
	    | false =>
	      let old : int = LOAD_DEQUE_OLD(deque)
	      let elt : any = @sub (deque, old)
	      do @update (deque, old, DEQUE_NIL_ELT)
	      let oldR : int = @move-right (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_MAX_SIZE(deque))
	      do STORE_DEQUE_OLD(deque, oldR)
	      do @check-deque (deque)
	      return (Option.SOME(elt))
	  end
	;

    (* returns the deques associated with the given vproc and the work group id. all the deques in this list are 
     * automatically claimed for the caller. *)
      define @local-deques-from-atomic (self : vproc, workGroupId : UID.uid) : (* [deque] *) List.list =
          let localDeques : List.list = ccall M_LocalDeques (self, workGroupId)
          return (localDeques)
        ;

      define inline @release-from-atomic (self : vproc, deque : deque) : () =
          do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
          do STORE_DEQUE_NCLAIMED(deque, I32Sub (LOAD_DEQUE_NCLAIMED(deque), 1))
          return ()
        ;

      define @release-deques-from-atomic (self : vproc, deques : List.list) : () =
          fun release (deque : [deque] / _ : exh) : () = @release-from-atomic (self, #0(deque))
          cont exh (_ : exn) = return ()
          PrimList.@app (release, deques / exh)
	;

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : UID.uid, deque : deque) : deque =
          do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	  let size : int = @size (deque)
	  let newDeque : deque = @new-from-atomic (self, workGroupId, I32Mul (LOAD_DEQUE_MAX_SIZE(deque), 2))
        (* maintain the original order of the deque by popping from the old end of the original deque
	 * and pushing on the new end of the fresh deque
	 *)
	  fun copy () : () =
	      let elt : Option.option = @pop-old-end-from-atomic (self, deque)
              case elt
	       of Option.NONE =>
		  return ()
		| Option.SOME (elt : any) =>
		  do @push-new-end-from-atomic (self, newDeque, elt)
                  apply copy ()
              end
          do apply copy ()
          do @release-from-atomic (self, deque)
	  return (newDeque)
	;

    (* the list of returned threads is ordered from oldest to youngest *)
      define @to-list-from-atomic (self : vproc, deque : deque) : (* ImplicitThread.thread *) List.list =
	  fun lp () : List.list =
	      let thd : Option.option = @pop-old-end-from-atomic (self, deque)
	      case thd
	       of Option.NONE =>
		  return (List.nil)
		| Option.SOME (thd : ImplicitThread.thread) =>
		  let rest : List.list = apply lp ()
		  return (CONS (thd, rest))
	      end
	  apply lp ()
	;

    (* the list of threads is inserting in order from oldest to youngest *)
      define @add-list-from-atomic (self : vproc, deque : deque, thds : List.list) : () =
          fun add (thd : ImplicitThread.thread / _ : exh) : () = @push-new-end-from-atomic (self, deque, thd)
          cont exh (_ : exn) = return ()
          PrimList.@app (add, thds / exh)
	;

    )

  end
