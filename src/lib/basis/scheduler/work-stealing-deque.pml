(* work-stealing-deque.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler. 
 *
 * Memory management:
 * Since we allocate deques in the C heap, we rely on a reference counting scheme to manage memory
 * associated with deques. To maintain the reference counts, we require that workers always claim their
 * deques before using them and release the deques once they are no longer needed.
 *
 * There is a potential space leak. The memory manager only frees deques that are empty and have a zero
 * reference count. So, we must avoid leaving nonempty deques around for an unbounded 
 * amount of time.
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

      define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : ();
      define inline @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option;
      define inline @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option;

      define inline @release-from-atomic (self : vproc, deque : deque) : ();
      define @release-deques-from-atomic (self : vproc, deques : List.list) : ();

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : ImplicitThread.work_group_id, deque : deque) : deque;

    (* returns the deques associated with the given vproc and the work group id *)
      define @local-deques-from-atomic (self : vproc, workGroupId : ImplicitThread.work_group_id) : List.list;
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
     * "old" and "new". The old pointer points to the oldest element on the deque and new points to the newest.
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

(*    local*)

      _primcode (

	define (* inline *) @num-elts (deque : deque) : int =
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		return (I32Sub (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	    else 
		return (I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 
				I32Sub (LOAD_DEQUE_OLD(deque), 
					LOAD_DEQUE_NEW(deque))))
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
		    do assert (false)
		    return ()
		else
		    do assert (I32Lt (i, LOAD_DEQUE_NEW(deque)))
  	            return ()
	  ;

	define (* inline *) @assert-ptr (deque : deque, i : int) : () =
#ifndef NDEBUG
	    do ccall M_AssertDequeAddr (deque, i, AdrAddI64 (&0(deque), 
				   I64Add (DEQUE_ELTS_OFFB:long,         (* the byte offset of elts *)
					 I32ToI64X (I32LSh (i, 3)))))
#endif
	    return ()
	  ;

	define (* inline *) @update (deque : deque, i : int, elt : any) : () =
	    do @assert-in-bounds (deque, i)
            do @assert-ptr (deque, i)
	    do AdrStore (AdrAddI64 (&0(deque), 
				   I64Add (DEQUE_ELTS_OFFB:long,         (* the byte offset of elts *)
				   I32ToI64X (I32LSh (i, 3)))), 
			  elt)
	    return ()
	  ;

	define (* inline *) @sub (deque : deque, i : int) : any =
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
	define (* inline *) @move-left (i : int, sz : int) : int =
	    if I32Lte (i, 0) then
		return (I32Sub (sz, 1))
	    else
		return (I32Sub (i, 1))
	  ;

      (* move the index i one position right w.r.t. the deque size sz *)
	define (* inline *) @move-right (i : int, sz : int) : int =
	    if I32Gte (i, I32Sub (sz, 1)) then
		return (0)
	    else
		return (I32Add (i, 1))
	  ;

      )

(*    in *)

    _primcode (

      define (* inline *) @is-empty (deque : deque) : bool =
	  return (I32Eq (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	;

      define (* inline *) @is-full (deque : deque) : bool =
	  let size : int = @num-elts (deque)
	  return (I32Gte (size, I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 1)))        (* leave one space open *)
	;

      define (* inline *) @new-from-atomic (self : vproc, workerId : UID.uid, size : int) : deque =
	  let deque : deque = ccall M_DequeAlloc (self, workerId, size)
          return (deque)
	;

       define (* inline *) @is-full-from-atomic (self : vproc, deque : deque) : bool =
           do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	   @is-full (deque)
	 ;

       define (* inline *) @is-empty-from-atomic (self : vproc, deque : deque) : bool =
           do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	   @is-empty (deque)
	 ;

     (* precondition: the deque is not full *)
       define (* inline *) @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : () =
           do assert (NotEqual (deque, enum(0):any))
           do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	   do @check-deque (deque)
	   do assert(NotEqual(elt, DEQUE_NIL_ELT))
	   let isFull : bool = @is-full (deque)
           do assert (BNot (isFull))
	   let new : int = LOAD_DEQUE_NEW(deque)
	   let newR : int = @move-right (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	   do STORE_DEQUE_NEW(deque, newR)
	   do @update (deque, new, elt)
	   do @check-deque (deque)
	   return ()
	 ;

     define (* inline *) @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option =
         do assert (NotEqual (deque, enum(0):any))
         do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
         do @check-deque (deque)
	 let isEmpty : bool = @is-empty (deque)
	 if isEmpty then
	     return (Option.NONE)
	 else
	     let newL : int = @move-left (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	     let elt : any = @sub (deque, newL)
	     do @update (deque, newL, DEQUE_NIL_ELT)
	     do STORE_DEQUE_NEW(deque, newL)
	     do @check-deque (deque)
	     do assert (NotEqual(elt, DEQUE_NIL_ELT))
	     return (Option.SOME (elt))
       ;

     define (* inline *) @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option =
         do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
  	 do @check-deque (deque)
	 let isEmpty : bool = @is-empty (deque)
	 if isEmpty then
	     return (Option.NONE)
	 else
	     let old : int = LOAD_DEQUE_OLD(deque)
	     let elt : any = @sub (deque, old)
	     do @update (deque, old, DEQUE_NIL_ELT)
	     let oldR : int = @move-right (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_MAX_SIZE(deque))
	     do STORE_DEQUE_OLD(deque, oldR)
    	     do @check-deque (deque)
	     return (Option.SOME(elt))
       ;

      define (* inline *) @release-from-atomic (self : vproc, deque : deque) : () =
          do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
          do STORE_DEQUE_NCLAIMED(deque, I32Sub (LOAD_DEQUE_NCLAIMED(deque), 1))
          return ()
        ;

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : UID.uid, deque : deque) : deque =
          do assert (I32Gt (LOAD_DEQUE_NCLAIMED(deque), 0))
	  let size : int = @num-elts (deque)
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

    (* returns the deques associated with the given vproc and the work group id *)
      define @local-deques-from-atomic (self : vproc, workGroupId : UID.uid) : List.list =
          let localDeques : List.list = ccall M_LocalDeques (self, workGroupId)
          return (localDeques)
        ;

      define @release-deques-from-atomic (self : vproc, deques : List.list) : () =
	  fun lp (deques : List.list) : () =
	      case deques
	       of List.nil =>
		  return ()
		| List.CONS(deque : [deque], deques : List.list) =>
		  do @release-from-atomic (self, #0(deque))
		  apply lp (deques)
              end
	   apply lp (deques)
	;

    )

(*    end *)

  end
