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
      define inline @new-from-atomic (self : vproc, workGroupId : long, size : int) : deque;

      define inline @is-full-from-atomic (self : vproc, deque : deque) : bool;
      define inline @is-empty-from-atomic (self : vproc, deque : deque) : bool;

      define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : ();
      define inline @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option;
      define inline @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option;

    (* the return value is true if the deque was claimed successfully *)
      define inline @claim-from-atomic (self : vproc, deque : deque) : bool;
      define inline @release-from-atomic (self : vproc, deque : deque) : ();

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : long, deque : deque) : deque;

    (* returns the deques associated with the given vproc and the work group id *)
      define @local-deques-from-atomic (self : vproc, workGroupId : long) : List.list;
    )

  end *) = struct

#define DEQUE_NIL_ELT        enum(0):any
#define INITIAL_DEQUE_SIZE   128

    _primcode (

      extern void* GetNthVProc (int);
      extern void* M_DequeAlloc (void*, long, int);
      extern void* M_LocalDeques (void*, long) __attribute__((alloc));

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
            Value_t       claimed;       // M_TRUE, if the deque is claimed by a scheduler
	    Value_t       elts[];        // elements of the deque
	};

    *)

    )

#define LOAD_DEQUE_OLD(deque)        AddrLoadI32 ((addr(int))&0(deque))
#define LOAD_DEQUE_NEW(deque)        AddrLoadI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (4:long)))
#define STORE_DEQUE_OLD(deque, i)    AddrStoreI32 ((addr(int))&0(deque), i)
#define STORE_DEQUE_NEW(deque, i)    AddrStoreI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (4:long)), i)

#define LOAD_DEQUE_MAX_SIZE(deque)   AddrLoadI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (8:long)))

#define LOAD_DEQUE_CLAIMED(deque)        AddrLoad ((addr(any))AddrAdd (&0(deque), I64ToAddr (12:long)))
#define STORE_DEQUE_CLAIMED(deque, c)    AddrStore ((addr(any))AddrAdd (&0(deque), I64ToAddr (12:long)), c)

    local

      _primcode (

	define inline @num-elts (deque : deque) : int =
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		return (I32Sub (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	    else 
		return (I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 
				I32Sub (LOAD_DEQUE_OLD(deque), 
					LOAD_DEQUE_NEW(deque))))
	  ;

	define inline @is-empty (deque : deque) : bool =
	    return (I32Eq (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	  ;

	define inline @is-full (deque : deque) : bool =
	    let size : int = @num-elts (deque)
	    return (I32Gte (size, I32Sub (LOAD_DEQUE_MAX_SIZE(deque), 1)))        (* leave one space open *)
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

	define inline @update (deque : deque, i : int, elt : any) : () =
	    do @assert-in-bounds (deque, i)
	    do AddrStore (AddrAdd (&0(deque), 
				   AddrAdd (I64ToAddr (16:long),         (* the byte offset of elts *)
				   I64ToAddr (I32ToI64X (I32LSh (i, 3))))), 
			  elt)
	    return ()
	  ;

	define inline @sub (deque : deque, i : int) : any =
	    do @assert-in-bounds (deque, i)
	    let elt : any = AddrLoad (AddrAdd (&0(deque),         (* the byte offset of elts *)
					       AddrAdd (I64ToAddr (16:long),
					       I64ToAddr (I32ToI64X (I32LSh (i, 3))))))
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

      )

    in

    _primcode (

      define inline @new-from-atomic (self : vproc, workerId : long, size : int) : deque =
	  let deque : deque = ccall M_DequeAlloc (self, workerId, size)
          return (deque)
	;

       define inline @is-full-from-atomic (self : vproc, deque : deque) : bool =
	   @is-full (deque)
	 ;

       define inline @is-empty-from-atomic (self : vproc, deque : deque) : bool =
	   @is-empty (deque)
	 ;

     (* precondition: the deque is not full *)
       define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any) : () =
	   do @check-deque (deque)
	   do assert(NotEqual(elt, DEQUE_NIL_ELT))
	   let isFull : bool = @is-full (deque)
	   do assert (BNot(isFull))
	   let new : int = LOAD_DEQUE_NEW(deque)
	   let newR : int = @move-right (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	   do STORE_DEQUE_NEW(deque, newR)
	   do @update (deque, new, elt)
	   do @check-deque (deque)
	   return ()
	 ;

     define inline @pop-new-end-from-atomic (self : vproc, deque : deque) : Option.option =
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
	     do assert(NotEqual(elt, DEQUE_NIL_ELT))
	     return (Option.SOME (elt))
       ;

     define inline @pop-old-end-from-atomic (self : vproc, deque : deque) : Option.option =
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

    (* the return value is true if the deque was claimed successfully *)
      define inline @claim-from-atomic (self : vproc, deque : deque) : bool =
          if LOAD_DEQUE_CLAIMED(deque) then
	      return (false)
	  else
	      do STORE_DEQUE_CLAIMED(deque, true)
              return (true)
        ;

      define inline @release-from-atomic (self : vproc, deque : deque) : () =
          do assert (LOAD_DEQUE_CLAIMED(deque))
          do STORE_DEQUE_CLAIMED(deque, false)
          return ()
        ;

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, workGroupId : long, deque : deque) : deque =
	  let size : int = @num-elts (deque)
	  let newDeque : deque = @new-from-atomic (self, workGroupId, I32Mul (LOAD_DEQUE_MAX_SIZE(deque), 2))
	  fun copyElts (i : int, j : int) : () =
	      if I32Lt (j, size) then
		  let elt : any = @sub (deque, i)
		  do @update(deque, i, DEQUE_NIL_ELT)
		  do @update (newDeque, j, elt)
		  let iR : int = @move-right (i, LOAD_DEQUE_MAX_SIZE(deque))
		  apply copyElts (iR, I32Add (j, 1))
	      else
		  return ()
	  do apply copyElts (LOAD_DEQUE_OLD(deque), 0)
          do STORE_DEQUE_NEW(deque, 0)
          do STORE_DEQUE_OLD(deque, 0)
	  return (newDeque)
	;

    (* returns the deques associated with the given vproc and the work group id *)
      define @local-deques-from-atomic (self : vproc, workGroupId : long) : List.list =
          let localDeques : List.list = ccall M_LocalDeques (self, workGroupId)
          return (localDeques)
        ;

    )

    end

  end
