(* work-stealing-deque.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler.
 *
 *)


structure WorkStealingDeque (* :
  sig

    _prim (

      typedef deque;

    (* the second argument is the number of elements *)
      define inline @new-from-atomic (self : vproc, size : int) : deque;
      define inline @free-from-atomic (self : vproc, deque : deque) : ();

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, deque : deque) : deque;

      define inline @is-full-from-atomic (self : vproc, deque : deque) : bool;

      define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any / exh : exh) : ();
      define inline @pop-new-end-from-atomic (self : vproc, deque : deque / exh : exh) : Option.option;
      define inline @pop-old-end-from-atomic (self : vproc, deque : deque / exh : exh) : Option.option;

    )

  end *) = struct

#define DEQUE_NIL_ELT        enum(0):any
#define INITIAL_DEQUE_SIZE   128

    _primcode (

      extern void* GetNthVProc (int);
      extern void* M_DequeAlloc (void*, int);
      extern void M_DequeFree (void*);

    (* Deque representation:
     *
     * For compactness, we represent deques as circular buffers. There are two pointers into this buffer:
     * old and new. The old pointer points to the oldest element on the deque and new points to the newest.
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
            bool          live;          // true, if the deque is being used by a scheduler
	    Value_t       elts[];        // elements of the deque
	};

    *)

    )

#define LOAD_DEQUE_OLD(deque)        AddrLoadI32 ((addr(int))&0(deque))
#define LOAD_DEQUE_NEW(deque)        AddrLoadI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (4:long)))
#define STORE_DEQUE_OLD(deque, i)    AddrStoreI32 ((addr(int))&0(deque), i)
#define STORE_DEQUE_NEW(deque, i)    AddrStoreI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (4:long)), i)

#define LOAD_DEQUE_MAX_SIZE(deque)   AddrLoadI32 ((addr(int))AddrAdd (&0(deque), I64ToAddr (8:long)))

    local

      _primcode (

	define inline @num-elts (deque : deque) : int =
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		return (I32Sub (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	    else 
		return (I32Sub (LOAD_DEQUE_SIZE(deque), I32Sub (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque))))
	  ;

	define inline @is-empty (deque : deque) : bool =
	    return (I32Eq (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_OLD(deque)))
	  ;

	define inline @is-full (deque : deque) : bool =
	    let size : int = @num-elts (deque)
	    return (I32Gte (size, LOAD_DEQUE_MAX_SIZE(deque)))
	  ;

	define @assert-in-bounds (deque : deque, i : int) : () =
	    do assert (I32Gte (i, 0))
	    do assert (I32Lt (i, LOAD_DEQUE_MAX_SIZE(deque)))
	    if I32Lte (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_NEW(deque)) then
		do assert (I32Gte (i, LOAD_DEQUE_NEW(deque)))
		do assert (I32Lt (i, LOAD_DEQUE_OLD(deque)))
  	        return ()
	    else
		do assert (I32Gte (i, LOAD_DEQUE_OLD(deque)))
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
	    if I32Gte (i, sz) then
		return (0)
	    else
		return (I32Add (i, 1))
	  ;

      )

    in

    _primcode (

      define inline @new-from-atomic (self : vproc, size : int) : deque =
	  ccall M_DequeAlloc (self, size : int)
	;

      define inline @free-from-atomic (self : vproc, deque : deque) : () =
	  ccall M_DequeFree (self, deque)
	;

    (* double the size of the deque *)
      define @double-size-from-atomic (self : vproc, deque : deque) : deque =
	  let size : int = @size (deque)
	  let newDeque : deque = @new-from-atomic (self, I32Mul (LOAD_DEQUE_MAX_SIZE(deque), 2))
	  fun copy (i : int, j : int) : () =
	      if I32Lt (j, size) then
		  let elt : any = @sub (deque, i)
		  do @update(deque, i, NIL_DEQUE_ELT)
		  do @update (newDeque, j, elt)
		  let iR : int = @move-right (i)
		  copy (iR, I32Add (j, 1))
	      else
		  return ()
	  do copy (LOAD_DEQUE_OLD(deque), 0)
          do STORE_DEQUE_NEW(deque, 0)
          do STORE_DEQUE_OLD(deque, 0)
	  return (newDeque)
	;

       define inline @is-full-from-atomic (self : vproc, deque : deque) : bool =
	   @is-full (deque)
	 ;

     (* precondition: the deque is not full *)
       define inline @push-new-end-from-atomic (self : vproc, deque : deque, elt : any / exh : exh) : () =
	   do @check-deque (deque)
	   do assert(NotEqual(elt, DEQUE_NIL_ELT))
	   let isFull : bool = @is-full (deque)
	   do assert (BNot(isFull))
	   let new : int = LOAD_DEQUE_NEW(deque)
	   let newR : int = @move-right (LOAD_DEQUE_NEW(deque), LOAD_DEQUE_MAX_SIZE(deque))
	   do STORE_DEQUE_NEW(deque, newR)
	   do @update (deque, new)
	   do @check-deque (deque)
	   return ()
	 ;

     define inline @pop-new-end-from-atomic (self : vproc, deque : deque / exh : exh) : Option.option =
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

     define inline @pop-old-end-from-atomic (self : vproc, deque : deque / exh : exh) : Option.option =
	 let isEmpty : bool = @is-empty (deque)
	 if isEmpty then
	     return (Option.NONE)
	 else
	     let old : int = LOAD_DEQUE_OLD(deque)
	     let elt : any = @sub (deque, old)
	     do @update (deque, old, DEQUE_NIL_ELT)
	     let oldR : int = @move-right (LOAD_DEQUE_OLD(deque), LOAD_DEQUE_MAX_SIZE(deque))
	     do STORE_DEQUE_OLD(deque, oldR)
	     return ()
       ;

    )

    end

  end
