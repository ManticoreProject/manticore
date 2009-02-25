(* cilk-5-deque.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Concurrent deque structure as defined by the Cilk-5 implementation.
 *
 * Deque representation
 * We represent a deque as an array bounded from below by a tail pointer and above by head pointer. The
 * deque structure contains a spin lock for manipulating the head pointer.
 *
 * |-------------------|  (low address)
 * |       ...         |   
 * |-------------------|
 * |                   |   <-- head
 * |-------------------|
 * |       ...         |   ..elements..
 * |-------------------|
 * |                   |   <-- tail
 * |-------------------|
 * |       ...         |   
 * |-------------------|  (high address)
 *
 *)

structure Cilk5Deque (* :
  sig

    typedef deque;

  (* create a deque *)
    define @new ( / exh : exh) : deque;

  (* push an element on the tail of the deque.
   * NOTE: this operation is single threaded.
   * PRECONDITION: assume that signals are masked
   *)
    define @push-tl-from-atomic (deq : deque, elt : any / exh : exh) : ();

  (* pop an element from the tail of the deque.
   * NOTE: this operation is single threaded.
   * PRECONDITION: assume that signals are masked
   *)
    define @pop-tl-from-atomic (deq : deque / exh : exh) : Option.option;

  (* pop an element from the head of the deque. 
   * PRECONDITION: assume that signals are masked
   *) 
    define @pop-hd-from-atomic (deq : deque / exh : exh) : Option.option;

  end *) = struct

#define DEQUE_SZ                      2048

#define TH_T_OFF                      0
#define TH_H_OFF                      1
#define TH_ARR_OFF                    2
#define TH_LOCK_OFF                   3

#include "../include/spin-lock.def"

    structure O = Option
    structure Arr = Array64

    _primcode(

      typedef deque = ![
		  int,                   (* tail *)
		  int,                   (* head *)
		  Arr.array,             (* array of implicit threads *)
		  bool                   (* lock on the head of the deque *)
	      ];

  (* create a deque *)
      define @new ( / exh : exh) : deque =
	let arr : Arr.array = Arr.@array(DEQUE_SZ, enum(0) / exh)
	let deq : deque = alloc(0, 0, arr, false)
	let deq : deque = promote(deq)
	return(deq)
      ;

   (* push an element on the tail of the deque.
    * NOTE: this operation is single threaded.
    * PRECONDITION: assume that signals are masked
    *)
      define @push-tl-from-atomic (deq : deque, elt : any / exh : exh) : () =
      (* copy the contents of the deque to a fresh array *)
	fun copyDeque (arr : Arr.array, i : int / exh : exh) : () =
	    if I32Lt(i, DEQUE_SZ)
	       then let elt : any = Arr.@sub(arr, i / exh)
		    do Arr.@update(arr, I32Sub(SELECT(TH_H_OFF, deq), i), elt / exh)
		    apply copyDeque(arr, I32Add(i, 1) / exh)
	       else return()
      (* free space on the deque by copying *)
	fun freeSpace (t : int /) : int =
	    if I32Lt(t, DEQUE_SZ)
	       then return(t)
	       else SPIN_LOCK(deq, TH_LOCK_OFF)
		    do apply copyDeque(SELECT(TH_ARR_OFF, deq), SELECT(TH_H_OFF, deq) / exh)
		    let t : int = I32Sub(t, SELECT(TH_T_OFF, deq))
		    do UPDATE(TH_H_OFF, deq, 0)
		    do UPDATE(TH_T_OFF, deq, t)
		    SPIN_UNLOCK(deq, TH_LOCK_OFF)
		    return(t)
	let t : int = SELECT(TH_T_OFF, deq)
       (* possibly need to free space if the tail has reached the end of the deque *)
	let t : int = apply freeSpace(t /)
	do assert(I32Lt(t, DEQUE_SZ))
	do assert(I32Gte(t, 0))
       (* put the element into the array *)
	do Arr.@update(SELECT(TH_ARR_OFF, deq), t, elt / exh)
       (* update the tail pointer *)
	(* NOTE: need a memory barrier here if stores are not guaranteed to be seen in order. *)
	do UPDATE(TH_T_OFF, deq, I32Add(t, 1))
	return()
      ;

   (* pop an element from the tail of the deque.
    * NOTE: this operation is single threaded.
    * PRECONDITION: assume that signals are masked
    *)
      define @pop-tl-from-atomic (deq : deque / exh : exh) : O.option =
	cont none () = return(O.NONE)
	let t : int = I32FetchAndAdd(&TH_T_OFF(deq), ~1)
	let t : int = I32Add(t, ~1)
	let h : int = SELECT(TH_H_OFF, deq)
	do if I32Gt(h, t)
	      then (* contention with a thief *)
		   let t : int = I32FetchAndAdd(&TH_T_OFF(deq), 1)
		   let t : int = I32Add(t, 1)
                   SPIN_LOCK(deq, TH_LOCK_OFF)

		  (* restart the protocol *)
		   let t : int = I32FetchAndAdd(&TH_T_OFF(deq), ~1)
		   let t : int = I32Add(t, ~1)

		   let h : int = SELECT(TH_H_OFF, deq)
		   do if I32Gt(h, t)
			 then (* the deque is empty *)
			      let t : int = I32FetchAndAdd(&TH_T_OFF(deq), 1)
			      let t : int = I32Add(t, 1)
			      SPIN_UNLOCK(deq, TH_LOCK_OFF)
			      throw none()
			 else return()

		   SPIN_UNLOCK(deq, TH_LOCK_OFF)
		   return()
	       else return()
	do assert(I32Lt(SELECT(TH_T_OFF, deq), DEQUE_SZ))
	do assert(I32Gte(SELECT(TH_T_OFF, deq), 0))
	let arr : Arr.array = SELECT(TH_ARR_OFF, deq)
	let frame : any = Arr.@sub(arr, t / exh)
	do assert(I32Gte(t,0))
       (* IMPORTANT: a pointer to frame still exists in the array; erase it to avoid a space leak *)
	do Arr.@update (arr, t, enum(0) / exh)
	return(O.SOME(frame))
      ;

    (* pop an element from the head of the deque. 
     * PRECONDITION: assume that signals are masked
     *) 
      define @pop-hd-from-atomic (deq : deque / exh : exh) : O.option =
	cont none () = return(O.NONE)
        SPIN_LOCK(deq, TH_LOCK_OFF)
	let h : int = I32FetchAndAdd(&TH_H_OFF(deq), 1)
	let h : int = I32Add(h, 1)
	let t : int = SELECT(TH_T_OFF, deq)
	let eltOpt : O.option = 
	    if I32Gt(h, t)
	       then (* contention with the victim; back off *)
		    let h : int = I32FetchAndAdd(&TH_H_OFF(deq), ~1)
		    let h : int = I32Add(h, ~1)
		    return(O.NONE)
	       else 
		    let arr : Arr.array = SELECT(TH_ARR_OFF, deq)
		    let frame : any = Arr.@sub(arr, I32Sub(h, 1) / exh)
		   (* IMPORTANT: a pointer to frame still exists in the array; erase it to avoid a space leak *)
		    do Arr.@update (arr, I32Sub(h, 1), enum(0) / exh)
		    return(O.SOME(frame))
	SPIN_UNLOCK(deq, TH_LOCK_OFF)
	return(eltOpt)
      ;

    )

  end
