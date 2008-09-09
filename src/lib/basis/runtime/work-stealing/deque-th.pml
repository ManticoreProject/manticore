(* deque-th.hlop
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Concurrent deques following the protocol used in Cilk-5.
 *)

structure DequeTH =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure SpinLock = SPIN_LOCK_NAME
    structure O = Option

    _primcode (

      typedef deque = DequeTHRep.deque;

      define @new ( / exh : PT.exh) : deque =
	let arr : Arr.array = Arr.@array(TH_DEQUE_LEN, enum(0) / exh)
	let deq : deque = alloc(0, 0, arr, PT.FALSE)
	let deq : deque = promote(deq)
	return(deq)
      ;

      define @push-tl (deq : deque, elt : any / exh : PT.exh) : () =
      (* copy the contents of the deque to a fresh array *)
	fun copyDeque (arr : Arr.array, i : int / exh : PT.exh) : () =
	    if I32Lt(i, TH_DEQUE_LEN)
	       then let elt : any = Arr.@sub(arr, i / exh)
(*		    do Arr.@update(arr, I32Sub(SELECT(TH_H_OFF, deq), i), NIL / exh)*)
		    do Arr.@update(arr, I32Sub(SELECT(TH_H_OFF, deq), i), elt / exh)
		    apply copyDeque(arr, I32Add(i, 1) / exh)
	       else return()
      (* free space on the deque by copying *)
	fun freeSpace (t : int /) : int =
	    if I32Lt(t, TH_DEQUE_LEN)
	       then return(t)
	       else let mask : PT.bool = SpinLock.@lock(deq / exh)
		    do apply copyDeque(SELECT(TH_ARR_OFF, deq), SELECT(TH_H_OFF, deq) / exh)
		    let t : int = I32Sub(t, SELECT(TH_T_OFF, deq))
		    do UPDATE(TH_H_OFF, deq, 0)
		    do UPDATE(TH_T_OFF, deq, t)
		    do SpinLock.@unlock (deq, mask / exh)
		    return(t)
	let t : int = SELECT(TH_T_OFF, deq)
       (* possibly need to free space if the tail has reached the end of the deque *)
	let t : int = apply freeSpace(t /)
	do assert(I32Lt(t, TH_DEQUE_LEN))
	do assert(I32Gte(t, 0))
       (* put the element into the array *)
	do Arr.@update(SELECT(TH_ARR_OFF, deq), t, elt / exh)
       (* update the tail pointer *)
	(* NOTE: need a memory barrier here if stores are not guaranteed to be seen in order. *)
	do UPDATE(TH_T_OFF, deq, I32Add(t, 1))
	return()
      ;

      define @pop-hd (deq : deque / exh : PT.exh) : Option.option =
	cont none () = return(O.NONE)
	let mask : PT.bool = SpinLock.@lock (deq / exh)
	let h : int = I32FetchAndAdd(&TH_H_OFF(deq), 1)
	let h : int = I32Add(h, 1)
	let t : int = SELECT(TH_T_OFF, deq)
	let eltOpt : Option.option = 
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
		    return(Option.SOME(frame))
	do SpinLock.@unlock (deq, mask / exh)
	return(eltOpt)
      ;

      define @pop-tl (deq : deque / exh : PT.exh) : Option.option =
	cont none () = return(O.NONE)
	let t : int = I32FetchAndAdd(&TH_T_OFF(deq), ~1)
	let t : int = I32Add(t, ~1)
	let h : int = SELECT(TH_H_OFF, deq)
	do if I32Gt(h, t)
	      then (* contention with a thief *)
		   let t : int = I32FetchAndAdd(&TH_T_OFF(deq), 1)
		   let t : int = I32Add(t, 1)
		   let mask : PT.bool = SpinLock.@lock (deq / exh)

		  (* restart the protocol *)
		   let t : int = I32FetchAndAdd(&TH_T_OFF(deq), ~1)
		   let t : int = I32Add(t, ~1)

		   let h : int = SELECT(TH_H_OFF, deq)
		   do if I32Gt(h, t)
			 then (* the deque is empty *)
			      let t : int = I32FetchAndAdd(&TH_T_OFF(deq), 1)
			      let t : int = I32Add(t, 1)
			      do SpinLock.@unlock (deq, mask / exh) 
			      throw none()
			 else return()

		   do SpinLock.@unlock (deq, mask / exh)
		   return()
	       else return()
	do assert(I32Lt(SELECT(TH_T_OFF, deq), TH_DEQUE_LEN))
	do assert(I32Gte(SELECT(TH_T_OFF, deq), 0))
	let arr : Arr.array = SELECT(TH_ARR_OFF, deq)
	let frame : any = Arr.@sub(arr, t / exh)
       (* IMPORTANT: a pointer to frame still exists in the array; erase it to avoid a space leak *)
	do assert(I32Gte(t,0))
	do Arr.@update (arr, t, enum(0) / exh)
	return(Option.SOME(frame))
      ;

    )

  end
