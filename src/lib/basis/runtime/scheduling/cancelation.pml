(* cancelation.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A cancelation mechanism for fibers.
 *)

#define CANCELED_OFF    0
#define INACTIVE_OFF    1
#define CHILDREN_OFF    2
#define PARENT_OFF      3
#define TAG_OFF         4

structure Cancelation =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure O = Option
    structure L = List

  (* communication channel for canceling fibers *)
    type cancelable = _prim (
		  ![
		     PT.bool,         (* canceled flag (TRUE=canceled) *)
		     PT.bool,         (* inactive flag (TRUE=inactive) *)
		     L.list,          (* children pointers (has type cancelable L.list) *)
		     O.option,        (* parent pointer (has type cancelable O.option) *)
		     FLS.fls_tag      (* tag to find the current cancelable *)
		  ] )

    _primcode (

    (* add c to the parent's list of children *)
      define @add-child (c : cancelable, parent : cancelable / exh : PT.exh) : () =
	let children : L.list = SELECT(CHILDREN_OFF, parent)
	let children : L.list = L.CONS(c, children)
	let children : L.list = promote(children)
	do UPDATE(CHILDREN_OFF, parent, children)
	return ()
      ;

    (* find the current cancelable (if it exists) *)
      define @get-current (cancelableTag : FLS.fls_tag / exh : PT.exh) : O.option =
	let fls : FLS.fls = FLS.@get(/ exh)
	let currentCOpt : O.option = FLS.@find(fls, cancelableTag / exh)
	case currentCOpt
	 of O.NONE => return(O.NONE)
	  | O.SOME (cOptC : ![O.option]) => 
	    return(#0(cOptC))
        end		     
      ;

    (* set the current cancelable *)
      define @set-current (cancelableTag : FLS.fls_tag, cOpt : O.option / exh : PT.exh) : () =
        let fls : FLS.fls = FLS.@get(/ exh)
	let currentCOpt : O.option = FLS.@find(fls, cancelableTag / exh)
	case currentCOpt
	 of O.NONE => return()
	  | O.SOME (cOptC : ![O.option]) =>
	    do UPDATE(0, cOptC, cOpt)
            return()
        end
      ;

    (* set the cancelable as terminated *)
      define @set-inactive (c : cancelable / exh : PT.exh) : () =
        let currentCOpt : O.option = @get-current(SELECT(TAG_OFF, c) / exh)
      (* set the parent as the current cancelable *)
        do @set-current(SELECT(TAG_OFF, c), SELECT(PARENT_OFF, c) / exh)
      (* mark the inactive flag; use CAS as a memory fence *)
        let x : PT.bool = CAS(ADDR_OF(INACTIVE_OFF,c), PT.false, PT.true)
        return()
      ;

    (* set the cancelable as ready to run *)
      define @set-active (c : cancelable / exh : PT.exh) : () =
        let cOpt : O.option = O.SOME(c)
        let cOpt : O.option = promote(cOpt)
 	do @set-current(SELECT(TAG_OFF, c), cOpt / exh)
      (* use CAS as a memory fence *)
        let x : PT.bool = CAS(ADDR_OF(INACTIVE_OFF,c), PT.true, PT.false) 
        return()
      ;

    (* attach the fiber k to the cancelable *)
      define @wrap (c : cancelable, k : PT.fiber / exh : PT.exh) : PT.fiber =

      (* terminate the wrapped fiber *)
        cont terminate () = 
             do @set-inactive(c / exh)
             let _ : PT.unit = Control.@stop(/ exh)
             return($0)
      (* run the wrapped fiber *)
        cont dispatch (wrapper : PT.sigact, k : PT.fiber) =
             if SELECT(CANCELED_OFF, c)
                then 
		(* the fiber has been canceled *)
		 throw terminate()
	     else
		(* run the fiber *)
		 do @set-active(c / exh)
                 do Control.@run(wrapper, k / exh)
                 return($0)
      (* scheduler action that polls for cancelation *)
        cont wrapper (s : PT.signal) =
             case s
	      of PT.STOP => 
		 throw terminate()
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 let _ : PT.unit = Control.@atomic-yield(/ exh)
                 throw dispatch(wrapper, k)
	       | PT.SUSPEND (k : PT.fiber, retK : cont(PT.fiber)) =>
               (* pass the return continuation a wrapped version of k *)
		 cont wrappedK (x : PT.unit) = 
		   throw dispatch(wrapper, k)
	         cont retK' (x : PT.unit) =
		   throw retK(wrappedK)
                 throw dispatch(wrapper, retK')
	       | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, x : any) =>
		 cont retK' (x : PT.unit) = 
		   throw dispatch(wrapper, retK)
		 do Control.@forward(PT.UNBLOCK(retK', k, x) / exh)
                 return($0)
             end

        cont wrappedK (x : PT.unit) =
             do vpstore(ATOMIC, host_vproc, PT.true)
             throw dispatch(wrapper, k)

        return(wrappedK)
      ;

    (* create a cancelable *)
      define @new (cTag : FLS.fls_tag / exh : PT.exh) : cancelable =
        let parent : O.option = @get-current(cTag / exh)
        let c : cancelable = alloc(PT.false, PT.true, L.NIL, parent, cTag)
        let c : cancelable = promote(c)
      (* add this new cancelable to the parent's list of children *)
        do case parent
	    of NONE => 
	       (* this cancelable is at the root of the spawn tree *)
               let dummyC : cancelable = alloc(PT.false, PT.true, L.NIL, NONE, cTag)
               let dummyC : cancelable = promote(dummyC)
               cont k (x : PT.unit) = return()
               let k : PT.fiber = @wrap(dummyC, k / exh)
               throw k(UNIT)
	     | SOME(parent : cancelable) => 
	       @add-child(parent, c / exh)
           end
        return(c)
      ;

    (* cancel a cancelable. we use a synchronous semantics for this operation. the cancelable and all of its
     * children must terminate before continuing.
     *)
      define @cancel (c : cancelable / exh : PT.exh) : () =
      (* walk down the spawn tree and wait for all canceled fibers to terminate *)
        fun lp (ins : L.list, outs : L.list / exh : PT.exh) : () =
	    case ins
	     of L.NIL => 
		case outs
		 of L.NIL => return()          (* all cancelables have terminated *)
		  | L.CONS (x : cancelable, xs : L.list) => 
		    let ins : L.list = PrimList.@rev(outs / exh)
                    apply lp(ins, L.NIL / exh)
                end
	      | L.CONS(c : cancelable, ins' : L.list) =>
              (* use the CAS as a memory fence *)
                let isCanceled : PT.bool = CAS(ADDR_OF(CANCELED_OFF,c), PT.false, PT.true) 
		if SELECT(INACTIVE_OFF, c)
		   then 
		  (* cancel the children *)
                    let outs : L.list = PrimList.@append(SELECT(CHILDREN_OFF, c), outs / exh)
		    apply lp(ins', outs / exh)
		else 
                  (* spin until the cancelable is inactive *)
		    apply lp(ins, outs / exh)		  
            end
        apply lp(L.CONS(c, L.NIL), L.NIL / exh)
      ;

    )

  end
