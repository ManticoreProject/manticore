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

    _primcode (

     (* communication channel for canceling fibers *)
       typedef cancelable =
		     ![
			bool,         (* canceled flag (TRUE=canceled) *)
			bool,         (* inactive flag (TRUE=inactive) *)
			L.list,          (* children pointers (has type cancelable L.list) *)
			O.option,        (* parent pointer (has type cancelable O.option) *)
			FLS.fls_tag      (* tag to find the current cancelable *)
		     ];

    (* add c to the parent's list of children *)
      define @add-child (c : cancelable, parent : cancelable / exh : exh) : () =
	let children : L.list = SELECT(CHILDREN_OFF, parent)
	let children : L.list = L.CONS(c, children)
	let children : L.list = promote(children)
	do UPDATE(CHILDREN_OFF, parent, children)
	return ()
      ;

    (* find the current cancelable (if it exists) *)
      define @get-current (cancelableTag : FLS.fls_tag / exh : exh) : O.option =
	let fls : FLS.fls = FLS.@get(/ exh)
	let currentCOpt : O.option = FLS.@find(fls, cancelableTag / exh)
	case currentCOpt
	 of O.NONE => return(O.NONE)
	  | O.SOME (cOptC : ![O.option]) => 
	    return(#0(cOptC))
        end		     
      ;

    (* set the current cancelable *)
      define @set-current (cancelableTag : FLS.fls_tag, cOpt : O.option / exh : exh) : () =
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
      define @set-inactive (c : cancelable / exh : exh) : () =
        let currentCOpt : O.option = @get-current(SELECT(TAG_OFF, c) / exh)
      (* set the parent as the current cancelable *)
        do @set-current(SELECT(TAG_OFF, c), SELECT(PARENT_OFF, c) / exh)
      (* mark the inactive flag; use CAS as a memory fence *)
        let x : bool = CAS(ADDR_OF(INACTIVE_OFF,c), false, true)
        return()
      ;

    (* set the cancelable as ready to run *)
      define @set-active (c : cancelable / exh : exh) : () =
        let cOpt : O.option = O.SOME(c)
        let cOpt : O.option = promote(cOpt)
 	do @set-current(SELECT(TAG_OFF, c), cOpt / exh)
      (* use CAS as a memory fence *)
        let x : bool = CAS(ADDR_OF(INACTIVE_OFF,c), true, false) 
        return()
      ;

    (* attach the fiber k to the cancelable *)
      define @wrap (c : cancelable, k : PT.fiber / exh : exh) : PT.fiber =

      (* terminate the wrapped fiber *)
        cont terminate () = 
             do @set-inactive(c / exh)
             let _ : unit = Control.@stop(/ exh)
             return($0)
      (* run the wrapped fiber *)
        cont dispatch (wrapper : PT.sched_act, k : PT.fiber) =
             do @set-active(c / exh)
             if SELECT(CANCELED_OFF, c)
                then 
		 throw terminate()
	     else
                 do Control.@run(wrapper, k / exh)
                 return($0)
      (* scheduler action that polls for cancelation *)
        cont wrapper (s : PT.signal) =
             case s
	      of PT.STOP => 
		 throw terminate()
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 let _ : unit = Control.@atomic-yield(/ exh)
                 throw dispatch(wrapper, k)
	       | _ =>
		 let e : exn = Match
                 throw exh (e)
             end

        cont wrappedK (x : unit) =
             do vpstore(ATOMIC, host_vproc, true)
             throw dispatch(wrapper, k)

        return(wrappedK)
      ;

    (* create a cancelable *)
      define @new (cTag : FLS.fls_tag / exh : exh) : cancelable =
        let parent : O.option = @get-current(cTag / exh)
        let c : cancelable = alloc(false, true, nil, parent, cTag)
        let c : cancelable = promote(c)
      (* add this new cancelable to the parent's list of children *)
        do case parent
	    of O.NONE => 
	       (* this cancelable is at the root of the spawn tree *)
               let dummyC : cancelable = alloc(false, true, nil, O.NONE, cTag)
               let dummyC : cancelable = promote(dummyC)
               cont k (x : unit) = return()
               let k : PT.fiber = @wrap(dummyC, k / exh)
               throw k(UNIT)
	     | O.SOME(parent : cancelable) => 
	       @add-child(parent, c / exh)
           end
        return(c)
      ;

    (* cancel a cancelable. we use a synchronous semantics for this operation. the cancelable and all of its
     * children must terminate before continuing.
     *)
      define @cancel (c : cancelable / exh : exh) : () =
      (* walk down the spawn tree and wait for all canceled fibers to terminate *)
        fun spin (ins : L.list, outs : L.list / exh : exh) : () =
	    case ins
	     of nil => 
		case outs
		 of nil => return()          (* all cancelables have terminated *)
		  | L.CONS (x : cancelable, xs : L.list) => 
		    let ins : L.list = PrimList.@rev(outs / exh)
                    apply spin(ins, nil / exh)
                end
	      | L.CONS(c : cancelable, ins' : L.list) =>
              (* use the CAS as a memory fence *)
                let isCanceled : bool = CAS(ADDR_OF(CANCELED_OFF,c), false, true) 
		if SELECT(INACTIVE_OFF, c)
		   then 
		  (* cancel the children *)
                    let outs : L.list = PrimList.@append(SELECT(CHILDREN_OFF, c), outs / exh)
		    apply spin(ins', outs / exh)
		else 
                  (* spin until the cancelable is inactive *)
		    apply spin(ins, outs / exh)		  
            end
        apply spin(L.CONS(c, nil), nil / exh)
      ;

    )

  end
