(* cancelation.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A mechanism for canceling implicit threads.
 *)

#define CANCELED_OFF    0
#define INACTIVE_OFF    1
#define CHILDREN_OFF    2
#define GCHILDREN_OFF   3
#define PARENT_OFF      4

structure Cancelation (* : sig

    _prim(

      typedef cancelable;

    (* create a cancelable. 
     * NOTE: this operation affects the fiber-local storage
     *)
      define @new ( / exh : exh) : cancelable;
    (* cancel a cancelable. *)
      define @cancel (c : cancelable / exh : exh) : ();
    (* wrap a fiber with a cancelable. the fiber is now cancelable by applying @cancel(c / exh) *)
      define @wrap (c : cancelable, k : PT.fiber / exh : exh) : PT.fiber;

    )

  end *) = struct

    structure PT = PrimTypes
    structure O = Option
    structure L = List

    _primcode (

     (* communication channel for canceling fibers *)
       typedef cancelable =
                 [ ![bool],            (* canceled flag (TRUE=canceled) *)
                   ![bool],            (* inactive flag (TRUE=inactive) *)
                   L.list,             (* child pointers *)
		   ![L.list],          (* globally-visible child pointers 
					* INVARIANT: must be equal to locally-visible child pointers whenever inactive=true.
					*)
	           O.option            (* parent pointer *)
                 ];

    (* add c to the parent's list of children *)
      define @add-child (c : cancelable, parent : cancelable / exh : exh) : cancelable =
	let children : L.list = L.CONS(c, SELECT(CHILDREN_OFF, parent))
        let c : cancelable = alloc(SELECT(CANCELED_OFF, c), 
				   SELECT(INACTIVE_OFF, c), 
				   children, 
				   SELECT(GCHILDREN_OFF, c), 
				   SELECT(PARENT_OFF,c))
        return(c)
      ;

    (* find the current cancelable (if it exists) *)
      define @get-current ( / exh : exh) : O.option =
        let ite : FLS.ite = FLS.@get-ite(/ exh)
        return(#1(ite))
      ;

    (* set the current cancelable *)
      define @set-current (cOpt : O.option / exh : exh) : () =
        let ite : FLS.ite = FLS.@get-ite(/ exh)
        let ite : FLS.ite = alloc(#0(ite), cOpt)
        FLS.@set-ite(ite / exh)
      ;

      define @get-canceled-flag (c : cancelable / exh : exh) : ![bool] =
	let canceled : ![bool] = SELECT(CANCELED_OFF, c)
	let canceled : ![bool] = promote(canceled)
	return(canceled)
      ;

      define @get-inactive-flag (c : cancelable / exh : exh) : ![bool] =
	let inactive : ![bool] = SELECT(INACTIVE_OFF, c)
	let inactive : ![bool] = promote(inactive)
	return(inactive)
      ;

    (* set the cancelable as terminated *)
      define @set-inactive (c : cancelable / exh : exh) : () =
        let currentCOpt : O.option = @get-current( / exh) 
      (* set the parent as the current cancelable *)
        do @set-current(SELECT(PARENT_OFF, c) / exh)
      (* make the children globally visible *)
	let gChildren : ![List.list] = SELECT(GCHILDREN_OFF, c)
        let children : List.list = promote(SELECT(CHILDREN_OFF, c))
        do UPDATE(0, gChildren, children)
      (* mark the inactive flag. the CAS acts as a memory barrier. *)
        let inactive : ![bool] = @get-inactive-flag(c / exh)
        let x : bool = CAS(&0(inactive), false, true)
        return()
      ;


    (* set the cancelable as ready to run *)
      define @set-active (c : cancelable / exh : exh) : () =
 	do @set-current(O.SOME(c) / exh)
      (* use CAS as a memory fence *)
        let inactive : ![bool] = @get-inactive-flag(c / exh)
        let x : bool = CAS(&0(inactive), true, false) 
        return()
      ;

    (* attach the fiber k to the cancelable *)
      define @wrap (c : cancelable, k : PT.fiber / exh : exh) : PT.fiber =
      (* terminate the wrapped fiber *)
        cont terminate () = 
             do @set-inactive(c / exh)
             let _ : unit = SchedulerAction.@stop()
             return($0)
      (* run the wrapped fiber *)
        cont dispatch (wrapper : PT.sched_act, k : PT.fiber) =
             do @set-active(c / exh)
             let canceledFlg : ![bool] = @get-canceled-flag(c / exh)
             if canceledFlg
                then 
		 throw terminate()
	     else
                 do SchedulerAction.@run(wrapper, k)
                 return($0)
      (* scheduler action that polls for cancelation *)
        cont wrapper (s : PT.signal) =
             case s
	      of PT.STOP => 
		 throw terminate()
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 let _ : unit = SchedulerAction.@yield-in-atomic()
                 throw dispatch(wrapper, k)
	       | _ =>
		 let e : exn = Match
                 throw exh (e)
             end
        cont wrappedK (x : unit) =
             do SchedulerAction.@atomic-begin()
             throw dispatch(wrapper, k)
        return(wrappedK)
      ;

      define @alloc (parent : O.option / exh : exh) : cancelable =
	let canceled : ![bool] = alloc(false)
	let active : ![bool] = alloc(true)
	let gChildren : ![List.list] = alloc(nil)
	let c : cancelable = alloc(canceled, active, nil, gChildren, parent)
	return(c)
      ;

    (* create a cancelable *)
      define @new ( / exh : exh) : cancelable =
        let parent : O.option = @get-current( / exh)
        let c : cancelable = @alloc(parent / exh)
        let parent : O.option =
           case parent
	    of O.NONE => 
	       (* this cancelable is at the root of the spawn tree *)
               let dummyC : cancelable = @alloc(O.NONE / exh)
               cont k (x : unit) = return(O.NONE)
               let k : PT.fiber = @wrap(dummyC, k / exh)
               throw k(UNIT)
	     | O.SOME(parent : cancelable) => 
	       let parent : cancelable = @add-child(parent, c / exh)
               return(O.SOME(parent))
           end
      (* add this new cancelable to the parent's list of children *)
        do @set-current(parent / exh)
        return(c)

      ;

    (* cancel a cancelable. we use a synchronous semantics for this operation. the cancelable and all of its
     * children must terminate before continuing.
     *)
      define @cancel (c : cancelable / exh : exh) : () =        
      (* walk down the spawn tree, cancel all nodes and wait those nodes to become inactive *)
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
		let canceled : ![bool] = @get-canceled-flag(c / exh)
                let inactive : ![bool] = @get-inactive-flag(c / exh)
              (* the CAS operation acts as a memory barrier *)
                let _ : bool = CAS(&0(canceled), false, true) 
		if SELECT(0, inactive)
		   then 
		  (* cancel the children *)
                    let outs : L.list = PrimList.@append(#0(SELECT(GCHILDREN_OFF, c)), outs / exh)
		    apply spin(ins', outs / exh)
		else 
                  (* spin until the cancelable is inactive *)
		    apply spin(ins, outs / exh)		  
            end
        apply spin(L.CONS(c, nil), nil / exh)
      ;

    )

  end
