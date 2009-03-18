(* cancelation.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A mechanism for canceling implicit threads.
 *)

structure Cancelation (* : sig

    _prim(

      typedef cancelable;

    (* create a cancelable. 
     * NOTE: this operation modifies the ITE.
     *)
      define @new ( / exh : exh) : cancelable;

    (* wrap a fiber with a cancelable. the fiber is now cancelable by applying @cancel(c / exh) *)
      define @wrap (c : cancelable, k : PT.fiber / exh : exh) : PT.fiber;

    (* cancel a cancelable. 
     * NOTE: this operation modifies the signal mask.
     *)
      define @cancel (c : cancelable / exh : exh) : ();

    )

  end *) = struct

    structure PT = PrimTypes
    structure O = Option
    structure L = List

    _primcode (

(* QUESTION: is it safe to store a state value in the inactive flag? *)
#define INACTIVE        $0

(* offsets *)
#define CANCELED_OFF    0
#define INACTIVE_OFF    1
#define CHILDREN_OFF    2
#define GCHILDREN_OFF   3
#define PARENT_OFF      4

     (* communication channel for canceling fibers *)
       typedef cancelable =
                 [ ![bool],            (* canceled flag *)
                   ![vproc],           (* inactive flag (NIL when inactive but is otherwise set to
					* the host vproc) 
					*)
                   L.list,             (* child pointers *)
		   ![L.list],          (* globally-visible child pointers 
					* INVARIANT: must be equal to locally-visible child pointers 
					* whenever inactive=true.
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
        do FLS.@set-ite(ite / exh)
        return()
      ;

      define inline @get-canceled-flag (c : cancelable) : ![bool] =
	let canceled : ![bool] = SELECT(CANCELED_OFF, c)
	let canceled : ![bool] = promote(canceled)
	return(canceled)
      ;

      define @get-inactive-flag (c : cancelable) : ![vproc] =
	let inactive : ![vproc] = SELECT(INACTIVE_OFF, c)
	let inactive : ![vproc] = promote(inactive)
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
        do #0(gChildren) := children
        let inactive : ![vproc] = @get-inactive-flag(c)
      (* FIXME: the CAS is unnecessary, since all we need here is to flush the write buffer *)
        let x : vproc = CAS(&0(inactive), SELECT(INACTIVE_OFF, c), INACTIVE)
        return()
      ;

    (* set the cancelable as ready to run *)
      define @set-active (self : vproc, c : cancelable / exh : exh) : () =
 	do @set-current(O.SOME(c) / exh)
        let inactive : ![vproc] = @get-inactive-flag(c)
      (* FIXME: the CAS is unnecessary, since all we need here is to flush the write buffer *)
        let x : vproc = CAS(&0(inactive), SELECT(INACTIVE_OFF, c), self) 
        do assert(Equal(x, INACTIVE))
        return()
      ;

    (* attach the fiber k to the cancelable *)
      define @wrap (c : cancelable, k : PT.fiber / exh : exh) : PT.fiber =

        cont impossible () = 
             let e : exn = Fail(@"Cancelation.@wrap: impossible")
             throw exh(e)

        cont terminate (self : vproc) = 
             do @set-inactive(c / exh)
             do SchedulerAction.@stop-from-atomic(self)
             throw impossible()

        cont dispatch (self : vproc, wrapper : PT.sched_act, k : PT.fiber) =
             do @set-active(self, c / exh)
             let canceledFlg : ![bool] = @get-canceled-flag(c)
             if SELECT(0, canceledFlg)
                then 
		 throw terminate(self)
	     else
                 do SchedulerAction.@run(self, wrapper, k)
                 throw impossible()

      (* poll for cancelation *)
        cont wrapper (s : PT.signal) =
             let self : vproc = host_vproc
             case s
	      of PT.STOP => 
		 throw terminate(self)
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 do SchedulerAction.@yield-in-atomic(self)
                 throw dispatch(self, wrapper, k)
	       | _ =>
		 let e : exn = Match
                 throw exh (e)
             end

        cont wrappedK (x : unit) =
             let self : vproc = SchedulerAction.@atomic-begin()
             throw dispatch(self, wrapper, k)
        return(wrappedK)
      ;

      define @alloc (parent : O.option / exh : exh) : cancelable =
	let canceled : ![bool] = alloc(false)
	let inactive : ![vproc] = alloc(INACTIVE)
	let gChildren : ![List.list] = alloc(nil)
	let c : cancelable = alloc(canceled, inactive, nil, gChildren, parent)
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

        let self : vproc = SchedulerAction.@atomic-begin()

      (* cancel the thread and wait for it and all of its children to terminate *)
        fun cancelAndWaitToTerm (ins : L.list, outs : L.list) : () =
	    case ins
	     of nil => 
		case outs
		 of nil => 
		  (* all cancelables have terminated *)
		    return()
		  | L.CONS (x : cancelable, xs : L.list) =>
		  (* some cancelables have not terminated after the first check, so start over *)
		    do Pause()
		    let ins : L.list = PrimList.@rev(outs / exh)
                    apply cancelAndWaitToTerm(ins, nil)
                end
	      | L.CONS(c : cancelable, ins' : L.list) =>
		let canceled : ![bool] = @get-canceled-flag(c)
                let inactive : ![vproc] = @get-inactive-flag(c)
                do if #0(canceled)
		      then return()
		   else
		     (* Because our memory model is not sequentially consistent, we rely on the CAS operation
		      * below to prevent a race condition. The race condition occurs when we reach the
		      * conditional below, but the other thread has not yet seen the updated canceled
		      * flag. Thus, the thread may wake up and continue evaluating before it sees that
		      * it has been canceled.
		      *
		      * NOTE: the CAS operation is unnecessarily heavyweight. It would suffice to use any
		      * instruction that is guaranteed to flush the processor's write buffer, but no
		      * such instruction is provided by the compiler.
		      *)
		       let _ : bool = CAS(&0(canceled), false, true) 
                       return()
              (* at this point, every processor must see that the thread is canceled *)
                let vp : vproc = #0(inactive)
		if Equal(vp, INACTIVE)
		   then 
		  (* the thread cannot run again, so it is safe to access the thread's children. *)
                    let outs : L.list = PrimList.@append(#0(SELECT(GCHILDREN_OFF, c)), outs / exh)
		    apply cancelAndWaitToTerm(ins', outs)
		else 
                    do assert(NotEqual(vp, self))
		    let dummyK : PT.fiber = vpload(VP_DUMMYK, self)
		    do VProc.@send-high-priority-signal-from-atomic(self, vp, dummyK)
                  (* tack the cancelable to the end of the waiting list, and try the next thread *)
		    apply cancelAndWaitToTerm(ins', L.CONS(c, outs))
            end

        do apply cancelAndWaitToTerm(L.CONS(c, nil), nil)

        do SchedulerAction.@atomic-end(self)
        return()
      ;

    )

  end
