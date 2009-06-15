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

    (* cancel the fiber associated with c. this operation blocks the calling fiber
     * until the target fiber and all of its children have terminated.
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
        do @set-current(SELECT(PARENT_OFF, c) / exh)
        let children : List.list = promote(SELECT(CHILDREN_OFF, c))
	let gChildren : ![List.list] = promote(SELECT(GCHILDREN_OFF, c))
        do #0(gChildren) := children
        let inactive : ![vproc] = @get-inactive-flag(c)
      (* FIXME: an atomic write would suffice. *)
      (* do AtomicWrite(inactive, INACTIVE) *)
        let x : vproc = CAS(&0(inactive), #0(inactive), INACTIVE)
        return()
      ;

    (* set the cancelable as ready to run *)
      define @set-active-from-atomic (self : vproc, c : cancelable / exh : exh) : () =
 	do @set-current(O.SOME(c) / exh)
        let inactive : ![vproc] = @get-inactive-flag(c)
      (* FIXME: an atomic write would suffice *)
      (* do AtomicWrite(inactive, self) *)
        let x : vproc = CAS(&0(inactive), #0(inactive), self) 
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

        cont dispatch (self : vproc, handler : PT.sched_act, k : PT.fiber) =
             do @set-active-from-atomic(self, c / exh)
             let canceledFlg : ![bool] = @get-canceled-flag(c)
             if #0(canceledFlg)
                then 
		 throw terminate(self)
	     else
                 do SchedulerAction.@run(self, handler, k)
                 throw impossible()

      (* poll for cancelation *)
        cont handler (s : PT.signal) =
             let self : vproc = host_vproc
             case s
	      of PT.STOP => 
		 throw terminate(self)
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 do SchedulerAction.@yield-in-atomic(self)
                 throw dispatch(self, handler, k)
	       | _ =>
		 let e : exn = Match
                 throw exh (e)
             end

        cont wrappedK (x : unit) =
             let self : vproc = SchedulerAction.@atomic-begin()
             throw dispatch(self, handler, k)

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

    (* cancel the fiber associated with c. this operation blocks the calling fiber
     * until the target fiber and all of its children have terminated.
     *)
      define @cancel (c : cancelable / exh : exh) : () =        
        let self : vproc = SchedulerAction.@atomic-begin()
	fun cancelAndWait (cs1 : L.list, cs2 : L.list) : () =
	    case cs1
	     of nil => 
		case cs2
		 of nil => 
		    return()
		  | _ => 
		    let cs2 : L.list = PrimList.@rev(cs2 / exh)
		    apply cancelAndWait(cs2, nil)
		end
	      | L.CONS(c : cancelable, cs1 : L.list) =>		
		let canceled : ![bool] = @get-canceled-flag(c)
		let inactive : ![vproc] = @get-inactive-flag(c)
		let isCanceled : bool = CAS(&0(canceled), false, true)
		if Equal(#0(inactive), INACTIVE)
		then
		  (* TRICKY! The up-to-date version of gChildren exists in the global heap, which
		   * is why we need the promotion below.
		   *)
                    let gChildren : ![L.list] = promote(SELECT(GCHILDREN_OFF, c))
		    let cs2 : L.list = PrimList.@append(#0(gChildren), cs2 / exh)
		    apply cancelAndWait(cs1, cs2)
		else
		    do if isCanceled
		       then
			   do Pause()
		           do SchedulerAction.@yield-in-atomic(self)
			   return()
		       else 
			   let dummyK : PT.fiber = vpload(VP_DUMMYK, self)
			   do VProc.@send-high-priority-signal-from-atomic(self, #0(inactive), dummyK)
			   return()
		     apply cancelAndWait(cs1, L.CONS(c, cs2))
	    end
	do apply cancelAndWait(L.CONS(c, L.nil), L.nil)
	do SchedulerAction.@atomic-end(self)
	return()
      ;

    )

  end
