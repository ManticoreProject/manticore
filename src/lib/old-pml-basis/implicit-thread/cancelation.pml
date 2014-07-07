(* cancelation.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Cancelation (* : sig

    _prim(

      typedef cancelable;
      typedef thunk = fun(unit / exh -> any);
      typedef fiber = PrimTypes.fiber;

      define @new (_ : unit / exh : exh) : cancelable;
      define @wrap-fiber (c : cancelable, k : fiber / exh : exh) : fiber;
      define @wrap-thunk (c : cancelable, t : thunk / exh : exh) : thunk;
      define @cancel (c : cancelable / exh : exh) : ();

    )

    type cancelable
    val new : unit -> cancelable
    val wrapThunk : cancelable * (unit -> 'a) -> (unit -> 'a)
    val cancel : cancelable -> unit

  end *) = struct

    structure PT = PrimTypes
    structure O = Option
    structure L = List

    _primcode (

(* QUESTION: is it safe to store a state value in the inactive flag? *)
#define INACTIVE        nullVP

#define CANCELED_OFF    0
#define INACTIVE_OFF    1
#define CHILDREN_OFF    2
#define GCHILDREN_OFF   3
#define PARENT_OFF      4

       typedef thunk = fun(unit / exh -> any);
       typedef fiber = PT.fiber;
       typedef cancelable =
                 [ ![bool],            (* true, if canceled *)
                   ![vproc],           (* nil, if inactive
					* vp, if corresponding implicit thread is executing on vproc vp
					*)
                   L.list,             (* local list of child pointers *)
		   ![L.list],          (* global list of child pointers *)
	           O.option            (* pointer to parent cancelable *)
                 ];

    )

    local

    _primcode (

    (* @alloc (parent / exh) *)
    (* allocates a cancelable object c where parent is the parent of c *)
      define @alloc (parent : O.option / exh : exh) : cancelable =
	  let canceled : ![bool] = alloc(false)
	  let inactive : ![vproc] = alloc(INACTIVE)
	  let gChildren : ![List.list] = alloc(nil)
	  return(alloc(canceled, inactive, nil, gChildren, parent))
	;

    (* @add-child (c, parent / exh) *)
    (* returns copy of parent where c is present in the list of children of parent *)
      define @add-child (c : cancelable, parent : cancelable / exh : exh) : cancelable =
	  let children : L.list = CONS(c, SELECT(CHILDREN_OFF, parent))
	  let parent : cancelable = alloc(SELECT(CANCELED_OFF, c), 
			   	           SELECT(INACTIVE_OFF, c), 
					   children, 
					   SELECT(GCHILDREN_OFF, c), 
					   SELECT(PARENT_OFF,c))
	  return(parent)
	;

    (* @get-current ( / exh) *)
    (* returns SOME c where c is the current cancelable and NONE if there is no current cancelable *)
      define @get-current ( / exh : exh) : O.option =
	  let ite : FLS.ite = FLS.@get-ite(/ exh)
	  return(#1(ite))
	;

    (* @set-current (cOpt / exh) *)
    (* sets the current cancelable to cOpt *)
      define @set-current (cOpt : O.option / exh : exh) : () =
	  let ite : FLS.ite = FLS.@get-ite(/ exh)
	  let ite : FLS.ite = alloc(#0(ite), cOpt)
	  do FLS.@set-ite(ite / exh)
	  return()
	;

    (* @get-canceled-flag (c) *)
    (* returns the canceled flag of c *)
      define inline @get-canceled-flag (c : cancelable) : ![bool] =
	  let canceled : ![bool] = SELECT(CANCELED_OFF, c)
	  let canceled : ![bool] = promote(canceled)
	  return(canceled)
	;

    (* @get-inactive-flag (c) *)
    (* returns the inactive flag of c *)
      define @get-inactive-flag (c : cancelable) : ![vproc] =
	  let inactive : ![vproc] = SELECT(INACTIVE_OFF, c)
	  let inactive : ![vproc] = promote(inactive)
	  return(inactive)
	;

    (* @set-inactive (c / exh) *)
    (* puts c in the inactive state *)
    (* postcondition: c appears in inactive state to all processors (i.e., pending *)
    (* writes to c have been flushed to main memory) *)
      define @set-inactive (c : cancelable / exh : exh) : () =
	  do @set-current(SELECT(PARENT_OFF, c) / exh)
        (* move local children list to global children list *)
	  let children : List.list = promote(SELECT(CHILDREN_OFF, c))
	  let gChildren : ![List.list] = promote(SELECT(GCHILDREN_OFF, c))
	  do #0(gChildren) := children
	  let inactive : ![vproc] = @get-inactive-flag(c)
	(* FIXME: an atomic write would suffice. *)
	(* do AtomicWrite(inactive, INACTIVE) *)
	  let x : vproc = CAS(&0(inactive), #0(inactive), INACTIVE)
	  return()
	;

    (* @set-active-in-atomic (vp, c) *)
    (* puts c in the active state where implicit thread associated with c is executing on vproc vp *)
      define @set-active-in-atomic (vp : vproc, c : cancelable / exh : exh) : () =
	  do @set-current(O.SOME(c) / exh)
	  let inactive : ![vproc] = @get-inactive-flag(c)
	(* FIXME: an atomic write would suffice *)
	(* do AtomicWrite(inactive, vp) *)
	  let x : vproc = CAS(&0(inactive), #0(inactive), vp) 
	  return()
	;

    )

    in

    _primcode (

    (* @wrap-fiber (c, k / exh) *)
    (* returns new fiber k' where k' has similar behavior to k, except that canceling c *)
    (* causes k' to terminate *)
      define @wrap-fiber (c : cancelable, k : fiber / exh : exh) : fiber =
	  cont impossible () = throw exh(Fail(@"Cancelation.@wrap-fiber: impossible"))
	  cont terminate () = 
	       do @set-inactive(c / exh)
	       let _ : unit = SchedulerAction.@stop()
	       throw impossible()
	  cont dispatch (act : PT.sched_act, k : fiber) =
               let self : vproc = SchedulerAction.@atomic-begin()
	       do @set-active-in-atomic(self, c / exh)
	       let canceledFlg : ![bool] = @get-canceled-flag(c)
	       case #0(canceledFlg)
		of true =>
		   throw terminate()
		 | false =>
		   do SchedulerAction.@run(self, act, k)
		   throw impossible()
               end
	  cont act (s : PT.signal) =
	       case s
		of PT.STOP => 
		   throw terminate()
		 | PT.PREEMPT(k : fiber) =>
		   do @set-inactive(c / exh)
		   do SchedulerAction.@yield()
		   throw dispatch(act, k)
		 | _ =>
		   throw exh (Match)
	       end
	  cont wrappedK (x : unit) = throw dispatch(act, k)
	  return(wrappedK)
	;

    (* @wrap-thunk (c, t / exh) *)
    (* returns new thunk t' where t' has similar behavior to t, except that canceling c causes *)
    (* t' to terminate *)
      define @wrap-thunk (c : cancelable, t : thunk / exh : exh) : thunk =
          cont k (_ : unit) =
            let x : any = apply t (UNIT / exh)
            return (x)
          let kw : fiber = @wrap-fiber (c, k / exh)
          fun tw (_ : unit / exh : exh) : any = throw kw (UNIT)
          return (tw)
        ;

      define @wrap-thunk-w (arg : [cancelable, thunk] / exh : exh) : thunk =
          @wrap-thunk (#0(arg), #1(arg) / exh)
        ;

    (* @new (_ / exh) *)
    (* create a cancelable *)
      define @new (_ : unit / exh : exh) : cancelable =
	  let parent : O.option = @get-current( / exh)
	  let c : cancelable = @alloc(parent / exh)
	  let parent : O.option =
	     case parent
	      of O.NONE => 
		 (* this cancelable is at the root of the spawn tree *)
		 let dummyC : cancelable = @alloc(O.NONE / exh)
		 cont k (x : unit) = return(O.NONE)
		 let k : fiber = @wrap-fiber(dummyC, k / exh)
		 throw k(UNIT)
	       | O.SOME(parent : cancelable) => 
		 let parent : cancelable = @add-child(parent, c / exh)
		 return(O.SOME(parent))
	     end
	  do @set-current(parent / exh)
	  return(c)
	;

    (* @cancel (c / exh) *)
    (* cancels implicit thread t corresponding to c and all implicit threads that are descendants of t *)
    (* postcondition: all canceled implicit threads have stopped executing *)
      define @cancel (c : cancelable / exh : exh) : unit =        
        (* cs1: cancelables waiting to be canceled and polled for termination *)
        (* cs2: cancelables that have recently been canceled but have not yet terminated *)
	  fun cancelAll (self : vproc, cs1 : L.list, cs2 : L.list) : () =
	      case cs1
	       of nil => 
		  case cs2
		   of nil => 
		      return()
		    | _ => 
		      let cs2 : L.list = PrimList.@rev(cs2 / exh)
		      apply cancelAll(self, cs2, nil)
		  end
		| CONS(c : cancelable, cs1 : L.list) =>		
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
		      apply cancelAll(self, cs1, cs2)
		  else
		      case isCanceled
		       of true =>
			  do Pause()
			  apply cancelAll(self, cs1, CONS(c, cs2))
			| false =>
			  let dummyK : fiber = vpload(VP_DUMMYK, self)
	                  let fls : FLS.fls = FLS.@get()
			  do VProc.@send-in-atomic(self, #0(inactive), fls, dummyK)
			  apply cancelAll(self, cs1, CONS(c, cs2))
                      end
		       
	      end
	  let self : vproc = SchedulerAction.@atomic-begin()
	  do apply cancelAll(self, CONS(c, L.nil), L.nil)
	  do SchedulerAction.@atomic-end(self)
	  return(UNIT)
	;

    )

    type cancelable = _prim(cancelable)
    val new : unit -> cancelable = _prim(@new)
    val wrapThunk : cancelable * (unit -> 'a) -> (unit -> 'a) = _prim(@wrap-thunk-w)
    val cancel : cancelable -> unit = _prim(@cancel)

    end (* local *)

  end

