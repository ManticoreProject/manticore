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
  (* communication channel for canceling fibers *)
    type cancelable = _prim (
		  ![
		     PT.bool,         (* canceled flag (TRUE=canceled) *)
		     PT.bool,         (* inactive flag (TRUE=inactive) *)
		     List.list,       (* children pointers (has type cancelable List.list) *)
		     Option.option,   (* parent pointer (has type cancelable Option.option) *)
		     FLS.fls_tag      (* tag to find the current cancelable *)
		  ] )

    _primcode (

    (* create a cancelable *)
      define @new (cancelableTag : FLS.fls_tag / exh : PT.exh) : cancelable =
      (* the current cancelable (might be nonexistant) *)
        let fls : FLS.fls = FLS.@get(/ exh)
        let currentCOpt : Option.option = FLS.@find(fls, cancelableTag / exh)
        let currentC : ![cancelable] = 
		       case currentCOpt
			of NONE => let c : ![cancelable] = (![cancelable])enum(0)
                                   return(c)
			 | SOME (c : ![cancelable]) => return(c)
                       end
      (* if the current cancelable exists, set it as our parent *)
        let parent : Option.option = 
		     if Equal(currentC, enum(0))
		        then return(NONE)
		     else
			 return(Option.SOME(#0(currentC)))

        let c : cancelable = alloc(FALSE, TRUE, NIL, parent, cancelableTag)
        let c : cancelable = promote(c)

        do if Equal(currentC, enum(0))
	      then return()
	   else
 	       (* update the parent's childrent list *)
	       let children : List.list = SELECT(CHILDREN_OFF, #0(currentC))
               let children : List.list = List.CONS(c, children)
	       let children : List.list = promote(children)
	       do UPDATE(CHILDREN_OFF, #0(currentC), children)
               return ()

        return(c)
      ;


    (* set the cancelable as terminated *)
      define @set-inactive (c : cancelable / exh : PT.exh) : () =
      (* make the parent the current cancelable *)
        let fls : FLS.fls = FLS.@get(/ exh)
        let currentCOpt : Option.option = FLS.@find(fls, SELECT(TAG_OFF, c) / exh)
        do case currentCOpt
	    of NONE => return()
	     | Option.SOME(currentC : ![cancelable]) =>
               let parentOpt : Option.option = SELECT(PARENT_OFF, c)
               case parentOpt
		of NONE => return()
		 | SOME (parent : cancelable) => 
		   do UPDATE(0, currentC, parent)
                   return()
               end
           end
      (* mark the inactive flag; use CAS as a memory fence *)
        let x : PT.bool = CAS(ADDR_OF(INACTIVE_OFF,c), FALSE, TRUE)
        return()
      ;

    (* set the cancelable as ready to run *)
      define @set-active (c : cancelable / exh : PT.exh) : () =
      (* set the current cancelable *)
        let fls : FLS.fls = FLS.@get(/ exh)
        let currentCOpt : Option.option = FLS.@find(fls, SELECT(TAG_OFF, c) / exh)
        do case currentCOpt
	    of NONE => return()
	     | Option.SOME(currentC : ![cancelable]) => 
	       do UPDATE(0, currentC, c)
               return()
           end
      (* use CAS as a memory fence *)
        let x : PT.bool = CAS(ADDR_OF(INACTIVE_OFF,c), TRUE, FALSE) 
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
             if SELECT(CANCELED_OFF,c)
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
	      of STOP => 
		 throw terminate()
	       | PT.PREEMPT(k : PT.fiber) =>
		 do @set-inactive(c / exh)
                 let _ : PT.unit = Control.@atomic-yield(/ exh)
                 throw dispatch(wrapper, k)
	       | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, x : any) =>
		 cont retK' (x : PT.unit) = 
		   throw dispatch(wrapper, retK)
		 do Control.@forward(PT.UNBLOCK(retK', k, x) / exh)
                 return($0)
             end

        cont wrappedK (x : PT.unit) =
             do vpstore(ATOMIC, host_vproc, TRUE)
             throw dispatch(wrapper, k)

        return(wrappedK)
      ;

    (* wait for all the cancelables to terminate *)
      define @wait-for-all (checks : List.list / exh : PT.exh) : () =
      (* spin until both checks and busy are finished *)
	fun lp (checks : List.list, busy : List.list / exh : PT.exh) : () =
	    case checks
	     of NIL =>
		case busy 
		 of NIL => return()
		  | _ => apply lp (busy, NIL / exh)
		end
	      | List.CONS (c : cancelable, checks : List.list) =>
		if SELECT(INACTIVE_OFF, c)
		   then apply lp (checks, busy / exh)
		else apply lp (checks, List.CONS(c, busy) / exh)
	    end
	apply lp(checks, NIL / exh)
      ;

    (* wait for a cancelable to terminate *)
      define @wait (c : cancelable / exh : PT.exh) : () =
	fun lp () : () =
	    if SELECT(INACTIVE_OFF, c)
	       then return()
	    else apply lp ()
	apply lp()
      ;

    (* cancel a cancelable. we use a synchronous semantics for this operation. the cancelable and all of its
     * children must terminate before continuing.
     *)
      define @cancel (c : cancelable / exh : PT.exh) : () =
      (* set as canceled, wait for termination, and do the same for all the children *)
	fun cancel (c : cancelable / exh : PT.exh) : () =
	    fun cancelChildren (children : List.list / exh : PT.exh) : () =
		case children
		 of NIL => return ()
		  | List.CONS (c : cancelable, children : List.list) =>
		    do UPDATE(CANCELED_OFF, c, TRUE)
		    apply cancelChildren (children / exh)
		end
	    do UPDATE(CANCELED_OFF, c, TRUE)
	    apply cancelChildren (SELECT(CHILDREN_OFF, c) / exh)
	do apply cancel(c / exh)
	@wait(c / exh)
      ;

    )

  end
