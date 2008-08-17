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

structure Cancelation =
  struct

    structure PT = PrimTypes

    _primcode (

    (* communication channel for canceling fibers *)
      typedef cancelable = ![
                 PT.bool,       (* canceled flag (TRUE=canceled) *)
		 PT.bool,       (* inactive flag (TRUE=inactive) *)
		 List.list,     (* children pointers (has type cancelable List.list) *)
		 Option.option  (* parent pointer (has type cancelable Option.option) *)
              ];

      define @new (x : PT.unit / exh : PT.exh) : cancelable =
        let c : cancelable = alloc(FALSE, TRUE, NIL, NONE)
        let c : cancelable = promote(c)
        return(c)
      ;

      define inline @set-inactive (c : cancelable / exh : PT.exh) : () =
        do UPDATE(INACTIVE_OFF, c, TRUE)
        return()
      ;

      define inline @set-active (c : cancelable / exh : PT.exh) : () =
        do UPDATE(INACTIVE_OFF, c, FALSE)
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
             end

        cont wrappedK (x : PT.unit) =
             do vpstore(ATOMIC, host_vproc, TRUE)
             throw dispatch(wrapper, k)

        return(wrappedK)
      ;
    )

  end
