(* par-susp.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions.
 *)

structure ParSusp (* : PAR_SUSP*) =
  struct

#define STOLEN_OFF     0
#define IVAR_OFF       1
#define THUNK_OFF      2
#define CANCELABLE_OFF 3

    _primcode(

      typedef suspension = ![
		 bool,                                (* true iff the suspension has been stolen *)
		 ImplicitThreadIVar.ivar,             (* state *)
		 fun(unit / exh -> any),              (* thunk *)
		 Option.option                        (* cancelable *)
      ];

    (* create a suspension. the second argument is a flag to determine whether the suspension is cancelable. *)
      define @delay (f : fun(unit / exh -> any), isCancelable : bool / exh : exh) : suspension =
	  let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@empty-ivar (/ exh)
	  let cOpt : Option.option =
		     case isCancelable
		      of true =>
			 let c : Cancelation.cancelable = Cancelation.@new (UNIT / exh)
			 return (Option.SOME(c))
		       | false =>
			 return(Option.NONE)
                     end
	  let susp : suspension = alloc (false, ivar, f, cOpt)
	  let susp : suspension = promote (susp)
	  return (susp)
	;

      define @delay-w (arg : [fun(unit / exh -> any), bool] / exh : exh) : suspension =
	  @delay (#0(arg), #1(arg) / exh)
	;

      define @eval (susp : suspension / exh : exh) : any =
	  let f : fun(unit / exh -> any) = SELECT(THUNK_OFF, susp)
	  do UPDATE(THUNK_OFF, susp, (fun(unit / exh -> any)) $0)    (* prevent a space leak *)
	  let x : any = apply f (UNIT / exh)
	  return (x)
	;

    (* evaluate the suspension and seed the ivar with the result *)
      define @steal (susp : suspension / exh : exh) : () =
	  let stolen : bool = CAS(ADDR_OF(STOLEN_OFF, susp), false, true)
	  case stolen
	   of true =>
	      return ()
	    | false =>
	      let x : any = @eval (susp / exh)
	      let nilThk : fun(unit / exh -> any) = (fun(unit / exh -> any))$0
	      do UPDATE(THUNK_OFF, susp, nilThk)                  (* prevent a space leak *)
	      ImplicitThreadIVar.@put (SELECT(IVAR_OFF, susp), x / exh)
          end
	;

    (* place the suspension on the ready queue *)
      define @run (susp : suspension / exh : exh) : unit =
	  cont k (x : unit) =
	    do @steal (susp / exh)
	    SchedulerAction.@stop ()
	  let thd : ImplicitThread.thread = 
		    case SELECT(CANCELABLE_OFF, susp)
		     of Option.NONE =>
			let thd : ImplicitThread.thread = ImplicitThread.@new-thread (k / exh)
			return (thd)
		      | Option.SOME (c : Cancelation.cancelable) =>
			let thd : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread (k, c / exh)
			return (thd)
		    end
	  do ImplicitThread.@spawn-thread (thd / exh)
	  return (UNIT)
	;

    (* synchronize on completion of the suspension *)
      define @force (susp : suspension / exh : exh) : any =
	  do @steal (susp / exh)
	  ImplicitThreadIVar.@get (SELECT(IVAR_OFF, susp) / exh)
	;

    (* cancel the suspension *)
      define @cancel (susp : suspension / exh : exh) : unit =
	  case SELECT(CANCELABLE_OFF, susp)
	   of Option.NONE =>
	      (* QUESTION: should this be considered an error? *)
	      return (UNIT)
	    | Option.SOME(c : Cancelation.cancelable) =>
	      let _ : unit = Cancelation.@cancel (c / exh)
	      return (UNIT)
	  end
	;

    )

    type 'a suspension = _prim (suspension)

    val delay : ((unit -> 'a) * bool) -> 'a suspension = _prim(@delay-w)
    val run : 'a suspension -> unit = _prim(@run)
    val force : 'a suspension -> 'a = _prim(@force)
    val cancel : 'a suspension -> unit = _prim(@cancel)

  end
