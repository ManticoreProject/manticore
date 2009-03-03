(* par-susp.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions.
 *)

structure ParSusp : PAR_SUSP =
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
	let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@empty-ivar(/ exh)
        let cOpt : Option.option =
		   if isCancelable
		      then 
		       let c : Cancelation.cancelable = Cancelation.@new(/ exh)
		       return(Option.SOME(c))
		   else 
		       return(Option.NONE)
        let susp : suspension = alloc(false, ivar, f, cOpt)
	let susp : suspension = promote(susp)
	return(susp)
      ;

      define @delay-w (arg : [fun(unit / exh -> any), bool] / exh : exh) : suspension =
	@delay(#0(arg), #1(arg) / exh)
      ;

    (* evaluate the suspension without synchronization *)
      define @eval (susp : suspension / exh : exh) : any =
	let f : fun(unit / exh -> any) = SELECT(THUNK_OFF, susp)
       (* clear the thunk pointer to avoid a space leak *)
	do UPDATE(THUNK_OFF, susp, (fun(unit / exh -> any)) $0)
	let x : any = apply f (UNIT / exh)
	return(x)
      ;

    (* evaluate the suspension and seed the ivar with the result *)
      define @steal (susp : suspension / exh : exh) : () =
        let stolen : bool = CAS(ADDR_OF(STOLEN_OFF, susp), false, true)
	if stolen
	   then
	    return()
	else
	    let x : any = @eval(susp / exh)
            let nilThk : fun(unit / exh -> any) = (fun(unit / exh -> any))$0
	    do UPDATE(THUNK_OFF, susp, nilThk)                  (* prevent a space leak *)
	    ImplicitThreadIVar.@put(SELECT(IVAR_OFF, susp), x / exh)
      ;

    (* place the suspension on the ready queue *)
      define @run (susp : suspension / exh : exh) : unit =
        cont k (x : unit) =
          do @steal(susp / exh)
          SchedulerAction.@stop()
        let thd : ImplicitThread.thread = ImplicitThread.@thread(k, SELECT(CANCELABLE_OFF, susp) / exh)
	do ImplicitThread.@spawn(thd / exh)
	return(UNIT)
      ;

    (* synchronize on completion of the suspension *)
      define @force (susp : suspension / exh : exh) : any =
	do @steal(susp / exh)
	ImplicitThreadIVar.@get(SELECT(IVAR_OFF, susp) / exh)
      ;

    (* cancel the suspension *)
      define @cancel (susp : suspension / exh : exh) : unit =
        case SELECT(CANCELABLE_OFF, susp)
	 of Option.NONE =>
	    (* QUESTION: is this an error? *)
	    return(UNIT)
	  | Option.SOME(c : Cancelation.cancelable) =>
	    do Cancelation.@cancel(c / exh)
            return(UNIT)
        end
      ;

    )

    type 'a suspension = _prim(suspension)

    val delay : ((unit -> 'a) * bool) -> 'a suspension = _prim(@delay-w)
    val run : 'a suspension -> unit = _prim(@run)
    val force : 'a suspension -> 'a = _prim(@force)
    val cancel : 'a suspension -> unit = _prim(@cancel)

  end
