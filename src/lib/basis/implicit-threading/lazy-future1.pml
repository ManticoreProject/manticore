(* par-susp1.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions with restricted communication.
 * 
 * Only a single thread may force the suspension. Behavior is undefined when multiple threads try to force
 * the suspension concurrently.
 *)

(* state values *)
#define EMPTY_F  $0
#define STOLEN_F $1
#define EVAL_F   $2

(* offsets *)
#define STATE_OFF            0
#define THUNK_OFF            1
#define CANCELABLE_OFF       2

structure ParSusp1 : PAR_SUSP = 
  struct

    structure PT = PrimTypes
    structure C = Cancelation
    structure O = Option

    type thunk = unit -> 'a

    _primcode (

      typedef thunk = fun (unit / exh -> any);

      typedef suspension = ![
		   any,                    (* a state word, with one of the following values *)
		   thunk,                  (* a thunk word *)
		   O.option                (* cancelable *)
               ];

    (* the state work contains one of the following values:
     *          EMPTY_F
     *          STOLEN_F
     *          EVAL_F
     *          FULL      value
     *          WAITING   cont
     *)

    (* create a suspension. the second argument is a flag to determine whether the suspension is cancelable. *)
      define @delay (f : fun(unit / exh -> any), isCancelable : bool / exh : exh) : suspension =
        let cOpt : Option.option =
		   if isCancelable
		      then 
		       let c : Cancelation.cancelable = Cancelation.@new(/ exh)
		       return(Option.SOME(c))
		   else 
		       return(Option.NONE)
        let fut : suspension = alloc(EMPTY_F, thunk, cOpt)
	let fut : suspension = promote(fut)
	return(fut)
      ;

    (* evaluate a suspension 
     * PRECONDITION: this function has unique access to write to the suspension
     *)
      define @eval (fut : suspension / exh : exh) : any =
	let f : thunk = SELECT(THUNK_OFF, fut)
       (* clear the thunk pointer to avoid a space leak *)
	do UPDATE(THUNK_OFF, fut, (thunk) $0)
	let result : any = apply f (UNIT / exh)
	return(result)
      ;

    (* steal a suspension, evaluate it, and store the result *)
      define @steal (fut : suspension / exh : exh) : () =
	let tmp : any = CAS (&STATE_OFF(fut), EMPTY_F, STOLEN_F)
	if Equal (tmp, EMPTY_F) 
	   then let result : any = @eval(fut / exh)
                let result : any = promote(result)
		let tmpX : any = CAS(&STATE_OFF(fut), STOLEN_F, result)
		if Equal (tmpX, STOLEN_F)
		   then return ()
		   else (* unblock the suspension *)
			do UPDATE(STATE_OFF, fut, result)
                        let blockedFut : ImplicitThread.thread = (ImplicitThread.thread)tmpX
                        do ImplicitThread.@spawn(blockedFut / exh)
                        return()
	    else (* suspension cell is already full *)
		 return ()
      ;

    (* PRECONDITION: at most one thread may apply this operation to a given suspension *)
      define @force (fut : suspension / exh : exh) : any =
        let tmp : any = CAS (&0(fut), EMPTY_F, EVAL_F)
        if Equal (tmp, EMPTY_F)
           then (* the suspension is ready for evaluation *)
             let result : any = @eval(fut / exh)
             let result : any = promote (result)
             do UPDATE(STATE_OFF, fut, result)
             return (result)
	else if Equal (tmp, STOLEN_F)
           then (* another implicit thread is evaluating the suspension; we need to block *)
                cont kBlk (_ : unit) = 
		     return (SELECT(STATE_OFF, fut))
                let thdBlk : ImplicitThread.thread = ImplicitThread.@capture(kBlk / exh)
                let thdBlk : ImplicitThread.thread = promote (thdBlk)
   	        let tmpX : any = CAS (&STATE_OFF(fut), STOLEN_F, thdBlk)
 	        if Equal (tmpX, STOLEN_F)
	           then SchedulerAction.@stop ()
	          else (* the suspension value is ready *)
                       return (tmpX)
        else (* the suspension value is ready *)	       
            return (tmp)
      ;

    (* place the suspension on the ready queue *)
      define @run (fut : suspension / exh : exh) : unit =
        cont k (x : unit) =
          do @steal(fut / exh)
          SchedulerAction.@stop()
        let thd : ImplicitThread.thread = ImplicitThread.@thread(k, SELECT(CANCELABLE_OFF, fut) / exh)
	do ImplicitThread.@spawn(thd / exh)
	return(UNIT)
      ;

    (* cancel the suspension *)
      define @cancel (fut : suspension / exh : exh) : unit =
        case SELECT(CANCELABLE_OFF, fut)
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

  end (* ParSusp1 *)
