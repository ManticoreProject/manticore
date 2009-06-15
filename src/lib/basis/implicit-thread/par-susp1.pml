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

structure ParSusp1 : PAR_SUSP = 
  struct

    structure PT = PrimTypes
    structure C = Cancelation
    structure O = Option

    type thunk = unit -> 'a

(* state values *)
#define EMPTY_ST  $0
#define STOLEN_ST $1
#define EVAL_ST   $2

(* offsets *)
#define STATE_OFF            0
#define THUNK_OFF            1
#define CANCELABLE_OFF       2

    _primcode (

      typedef thunk = fun (unit / exh -> any);

      typedef suspension = ![
		   any,                    (* a state word, with one of the following values *)
		   thunk,                  (* a thunk word *)
		   O.option                (* cancelable *)
               ];

    (* the state work contains one of the following values:
     *          EMPTY_ST
     *          STOLEN_ST
     *          EVAL_ST
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
	  let susp : suspension = alloc(EMPTY_ST, f, cOpt)
	  let susp : suspension = promote(susp)
	  return(susp)
	;

      define @delay-w (arg : [fun(unit / exh -> any), bool] / exh : exh) : suspension =
	  @delay(#0(arg), #1(arg) / exh)
	;

    (* evaluate a suspension 
     * PRECONDITION: this function has unique access to write to the suspension
     *)
      define @eval (susp : suspension / exh : exh) : any =
	  let f : thunk = SELECT(THUNK_OFF, susp)
	 (* clear the thunk pointer to avoid a space leak *)
	  do UPDATE(THUNK_OFF, susp, (thunk) $0)
	  let result : any = apply f (UNIT / exh)
	  return(result)
	;

    (* steal a suspension, evaluate it, and store the result *)
      define @steal (susp : suspension / exh : exh) : () =
	  let tmp : any = CAS (&STATE_OFF(susp), EMPTY_ST, STOLEN_ST)
	  if Equal (tmp, EMPTY_ST) 
	     then let result : any = @eval(susp / exh)
		  let result : any = promote(result)
		  let tmpX : any = CAS(&STATE_OFF(susp), STOLEN_ST, result)
		  if Equal (tmpX, STOLEN_ST)
		     then return ()
		     else (* unblock the suspension *)
			  do UPDATE(STATE_OFF, susp, result)
			  let blockedSusp : ImplicitThread.thread = (ImplicitThread.thread)tmpX
			  do ImplicitThread.@spawn-thread (blockedSusp / exh)
			  return()
	      else (* suspension cell is already full *)
		   return ()
	;

    (* PRECONDITION: at most one thread may apply this operation to a given suspension *)
      define @force (susp : suspension / exh : exh) : any =
	  let tmp : any = CAS (&0(susp), EMPTY_ST, EVAL_ST)
	  if Equal (tmp, EMPTY_ST)
	     then (* the suspension is ready for evaluation *)
	       let result : any = @eval(susp / exh)
	       let result : any = promote (result)
	       do UPDATE(STATE_OFF, susp, result)
	       return (result)
	  else if Equal (tmp, STOLEN_ST)
	     then (* another implicit thread is evaluating the suspension; we need to block *)
		  cont kBlk (_ : unit) = 
		       return (SELECT(STATE_OFF, susp))
		  let thdBlk : ImplicitThread.thread = ImplicitThread.@capture(kBlk / exh)
		  let thdBlk : ImplicitThread.thread = promote (thdBlk)
		  let tmpX : any = CAS (&STATE_OFF(susp), STOLEN_ST, thdBlk)
		  if Equal (tmpX, STOLEN_ST)
		     then SchedulerAction.@stop ()
		    else (* the suspension value is ready *)
			 return (tmpX)
	  else (* the suspension value is ready *)	       
	      return (tmp)
	;

    (* place the suspension on the ready queue *)
      define @run (susp : suspension / exh : exh) : unit =
	  cont k (x : unit) =
	    do @steal(susp / exh)
	    SchedulerAction.@stop()
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
	  return(UNIT)
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

  end (* ParSusp1 *)
