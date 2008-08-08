(* future1.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Runtime support for futures where at most one fiber can perform a touch operation.
 *)

(* state values *)
#define EMPTY_F  $0
#define STOLEN_F $1
#define EVAL_F   $2

(* offsets *)
#define STATE_OFF            0
#define THUNK_OFF            1
#define CANCELABLE_OFF       2
#define FGS_OFF              3

structure Future1 =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure C = Cancelation
    structure LQ = LockedQueue

    type thunk = unit -> unit
    _primcode ( typedef thunk = fun (PT.unit / PT.exh -> any); )

    (* the future1 structure contains:
     *     1) a _state_ word, with one of the following values:
     *          EMPTY_F
     *          STOLEN_F
     *          EVAL_F
     *          FULL      value
     *          WAITING   cont
     *     2) a _thunk_ word 
     *     3) a cancel cell for cancelling the future's evaluation
     *     4) fiber-local storage for the future (tracks parent->child relationships)
     *)

    type future = _prim ( ![any, thunk, C.cancelable, FLS.fls] )

    _primcode (

      define @eval (fut : future / exh : PT.exh) : any =
        let f : thunk = SELECT(THUNK_OFF, fut)
       (* clear the thunk pointer to avoid a space leak *)
        do UPDATE(THUNK_OFF, fut, (thunk) $0)
        let result : any = apply f (UNIT / exh)
        return(result)
      ;

      define @touch (fut : future / exh : PT.exh) : any =
        let tmp : any = CAS (&0(fut), EMPTY_F, EVAL_F)
        if Equal (tmp, EMPTY_F)
           then (* the future is ready for evaluation *)
             let result : any = @eval(fut / exh)
             let result : any = promote (result)
             do UPDATE(STATE_OFF, fut, result)
             return (result)
	else if Equal (tmp, STOLEN_F)
           then (* another fiber is evaluating the future; we need to block *)
                cont kLocal (_ : PT.unit) = 
                    (* resume the future *)
		     return (SELECT(STATE_OFF, fut))
                let kLocal : PT.fiber = (PT.fiber)kLocal
                (* make the future cancelable *)
                let kLocal : PT.fiber = C.@mk-cancelable(SELECT(CANCELABLE_OFF, fut), kLocal / exh)
                let k : PT.fiber = promote (kLocal)
   	        let tmpX : any = CAS (&0(fut), STOLEN_F, k)
 	        if Equal (tmpX, STOLEN_F)
	           then (* transfer control to the futures scheduler *)
                        Control.@stop (/ exh)
	          else (* the future value is ready *)
                       return (tmpX)
        else (* the future value is ready *)	       
            return (tmp)
	;

	define @steal (futuresQ : LQ.queue, fut : future / exh : PT.exh) : () =
	  let tmp : any = CAS (&0(fut), EMPTY_F, STOLEN_F)
	  if Equal (tmp, EMPTY_F) 
	     then let result : any = @eval(fut / exh)
		  let tmpX : any = CAS(&0(fut), STOLEN_F, result)
		  if Equal (tmpX, STOLEN_F)
		     then return ()
		     else (* unblock the future *)
			  do UPDATE(STATE_OFF, fut, result)
			  let k : PT.fiber = (PT.fiber) tmpX
			  do LQ.@enqueue (futuresQ, k / exh)
			  return ()
	      else (* future cell is already full *)
		   return ()
	;

    )

  end
