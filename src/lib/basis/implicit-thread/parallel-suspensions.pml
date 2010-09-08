(* parallel-suspensions.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel suspensions
 *
 *)

signature PARALLEL_SUSPENSIONS =
  sig

    type 'a susp

(*    exception Canceled *)

    val new : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
    val spawn : 'a susp -> unit
    val cancel : 'a susp -> unit

  end

structure ParallelSuspensions (* : PARALLEL_SUSPENSIONS *) =
  struct

#define BLACK_HOLE       $0

(* offsets into the susp record type *)
#define RESULT_OFF       0
#define THUNK_OFF        1
#define CANCELABLE_OFF   2

    _primcode (

      typedef thunk = fun(unit / exh -> any);

      typedef susp = ![
		 ImplicitThreadIVar.ivar,             (* result *)
		 thunk,                               (* thunk *)
		 Cancelation.cancelable
      ];

      define @new (t : thunk / exh : exh) : susp =
	  let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@empty-ivar (/ exh)
	  let c : Cancelation.cancelable = Cancelation.@new (/ exh)
	  let susp : susp = alloc (ivar, t, c)
	  let susp : susp = promote (susp)
	  return (susp)
	;

      define @access-eval-update (susp : susp / exh : exh) : () =
          fun lp () : () =
	      let t : thunk = SELECT(THUNK_OFF, susp)
	      let isBH : bool = Equal(t, BLACK_HOLE)
              case isBH
	       of true => return()
		| false =>
		  let tC : thunk = CAS(ADDR_OF(THUNK_OFF, susp), t, BLACK_HOLE)
		  let retry : bool = NotEqual(t, tC)
		  case retry
		   of true => apply lp ()
		    | false =>
   	              let x : any = apply t (UNIT / exh)
	              ImplicitThreadIVar.@put (SELECT(RESULT_OFF, susp), x / exh)
                  end
              end
          apply lp ()
	;


      define @spawn (susp : susp / exh : exh) : unit =
	  cont k (x : unit) =
	    do @access-eval-update (susp / exh)
	    SchedulerAction.@stop ()
          let c : Cancelation.cancelable = SELECT(CANCELABLE_OFF, susp)
	  let thd : ImplicitThread.thread = ImplicitThread.@new-cancelable-thread (k, c / exh)
	  do ImplicitThread.@spawn-thread (thd / exh)
	  return (UNIT)
	;

      define @force (susp : susp / exh : exh) : any =
	  do @access-eval-update (susp / exh)
	  ImplicitThreadIVar.@get (SELECT(RESULT_OFF, susp) / exh)
	;

      define @cancel (susp : susp / exh : exh) : unit =
          Cancelation.@cancel (SELECT(CANCELABLE_OFF, susp) / exh)
	;

    )

    type 'a susp = _prim (susp)

    val new : ((unit -> 'a) * bool) -> 'a susp = _prim(@new)
    val force : 'a susp -> 'a = _prim(@force)
    val spawn : 'a susp -> unit = _prim(@spawn)
    val cancel : 'a susp -> unit = _prim(@cancel)

  end
