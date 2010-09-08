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
    val spawnTask : 'a susp -> unit
    val cancel : 'a susp -> unit

  end

structure ParallelSuspensions (* : PARALLEL_SUSPENSIONS *) =
  struct

#include "../include/debug.def"

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
	  let c : Cancelation.cancelable = Cancelation.@new (UNIT / exh)
	  let susp : susp = alloc (ivar, t, c)
	  let susp : susp = promote (susp)
	  return (susp)
	;

      define @access-eval-update (susp : susp / exh : exh) : () =
          cont exit () = return ()
          fun lp () : () =
	      let t : thunk = SELECT(THUNK_OFF, susp)
	      if Equal(t, BLACK_HOLE) then
		  throw exit ()
	      else
		  let tC : thunk = CAS(ADDR_OF(THUNK_OFF, susp), t, (thunk)BLACK_HOLE)
		  if NotEqual(t, tC) then
		      PRINT_MSG("loop")
		      apply lp ()
		  else
   	              let x : any = apply t (UNIT / exh)
	              ImplicitThreadIVar.@put (SELECT(RESULT_OFF, susp), x / exh)
          apply lp ()
	;


      define @spawn-task (susp : susp / exh : exh) : unit =
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

    val new : (unit -> 'a) -> 'a susp = _prim(@new)
    val force : 'a susp -> 'a = _prim(@force)
    val spawnTask : 'a susp -> unit = _prim(@spawn-task)
    val cancel : 'a susp -> unit = _prim(@cancel)

  end
