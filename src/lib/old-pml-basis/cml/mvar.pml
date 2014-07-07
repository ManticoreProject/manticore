(* mvar.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * M-variables, which are updatable synchronous memory cells
 *)

#include "debug.def"
#include "spin-lock.def"

structure MVar (*: sig

    type 'a mvar

    val new : 'a -> 'a mvar
    val put  : ('a mvar, 'a) -> unit
    val take : 'a mvar -> 'a 
  end*) = struct

    structure PT = PrimTypes

    _primcode (
	typedef waiter = [
	    vproc,			(* vproc affinity *)
	    FLS.fls,			(* FLS of thread *)
	    cont(any) 			(* thread's continuation *)
	  ];
	typedef mvar = ![
	    int,			(* spinlock *)
	    bool,			(* state *)
	    any,                        (* value *)
	    List.list			(* list of waiters *)

	  ];

      (* the fields of a mvar *)
#	define MV_LOCK		0
#	define MV_STATE		1
#       define MV_VALUE         2 
#	define MV_WAITING       3	

#       define V_NULL enum(0) : any 

      (* create a new signal variable *)
	define inline @mNew (item : any / _ : exh) :  mvar =
	    let mv : mvar = alloc(0, true, item, nil)
	    let mv : mvar = promote (mv)
	    return (mv)
	  ;


      (* put operation on M-variable *)
	define @mPut (arg :[mvar, any] / exh : exh) : unit = 
	    let mv : mvar = #0(arg)
	    let mval : any = (any)#1(arg) 
	    let self : vproc = SchedulerAction.@atomic-begin () 
	    SPIN_LOCK(mv, MV_LOCK) 
	      case SELECT(MV_STATE, mv)
	       of true =>  
	            PRINT_MSG ("reached the true branch by put \n")
	            SPIN_UNLOCK(mv, MV_LOCK)
		    do SchedulerAction.@atomic-end(self)
	            let e: exn = Fail(@"Put on full M-variable")
		    throw exh (e)
	        | false =>   
		    PRINT_MSG ("reached the false branch by put \n")
	            case SELECT(MV_WAITING, mv)
		     of nil => 
		          do UPDATE(MV_STATE, mv, true)
			  PRINT_MSG ("set the state to true by put\n")
                          do UPDATE(MV_VALUE, mv, mval)
                          SPIN_UNLOCK(mv, MV_LOCK)
			  do SchedulerAction.@atomic-end(self)
			  return (UNIT)
		      | CONS(hd : waiter, tl : List.list) =>  
		          do UPDATE(MV_WAITING, mv, tl) 
			  SPIN_UNLOCK(mv, MV_LOCK) 
			  do SchedulerAction.@atomic-end(self) 
			  let k : cont(any) = SELECT(2, hd)
			  cont takeK (_ : unit) =  throw k (mval)
			  (* in *)
			    do VProcQueue.@enqueue-on-vproc (
			           SELECT(0, hd),
				   SELECT(1, hd),
				   takeK)  
			    return (UNIT) 
			    
		    end		 
	      end    
	  ;

      (* take operation on M-variable *)
	define @mTake (mv : mvar / _ : exh) : any =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(mv, MV_LOCK)
	      case SELECT(MV_STATE, mv)
	       of true =>
	            PRINT_MSG ("reached the true branch by take\n")
	            let mval : any = SELECT(MV_VALUE, mv)
	            do UPDATE(MV_STATE, mv, false)
		    PRINT_MSG ("set the state to false by take\n")
		    SPIN_UNLOCK(mv, MV_LOCK)
		    do SchedulerAction.@atomic-end(self)
		    return (mval)
	        | false  => 
		    PRINT_MSG ("reached the false branch by take\n") 
	            cont takeK (x : any) = return (x)
		    (* in *)
		      let fls : FLS.fls = FLS.@get-in-atomic(self)
		      let item : waiter = alloc (self, fls, takeK)
		      let l : list = CONS(item, SELECT(MV_WAITING, mv))
		      let l : list = promote (l)
		      do UPDATE(MV_WAITING, mv, l) 
                      SPIN_UNLOCK(mv, MV_LOCK)
		      SchedulerAction.@stop-from-atomic (self)
		end
	  ;

      )

    type 'a mvar = _prim (mvar)

    val new	: 'a -> 'a mvar = _prim(@mNew)
    val put	: 'a mvar * 'a -> unit = _prim(@mPut)
    val take	: 'a mvar -> 'a = _prim(@mTake)
  end
