(* ivar.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * I-Structures
 *)

#include "debug.def"
#include "spin-lock.def"

structure MVar (*: sig

    type 'a ivar

    val new : () -> 'a ivar
    val put  : ('a ivar, 'a) -> unit
    val get : 'a ivar -> 'a 
  end*) = struct

    structure PT = PrimTypes

    _primcode (
	typedef waiter = [
	    vproc,			(* vproc affinity *)
	    FLS.fls,			(* FLS of thread *)
	    cont(any) 			(* thread's continuation *)
	  ];
	typedef ivar = ![
	    int,			(* spinlock *)
	    bool,			(* speculative? *)
	    any,                        (* value *)
	    bool,                       (*full?*)
	    List.list,			(* list of waiters *)
            List.list,                  (*Restart info*)
            any,      (*Thread id (TODO: find a representation for thread ids)*)
            List.list (*List of writers if this is spec full*)
	  ];

      (* the fields of a mvar *)
#	define IV_LOCK		0
#	define IV_SPEC		1
#       define IV_VALUE         2 
#	define IV_FULL          3
#       define IV_WAITERS       4
#       define IV_RESTART       5
#       define IV_TID           6
#       define IV_WRITERS       7   	

#       define V_NULL enum(0) : any 


      )

    type 'a ivar = _prim (ivar)

    val new	: () -> 'a ivar = _prim(@iNew)
    val put	: 'a ivar * 'a -> unit = _prim(@iPut)
    val take	: 'a ivar -> 'a = _prim(@iGet)
  end
