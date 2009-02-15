(* deque-th-rep.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure DequeTHRep =
  struct

    structure PT = PrimTypes
    structure Arr = Array64

    _primcode (

      typedef deque = ![
		  int,                   (* T *)
		  int,                   (* H *)
		  Arr.array,             (* deque memory *)
		  PT.bool                (* lock *)
	      ];
    )

  end
