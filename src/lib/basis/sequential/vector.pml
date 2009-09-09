(* vector.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Vector = 
  struct

    _primcode (

      typedef vector = [ (* array data *) [any], (* number of elements *) int ];

      extern void* AllocVector (void*, void*);

      define inline @from-list (arg : [ml_int, List.list] / exh : exh) : vector =
	  let self : vproc = SchedulerAction.@begin-atomic ()
	  let rawVec : any = ccall AllocVector (self, #1(arg))
	  do SchedulerAction.@end-atomic (self)
	  return (alloc (rawVec, unwrap(#0(arg))))
	;

      define inline @length (vec : vector / exh : exh) : ml_int =
	  return (alloc(#1(vec)))
	;

      define inline @sub (arg : [vector, int] / exh : exh) : any =
	  let vec : vector = #0(arg)
	  let i : int = #1(arg)
	  do assert(I32Gte(i,0))
	  do assert(I32Lt(i,#1(vec)))
	  return (ArrLoad(#0(vec), i))
	;

    )

    type 'a vector = _prim (vector)

    val fromList : 'a list -> 'a vector = _prim (@from-list)
    val length : 'a vector -> int = _prim (@length)
    val sub : 'a vector * int -> 'a = _prim (@sub)

  end
