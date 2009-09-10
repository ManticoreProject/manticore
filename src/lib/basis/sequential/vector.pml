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

      extern void* AllocVector (void*, void*) __attribute__((alloc));

      define inline @from-list (values : List.list / exh : exh) : vector =
	  let vec : vector = ccall AllocVector (host_vproc, values)
	  return (vec)
	;

      define inline @length (vec : vector / exh : exh) : ml_int =
	  return (alloc(#1(vec)))
	;

      define inline @sub (arg : [vector, ml_int] / exh : exh) : any =
	  let vec : vector = #0(arg)
	  let i : int = unwrap(#1(arg))
	  do assert(I32Gte(i,0))
	  do assert(I32Lt(i,#1(vec)))
          let x : any = ArrLoad(#0(vec), i)
	  return (x)
	;

    )

    type 'a vector = _prim (vector)

    val fromList : 'a list -> 'a vector = _prim (@from-list)
    val length : 'a vector -> int = _prim (@length)
    val sub : 'a vector * int -> 'a = _prim (@sub)

  end
