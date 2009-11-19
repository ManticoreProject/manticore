(* vector.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Vector = 
  struct

    _primcode (

      typedef vector = [ (* array data *) ![any], (* number of elements *) int ];

      extern void* AllocVector (void*, void*) __attribute__((alloc,pure));
      extern void* AllocVectorRev (void*, int, void*) __attribute__((alloc,pure));

      define inline @from-list (values : List.list / exh : exh) : vector =
	  let vec : vector = ccall AllocVector (host_vproc, values)
	  return (vec)
	;

      define inline @from-list-rev (arg : [List.list, ml_int] / exh : exh) : vector =
	  let vec : vector = ccall AllocVectorRev (host_vproc,  unwrap(#1(arg)), #0(arg))
	  return (vec)
	;

      define inline @length (vec : vector / exh : exh) : ml_int =
	  return (alloc(#1(vec)))
	;

      define inline @sub (arg : [vector, ml_int] / exh : exh) : any =
          let vec : vector = #0(arg)
          let i : int = #0(#1(arg))
	  do assert(I32Gte(i,0))
	  do assert(I32Lt(i,#1(vec)))
          let data : any = #0(vec)
          let x : any = ArrLoad(data, i)
	  return (x)
	;

    )

    type 'a vector = _prim (vector)

    val fromList : 'a list -> 'a vector = _prim (@from-list)
  (* same as fromList, but expects that the list is in reverse order *)
    val fromListRev : 'a list * int -> 'a vector = _prim (@from-list-rev)
    val length : 'a vector -> int = _prim (@length)
    val sub : 'a vector * int -> 'a = _prim (@sub)

  end
