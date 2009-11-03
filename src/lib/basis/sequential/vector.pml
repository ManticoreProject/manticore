(* vector.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Vector (* : sig

    type vector

    val length : 'a vector -> int
    val sub : 'a vector * int -> 'a
    val fromList : 'a list -> 'a vector
  (* the same as `fromList o List.rev', except that we manually optimize away the list reversal *)
    val fromListRev : 'a list -> 'a vector

  (* returns a pointer to a copy of the vector that is allocated in the global heap *)
    val promote : 'a vector -> 'a vector

  end *) = struct

    _primcode (

      typedef vector = [ (* array data *) ![any], (* number of elements *) int ];

      extern void* AllocVector (void*, void*) __attribute__((alloc));
      extern void* AllocVectorRev (void*, int, void*) __attribute__((alloc));

      define inline @from-list (values : List.list / exh : exh) : vector =
	  let vec : vector = ccall AllocVector (host_vproc, values)
	  return (vec)
	;

      define inline @promote (vec : vector / exh : exh) : vector =
          let vec : vector = promote (vec)
          return (vec)
	;

      define inline @from-list-rev (arg : [List.list, ml_int] / exh : exh) : vector =
	  let vec : vector = ccall AllocVector (host_vproc, #0(arg), #1(arg))
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

    val length : 'a vector -> int = _prim (@length)
    val sub : 'a vector * int -> 'a = _prim (@sub)
    val fromList : 'a list -> 'a vector = _prim (@from-list)
  (* the same as `fromList o List.rev', except that we manually optimize away the list reversal *)
    val fromListRev : 'a list * int -> 'a vector = _prim (@from-list-rev)

  (* returns a pointer to a copy of the vector that is allocated in the global heap *)
    val promote : 'a vector -> 'a vector = _prim (@promote)

  end
