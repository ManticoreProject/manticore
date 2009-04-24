(* float-array.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flat arrays of floats.
 *)

#define DATA_OFF        0
#define LENGTH_OFF      1

structure FloatArray =
  struct

    structure PT = PrimTypes

    _primcode (

      typedef array = PT.array;

      extern void* GlobalAllocFloatArray (void*, int, float);

    (* allocate and initialize an array *)
      define inline @empty-array (x : unit / exh : exh) : array =
        let data : any = ccall GlobalAllocFloatArray(host_vproc, 0, 0.0)
        let arr : array = alloc(data, 0)
        return(arr)
      ;
 
    (* allocate and initialize an array *)
      define inline @array (n : int, elt : ml_float / exh : exh) : array =
        let data : any = ccall GlobalAllocFloatArray(host_vproc, n, #0(elt))
        let arr : array = alloc(data, n)
        return(arr)
      ;

      define inline @length (arr : array / exh : exh) : int =
	return(SELECT(LENGTH_OFF, arr))
      ;

      define inline @update (arr : array, i : int, x : ml_float / exh : exh) : () =
	do assert(I32Gte(i,0))
	let len : int = @length(arr / exh)
	do assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	let x : unit  = ArrayStoreF64(data, i, #0(x))
	return()
      ;

      define inline @sub (arr : array, i : int / exh : exh) : ml_float =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	let x : float = ArrayLoadF32(data, i)
        let x : ml_float = alloc(x)
	return(x)
      ;

      define inline @array-w (arg : [ml_int, ml_float] / exh : exh) : array =
	@array(#0(#0(arg)), #1(arg) / exh)
      ;

      define inline @length-w (arr : array / exh : exh) : ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      define inline @sub-w (arg : [array, ml_int] / exh : exh) : ml_float =
	@sub(#0(arg), #0(#1(arg)) / exh)
      ;

      define inline @update-w (arg : [array, ml_int, ml_float] / exh : exh) : PT.unit =
	do @update(#0(arg), #0(#1(arg)), #2(arg) / exh)
	return(UNIT)
      ;

    )

    type array = _prim( PT.array )

    val emptyArray : unit -> array = _prim(@empty-array)
    val array : int * float -> array = _prim(@array-w)
    val length : array -> int = _prim(@length-w)
    val sub : array * int -> float = _prim(@sub-w)
    val update : array * int * float -> unit = _prim(@update-w)

    fun tabulate (n, f : int -> 'a) = 
	if n = 0
	   then emptyArray ()
	else let
	  val a = array(n, f 0)
	  fun tab i = if i < n 
		then (update(a, i, f i); tab(i + 1))
		else a
          in
	    tab 1
	  end

    fun app f arr = let
	  val len = length arr
	  fun app i = if i < len then (f (sub (arr, i)); app (i + 1)) else ()
	  in
	    app 0
	  end

  end
