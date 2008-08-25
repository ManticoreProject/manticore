(* array.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mutable arrays.
 *)

structure Array64 =
  struct

    structure PT = PrimTypes

    type 'a array = _prim( PT.array )

    _primcode (

      extern void* M_NewArray (void*, int, void*);

    (* allocate and initialize an array. we allocate an extra 64-bit length field for each array. so arrays 
     * have the following layout
     *    | length | elt0 | ... | eltn |
     * where each field is 64 bits wide. the array handle points to elt0, not the length field.
     *)
      define @array (n : int, elt : any / exh : PT.exh) : array =
	let elt : any = (any)elt
	let elt : any = promote(elt)
	let arr : array = ccall M_NewArray(host_vproc, I32Add(n,1), elt)  
        let x : PT.bool = ArrayStoreI64(arr, 0, n)
        let arr : array = (any)&1(([any,any])arr)
	return(arr)
      ;

      define inline @length (arr : array / exh : PT.exh) : int =
        let len : int = ArrayLoadI64(arr, ~1)
	return(len)
      ;

      define inline @sub (arr : array, i : int / exh : PT.exh) : any =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
	let x : any = ArrayLoadI64(arr, i)
	return(x)
      ;

      define inline @update (arr : array, i : int, x : any / exh : PT.exh) : () =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
       (* since the array is in the global heap, x must also be in the global heap *)
	let x : any = (any)x
	let x : any = promote(x)
	let x : PT.bool  = ArrayStoreI64(arr, i, x)
	return()
      ;

      define inline @array-w (arg : [PT.ml_int, any] / exh : PT.exh) : array =
	@array(#0(#0(arg)), #1(arg) / exh)
      ;

      define inline @length-w (arr : array / exh : PT.exh) : PT.ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      define inline @sub-w (arg : [array, PT.ml_int] / exh : PT.exh) : any =
	@sub(#0(arg), #0(#1(arg)) / exh)
      ;

      define inline @update-w (arg : [array, PT.ml_int, any] / exh : PT.exh) : PT.unit =
	do @update(#0(arg), #0(#1(arg)), #2(arg) / exh)
	return(UNIT)
      ;

    )

    val array : int * 'a -> 'a array = _prim(@array-w)
    val length : 'a array -> int = _prim(@length-w)
    val sub : 'a array * int -> 'a = _prim(@sub-w)
    val update : 'a array * int * 'a -> unit = _prim(@update-w)

(*    fun t() = 
	let
	    val arr = array(8, 1024)
	    val _ = update(arr, 0, 1023)
	in
	    Print.printLn(Int.toString(sub(arr, 0)))
	end
    val _ = t()
*)
	
  end
