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

    (* @sub-u : unchecked, unwrapped sub operation *)
      define inline @sub-u (arr : array, i : int / exh : exh) : float =
        let data : any = SELECT(DATA_OFF, arr)
        let x : float = ArrayLoadF32(data, i)
        return(x)
      ;

    (* @update-u : unchecked, unwrapped update operation *)
      define inline @update-u (arr : array, i : int, x : float / exh : exh) : () = 
        let data : any = SELECT(DATA_OFF, arr)
	let x : unit  = ArrayStoreF64(data, i, x)
	return()
      ;

    (* @prefix-plus-scan : float * array -> array *)
    (* The argument is a starting value, and the array to scan. *)
    (* A new array is built and returned by this operation. *)
    (* (Unless the given array is empty, in which case a pointer to it is returned.) *)
    (* The array consumed is only read from, not modified. *)
    (* Bounds are not checked in the loop here. *)
      define inline @prefix-plus-scan (arg : [ml_float, array] / exh : exh) : array =
        let arr : array = #1(arg)      
        let len : int = @length(arr / exh)
        if I32Eq(len, 0)
          then 
            return (arr)
          else
            let mlSeed : ml_float = #0(arg)
            let newArray : array = @array (len, mlSeed / exh)
	    let seed : float = #0(mlSeed)
         (* note: bounds are not checked in this loop, since they'd only be checked against len *)
            fun loop (i : int, last : float / ) : () =
              if I32Gte(i,len)
                then 
                  return()
	        else
                  do @update-u (newArray, i, last / exh)
                  let x : float = @sub-u (arr, i / exh)
                  let j : int = I32Add(i,1)
                  let next : float = F64Add(last, x)
                  do apply loop (j, next)
                  return()
             do apply loop (0, seed)
             return (newArray)
      ;

  (* sum : array -> ml_float *)
  (* Bounds are not checked. *)
    define inline @sum (arr : array / exh : exh) : ml_float =
      let len : int = @length (arr / exh)
      fun loop (i : int, acc : float / ) : float =
        if I32Gte (i, len)
        then
          return (acc)
        else
          let curr : float = @sub-u (arr, i / exh)
          let newAcc : float = F64Add(curr, acc)
	  let j : int = I32Add (i, 1)
          let s : float = apply loop (j, newAcc)
          return (s)
      let s : float = apply loop (0, 0.0)
      let ml_s : ml_float = wrap(s)
      return (ml_s)
    ;

    )

    type array = _prim( PT.array )

    val emptyArray : unit -> array = _prim(@empty-array)
    val array : int * float -> array = _prim(@array-w)
    val length : array -> int = _prim(@length-w)
    val sub : array * int -> float = _prim(@sub-w)
    val update : array * int * float -> unit = _prim(@update-w)
    val prefixPlusScan : float * array -> array = _prim(@prefix-plus-scan)
    val sum : array -> float = _prim(@sum)

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
