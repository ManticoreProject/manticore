(* int-array.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flat arrays of ints.
 *)

#define DATA_OFF        0
#define LENGTH_OFF      1

structure IntArray =
  struct

    structure PT = PrimTypes

    _primcode (

      typedef array = PT.array;

      extern void* GlobalAllocIntArray (void*, int, int);

    (* allocate and initialize an array *)
      define inline @empty-array (x : unit / exh : exh) : array =
        let data : any = ccall GlobalAllocIntArray (host_vproc, 0, 0)
        let arr : array = alloc (data, 0)
        return(arr)
      ;
 
    (* allocate and initialize an array *)
      define inline @array (n : int, elt : ml_int / exh : exh) : array =
        let data : any = ccall GlobalAllocIntArray (host_vproc, n, #0(elt))
        let arr : array = alloc (data, n)
        return(arr)
      ;

      define inline @length (arr : array / exh : exh) : int =
	return(SELECT(LENGTH_OFF, arr))
      ;

      define inline @update (arr : array, i : int, n : ml_int / exh : exh) : () =
	do assert(I32Gte(i,0))
	let len : int = @length(arr / exh)
	do  assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	do ArrayStoreI32(data, i, #0(n))
	return()
      ;

      define inline @sub (arr : array, i : int / exh : exh) : ml_int =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	let n : int = ArrayLoadI32(data, i)
        let wn : ml_int = alloc(n)
	return(wn)
      ;

      define inline @array-w (arg : [ml_int, ml_int] / exh : exh) : array =
	@array(#0(#0(arg)), #1(arg) / exh)
      ;

      define inline @length-w (arr : array / exh : exh) : ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      define inline @sub-w (arg : [array, ml_int] / exh : exh) : ml_int =
	@sub(#0(arg), #0(#1(arg)) / exh)
      ;

      define inline @update-w (arg : [array, ml_int, ml_int] / exh : exh) : PT.unit =
	do @update(#0(arg), #0(#1(arg)), #2(arg) / exh)
	return(UNIT)
      ;

    (* @prefix-plus-scan : int * array -> array *)
    (* The argument is a starting value, and the array to scan. *)
    (* A new array is built and returned by this operation. *)
    (* (Unless the given array is empty, in which case a pointer to it is returned.) *)
    (* The array consumed is only read from, not modified. *)
    (* Bounds are not checked in the loop here. *)
      define inline @prefix-plus-scan (arg : [ml_int, array] / exh : exh) : array =
        let arr : array = #1(arg)      
        let len : int = @length(arr / exh)
        if I32Eq (len, 0)
          then 
            return (arr)
          else
            let mlSeed : ml_int = #0(arg)
            let newArray : array = @array (len, mlSeed / exh)
            let srcData : any = SELECT(DATA_OFF, arr)
            let newData : any = SELECT(DATA_OFF, newArray)
	    let seed : int = #0(mlSeed)
         (* note: bounds are not checked in this loop, since they'd only be checked against len *)
            fun loop (i : int, last : int / ) : () =
              if I32Gte(i,len)
                then 
                  return()
	        else
                  do ArrayStoreI32 (newData, i, last)
                  let x : int = ArrayLoadI32 (srcData, i)
                  let j : int = I32Add(i,1)
                  let next : int = I32Add(last, x)
                  apply loop (j, next)
             do apply loop (0, seed)
             return (newArray)
      ;

  (* sum : array -> int *)
  (* Bounds are not explicitly checked. *)
    define inline @sum (arr : array / exh : exh) : int =
      let len : int = @length (arr / exh)
      let data : any = SELECT(DATA_OFF, arr)
      fun loop (i : int, acc : int / ) : int =
        if I32Gte (i, len)
        then
          return (acc)
        else
          let curr : int = ArrayLoadI32 (data, i)
          let newAcc : int = I32Add (curr, acc)
	  let j : int = I32Add (i, 1)
          apply loop (j, newAcc)
      let s : int = apply loop (0, 0)
      return (s)
    ;

   (* sum-w : array -> ml_int *)
    define inline @sum-w (arr : array / exh : exh) : ml_int =
      let s : int = @sum (arr / exh)
      let ml_s : ml_int = wrap(s)
      return (ml_s)
    ;

    )

    type array = _prim( PT.array )

    val emptyArray : unit -> array = _prim(@empty-array)
    val array : int * int -> array = _prim(@array-w)
    val length : array -> int = _prim(@length-w)
    val sub : array * int -> int = _prim(@sub-w)
    val update : array * int * int -> unit = _prim(@update-w)
    val prefixPlusScan : int * array -> array = _prim(@prefix-plus-scan)
    val sum : array -> int = _prim(@sum-w)

    fun tabulate (n : int, f : int -> int) = 
      if n = 0 then emptyArray ()
      else let
        val a = array (n, f 0)
	fun tab i = 
	  if i < n then 
	   (update(a, i, f i); 
	    tab(i + 1))
	  else a
	val retval = tab 1
        in
	  retval
	end

    fun app f arr = let
      val len = length arr
      fun app i = 
        if i < len then 
         (f (sub (arr, i)); 
	  app (i + 1)) 
	else ()
      in
        app 0
      end

  end
