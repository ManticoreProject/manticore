(* word64-array.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flat arrays of 64-bit words.
 *)

#define DATA_OFF        0
#define LENGTH_OFF      1

structure Word64Array =
  struct

    structure PT = PrimTypes

    _primcode (

      typedef array = PT.array;
      typedef word64 = Word64.word;
      typedef ml_word64 = Word64.ml_word;

      extern void* GlobalAllocWord64Array (void*, int, long);

    (* allocate and initialize an array *)
      define inline @empty-array (x : unit / exh : exh) : array =
        let data : any = ccall GlobalAllocWord64Array (host_vproc, 0, 0)
        let arr : array = alloc (data, 0)
        return(arr)
      ;
 
    (* allocate and initialize an array *)
      define inline @array (n : int, elt : ml_word64 / exh : exh) : array =
        let data : any = ccall GlobalAllocWord64Array (host_vproc, n, #0(elt))
        let arr : array = alloc (data, n)
        return(arr)
      ;

      define inline @length (arr : array / exh : exh) : int =
	return(SELECT(LENGTH_OFF, arr))
      ;

      define inline @update (arr : array, i : int, n : ml_word64 / exh : exh) : () =
	do assert(I32Gte(i,0))
	let len : int = @length(arr / exh)
	do  assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	do ArrayStoreI64(data, i, #0(n))
	return()
      ;

      define inline @sub (arr : array, i : int / exh : exh) : ml_word64 =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
        let data : any = SELECT(DATA_OFF, arr)
	let n : word64 = ArrayLoadI64(data, i)
        let wn : ml_word64 = alloc(n)
	return(wn)
      ;

      define inline @array-w (arg : [ml_int, ml_word64] / exh : exh) : array =
	@array(#0(#0(arg)), #1(arg) / exh)
      ;

      define inline @length-w (arr : array / exh : exh) : ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      define inline @sub-w (arg : [array, ml_int] / exh : exh) : ml_word64 =
	@sub(#0(arg), #0(#1(arg)) / exh)
      ;

      define inline @update-w (arg : [array, ml_int, ml_word64] / exh : exh) : PT.unit =
	do @update(#0(arg), #0(#1(arg)), #2(arg) / exh)
	return(UNIT)
      ;

    )

    type array = _prim( PT.array )

    val emptyArray : unit -> array = _prim(@empty-array)
    val array : int * Word64.word -> array = _prim(@array-w)
    val length : array -> int = _prim(@length-w)
    val sub : array * int -> Word64.word = _prim(@sub-w)
    val update : array * int * Word64.word -> unit = _prim(@update-w)

    fun tabulate (n : int, f : int -> Word64.word) = 
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
