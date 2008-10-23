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

    _primcode (

      typedef array = PT.array;

      extern void* M_NewArray (void*, int, void*);
 
      typedef function = fun (any / PT.exh -> any);

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

      define @length (arr : array / exh : PT.exh) : int =
        let len : int = ArrayLoadI64(arr, ~1)
	return(len)
      ;

      define @sub (arr : array, i : int / exh : PT.exh) : any =
	let len : int = @length(arr / exh)
	do assert(I32Gte(i,0))
	do assert(I32Lt(i,len))
	let x : any = ArrayLoadI64(arr, i)
	return(x)
      ;

      define @update (arr : array, i : int, x : any / exh : PT.exh) : () =
	do assert(I32Gte(i,0))
	let len : int = @length(arr / exh)
	do assert(I32Lt(i,len))
       (* since the array is in the global heap, x must also be in the global heap *)
	let x : any = (any)x
	let x : any = promote(x)
	let x : PT.bool  = ArrayStoreI64(arr, i, x)
	return()
      ;

      define @functional-map (f : function, arr : array / exh : PT.exh) : array =
        let len : int = @length (arr / exh)
        if I32Eq(len,0)
          then
            return(arr)
          else
            let zeroth : any = @sub (arr, 0 / exh)
            let representative : any = apply f (zeroth / exh)	    
            let brr : array = @array (len, representative / exh)
            fun bang (i : int / ) : () = 
              if I32Lt(i, len)
                then
                  let x : any = @sub (arr, i / exh)
                  let y : any = apply f (x / exh)
	          let j : int = I32Add(i, 1)
                  do @update (brr, j, y / exh)
                  do apply bang (j)
                  return ()
                else
                  return ()
            do apply bang (1) (* the 0th element is already taken care of *)
            return(brr)
      ;

      define @app (f : fun (any / PT.exh -> PT.unit), arr : array / exh : PT.exh) : () =
        let len : int = @length (arr / exh)
        fun loop (i : int / ) : () =
          if I32Eq(i, len)
            then
              return ()
	    else
              let x : any = @sub (arr, i / exh)
 	      let j : int = I32Add(i,1)
              let _ : PT.unit =  apply f (x / exh)
              do apply loop (j)
              return ()
        do apply loop (0)
        return ()
      ; 

      define @tabulate (n : int, f : fun (int / PT.exh -> any) / exh : PT.exh) : array =
        let representative : any = apply f (0 / exh)
        let arr : array = @array (n, representative / exh)
        fun loop (i : int) : () =
          if I32Eq(i, n)
            then
              return ()
            else
              let x : any = apply f (i / exh)
              do @update (arr, i, x / exh)
              let j : int = I32Add(i, 1)
              do apply loop (j)
              return ()
        do apply loop (1)
        return (arr)
      ;

      define @array-w (arg : [PT.ml_int, any] / exh : PT.exh) : array =
	@array(#0(#0(arg)), #1(arg) / exh)
      ;

      define @length-w (arr : array / exh : PT.exh) : PT.ml_int =
	let len : int = @length(arr / exh)
	return(alloc(len))
      ;

      define @sub-w (arg : [array, PT.ml_int] / exh : PT.exh) : any =
	@sub(#0(arg), #0(#1(arg)) / exh)
      ;

      define @update-w (arg : [array, PT.ml_int, any] / exh : PT.exh) : PT.unit =
	do @update(#0(arg), #0(#1(arg)), #2(arg) / exh)
	return(UNIT)
      ;

      define @functional-map-w (arg : [function, array] / exh : PT.exh) : array =
        let b : array = @functional-map (#0(arg), #1(arg) / exh)
        return(b)
      ;

      define @app-w (arg : [fun (any / PT.exh -> unit), array] / exh : PT.exh) : PT.unit =
        do @app (#0(arg), #1(arg) / exh)
        return (UNIT)
      ;

      define @tabulate-w (arg : [PT.ml_int, fun (PT.ml_int / PT.exh -> any)] / exh : PT.exh) : array =
        let f : fun (PT.ml_int / PT.exh -> any) = #1(arg)
        fun g (n : int / exh : PT.exh) : any =
          let w : PT.ml_int = wrap(n)
          let answer : any = apply f (w / exh)
          return (answer)        
        let arr : array = @tabulate (#0(#0(arg)), g / exh)
        return (arr)
      ;

    )

    type 'a array = _prim( PT.array )

    val array : int * 'a -> 'a array = _prim(@array-w)
    val length : 'a array -> int = _prim(@length-w)
    val sub : 'a array * int -> 'a = _prim(@sub-w)
    val update : 'a array * int * 'a -> unit = _prim(@update-w)
    val functionalMap : ('a -> 'b) * 'a array -> 'b array = _prim(@functional-map-w)
    val app : ('a -> unit) * 'a array -> unit = _prim(@app-w)
    val tabulate : int * (int -> 'a) -> 'a array = _prim(@tabulate-w)

  end
