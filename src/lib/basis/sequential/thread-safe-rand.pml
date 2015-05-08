(* thread-safe-rand.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * uses the rand_r function for thread safety
 *)


structure ThreadSafeRand =
struct

    structure PT = PrimTypes

    _primcode(
      
        extern int M_ThreadSafeRandomInt(int, int, void*);

        typedef seed = [int];

        define @alloc-seed(x : ml_int / _ : exh) : seed =
            let x : seed = alloc(#0(x))
            let x : seed = promote(x)
            return(x)
        ;

        define @in-range-int(arg : [ml_int, ml_int, seed] / exh : exh) : ml_int =
            let lo : int = #0(#0(arg))
            let hi : int = #0(#1(arg))
            let seed : seed = #2(arg)
            let r : int = ccall M_ThreadSafeRandomInt(lo, hi, seed)
            return(alloc(r))
        ;
        
    )

    type seed = _prim(seed)
    val newSeed : int -> seed = _prim(@alloc-seed)
    val inRangeInt : int * int * seed -> int = _prim(@in-range-int)
end
