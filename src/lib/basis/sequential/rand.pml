(* rand.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Rand =
  struct

    structure PT = PrimTypes

    _primcode(
      
      extern long M_Random(long, long);
      extern int M_RandomInt(int, int);
      extern void M_SeedRand();
      extern double M_DRand (double, double);

      define inline @in-range-long(lo : long, hi : long / exh : exh) : long =
        let r : long = ccall M_Random(lo, hi)
        return(r)
      ;

      define inline @in-range-long-wrap(arg : [ml_long, ml_long] / exh : exh) : ml_long =
        let r : long = @in-range-long(#0(#0(arg)), #0(#1(arg)) / exh)
        return(alloc(r))
      ;

      define inline @in-range-int(lo : int, hi : int / exh : exh) : int =
        let r : int = ccall M_RandomInt(lo, hi)
        return(r)
      ;

      define inline @in-range-int-wrap(arg : [ml_int, ml_int] / exh : exh) : ml_int =
        let r : int = @in-range-int(#0(#0(arg)), #0(#1(arg)) / exh)
        let r : ml_int = alloc(r)
        return(r)
      ;

    (* seed the random number generator *)
      define inline @seed(x : unit / exh : exh) : unit =
        do ccall M_SeedRand()
        return(UNIT)
      ;

      define inline @rand-double (arg : [ml_double, ml_double] / exh : exh) : ml_double =
	let r : double = ccall M_DRand (#0(#0(arg)), #0(#1(arg)))
	return (alloc(r))
      ;

    )

    val inRangeLong : (long * long) -> long = _prim(@in-range-long-wrap)
    val inRangeInt : (int * int) -> int = _prim(@in-range-int-wrap)
    val seed : unit -> unit = _prim(@seed)
    val randDouble : (double * double) -> double = _prim(@rand-double)

  end
