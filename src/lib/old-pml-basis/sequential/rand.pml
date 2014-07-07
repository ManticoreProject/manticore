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
      extern float M_FRand (float, float);

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

      define inline @rand-float (arg : [ml_float, ml_float] / exh : exh) : ml_float =
	let r : float = ccall M_FRand (#0(#0(arg)), #0(#1(arg)))
	return (alloc(r))
      ;

    )

  (* The random long generated is inclusive of the lower bound, exclusive of the upper. *)
  (* ex: inRangeLong (0, 10) is in {0,1,2,3,4,5,6,7,8,9} *)
    val inRangeLong : (long * long) -> long = _prim(@in-range-long-wrap)

  (* The random int generated is inclusive of the lower bound, exclusive of the upper. *)
  (* ex: inRangeInt (0, 10) is in {0,1,2,3,4,5,6,7,8,9} *)
    val inRangeInt : (int * int) -> int = _prim(@in-range-int-wrap)

    val seed : unit -> unit = _prim(@seed)

    val randFloat : (float * float) -> float = _prim(@rand-float)

    val randDouble : (double * double) -> double = _prim(@rand-double)

  end
