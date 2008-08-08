structure Rand =
  struct

    structure PT = PrimTypes

    _primcode(
      
      extern long M_Random(long, long);
      extern void M_SeedRand();

      define inline @in-range(arg : [PT.ml_long, PT.ml_long] / exh : PT.exh) : PT.ml_long =
        let r : long = ccall M_Random(#0(#0(arg)), #0(#1(arg)))
        return(alloc(r))
      ;

      define inline @in-range(lo : long, hi : long / exh : PT.exh) : long =
        let r : long = ccall M_Random(lo, hi)
        return(r)
      ;

      define inline @in-range-wrap(arg : [PT.ml_long, PT.ml_long] / exh : PT.exh) : PT.ml_long =
        let r : long = @in-range(#0(#0(arg)), #0(#1(arg)) / exh)
        return(alloc(r))
      ;

    (* seed the random number generator *)
      define @seed(x : PT.unit / exh : PT.exh) : PT.unit =
        do ccall M_SeedRand()
        return(UNIT)
      ;

    )

    val inRange : (long * long) -> long = _prim(@in-range-wrap)
    val seed : unit -> unit = _prim(@seed)
    val _ = seed()

  end
