structure Int =
  struct

    type ml_int = _prim ( [int] )
    type int = ml_int

    _primcode (
      define @plus (arg : [ml_int, ml_int] / exh : cont(exn)) : ml_int =
        return(alloc(I32Add(unwrap(#0(arg)), unwrap(#1(arg)))))
      ;
    )

    val plus : (int * int) -> int = _prim (@plus)

  end
