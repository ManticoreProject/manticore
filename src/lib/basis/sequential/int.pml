structure Int =
  struct

    structure PT = PrimTypes

    type int = int

    _primcode (

      extern void *M_IntToString (int) __attribute__((alloc,pure));

      define @to-string (n : PT.ml_int / exh : PT.exh) : PT.ml_string =
	  let res : PT.ml_string = ccall M_IntToString (unwrap(n))
	    return (res)
      ;

    )

    val toString : int -> string = _prim(@to-string)

  end
