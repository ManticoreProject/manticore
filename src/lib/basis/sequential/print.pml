structure Print =
  struct

    structure PT = PrimTypes

    _primcode(
      extern void M_Print(void*);
      define @print (s : String.ml_string / exh : PT.exh) : PT.unit =
	  let data : any = hlop String.@data(s / exh)
	  do ccall M_Print (data)
	    return (UNIT)
      ;
    )

    val print : string -> unit = _prim(hlop @print)

  end
