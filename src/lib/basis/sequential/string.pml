structure String =
  struct

    structure PT = PrimTypes

    _primcode(
      typedef string_data = any;
      typedef ml_string = [string_data, int];

      define  @data (s : ml_string / exh : PT.exh) : any =
	  let res : any = #0(s)
	    return (res)
      ;

      define  @lit (s : string_data, len : int / exh : PT.exh) : ml_string =
	  let res : ml_string = alloc (s, len)
	    return (res)
      ;

      define  @size (s : ml_string / exh : PT.exh) : Int.ml_int =
	  let len : int = #1(s)
	  let res : Int.ml_int = alloc(len)
	    return (res)
      ;


    )

  end
