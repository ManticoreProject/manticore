(* string.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure String =
  struct

    structure PT = PrimTypes

    _primcode(
      typedef ml_string = PT.ml_string;
  
      define  @data (s : ml_string / exh : PT.exh) : any =
	  let res : any = #0(s)
	    return (res)
      ;

      define  @lit (s : PT.string_data, len : int / exh : PT.exh) : ml_string =
	  let res : ml_string = alloc (s, len)
	    return (res)
      ;

      define  @size (s : ml_string / exh : PT.exh) : PT.ml_int =
	  let len : int = #1(s)
	  let res : PT.ml_int = alloc(len)
	    return (res)
      ;


    )

  end
