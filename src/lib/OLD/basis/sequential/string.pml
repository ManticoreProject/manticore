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

      extern void *M_StringConcatList (void *) __attribute__((pure,alloc));
  
      define inline @data (s : ml_string / exh : PT.exh) : any =
	  let res : any = #0(s)
	    return (res)
      ;

      define inline @lit (s : PT.string_data, len : int / exh : PT.exh) : ml_string =
	  let res : ml_string = alloc (s, len)
	    return (res)
      ;

      define inline @size (s : ml_string / exh : PT.exh) : PT.ml_int =
	  let len : int = #1(s)
	  let res : PT.ml_int = alloc(len)
	    return (res)
      ;

      define inline @string-concat-list (arg : List.list / exh : PT.exh) : ml_string =
	  let res : ml_string = ccall M_StringConcatList (arg)
	    return (res)
      ;

    )

    val concat : string list -> string = _prim(@string-concat-list)
    val size : string -> int = _prim(@size)

    fun concatWith s ss = let
	  fun lp xs = (case xs
		 of nil => nil
		  | x :: nil => x :: nil
		  | x :: xs => x :: s :: lp xs
		(* end case *))
	  in
	    concat(lp ss)
	  end

  end
