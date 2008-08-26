structure PrimList =
  struct

    structure L = List
    structure PT = PrimTypes

  (* list utilities for inline BOM *)
    _primcode (

      typedef list = List.list;

      define @app (f : fun(any / PT.exh -> ), ls : List.list / exh : PT.exh) : () =
	fun lp (f : fun(any / PT.exh -> ), xs : List.list / exh : PT.exh) : () =
	    case xs
	     of NIL => return()
	      | L.CONS(x : any, xs : list) =>
		do apply f(x / exh)
		apply lp(f, xs / exh)
	    end
	apply lp(f, ls / exh)
      ;

    )


  end
