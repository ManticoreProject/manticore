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
	     of List.NIL => return()
	      | L.CONS(x : any, xs : list) =>
		do apply f(x / exh)
		apply lp(f, xs / exh)
	    end
	apply lp(f, ls / exh)
      ;

      define @rev (xs : list / exh : PT.exh) : List.list =
	fun rev (xs : List.list, ys : List.list / exh : PT.exh) : List.list =
	    case xs
	     of List.NIL => return(ys)
	      | List.CONS(x : any, xs : List.list) => apply rev(xs, List.CONS(x, ys) / exh)
	    end
	apply rev(xs, List.NIL / exh)
      ;

      define @append (l1 : List.list, l2 : List.list / exh : PT.exh) : List.list =
	  fun append (l1 : List.list / exh : PT.exh) : List.list =
		case l1
		 of List.CONS(hd:any, tl:List.list) =>
		      let l : List.list = apply append (tl / exh)
			return (List.CONS(hd, l))
		  | List.NIL => return (l2)
		end
	    apply append (l1 / exh)
      ;

    )


  end
