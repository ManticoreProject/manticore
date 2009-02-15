(* prim-stk.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In-place stack.
 *)

structure PrimStk =
  struct

    _primcode (

      typedef stk = ![List.list];

      define @new (x : unit / exh : exh) : stk =
	let stk : stk = alloc(List.nil)
	let stk : stk = promote(stk)
	return(stk)
      ;

      define @push (stk : stk, elt : any / exh : exh) : () =
	let hd : List.list = List.CONS(elt, #0(stk))
	let hd : List.list = promote(hd)
	do #0(stk) := hd
	return()
      ;

      define @push-w (arg : [stk, any] / exh : exh) : unit =
	do @push(#0(arg), #1(arg) / exh)
	return(UNIT)
      ;

      define @pop (stk : stk / exh : exh) : Option.option =
	case #0(stk)
	 of List.nil => 
	    return(Option.NONE)
	  | List.CONS(x : any, xs : List.list) =>
	    do #0(stk) := xs
	    return(Option.SOME(x))
	end
      ;

      define @peek (stk : stk / exh : exh) : Option.option =
	case #0(stk)
	 of List.nil => 
	    return(Option.NONE)
	  | List.CONS(x : any, xs : List.list) =>
	    return(Option.SOME(x))
	end
      ;

      define @copy (stk : stk / exh : exh) : stk =
	let stk' : stk = alloc(#0(stk))
	let stk' : stk = promote(stk')
	return(stk')
      ;

    )

    type 'a stk = _prim(stk)

    val new : unit -> 'a stk = _prim(@new)
    val push : ('a stk * 'a) -> unit = _prim(@push-w)
    val pop : 'a stk -> 'a Option.option = _prim(@pop)
    val peek : 'a stk -> 'a Option.option = _prim(@peek)
    val copy : 'a stk -> 'a stk = _prim(@copy)

  end
