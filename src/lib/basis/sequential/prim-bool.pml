(* prim-bool.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure PrimBool =
  struct

    _primcode (

      define @not (arg : bool) : bool = 
	  case arg of true => return(false) | false => return(true) end
	;

      define @and (b1 : bool, b2 : bool) : bool =
	  case b1 of false => return (false) | true => return (b2) end
	;

      define @or (b1 : bool, b2 : bool) : bool =
	  case b1 of false => return (b2) | true => return (true) end
	;

    )

  end
