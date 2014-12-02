(* special-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Special Types for allocation.
 *)

structure SpecialTypes : sig

	type ty

	val name : ty -> string

	val proxyTy : ty

  end = struct

	(* int for the entryin the header table *)
	type ty = int
	
	val proxyTy = 4
	
	
	fun name k = (case k 
	    of 4 => "PROXY"
	     | _ => raise Fail("bogus type in SpecialTypes " ^ Int.toString k)
	    (* end case *))
	    
  end
