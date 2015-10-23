(* amd64-extension.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure AMD64Extension =
  struct

  datatype ('s,'r,'f,'c) sx = EXT of ('s,'r,'f,'c) AMD64InstrExt.sext
			    | LOCK_ANDL of 'r * 'r
			    | LOCK_ANDQ of 'r * 'r
			    | LOCK_ORL of 'r * 'r
			    | LOCK_ORQ of 'r * 'r
	   
  type ('s,'r,'f,'c) rx = unit
  type ('s,'r,'f,'c) fx = unit
  type ('s,'r,'f,'c) ccx = unit

  end
