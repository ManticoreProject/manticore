(* amd64-extension.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure AMD64Extension =
  struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) AMD64InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

  end
