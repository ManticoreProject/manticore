(* runtime-init.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Runtime initialization that occurs just once, after entering the Manticore root
 * thread but before running any scheduling code.
 *)

local

  _primcode(

    define @runtime-init (x : unit / exh : exh) : unit = 
      (* assert that signals are masked *)
	let mask : bool = vpload(ATOMIC, host_vproc)
	do assert(mask)
      (* set the FLS for the root thread *)
	let fls : FLS.fls = FLS.@new (UNIT / exh)
	do FLS.@set(fls)
	return (UNIT)
      ;

  )

  val runtimeInit : unit -> unit = _prim(@runtime-init)

in
  val () = runtimeInit()
end
