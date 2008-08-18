(* default-thread-capabilities.pml
 *
 * Seed the default capabilities for a thread.
 *)

structure DefaultThreadCapabilities =
  struct

    structure PT = PrimTypes
    structure TC = ThreadCapabilities
    structure FLS = FiberLocalStorage

    _primcode(

      define @defaults (fls : FLS.fls / exh : PT.exh) : FLS.fls =
	let c0 : TC.capability = Future1.@capability-init(/ exh)
        let fls : FLS.fls = TC.@add(fls, c0 / exh)
	return(fls)
      ;

      define @init (x : PT.unit / exh : PT.exh) : PT.unit = 
	let fls : FLS.fls = FLS.@get(/ exh)
	let fls : FLS.fls = @defaults(fls / exh)
	let _ : PT.unit = FLS.@set(fls / exh)
	return(UNIT)
      ;

    )

  (* seed the initial thread with the default capabilities *)
    val init : unit -> unit = _prim(@init)
    val _ = init()

  end
