(* float1.cps *)

module Float1
  fun init (arg : any, mk : cont(any), exh : cont(any)) =
    fun retFloat (i : int, k : cont([float]), exh : cont(any)) =
	let f : float = 3.14
	let f' : float = F32Add (f, f)
	let wf : [float] = wrap (f')
	throw k(wf) 
    fun rf (i : int, k : cont([int]), exh : cont(any)) =
	cont k1 (f : float) =
             let wi : [int] = wrap (i)
             throw k(wi)
	apply retFloat (i, k1, exh) 
    fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
	let i : int = unwrap (wi)
	apply rf (i, k, exh)
    throw mk(doit)
