(* float2.cps *)

module Float2
  fun init (arg : any, mk : cont(any), exh : cont(any)) =

    fun iterate (ar : (int, float), k : cont([float]), exh : cont(any)) =
	let i : int = #0(ar)
	let f : float = #1(ar)
	switch i
          case 0 : 
	    let wf : [float] = wrap (f)
	    throw k(wf)
          default :
            let f1 : float = 2.3
	    let f' : float = F32Add (f, f1)
            let i1 : int = 1
            let i': int = I32Sub (i, i1)
            let ar' : (int, float) = alloc (i', f')
            apply iterate (ar', k, exh)
	 end

     fun doit (wi : [int], k : cont([float]), exh : cont(any)) =
	 let i : int = unwrap (wi)
	 let f : float = 0.1
	 let arg : (int, float) = alloc (i, f)
	 apply iterate (arg, k, exh)

     throw mk(doit)
