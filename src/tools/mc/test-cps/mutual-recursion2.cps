(* mutual-recursion2.cps *)

module MutualRec2
  fun init (arg : any, mk : cont(any), exh : cont(any)) =

  (* "outer" returns i^n *)
    fun outer (a : (int, int), k : cont(int), exh : cont(any)) =
	let i : int = #0(a)
	let n : int = #1(a)

	fun f (j : int, k : cont(int), exh : cont(any)) =
            cont k1 (x : int) =
        	 let x' : int = I32Mul (i, x)
        	 throw k(x')
            apply g (j, k1, exh)

	and g (j : int, k : cont(int), exh : cont(any)) =
	    if I32Gte (j, n)
	    then throw k(1 : int)
	    else let j' : int = I32Add (j, 1 : int)
		 apply f (j', k, exh)

	apply f (1 : int, k, exh)


    fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
	let i : int = unwrap (wi)
	cont k1 (i : int) =
             let wi : [int] = wrap (i)
             throw k(wi)
	let a : (int, int) = alloc (2 : int, i)
	apply outer (a, k1, exh)

    throw mk(doit)
