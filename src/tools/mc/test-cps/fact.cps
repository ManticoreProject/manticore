(* fact.cps *)

module Fact
  fun init (i : [int]; k : cont([int]), exh : cont(any)) =

    fun fact (i : int; k2 : cont(int), exh : cont(any)) =
	if I32Lte (i, 1:int) 
          then throw k2(1:int)
	  else cont k3 (i' : int) = throw k2 (I32Mul (i', i))
	            apply fact (I32Sub (i, 1:int); k3, exh)

    cont kWrap (i : int) =
	 let wi : [int] = wrap(i)
         throw k(wi)

    let n : int = 10
    apply fact (n; kWrap, exh)
