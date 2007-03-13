(* fib.cps *)

module Fib (wi : [int], k : cont([int]), exh : cont(any)) =

  fun fib (i : int, k : cont(int), exh : cont(any)) =
      if I32Lte(i, 0:int) then throw k(0:int)
      else if I32Eq(i, 1:int) then throw k(1:int)
      else
	  cont k1 (a : int) =
	       cont k2 (b : int) = throw k (I32Add(a, b))
		    apply fib (I32Sub(i, 2:int), k2, exh)
          apply fib (I32Sub(i, 1:int), k1, exh)

  cont kWrap (i : int) =
       let wi : [int] = wrap(i)
        throw k (wi)
  apply fib (unwrap(wi), kWrap, exh)
