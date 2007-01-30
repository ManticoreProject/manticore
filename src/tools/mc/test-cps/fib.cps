(* fib.cps *)

module Fib (mk : fun(any)) =
  fun fib (i : [int], k : fun(int), exh : any) =
	fun fib' (i : int, k : fun(int)) =
	      if I32Lte(i, 0 : int) then throw k(0 : int)
	      else if I32Eq(i, 1 : int) then throw k(1 : int)
	      else
		cont k' (a : int) =
		      cont k'' (b : int) = throw k (I32Add(a, b))
		      apply fib' (I32Sub(i, 2 : int), k'')
		apply fib' (I32Sub(i, 1 : int), k')
	cont k''' (i : int) =
		let wi : [int] = wrap(i)
		throw k (wi)
	apply fib' (unwrap(i), k''')
  throw mk(fib)
