(* mutual-recursion.cps *)

module MutualRec (arg : any, mk : cont(any), exh : cont(any)) =

  fun outer (a : (int, int), k : cont(int), exh : cont(any)) =
      let i : int = #0(a)
      let n : int = #1(a)
      
      fun f (j : int, k : cont(int), exh : cont(any)) =
	  let x : int = I32Add (i, j)
          apply g (x, k, exh)

      and g (j : int, k : cont(int), exh : cont(any)) =
	  if I32Gte (j, n)
	  then throw k(j)
	  else apply f (j, k, exh)
		     
      apply f (0 : int, k, exh)


  fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)
      cont k1 (i : int) =
           let wi : [int] = wrap (i)
           throw k(wi)
      let a : (int, int) = alloc (1 : int, i)
      apply outer (a, k1, exh)

  throw mk(doit)
