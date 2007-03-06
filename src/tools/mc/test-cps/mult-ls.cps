(* mult-ls.cps
 * Multiply a list of integers, short circuiting the operation upon encountering
 * a zero element *)

module MultLs (arg : any, mk : cont(any), exh : cont(any)) =

  fun multLs (ls : (any, any), k : cont(int), exh : cont(any)) =
      if I64Eq (ls, 0) then throw k(1)
      else let hd : int = #0(ls)
           if I32Eq (hd, 0 : int) then throw k(0)
	   else 
               let tl : any = #1(ls)
	       cont k1 (prod : int) =
		    let prod' : int = I32Mul (hd, prod)
                    throw k(prod')
               apply multLs (tl, k1, exh)

    (* returns the list [i,..,1] *)
  fun tabulate (i : int, k : cont(any), exh : cont(any)) =
      if I32Lte (i, 0:int) then throw k(0)
      else cont k1 (l : any) =
	        let l' : (int,any) = alloc (i, l)
                let l' : any = (any)l'
                throw k(l')
           apply tabulate (I32Sub (i, 1:int), k1, exh)

  fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)
      cont k1 (ls : any) =
	   let ls' : (any, any) = alloc (0 : int, ls)
           cont k2 (i : int) = 
                let wi : [int] = wrap (i)
                throw k(wi)
	   apply multLs (ls', k2, exh)
      apply tabulate (i, k1, exh)

  throw mk(doit)
