(* sum-ls-foldr.cps 
 * Given an integer i, create the list [i,..,1], and return the summation of
 * the list (using foldr).
 *)

module SumLsFoldr (arg : any, mk : cont(any), exh : cont(any)) =

  fun add (p : (int, int), k : cont(int), exh : cont(any)) =
      let x : int = #0(p)
      let y : int = #1(p)
      throw k(I32Add (x, y))

  (* returns the list [i,..,1] *)
  fun tabulate (i : int, k : cont(any), exh : cont(any)) =
      if I32Lte (i, 0:int) then throw k(0)
      else cont k1 (l : any) =
	        let l' : any = alloc (i, l)
                throw k(l')
           apply tabulate (I32Sub (i, 1:int), k1, exh)

  fun foldr (p : (fun((any,any), cont(int), cont(any)), any, any),
	     k : cont(any), exh : cont(any)) =
      let f : fun((any,any), cont(int), cont(any)) = #0(p)
      let ls : (any,any) = #1(p)
      let acc : any = #2(p)
      if I64Eq (ls, 0) then throw k(acc)
      else let hd : any = #0(ls)
           let tl : any = #1(ls)
           cont k1 (acc' : any) =
                let a : (any,any) = alloc (hd, acc')
                apply f (a, k, exh)
           let a : any = alloc (f, tl, acc)
           apply foldr (a, k1, exh)

  fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)
      cont k1 (ls : any) =
	   let a : any = alloc (add, ls, 0)
           cont k2 (i : int) = 
                let wi : [int] = wrap (i)
                throw k(wi)
	   apply foldr (a, k2, exh)
      apply tabulate (i, k1, exh)

  throw mk(doit)
