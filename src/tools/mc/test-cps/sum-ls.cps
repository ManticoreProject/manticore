(* sum-ls.cps 
 * Given an integer i, create the list [i,..,1], and return the summation of
 * the list (using foldl).
 *)

module SumLs
  fun init (arg : any, mk : cont(any), exh : cont(any)) =

    fun add (p : (int, int), k : cont(int), exh : cont(any)) =
	let x : int = #0(p)
	let y : int = #1(p)
	throw k(I32Add (x, y))

    (* returns the list [i,..,1] *)
    fun tabulate (i : int, k : cont(any), exh : cont(any)) =
	if I32Lte (i, 0:int) then throw k(0)
	else cont k1 (l : any) =
	          let l' : (int,any) = alloc (i, l)
                  let l' : any = (any)l'
                  throw k(l')
             apply tabulate (I32Sub (i, 1:int), k1, exh)

    fun foldl (p : (fun((any,any), cont(int), cont(any)), any, any),
	       k : cont(any), exh : cont(any)) =
	let f : fun((any,any), cont(int), cont(any)) = #0(p)
	let ls : (any,any) = #1(p)
	let acc : any = #2(p)
	if I64Eq (ls, 0) then throw k(acc)
	else let hd : any = #0(ls)
             let tl : any = #1(ls)
             let a : (any, any) = alloc (hd, acc)
             cont k1 (x : any) =
                  let a : (fun((any,any),cont(int),cont(any)),any,any) = alloc (f, tl, x)
                  let a : any = (any)a
                  apply foldl (a, k, exh)
             apply f (a, k1, exh)

    fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
	let i : int = unwrap (wi)
	cont k1 (ls : any) =
	     let a : (fun((int,int),cont(int),cont(any)),any,int) = alloc (add, ls, 0)
             let a : any = (any)a
             cont k2 (i : int) = 
                  let wi : [int] = wrap (i)
                  throw k(wi)
	     apply foldl (a, k2, exh)
	apply tabulate (i, k1, exh)

    throw mk(doit)
