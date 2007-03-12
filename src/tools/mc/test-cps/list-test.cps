(* list-test.cps 
 * Given an integer i, create the list [i,..,1], then do the following
 *    - add 1 to each element
 *    - sum over the list using foldr
 *)

module ListTest (arg : any, mk : cont(any), exh : cont(any)) =

  let nil : any = 1

  fun add (p : (int, int), k : cont(int), exh : cont(any)) =
      let x : int = #0(p)
      let y : int = #1(p)
      throw k(I32Add (x, y))

  fun add1 (i : int, k : cont(int), exh : cont(any)) = 
      let i' : int = I32Add (i, 1 : int)
      throw k(i')

  fun map (a : (fun(int, cont(int), cont(any)), any), 
	   k : cont(any), exh : cont(any)) =
      let f : fun(int, cont(int), cont(any)) = #0(a)
      let l0 : any = #1(a)
      fun map' (l : (int, any), k : cont(any), exh : cont(any)) =
	  switch l
	    case 1 : throw k(nil)
            default :
               let hd : int = #0(l)
               let tl : any = #1(l)
               cont k1 (hd' : int) =
                    cont k2 (tl' : any) =
                         let l' : (int, any) = alloc (hd', tl')
                         let l' : any = (any)l'
                         throw k(l')
                    apply map' (tl, k2, exh)
               apply f (hd, k1, exh)
          end
      apply map' (l0, k, exh)

  (* returns the list [i,..,1] *)
  fun tabulate (i : int, k : cont(any), exh : cont(any)) =
      if I32Lte (i, 0:int) then throw k(nil)
      else cont k1 (l : any) =
	        let l' : (int,any) = alloc (i, l)
                let l' : any = (any)l'
                throw k(l')
           apply tabulate (I32Sub (i, 1:int), k1, exh)

  fun foldr (p : (fun((any,any), cont(int), cont(any)), any, any),
	     k : cont(any), exh : cont(any)) =
      let f : fun((any,any), cont(int), cont(any)) = #0(p)
      let ls : (any,any) = #1(p)
      let acc : any = #2(p)
      if I64Eq (ls, nil) then throw k(acc)
      else let hd : any = #0(ls)
           let tl : any = #1(ls)
           cont k1 (acc' : any) =
                let a : (any,any) = alloc (hd, acc')
                apply f (a, k, exh)
           let a : (fun((any,any),cont(int),cont(any)),any,any) = alloc (f, tl, acc)
           let a : any = (any)a
           apply foldr (a, k1, exh)

  fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)      
      cont k1 (ls : any) =
	   let a : (fun(int,cont(int),cont(any)), any) = alloc (add1, ls)
           let a : any = (any)a
           cont k2 (ls : any) = 
   	        let a : (fun((int,int),cont(int),cont(any)),any,int) = alloc (add, ls, 0)
                let a : any = (any)a
                cont k3 (i : int) = 
                     let wi : [int] = wrap (i)
                     throw k(wi)
	        apply foldr (a, k3, exh)
           apply map (a, k2, exh)
      apply tabulate (i, k1, exh)

  throw mk(doit)
