(* map2.cps 
 * Take an integer i, create a list [i,..,1], and add 1 to each element of the list.
 * Lists have the basic cons-cell representation: (elt, ptr).
 *)

module Map2 (arg : any, mk : cont(any), exh : cont(any)) =

  fun add1 (i : int, k : cont(int), exh : cont(any)) = 
      throw k(I32Add (i, 1 : int))

  fun map (a : (fun(int, cont(int), cont(any)), any), 
	   k : cont(any), exh : cont(any)) =
      let f : fun(int, cont(int), cont(any)) = #0(a)
      let l0 : any = #1(a)
      fun map' (l : (int, any), k : cont(any), exh : cont(any)) =
	  switch l
	    case 0 : throw k(0)
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
      if I32Lte (i, 0:int) then throw k(0)
      else cont k1 (l : any) =
	        let l' : (int, any) = alloc (i, l)
                let l' : any = (any)l'
                throw k(l')
           apply tabulate (I32Sub (i, 1:int), k1, exh)

  fun doit (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)
      cont k1 (l : any) =
	   let a : (fun(int,cont(int),cont(any)), any) = alloc (add1, l)
           let a : any = (any)a
	   apply map (a, k, exh)
      apply tabulate (i, k1, exh)

  throw mk(doit)
  
