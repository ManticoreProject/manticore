(* map.cps *)

module Map (arg : any, mk : cont(any), exh : cont(any)) =
  fun map (fl : any, k : cont(any), exh : cont(any)) =
    let f : fun(any,cont(any),cont(any)) = #0(((fun(any,cont(any),cont(any)),any))fl)
    let l : any = #1(((fun(any,cont(any),cont(any)),any))fl)
    fun map' (l : any, k : cont(any), exh : cont(any)) =
      let tag : int = #0(((int,any))l)
      switch tag
        case 1 : let tag' : int = 0
                 let l' : any = alloc(tag')
                 throw k(l')
        default : let cons : any = #1(((int,any))l)
                  let hd : any = #0(((any,any))cons)
                  let tl : any = #1(((any,any))cons)
                  cont k' (hd' : any) =
                    cont k'' (tl' : any) =
                      let cons' : any = alloc(hd', tl')
                      let tag' : int = 1
                      let l' : any = alloc(tag', cons')
                      throw k(l')
                    apply map' (tl, k'', exh)
                  apply f (hd, k', exh)
        end
    apply map' (l, k, exh)
  throw mk(map)
