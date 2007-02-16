(* add1.cps *)

module Add1 (arg : any, mk : cont(any), exh : cont(any)) =
  fun add1' (i : int, k : cont(int), exh : cont(any)) = 
      throw k(I32Add (i, 1 : int))
  fun add1 (wi : [int], k : cont([int]), exh : cont(any)) =
      let i : int = unwrap (wi)
      cont k1 (i' : int) =
           let wi : [int] = wrap (i')
           throw k(wi)
      apply add1' (i, k1, exh)
  fun doit (wi :[int], k : cont([int]), exh : cont(any)) =
      apply add1 (wi, k, exh)
  throw mk(doit)
