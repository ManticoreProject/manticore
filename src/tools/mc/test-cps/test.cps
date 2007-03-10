(* test.cps *)

module Test (arg : any, mk : cont(any), exh : cont(any)) =
  fun doit (wi :[int], k : cont([int]), exh : cont(any)) =
      let i : int = 42
      let wi' : [int] = wrap (i)
      throw k(wi)
  throw mk(doit)
      
