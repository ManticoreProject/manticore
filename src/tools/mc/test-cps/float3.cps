(* float3.cps *)

module Float3 (arg : any, mk : cont(any), exh : cont(any)) =
  fun iterate (ar : (int, float), k : cont([float]), exh : cont(any)) =
      let i : int = #0(ar)
      let fl : float = #1(ar)
      switch i
        case 0:
           let wf : [float] = wrap (fl)
           throw k (wf)
        case 1:
           let f1 : float = 2.3
           let fl' : float = F32Add (fl, f1)
           let wf : [float] = wrap (fl')
           throw k (wf)
      end
   fun doit (wi : [int], k : cont([float]), exh : cont(any)) =
       let i : int = unwrap (wi)
       let f : float = 0.1
       let arg : (int, float) = alloc (i, f)
       apply iterate (arg, k, exh)
   throw mk(doit)
