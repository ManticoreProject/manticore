(* cancelation.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Cancelation =
  struct

    structure PT = PrimTypes

    _primcode (
      typedef cancelable = any;  (* placeholder *)

      define @mk-cancelable (c : cancelable, k : PT.fiber / exh : PT.exh) : PT.fiber =
        return(k)
      ;
    )

  end
