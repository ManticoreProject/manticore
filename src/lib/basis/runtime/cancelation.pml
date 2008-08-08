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
