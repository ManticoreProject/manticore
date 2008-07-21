structure FiberLocalStorage =
  struct

    structure PT = PrimTypes
    structure AL = AssocList

    type fls = _prim ( ![PT.bool, AL.assoc_list] ) 

    _primcode (
      define @new (x : PT.unit / exh : PT.exh) : fls =
        return(promote(alloc(TRUE, NIL)))
      ;
    )

    val _ = print "primcode\n"

  end
