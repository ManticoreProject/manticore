structure AssocList =
  struct

    structure O = Option

    _primcode (
      define @tst(x : enum(0) / exh : cont(any)) : Option.option =
        return(Option.SOME(alloc(123)))
      ;
    )

    fun o2s opt = (case opt
           of O.SOME i => itos i
	    | Option.NONE => "none"
		  )

    val f = _prim (hlop @tst)

    val _ = print (o2s (f())^"\n")

  end
