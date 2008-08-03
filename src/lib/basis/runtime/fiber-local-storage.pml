structure FiberLocalStorage =
  struct

    structure PT = PrimTypes
    structure AL = AssocList

    type fls = _prim ( ![PT.bool, AL.assoc_list] ) 

    _primcode (

    (* create fls *)
      define @new (x : PT.unit / exh : PT.exh) : fls =
        let fls : fls = alloc(TRUE, NIL)
        let fls : fls = promote(fls)
        return(fls)
      ;

    (* set the fls on the host vproc *)
      define @set (fls : fls / exh : PT.exh) : PT.unit =
        do assert(NotEqual(fls, NIL))
        do vpstore (CURRENT_FG, host_vproc, fls)
        return(UNIT)
      ;

    (* get the fls from the host vproc *)
      define @get ( / exh : PT.exh) : fls =
        let fls : fls = vpload (CURRENT_FG, host_vproc)
        do assert(NotEqual(fls, NIL))
        return(fls)
      ;

    (* add an element to the fiber-local storage dictionary. NOTE: this function is not thread safe. *)
      define @add (fls : fls, tg : AL.assoc_tag, elt : any / exh : PT.exh) : PT.unit =
        let als : AL.assoc_list = #1(fls)
        let als : AL.assoc_list = AL.@insert(als, tg, elt / exh)
        let als : AL.assoc_list = promote(als)
        do #1(fls) := als
        return(UNIT)
      ;

    (* find an entry in the fiber-local storage *)
      define @find (fls : fls, tg : AL.assoc_tag, elt : any / exh : PT.exh) : Option.option =
        let als : AL.assoc_list = #1(fls)
        AL.@find(als, tg / exh)
      ;

    )

    val _ = print "primcode\n"

  end
