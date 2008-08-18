(* fiber-local-storage.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Local storage for fibers. 
 *
 * NOTE: these functions are not synchronized.
 *)


structure FiberLocalStorage =
  struct

    structure PT = PrimTypes
    structure AL = AssocList

    type fls = _prim ( [PT.bool, AL.assoc_list] ) 

    _primcode (

      typedef fls_tag = AL.assoc_tag;

    (* create fls *)
      define @new (x : PT.unit / exh : PT.exh) : fls =
        let fls : fls = alloc(TRUE, NIL)
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
      define @add (fls : fls, tg : fls_tag, elt : any / exh : PT.exh) : fls =
        let als : AL.assoc_list = AL.@insert(#1(fls), tg, elt / exh)
        let fls : fls = alloc(#0(fls), als)
        return(fls)
      ;

    (* find an entry in the fiber-local storage *)
      define @find (fls : fls, tg : fls_tag / exh : PT.exh) : Option.option =
        AL.@find(#1(fls), tg / exh)
      ;

    )

  end
