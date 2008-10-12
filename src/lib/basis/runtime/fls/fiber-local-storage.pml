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
    structure L = List

  (* see initial-basis.pml for the definition of fiber_local_storage *)
    type fls = _prim (fiber_local_storage) 

    #define PINNED_OFF 0
    #define ASSOC_OFF  1

    _primcode (

      typedef fls_tag = AL.assoc_tag;

      define inline @new (x : PT.unit / exh : PT.exh) : fls =
        let fls : fls = alloc(PT.false, nil)
        return(fls)
      ;

      define inline @new-pinned (x : PT.unit / exh : PT.exh) : fls =
        let fls : fls = alloc(PT.true, nil)
        return(fls)
      ;

    (* set the fls on the host vproc *)
      define inline @set (fls : fls / exh : PT.exh) : PT.unit =
        do assert(NotEqual(fls, nil))
        do vpstore (CURRENT_FG, host_vproc, fls)
        return(UNIT)
      ;

    (* get the fls from the host vproc *)
      define inline @get ( / exh : PT.exh) : fls =
        let fls : fls = vpload (CURRENT_FG, host_vproc)
        do assert(NotEqual(fls, nil))
        return(fls)
      ;

    (* add an element to the fiber-local storage dictionary. NOTE: this function is not thread safe. *)
      define inline @add (fls : fls, tg : fls_tag, elt : any / exh : PT.exh) : fls =
        let als : AL.assoc_list = AL.@insert(SELECT(ASSOC_OFF, fls), tg, elt / exh)
        let fls : fls = alloc(SELECT(PINNED_OFF, fls), als)
        return(fls)
      ;

    (* find an entry in the fiber-local storage *)
      define inline @find (fls : fls, tg : fls_tag / exh : PT.exh) : Option.option =
        AL.@find(SELECT(ASSOC_OFF, fls), tg / exh)
      ;

      define inline @is-pinned (fls : fls / exh : PT.exh) : PT.bool =
	return(SELECT(PINNED_OFF, fls))
      ;

      define inline @set-pinned ( / exh : PT.exh) : () =
        let fls : fls = @get(/ exh)
	let fls : fls = alloc(PT.true, SELECT(ASSOC_OFF, fls))
	let _ : PT.unit = @set(fls / exh)
        return()
      ;

    )

  end
