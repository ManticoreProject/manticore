(* ref.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Ref cells are needed by the pcase code generator.
 *)

structure Ref =
  struct

    _primcode (

      typedef ref = ![any];

    (* allocate and initialize a ref cell *)
      define inline @new (x: any / exh: exh) : ref =
        let cell : ref = alloc(x)
        let cell': ref = promote(cell)
        return(cell')
        ;

    (* destructive update of the cell *)
      define inline @set (r: ref, x: any / exh: exh) : unit =
        let x' : any = promote(x)
        do #0(r) := x'
        return(UNIT)
        ;

    (* read the value out of the cell *)
      define inline @get (r: ref / exh: exh) : any = 
        let x : any = #0(r)
        return(x)
        ;
    )

    type 'a ref = _prim(ref)

    val new : 'a -> 'a ref        = _prim(@new)
    val set : 'a ref * 'a -> unit = _prim(@set)
    val get : 'a ref -> 'a        = _prim(@get)

  end
