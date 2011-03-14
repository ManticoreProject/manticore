(* int-ref.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

#include <prim.def>

structure IntRef =
  struct

    _primcode (

      typedef ref = ![int];

      define inline @new (x : ml_int / exh: exh) : ref =
        let cell : ref = alloc(#0(x))
        return(cell)
        ;

      define inline @set (args : [ref, ml_int] / exh: exh) : unit =
        let r : ref = #0(args)
        let x : int = #0(#1(args))
        do #0(r) := x
        return(UNIT)
        ;

      define inline @get (r: ref / exh: exh) : ml_int = 
        return(alloc(#0(r)))
        ;
    )

    type ref = _prim(ref)

    val new : int -> ref        = _prim(@new)
    val set : ref * int -> unit = _prim(@set)
    val get : ref -> int        = _prim(@get)

    fun incr r = let
      val x = get r
      in
	set (r, x + 1)
      end

    fun decr r = let
      val x = get r
      in
	set (r, x - 1)
      end

  end
