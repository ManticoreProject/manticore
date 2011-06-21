(* cycle-counter.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Machine-dependent cycle counters.
 *
 * The type ticks is an opaque type that represents the current
 * time. You can obtain the time elapsed between two calls of
 * getTicks() by calling
 * 
 *         elapsed (t1, t2)
 * 
 * whose return value is a positive 64-bit integer in arbitrary
 * units. Thes elapsed time is useful only for comparisons, not
 * conversions into human units like seconds, because the units are
 * arbitrary. Also, note that, in some machines, the units can change
 * dynamically because of CPU frequency adjustment.
 *
 *)

structure CycleCounter (* :> sig
    type ticks
    val getTicks : unit -> ticks
    val elapsed : ticks * ticks -> Word64.word
  end *) = struct

    _primcode (
      define @get-ticks (_ : unit / exh : exh) : ml_long =
          let t : long = TimeStampCounter ()
	  return (alloc (t))
	;
    )

    type ticks = Word64.word

    val getTicks : unit -> ticks = _prim (@get-ticks)
    fun elapsed (t1:Word64.word, t2) = Word64.sub (t2, t1)

end
