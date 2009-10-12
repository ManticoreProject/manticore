(* vector2.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * 2 dimensional vector arithmetic.
 *)

structure Vector2 (* : VECTOR *) =
  struct

    type 'a vec = ('a * 'a)
    type double_vec = Double.double vec

    val dim = 2

    fun tabulate f = (f 0, f 1)

    val zerov : double_vec = (0.0, 0.0)

    fun addv ((x1, y1) : double_vec, (x2, y2)) = (x1+x2, y1+y2)

    fun subv ((x1, y1) : double_vec, (x2, y2)) = (x1-x2, y1-y2)

    fun dotvp ((x1, y1) : double_vec, (x2, y2)) = x1*x2 + y1*y2

    fun crossvp ((x1, y1) : double_vec, (x2, y2)) = (x1*y2, x2*y1)

    fun addvs ((x, y) : double_vec, s) = (x+s, y+s)

    fun mulvs ((x, y) : double_vec, s) = (x*s, y*s)

    fun divvs ((x, y) : double_vec, s) = (x/s, y/s)

  end
