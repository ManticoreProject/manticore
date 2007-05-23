(* overload.sml
 *
 * Utility functions for dealing with overload resolution.
 *)

structure Overload =
struct

val vars = ref []

fun add ov = vars := (ov :: vars)

end
