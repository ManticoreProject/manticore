(* curry1.pml --- test uncurry optimization *)

(* should be
    fun f (x : int) y z = x*y+z
*)
fun f (x : int) = let fun f' y = let fun f'' z = x*y+z in f'' end in f' end;

type point3 = (int * int * int);

fun dot ((x1, y1, z1), (x2, y2, z2)) = f x1 x2 (f y1 y2 (f z1 z2 0));

val g = f 1 2;

val x = if (dot((1,2,3), (2, 3, 4)) > g 3) then Print.print "true\n" else Print.print "false"

