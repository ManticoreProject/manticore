type vec = (double * double * double);
datatype Sphere = Sphere of vec * double * double list; (* pos, radius, surface type *)

fun vecscale ((x,y,z) : vec) = let fun scale a = (a*x, a*y, a*z) in scale end;
fun vecsub ((x1,y1,z1) : vec) = let fun sub (x2,y2,z2) = (x1-x2, y1-y2, z1-z2) in sub end;
fun vec2s ( (x1,y1,z1):vec) = "["^dtos x1^", "^dtos y1^", "^dtos z1^"]";
fun vecdot ((x1,y1,z1) : vec) = let fun dot (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2 in dot end;

fun t (sp, spos) = let
    fun unpack1 sp = (case sp
        of Sphere(center, rad, _) => center)
    fun unpack2 sp = (case sp
        of Sphere(center, rad, _) => rad)

    val pos = unpack1 sp
    val rad = unpack2 sp

    val m = vecscale (vecsub pos pos) (1.0/rad)
    val y = vecdot m m
    val x = vecscale pos (1.0/y)
    in
        x
    end
;

val spos = (2.1,1.1,3.2);
val _ = print (vec2s (t (Sphere ((1.0,1.0,1.0), 1.0, nil), spos)));
()
