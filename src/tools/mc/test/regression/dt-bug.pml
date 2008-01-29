type vec = (double * double * double);
datatype Sphere = Sphere of vec * double * double list (* pos, radius, surface type *)
		| Pt1 of int
		| TheVoid; 

fun vecsum (x,y,z) = x+y+z;

fun t sp = let
    fun unpack1 sp = (case sp
        of Sphere(center, rad, _) => center)
    val center = unpack1 sp
    in
        vecsum center
    end
;

val _ = print (dtos (t (Sphere ((1.0,1.0,1.0), 1.0, nil)))^"\n");
val _ = print (dtos (t (Sphere ((1.0,2.0,1.0), 1.0, nil)))^"\n");
()
