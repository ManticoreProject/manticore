(* A test of the sequential basis function map. *)
fun y n = (n>0);
val ns = 0::1::nil;
val ys = map (y, ns);
case ys 
  of (s :: ss) => print ("n\n")
   | _ => print "Bug.\n"


