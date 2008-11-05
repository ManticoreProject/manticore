fun f x y z = if y then x else z
val z1 = f 1 true 2
val z2 = f 1 false 2
val _ = Print.print (Int.toString z1^" "^Int.toString z2^"\n")
