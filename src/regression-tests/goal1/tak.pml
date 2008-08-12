fun tak(x,y,z) =
   if y >= x
      then z
   else tak(tak(x - 1, y, z),
            tak(y - 1, z, x),
            tak(z - 1, x, y))
;

val _ = print(itos(tak(18,12,6))^"\n")
