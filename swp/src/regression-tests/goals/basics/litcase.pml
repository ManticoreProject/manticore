fun f(i) =(
    print " ";
    (case i
     of 1 => "a"
      | 2 => "b"
      | 3 => "c"
      | 4 => "e"
      | 5 => "f"
      | _ => "d"))

val _ = print (f 2)
val _ = print (f 8)
