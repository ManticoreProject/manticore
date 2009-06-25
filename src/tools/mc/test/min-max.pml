fun minMax (a:int, (min, max)) =
      if (a < min) then (a, max)
      else if (a > max) then (min, max)
      else (min, max)

val (min, max) = List.foldl minMax (100, 0) [1, 2, 3, 4]

val _ = print("min = " ^ Int.toString min ^ "\n")
val _ = print("max = " ^ Int.toString max ^ "\n")

