val itos = Int.toString

fun max (m, n) = if (m>n) then m else n

fun predict (from, to_, step) = 
  if step=0 then raise Fail "0 step"
  else 1 + max (0, (to_-from) div step)

fun go args = (case args
  of (from, to_, step) => let
    val r = [| from to to_ by step |]
    val actualLen = PArray.length r
    val predictedLen = predict args
    in
      Print.printLn ("actual: " ^ itos actualLen ^ ", " ^
		     "computed: " ^ itos predictedLen ^ "..." ^  
		     (if (actualLen=predictedLen) 
                      then "success" else "failure"))
    end
  (* end case *))

val _ = go (1, 4, 1)
val _ = go (1, 4, 2)
val _ = go (1, 4, 3)
val _ = go (1, 4, 4)
val _ = go (1, 4, ~1)
val _ = go (1, 4, ~2)
val _ = go (1, 4, ~3)
val _ = go (4, 1, 1)
val _ = go (4, 1, 2)
val _ = go (4, 1, 3)
val _ = go (4, 1, 4)
val _ = go (4, 1, ~1)
val _ = go (4, 1, ~2)
val _ = go (4, 1, ~3)
val _ = go (4, 1, ~4)
val _ = go (~176312, 3274823, 172)
val _ = go (~176312, 3274823, ~172)
val _ = go (28376, ~2187363, 3874)
val _ = go (28376, ~2187363, ~3874)

val _ = Print.printLn "done"
