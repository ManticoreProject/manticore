fun add (a, b) = a+b
fun mul (a, b) = a*b

fun mkPair n = (Float.fromInt(n) * 0.0000000001, Float.fromInt(n) * 0.0000001)

val arr = PArray.tabFromToStep (0, 9999999, 1, mkPair)

fun go n = let
  fun lp i =
    if i>=n then () else let
      val bump = 0.0 + Float.fromInt(i)
      val dotp = PArray.reduce add bump (PArray.map mul arr)
      val msg = Int.toString(i) ^ ": " ^ Float.toString(dotp)
      in
        (Print.printLn msg; lp (i+1))
      end
  in
    lp 0
  end

val _ = go 30

val _ = Print.printLn "done."
