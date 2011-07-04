fun add (x:double, y:double) = x+y

fun sum a = PArray.reduce add 0.0 a

fun dotp (sv, v) = sum [| x * (v!i) | (i, x) in sv |]

fun smvm (sm, v) = [| dotp (sv, v) | sv in sm |]

fun computeResult () = let
  val sv : (int * double) parray = [| (i*10, 1.0) | i in [| 1 to 100 |] |]
  val v : double parray = [| 1.0 | _ in [| 1 to 1111 |] |]
  in
    smvm ([|sv | _ in [| 1 to 100 |] |], v)
  end

fun workaround th = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), th)

val res = workaround computeResult

val _ = Print.printLn (Double.toString (res!0))
val _ = Print.printLn (Double.toString (res!1))

val _ = Print.printLn "done"

