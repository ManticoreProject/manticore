(* I want to see if simple image processing makes it through the FT *)
(* (ramping up to ray tracing) *)

val sz = 64
val img = Image.new (sz, sz)
(* val field = [| [| (i,j) | j in [| 0 to sz-1 |] |] | i in [| 0 to sz-1 |] |] *)
val field = PArray.tab (sz, fn (i:int) => PArray.tab (sz, (fn (j:int) => (i, j))))

val _ = Print.printLn (PArray.tos_intPair (field!0))

fun stuff (i, j) = let
  val itos = Int.toString
  val _ = Print.printLn ("setting " ^ itos i ^ "," ^ itos j ^ " to white")
  in
    Image.update3d (img, i, j, 1.0, 1.0, 1.0)
  end

val _ = PArray.app stuff (field!0)
val _ = PArray.app stuff (field!1)
val _ = PArray.app stuff (field!2)
val _ = PArray.app stuff (field!3)
val _ = PArray.app stuff (field!4)
val _ = PArray.app stuff (field!5)
val _ = PArray.app stuff (field!6)

(* val _ = PArray.app (PArray.app (fn (i, j) => Image.update3d (img, i, j, 0.0, 0.0, 0.0))) field *)
val _ = Image.output("out-img000.ppm", img)

val _ = Image.free img

val _ = Print.printLn "done."
